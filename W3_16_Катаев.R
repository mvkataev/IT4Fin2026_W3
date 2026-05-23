# Мини-проект №3, Катаев Максим

# Подключаются необходимые библиотеки
library(httr)
library(jsonlite)
library(RSQLite)
library(dplyr)
library(stringdist)
library(logger)
library(readr)


#####  7. Настройка логирования (вынесено вперед, нужно для всего кода)  #####

log_appender(appender_console)

# Создадим файл лога
log_file <- "project_log.txt"
log_appender(appender_file(log_file))

# Настройка уровней: консоль (INFO+), файл (DEBUG+)
log_threshold(DEBUG) 

# Функция для логирования по уровням
log_custom <- function(level, msg) {
  if (level == "DEBUG") log_debug(msg)
  if (level == "INFO") log_info(msg)
  if (level == "WARNING") log_warn(msg)
  if (level == "ERROR") log_error(msg)
}


#####  0. Знакомство с данными  #####

cmc_api_key <- "69eb9cfc67364194a8a9d0cd0abd37b3" # мой ключ API
base_url <- "https://pro-api.coinmarketcap.com"

download_cmc_data <- function() {
  log_custom("INFO", "Начало загрузки данных с помощью API CoinMarketCap")
  
  headers <- add_headers(`X-CMC_PRO_API_KEY` = cmc_api_key)
  
  # 1. cmc_latest.json
  resp_latest <- GET(paste0(base_url, "/v1/cryptocurrency/listings/latest"), headers)
  if (status_code(resp_latest) == 200) {
    write_json(content(resp_latest, "text", encoding = "UTF-8"), "cmc_latest.json")
    log_custom("INFO", "Файл cmc_latest.json успешно скачан")
  } else {
    log_custom("ERROR", paste("Ошибка при загрузке listings: ", status_code(resp_latest)))
  }
  
  # 2. cmc_map.json
  resp_map <- GET(paste0(base_url, "/v1/cryptocurrency/map"), headers)
  if (status_code(resp_map) == 200) {
    write_json(content(resp_map, "text", encoding = "UTF-8"), "cmc_map.json")
    log_custom("INFO", "Файл cmc_map.json успешно скачан")
  } else {
    log_custom("ERROR", paste("Ошибка при загрузке map: ", status_code(resp_map)))
  }
  
  # 3. cmc_info.json (Пока этот файл не нужен, он загрузится после)
  log_custom("INFO", "Загрузка info будет произведена по мере необходимости для конкретных ID")
}

# Запуск загрузки с помощью API CoinMarketCap.
# Можно раскомментировать строчку ниже, но для воспроизводимости результатов
# не рекомендую это делать: файлы есть на GitHub по ссылке https://github.com/mvkataev/IT4Fin2026_W3/blob/main/README.md

# При повторном парсинге с сайта есть риск возникновения неточности в данных,
# так как они могут обновляться на сайте. На GitHub при этом все стабильно,
# с файлами на GitHub корректность работы кода гарантирована.
# download_cmc_data()


# Загрузка локальных файлов

# path.expand("~") автоматически подставляет путь к домашней папке.
# Ниже реализован вариант для файла в папке "Загрузки",
# при необходимости можно изменить название папки
db_path <- file.path(path.expand("~"), "Downloads", "w3_database.sqlite")
if (!file.exists(db_path)) stop("Файл базы данных не найден по пути: ", db_path)
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Загрузка данных из нескольких файлов
coins_db <- dbReadTable(con, "coins")
coins_prices <- read.csv("coins_latest.csv", sep = ";", stringsAsFactors = FALSE)
cmc_latest_raw <-  fromJSON("cmc_latest (1).json")
head(cmc_latest_raw)

# Объединим базу данных и цены для сопоставления
coins_full <- coins_db %>% 
  left_join(coins_prices, by = "id")

# Для отчета (Задание 8) создадим таблицу в памяти
matching_report <- coins_full %>% 
  select(id, symbol) %>% 
  mutate(cmc_id = NA_integer_, match_method = "unmatched", match_details = "")


#####  1. Точное сопоставление  #####

log_custom("INFO", "Запуск Задания 1: Точное сопоставление")

# Реализуем такой критерий близости цен: цены близки, если они 
# отличаются друг от друга не более, чем на 2%
PRICE_THRESHOLD <- 0.02
print(cmc_latest_raw)
for (i in 1:nrow(coins_full)) {
  coin <- coins_full[i, ]
  print(coin$symbol)
  
  # Поиск совпадений по тикеру
  matches <- cmc_latest_raw %>% filter(symbol == coin$symbol)
  
  if (nrow(matches) > 0) {
    for (j in 1:nrow(matches)) {
      cmc_coin <- matches[j, ]
      price_diff <- abs(coin$price_usd - as.numeric(cmc_coin$quote$USD$price)) / coin$price_usd
      
      log_custom("DEBUG", sprintf("Сравнение %s: coins=%.4f, CMC=%.4f, разница=%.2f%%", 
                                  coin$symbol, coin$price, cmc_coin$quote$USD$price, price_diff*100))
      
      if (!is.na(price_diff) && price_diff <= PRICE_THRESHOLD) {
        matching_report$cmc_id[i] <- cmc_coin$id
        matching_report$match_method[i] <- "exact"
        matching_report$match_details[i] <- sprintf("diff=%.2f%%", price_diff*100)
        
        # Обновление в базе данных (SQL запрос)
        dbExecute(con, "UPDATE coins SET cmc_id = ? WHERE id = ?", 
                  params = list(cmc_coin$id, coin$id))
        break
      }
    }
  }
}
log_custom("INFO", sprintf("Задание 1 завершено: %d монет сопоставлено", sum(matching_report$match_method == "exact")))


#####  2. Нечеткое сопоставление  #####

log_custom("INFO", "Запуск Задания 2: Нечеткое сопоставление")

unmatched_indices <- which(is.na(matching_report$cmc_id))
cmc_available <- cmc_latest_raw %>% filter(!(id %in% matching_report$cmc_id))

for (idx in unmatched_indices) {
  coin <- coins_full[idx, ]
  
  # Поиск ближайших по цене в пределах порога
  price_candidates <- cmc_available %>% 
    mutate(diff = abs(quote$USD$price - coin$price) / coin$price) %>% 
    filter(diff <= PRICE_THRESHOLD)
  
  if (nrow(price_candidates) > 0) {
    # Поиск минимального среди кандидатов расстояния Левенштейна по тикеру
    price_candidates$dist <- stringdist::stringdist(coin$symbol, price_candidates$symbol, method = "lv")
    best_match <- price_candidates %>% arrange(dist) %>% slice(1)
    
    # Реализуем вариант, в котором порог расстояния равен 1 символу
    if (best_match$dist <= 1) { 
      matching_report$cmc_id[idx] <- best_match$id
      matching_report$match_method[idx] <- "fuzzy"
      matching_report$match_details[idx] <- sprintf("dist=%d, price_diff=%.2f%%", 
                                                    best_match$dist, best_match$diff*100)
      
      dbExecute(con, "UPDATE coins SET cmc_id = ? WHERE id = ?", 
                params = list(best_match$id, coin$id))
      
      log_custom("WARNING", sprintf("Монета %s: найден кандидат %s, выбран по нечетким критериям", 
                                    coin$symbol, best_match$symbol))
    }
  }
}

#####  3. Наивное сопоставление и добавление новых монет  #####

log_custom("INFO", "Запуск Задания 3: Наивное сопоставление и добавление новых монет")

# С помощью API предварительно скачался файл, но его прямо здесь в коде нужно предобработать
json_content <- readLines("cmc_map (1).json", warn = FALSE)
raw_string <- json_content[1]

# Очистим от экранированных кавычек и некоторых других символов
clean_string <- gsub('\\\\"', '"', raw_string)
clean_json <- sub('^\\["', '', clean_string)
clean_json <- sub('"]$', '', clean_json)

if (substr(clean_json, 1, 1) != "{") clean_json <- paste0("{", clean_json)
if (substr(clean_json, nchar(clean_json), nchar(clean_json)) != "}") clean_json <- paste0(clean_json, "}")

parsed_json <- tryCatch(
  fromJSON(clean_json),
  error = function(e) {
    stop("Ошибка парсинга JSON: ", e$message, "\nПроверьте строку: ", substr(clean_json, 1, 200))
  }
)

# Эта подготовка нужна была для того, чтобы из всего файла выбрать только 
# нужную для нас часть data
if ("data" %in% names(parsed_json)) {
  cmc_map <- as.data.frame(parsed_json$data)
} else {
  cmc_map <- as.data.frame(parsed_json)
}

# Наивное сопоставление для оставшихся в базе данных
unmatched_indices <- which(is.na(matching_report$cmc_id))
for (idx in unmatched_indices) {
  coin <- coins_full[idx, ]
  
  # Первый подходящий по символу из тех, которые еще не привязаны
  match <- cmc_map %>% 
    filter(symbol == coin$symbol, !(id %in% matching_report$cmc_id)) %>% 
    slice(1)
  
  if (nrow(match) > 0) {
    matching_report$cmc_id[idx] <- match$id
    matching_report$match_method[idx] <- "naive"
    dbExecute(con, "UPDATE coins SET cmc_id = ? WHERE id = ?", 
              params = list(match$id, coin$id))
  }
}

# Добавление новых монет из cmc_map
active_cmc <- cmc_map %>% filter(is_active == 1 | is_active == TRUE | is_active == "active")
existing_cmc_ids <- dbGetQuery(con, "SELECT cmc_id FROM coins WHERE cmc_id IS NOT NULL")$cmc_id
new_coins <- active_cmc %>% filter(!(id %in% existing_cmc_ids))

for (i in 1:nrow(new_coins)) {
  coin_new <- new_coins[i, ]
  dbExecute(con, "INSERT INTO coins (name, symbol, cmc_id) VALUES (?, ?, ?)", 
            params = list(coin_new$name, coin_new$symbol, coin_new$id))
  
  # Добавление в отчет
  matching_report <- rbind(matching_report, 
                           data.frame(id = NA, symbol = coin_new$symbol, 
                                      cmc_id = coin_new$id, 
                                      match_method = "added_from_map", 
                                      match_details = ""))
}

#####  4-6. Загрузка name и explorers, обязательные socials и топ-5 #####

log_custom("INFO", "Запуск обработки расширенной информации (Задания 4-6)")

# Все cmc_id, которые теперь есть в базе
all_cmc_ids <- dbGetQuery(con, "SELECT cmc_id FROM coins WHERE cmc_id IS NOT NULL")$cmc_id

# API для cmc_info принимает ID через запятую. 
# Для плавного парсинга передадим несколько запросов по 100 штук
chunk_size <- 100
id_chunks <- split(all_cmc_ids, ceiling(seq_along(all_cmc_ids)/chunk_size))

# Функция для определения сервиса по URL
get_service_name <- function(url) {
  if (is.na(url) || url == "") return(NA)
  if (grepl("t.me", url)) return("telegram")
  if (grepl("discord.gg", url)) return("discord")
  if (grepl("medium.com", url)) return("medium")
  if (grepl("reddit.com", url)) return("reddit")
  if (grepl("facebook.com", url)) return("facebook")
  return("other")
}

# Сбор данных по всем монетам
all_info_data <- list()

for (chunk in id_chunks) {
  ids_str <- paste(chunk, collapse = ",")
  resp <- GET(paste0(base_url, "/v2/cryptocurrency/info?id=", ids_str), 
              add_headers(`X-CMC_PRO_API_KEY` = cmc_api_key))
  
  if (status_code(resp) == 200) {
    info_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    info_json <- info_json$data
    all_info_data <- c(all_info_data, info_json)
  } else {
    log_custom("ERROR", paste("Ошибка API info для чанка: ", status_code(resp)))
  }
}

# Обработка каждой монеты
for (info in all_info_data) {
  c_id <- info$id
  
  # Задание 4: name и explorers
  name <- info$name
  explorers <- toJSON(info$explorers, auto_unbox = TRUE)
  
  # Задание 5: обязательные socials
  mandatory_types <- c("website", "whitepaper", "github", "twitter")
  socials_list <- list()
  
  for (type in mandatory_types) {
    val <- info[[type]]
    if (!is.null(val) && !is.na(val)) {
      socials_list[[length(socials_list) + 1]] <- list(type = type, link = val)
    }
  }
  
  # Задание 6: Анализ популярности socials и топ-5
  
  # Сбор всех возможных ссылок из всех полей
  all_fields <- names(info)
  all_links <- c()
  
  for (f in all_fields) {
    val <- info[[f]]
    if (is.character(val)) {
      links_found <- val[grepl("http", val)]
      all_links <- c(all_links, links_found)
    }
  }
  
  # Для простоты реализации Задания 6 здесь считается популярность 
  # по всему массиву данных перед обновлением базы данных
}

# Реализация Задания 6 по всей выборке
all_social_links <- list()
for (info in all_info_data) {
  links <- unlist(info)
  links <- links[grepl("http", links)]
  services <- sapply(links, get_service_name)
  all_social_links[[as.character(info$id)]] <- services
}

# Рассчитаем популярность (исключая обязательные socials)
pop_table <- table(unlist(all_social_links))
pop_table <- pop_table[!(names(pop_table) %in% mandatory_types)]

# В топ-5 заметна роль Telegram
top_5_services <- names(sort(pop_table, decreasing = TRUE)[1:min(5, length(pop_table))])

# Финальное обновление базы данных
for (info in all_info_data) {
  c_id <- info$id
  
  # Итоговый JSON для socials
  final_socials <- list()
  
  # Обязательные socials
  for (type in mandatory_types) {
    val <- info[[type]]
    if (!is.null(val) && !is.na(val)) final_socials[[length(final_socials)+1]] <- list(type=type, link=val)
  }
  
  # Топ-5 популярных socials
  all_links <- unlist(info)
  all_links <- all_links[grepl("http", all_links)]
  for (link in all_links) {
    svc <- get_service_name(link)
    if (svc %in% top_5_services) {
      final_socials[[length(final_socials)+1]] <- list(type=svc, link=link)
    }
  }
  
  socials_json <- toJSON(final_socials, auto_unbox = TRUE)
  
  # Обновление в базе данных
  dbExecute(con, "UPDATE coins SET name = ?, explorers = ?, socials = ? WHERE cmc_id = ?", 
            params = list(info$name, toJSON(info$explorers, auto_unbox=TRUE), socials_json, c_id))
}


#####  8. Отчет о сопоставлении #####

write_csv(matching_report, "matching_report.csv")

# Статистика
stats <- matching_report %>% group_by(match_method) %>% summarise(count = n())
cat("\n Сводная статистика сопоставления \n")
print(stats)
cat(sprintf("Всего монет до: %d\n", nrow(coins_db)))
cat(sprintf("Всего монет после: %d\n", nrow(dbReadTable(con, "coins"))))

# Отключенние от базы данных
dbDisconnect(con)
log_custom("INFO", "Проект успешно завершен. Отчет сохранен в matching_report.csv")

