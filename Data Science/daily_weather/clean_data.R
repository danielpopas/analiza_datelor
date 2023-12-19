# Применение функции очистки к данным, отфильтрованным за последние 10 лет
daily_weather_data_cleaned <- clean_data(daily_weather_data_last_ten_years)

# Проверка диапазона дат в очищенном датасете
date_range_summary <- check_date_range(daily_weather_data_cleaned)
print(date_range_summary)

# Проверка уникальных значений года в очищенном датасете
unique_years <- daily_weather_data_cleaned %>% 
  mutate(year = year(ymd_hms(date))) %>% 
  distinct(year) %>% 
  arrange(year)
print(unique_years)

# Подсчет количества строк в очищенном датасете за последние 10 лет
number_of_rows <- nrow(daily_weather_data_cleaned)
print(number_of_rows)




# Подключение библиотеки arrow для сохранения файла в формате Parquet
library(arrow)

# Сохранение датасета в формате Parquet
write_parquet(daily_weather_data_cleaned, "D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/daily_weather/daily_weather_data_last_ten_years.parquet")



# Загрузка данных о городах
cities_data <- read_csv("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/daily_weather/cities.csv")

# Анализ связи с погодными данными
# Проверяем, существует ли столбец, который может быть связан с погодными данными
print(colnames(cities_data))

# Если в cities_data есть столбец, связанный с погодными данными, например, station_id:
if("station_id" %in% colnames(cities_data)) {
  # Фильтрация не требуется, если столбец station_id содержит уникальные идентификаторы, которые не меняются со временем
  # Если требуется фильтрация, используйте список идентификаторов из погодного датасета
}

# Аналогичный анализ проводим для данных о странах
countries_data <- read_csv("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/daily_weather/countries.csv")
print(colnames(countries_data))

# Если страны связаны с погодными данными через города или станции, нужно будет провести фильтрацию аналогично cities_data


# Загрузка списка уникальных station_id из погодных данных за последние 10 лет
unique_station_ids <- unique(daily_weather_data_last_ten_years$station_id)

# Фильтрация данных о городах, чтобы оставить только те, которые присутствуют в погодных данных
filtered_cities_data <- cities_data %>%
  filter(station_id %in% unique_station_ids)

# Сохранение отфильтрованных данных о городах
write_csv(filtered_cities_data, "D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/daily_weather/filtered_cities.csv")



# Подключение библиотеки dplyr
library(dplyr)

# Функция для очистки данных
clean_data <- function(data) {
  # Ваш код для удаления ненужных столбцов и обработки пропущенных значений
  
  return(data)
}

# Применение функции очистки к отфильтрованным данным
daily_weather_data_cleaned <- clean_data(daily_weather_data_filtered)


# Подключение необходимых библиотек
library(dplyr)
library(ggplot2)

# Подготовка данных
# Пример: Создание сводной таблицы по сезонам с расчетом средних значений
weather_summary <- daily_weather_data_cleaned %>%
  group_by(season) %>%
  summarise(avg_temp = mean(avg_temp_c, na.rm = TRUE),
            avg_precipitation = mean(precipitation_mm, na.rm = TRUE),
            avg_wind_speed = mean(avg_wind_speed_kmh, na.rm = TRUE))

# Визуализация средних значений температуры по сезонам
ggplot(weather_summary, aes(x = season, y = avg_temp, fill = season)) +
  geom_bar(stat = "identity") +
  labs(title = "Средняя Температура по Сезонам",
       x = "Сезон",
       y = "Средняя Температура") +
  theme_minimal()

# Подключение библиотеки dplyr для обработки данных
library(dplyr)
library(lubridate)

# Проверка диапазона дат в датасете
check_date_range <- function(data) {
  data %>% 
    mutate(year = year(ymd_hms(date))) %>%
    summarise(min_year = min(year, na.rm = TRUE), 
              max_year = max(year, na.rm = TRUE))
}

# Применение функции к очищенным данным
date_range_summary <- check_date_range(daily_weather_data_cleaned)

# Вывод результатов
print(date_range_summary)

# Проверка уникальных значений года
unique_years <- daily_weather_data_cleaned %>% 
  mutate(year = year(ymd_hms(date))) %>% 
  distinct(year) %>% 
  arrange(year)

# Вывод уникальных значений года
print(unique_years)




# Подсчет количества строк в очищенном датасете
number_of_rows <- nrow(daily_weather_data_filtered)
print(number_of_rows)

