# Загрузка необходимых библиотек
library(dplyr)
library(arrow)
library(readr)
library(ggplot2)
library(tidyverse)
library(broom)292
library(caret)
install.packages("corrplot")
library(corrplot)

# Загрузка данных за 2020-2023 годы
weather_data <- read_parquet("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/3/Laboratory 3/AnalysisLab3/weather_data_2020_2023.parquet")
cities_data <- read_csv("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/3/Laboratory 3/AnalysisLab3/cities.csv")
countries_data <- read_csv("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/3/Laboratory 3/AnalysisLab3/countries.csv")

# Проверяем названия столбцов в weather_data и cities_data
colnames(weather_data)
colnames(cities_data)

# Если названия столбцов не совпадают, их нужно переименовать
# Предположим, что столбец для объединения в weather_data называется "station_id.x"
# Также предположим, что в cities_data он называется "station_id"

# Переименовываем столбцы для совпадения
weather_data <- weather_data %>%
  rename(station_id = station_id.x)

# Теперь объединим данные повторно
weather_cities_joined <- weather_data %>%
  inner_join(cities_data, by = "station_id")

# Сначала проверим названия столбцов в weather_cities_joined и countries_data
colnames(weather_cities_joined)
colnames(countries_data)

# Если названия столбцов не совпадают, переименуем их для успешного объединения
# Например, если в weather_cities_joined столбец с названием страны называется "country.x"
weather_cities_joined <- weather_cities_joined %>%
  rename(country = country.x)

# Теперь попробуем снова выполнить объединение
full_data <- weather_cities_joined %>%
  inner_join(countries_data, by = "country")

# Удаляем лишние столбцы country.y, state.x, state.y, и т.д., которые могут вызвать путаницу
weather_cities_joined <- weather_cities_joined %>%
  select(-country.y, -state.x, -state.y, -iso2.x, -iso3.x, -latitude.x, -longitude.x, -city_name.x, -city_name.y, -iso2.y, -iso3.y, -latitude.y, -longitude.y)

# Теперь попробуем снова выполнить объединение
full_data <- weather_cities_joined %>%
  inner_join(countries_data, by = "country")

# Выводим имена всех столбцов в наборе данных full_data
print(colnames(full_data))

# Убедимся, что столбцы, необходимые для анализа, присутствуют
colnames(full_data)

# Laboratory 3
# Упражнение 1: Анализ Температур по Континентам

# Убираем суффиксы .x и .y у столбцов, которые требуются для анализа
full_data <- full_data %>%
  rename(continent = continent.x, 
         native_name = native_name.x, 
         population = population.x, 
         area = area.x, 
         capital = capital.x,
         capital_lat = capital_lat.x,
         capital_lng = capital_lng.x,
         region = region.x)

# Визуализация данных по температурам

# Группировка по континентам и расчет средней и медианной температуры
temp_by_continent <- full_data %>%
  group_by(continent) %>%
  summarize(mean_temp = mean(avg_temp_c, na.rm = TRUE),
            median_temp = median(avg_temp_c, na.rm = TRUE))

# Вывод результатов
print(temp_by_continent)

# Построение boxplot для распределения температур по континентам
ggplot(full_data, aes(x = continent, y = avg_temp_c)) +
  geom_boxplot() +
  labs(title = "Распределение температуры по континентам",
       x = "Континент",
       y = "Средняя температура (°C)") +
  theme_minimal()

# Упражнение 2: Вычисление мер разброса

# Группировка данных по континентам
spread_by_continent <- full_data %>%
  group_by(continent) %>%
  summarize(sd_temp = sd(avg_temp_c, na.rm = TRUE),
            iqr_temp = IQR(avg_temp_c, na.rm = TRUE),
            n_countries = n_distinct(country))

# Вывод результатов
print(spread_by_continent)

# Создание наложенных графиков плотности
ggplot(full_data, aes(x = avg_temp_c, fill = continent, color = continent)) + 
  geom_density(alpha = 0.3) +
  labs(title = "График плотности температур по континентам",
       x = "Средняя температура (°C)",
       y = "Плотность") +
  theme_minimal()


# Упражнение 3: Америка - Меры Центра и Разброса
# Фильтрация данных для Америки
americas_data <- full_data %>%
  filter(continent %in% c("North America", "South America"))

# Расчет мер центра и разброса для средней температуры
summary_measures <- americas_data %>%
  summarize(mean_temp = mean(avg_temp_c, na.rm = TRUE),
            sd_temp = sd(avg_temp_c, na.rm = TRUE),
            iqr_temp = IQR(avg_temp_c, na.rm = TRUE),
            n_countries = n_distinct(country))

# Вывод результатов
print(summary_measures)

# Упражнение 4: График плотности для переменной населения
# Проверка наличия переменной населения и создание графика плотности
if ("population" %in% colnames(full_data)) {
  # Создание графика плотности для переменной населения
  ggplot(full_data, aes(x = population)) + 
    geom_density() +
    labs(title = "График плотности населения",
         x = "Население",
         y = "Плотность")
  
  # Преобразование переменной населения и добавление в набор данных
  full_data <- full_data %>% 
    mutate(log_population = log(population + 1)) # Используем +1 для избежания log(0)
  
  # Создание графика плотности для преобразованной переменной населения
  ggplot(full_data, aes(x = log_population)) + 
    geom_density() +
    labs(title = "График плотности логарифмированного населения",
         x = "Логарифм(Население)",
         y = "Плотность")
}


# Упражнение 5: Анализ и Визуализация Данных по Азии Без Выбросов
# Фильтрация данных для Азии
asia_data <- full_data %>%
  filter(continent == "Asia")

# Исключение стран с очень низкой средней температурой
non_outlier_data <- asia_data %>%
  filter(avg_temp_c >= -20)

# Выводим количество строк после исключения выбросов
print(nrow(non_outlier_data))

# Построение боксплота для азиатских стран без экстремальных температур
ggplot(non_outlier_data, aes(y = avg_temp_c)) +
  geom_boxplot() +
  labs(title = "Боксплот средней температуры в азиатских странах без экстремального холода",
       y = "Средняя температура (°C)")

# Laborator 4
# Упражнение 1

# Проверка наличия пропущенных значений
summary(full_data)
colSums(is.na(full_data))

# Исключение строк с пропущенными значениями
clean_data <- full_data %>%
  filter(!is.na(avg_temp_c) & !is.na(precipitation_mm))

# Построение диаграммы рассеяния на очищенных данных
ggplot(clean_data, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point() +
  labs(title = "Диаграмма рассеяния средней температуры и осадков (без пропущенных значений)",
       x = "Средняя температура (°C)",
       y = "Осадки (мм)")

# Сохранение графика
ggsave("scatterplot_cleaned.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Упражнение 2: Визуализация взаимосвязи с помощью боксплотов
# Дискретизация переменной средней температуры и создание групп
full_data$temperature_group <- cut(full_data$avg_temp_c, breaks = 4)

# Создание боксплотов для визуализации взаимосвязи между группами температур и осадками
ggplot(full_data, aes(x = temperature_group, y = precipitation_mm)) +
  geom_boxplot() +
  labs(title = "Боксплот осадков по группам температур",
       x = "Группа температур",
       y = "Осадки (мм)")

# Сохранение графика в файл
ggsave("scatterplot_temperature_groups.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Упражнение 3: Анализ взаимосвязи между погодными условиями
# Создание диаграммы рассеяния для средней температуры и количества осадков с различением по сезонам
ggplot(full_data, aes(x = avg_temp_c, y = precipitation_mm, color = season)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Диаграмма рассеяния осадков и средней температуры по сезонам",
       x = "Средняя температура (°C)",
       y = "Осадки (мм)") +
  theme_minimal()

# Сохранение диаграммы рассеяния в файл
ggsave("scatterplot_seasons.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Фильтрация "игроков" с осадками больше или равно 200 мм
players_with_enough_precipitation <- full_data %>%
  filter(precipitation_mm >= 200)

# Создание диаграммы рассеяния для отфильтрованной подгруппы
ggplot(players_with_enough_precipitation, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point() +
  labs(title = "Диаграмма рассеяния средней температуры и осадков (более 200 мм осадков)",
       x = "Средняя температура (°C)",
       y = "Осадки (мм)")

# Выводим график на экран
print(last_plot())

# Laboratory -- Linear simple models

# Упражнение 1: Визуализация взаимосвязи между средней температурой и осадками
# Создание диаграммы рассеяния с добавлением линии линейной регрессии, исключая нечисловые значения
ggplot(full_data %>% filter(!is.na(avg_temp_c) & !is.na(precipitation_mm)), aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point(color = 'blue', size = 2, alpha = 0.6) + # Синие точки с небольшой прозрачностью
  geom_smooth(method = "lm", color = 'red', se = FALSE) + # Красная линия линейной регрессии без доверительного интервала
  labs(title = "Взаимосвязь средней температуры и осадков",
       subtitle = "Линейная регрессия",
       x = "Средняя температура (°C)",
       y = "Осадки (мм)") +
  theme_minimal() + # Минималистичная тема
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Жирный шрифт и центровка заголовка
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12), # Курсивный шрифт для подзаголовка
    axis.title.x = element_text(face = "bold", size = 12), # Жирный шрифт для подписи оси X
    axis.title.y = element_text(face = "bold", size = 12)  # Жирный шрифт для подписи оси Y
  )

# Сохраняем график в файл
ggsave("D:/UCA/Documents/Daniel/Third Year/First Semester/AD/R/LAB/3/Laboratory 3/AnalysisLab3/Lab -- Linear simple models/plot_simple_linear_regression.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Упражнение 2: Создание линейной модели для средней температуры и осадков
# Создаем модель для переменных avg_temp_c (средняя температура) и precipitation_mm (осадки)
weather_model <- lm(precipitation_mm ~ avg_temp_c, data = full_data)

# Визуализируем линейную модель с дополнительными настройками дизайна
ggplot(full_data, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point(color = 'blue', size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = 'red', se = FALSE) +
  labs(
    title = "Взаимосвязь средней температуры и осадков",
    subtitle = "Линейная регрессия",
    x = "Средняя температура (°C)",
    y = "Осадки (мм)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Сохраняем график в файл
ggsave("linear_regression_plot.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Упражнение 3: Анализ и Визуализация Линейной Регрессии

# Создание линейной модели для осадков как функции от средней температуры
weather_model <- lm(precipitation_mm ~ avg_temp_c, data = full_data)

# Визуализация линейной модели с улучшенным дизайном
ggplot(full_data, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point(color = 'blue', size = 1, alpha = 0.6) +
  geom_smooth(method = "lm", color = 'red', se = FALSE) +
  labs(title = "Взаимосвязь средней температуры и осадков",
       subtitle = "Линейная регрессия",
       x = "Средняя температура (°C)",
       y = "Осадки (мм)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

# Сохранение графика в файл
ggsave("linear_regression_plot.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# Создание аугментированного датафрейма с информацией о модели
weather_model_tidy <- augment(weather_model)

# Просмотр структуры аугментированного датафрейма
glimpse(weather_model_tidy)

# Здесь мы создаем линейную модель регрессии
weather_model <- lm(precipitation_mm ~ avg_temp_c, data = full_data)

# Вычисление и вывод основных метрик для оценки качества модели
# MSE - Среднеквадратическая ошибка
# MAE - Средняя абсолютная ошибка
# R² - Коэффициент детерминации
model_metrics <- weather_model %>%
  augment() %>%
  summarise(
    MSE = mean(.resid^2),  # Вычисление MSE
    MAE = mean(abs(.resid)),  # Вычисление MAE
    R2 = summary(weather_model)$r.squared  # Вычисление R²
  )

# Вывод результатов
print(model_metrics)

# Установка и загрузка пакета ggrepel для аннотаций
if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

# Создание диаграммы рассеяния с линией линейной регрессии
ggplot(full_data, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point(color = 'blue', size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = 'red', se = TRUE, linewidth = 1.5) +
  labs(
    title = "Анализ Взаимосвязи Между Средней Температурой и Осадками",
    subtitle = "Линейная регрессия и диаграмма рассеяния",
    x = "Средняя Температура (°C)",
    y = "Осадки (мм)",
    caption = "Источник данных: weather_data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  ) +
  geom_text_repel(
    aes(label = ifelse(avg_temp_c > 30 & precipitation_mm > 100, as.character(country), '')), 
    size = 4, 
    max.overlaps = 10
  )

# Сохраняем график в файл
ggsave("enhanced_linear_regression_plot.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

# Подготовка данных
full_data_clean <- full_data %>% 
  filter(!is.na(avg_temp_c) & !is.na(precipitation_mm)) %>% 
  mutate(label = ifelse(avg_temp_c > 30 & precipitation_mm > 100, as.character(country), NA))

# Создание улучшенной диаграммы рассеяния
ggplot(full_data_clean, aes(x = avg_temp_c, y = precipitation_mm)) +
  geom_point(color = 'blue', size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = 'red', se = TRUE, size = 1.5) +
  geom_text_repel(
    aes(label = label),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    max.overlaps = Inf
  ) +
  labs(
    title = "Анализ Взаимосвязи Между Средней Температурой и Осадками",
    subtitle = "Линейная регрессия и диаграмма рассеяния",
    x = "Средняя Температура (°C)",
    y = "Осадки (мм)",
    caption = "Источник данных: weather_data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, size = 10)
  )


# Сохранение улучшенного графика в файл
ggsave("enhanced_linear_regression_plot.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

# Логистическая регрессия
# Замените 'target' на имя вашего целевого столбца в датасете 'full_data'.

# Проверка на наличие пропущенных значений в датасете
na_check <- colSums(is.na(full_data))
print(na_check)

# Проверка, что целевая переменная является бинарной
unique_target_values <- unique(full_data$target)
print(unique_target_values)

# Если целевая переменная 'target' не является фактором с двумя уровнями, преобразуйте ее
if (!is.factor(full_data$target) || length(unique_target_values) != 2) {
  full_data$target <- as.factor(full_data$target)
  unique_target_values <- unique(full_data$target)
  print(unique_target_values) # Проверка после преобразования
}

# Удалить строки с пропущенными значениями, если они есть
if (any(na_check > 0)) {
  full_data <- na.omit(full_data)
}

# Проверка, успешно ли удалены пропущенные значения
na_check <- colSums(is.na(full_data))
print(na_check)

# Замените 'threshold' на пороговое значение для определения классов целевой переменной
threshold <- 25
full_data$target <- ifelse(full_data$avg_temp_c > threshold, 1, 0)

# Проверяем, что целевой столбец теперь существует и является бинарным
print(table(full_data$target))

# Преобразуем целевой столбец в фактор
full_data$target <- as.factor(full_data$target)

# Проверяем, что преобразование выполнено правильно
print(summary(full_data$target))

# Установка случайного зерна для воспроизводимости результатов
set.seed(123)

# Разделяем данные на обучающий и тестовый наборы
# p = 0.8 означает, что 80% данных будут использоваться для обучения, а 20% - для тестирования
index <- createDataPartition(full_data$target, p = 0.8, list = FALSE)
train_data <- full_data[index, ]
test_data <- full_data[-index, ]

# Обучаем модель логистической регрессии на обучающем наборе данных
# glm - это функция обобщенной линейной модели, используемая для моделей логистической регрессии
# family = binomial указывает, что это бинарная классификация
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# Делаем предсказание вероятностей принадлежности к классу '1' на тестовом наборе данных
predicted_probabilities <- predict(logistic_model, test_data, type = "response")

# Конвертируем вероятности в классы (1 или 0) на основе порога 0.5
# Если вероятность > 0.5, предсказываем класс 1, в противном случае - класс 0
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Вычисляем точность модели как долю правильно предсказанных наблюдений
accuracy <- mean(predicted_classes == test_data$target)

# Создаем матрицу ошибок для оценки качества модели
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), test_data$target)

# Выводим общую точность модели
cat("Точность (Accuracy):", accuracy, "\n")

# Выводим матрицу ошибок
cat("Матрица ошибок (Confusion Matrix):\n")
print(confusion_matrix)

# Выводим дополнительные метрики из матрицы ошибок
# Точность (Precision) - доля истинно положительных результатов среди всех положительных предсказаний модели
# Полнота (Recall, Sensitivity) - доля истинно положительных результатов среди всех реальных положительных случаев
# F1 Score - среднее гармоническое между точностью и полнотой
cat("Точность (Precision):", confusion_matrix$byClass['Pos Pred Value'], "\n")
cat("Полнота (Recall, Sensitivity):", confusion_matrix$byClass['Sensitivity'], "\n")
cat("F1-мера (F1 Score):", confusion_matrix$byClass['F1'], "\n")


# Удаляем столбцы с одним уникальным значением или только с NA
train_data <- train_data[, sapply(train_data, function(x) length(unique(x[!is.na(x)])) > 1)]

# Преобразование всех категориальных переменных в факторы
categorical_columns <- sapply(train_data, is.character)
train_data[, categorical_columns] <- lapply(train_data[, categorical_columns], as.factor)

# Повторное обучение модели логистической регрессии
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# Предсказание вероятностей на тестовом наборе данных
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")

# Конвертация вероятностей в классы
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Вычисление точности
accuracy <- mean(predicted_classes == test_data$target)

# Вывод результатов
cat("Точность:", accuracy, "\n")

# Считаем количество наблюдений в каждом классе
class_counts <- table(full_data$target)
print(class_counts)

# Создаем корреляционную матрицу для предикторов
correlation_matrix <- cor(train_data[, sapply(train_data, is.numeric)])
print(correlation_matrix)

# Визуализация корреляционной матрицы (опционально)
corrplot(correlation_matrix, method = "circle")

# Убедитесь, что ваши данные не содержат NA и выбросов
full_data <- na.omit(full_data)
# Для простоты примера мы не рассматриваем выбросы, но на практике следует их обработать

# Разбиение данных на обучающую и тестовую выборки
set.seed(123)
index <- createDataPartition(full_data$target, p = 0.8, list = FALSE)
train_data <- full_data[index, ]
test_data <- full_data[-index, ]

# Обучение модели логистической регрессии
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# Предсказание классов на тестовой выборке
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Вычисление точности и других метрик
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$target)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Вывод результатов
print(confusion_matrix)
cat("Точность:", accuracy, "\n")

# Проверка переменных типа фактор на наличие достаточного количества уровней
factor_columns <- sapply(train_data, is.factor)
factor_levels_count <- sapply(train_data[, factor_columns], function(x) length(levels(x)))
print(factor_levels_count)

# Удаление переменных-факторов с одним уровнем
single_level_factors <- names(factor_levels_count[factor_levels_count < 2])
train_data <- train_data[, !names(train_data) %in% single_level_factors]
test_data <- test_data[, !names(test_data) %in% single_level_factors]

# Проверка на мультиколлинеарность
numeric_columns <- sapply(train_data, is.numeric)
cor_matrix <- cor(train_data[, numeric_columns])
print(cor_matrix)

# Если есть высокая корреляция (например, > 0.8), можно удалить одну из коррелированных переменных
# Это примерный код, и вам нужно будет решить, какие переменные удалить на основе корреляционной матрицы
# high_correlation_pairs <- findCorrelation(cor_matrix, cutoff = 0.8)
# train_data <- train_data[, -high_correlation_pairs]
# test_data <- test_data[, -high_correlation_pairs]

# Повторное обучение модели после удаления переменных
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# Предсказание и вычисление точности модели
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$target)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Вывод результатов
print(confusion_matrix)
cat("Точность:", accuracy, "\n")


# Убедитесь, что целевая переменная преобразована в фактор
full_data$target <- as.factor(full_data$target)

# Стратифицируйте данные по целевой переменной при разделении
set.seed(123)
index <- createDataPartition(full_data$target, p = 0.8, list = FALSE, times = 1)
train_data <- full_data[index, ]
test_data <- full_data[-index, ]

# Проверка, что в обучающем наборе данных есть оба уровня целевой переменной
print(table(train_data$target))

# Удаление факторов с одним уровнем
factor_columns <- sapply(train_data, is.factor)
factor_levels_count <- sapply(train_data[, factor_columns], function(x) length(levels(x)))
single_level_factors <- names(factor_levels_count[factor_levels_count < 2])
train_data <- train_data[, !names(train_data) %in% single_level_factors]
test_data <- test_data[, !names(test_data) %in% single_level_factors]

# Повторное обучение модели логистической регрессии
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# Повторное предсказание и вычисление точности модели
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$target)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Вывод результатов
print(confusion_matrix)
cat("Точность:", accuracy, "\n")

# Подготовка данных для оценки модели
actual <- test_data$target
predicted <- factor(predicted_classes, levels = levels(test_data$target))

# Создание матрицы ошибок с помощью пакета caret
conf_matrix <- confusionMatrix(predicted, actual)

# Вывод матрицы ошибок и сводки метрик
print(conf_matrix)

# Извлечение и вывод отдельных метрик
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
F1 <- 2 * (precision * recall) / (precision + recall)

cat("Точность (Accuracy):", accuracy, "\n")
cat("Точность (Precision):", precision, "\n")
cat("Полнота (Recall):", recall, "\n")
cat("F1-мера (F1 Score):", F1, "\n")