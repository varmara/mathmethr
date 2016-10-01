#' ---
#' title: "Регрессионный анализ, часть 2"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"

#' ## Скачиваем данные с сайта

library(downloader)

# в рабочем каталоге создаем суб-директорию для данных
if(!dir.exists("data")) dir.create("data")

# скачиваем файл в xlsx, либо в текстовом формате
if (!file.exists("data/loyn.xlsx")) {
  download(
    url = "https://varmara.github.io/mathmethr/data/loyn.xlsx",
    destfile = "data/loyn.xlsx")
}

if (!file.exists("data/loyn.csv")) {
  download(
    url = "https://varmara.github.io/mathmethr/data/loyn.xls",
    destfile = "data/loyn.csv")
}

#' ## Читаем данные из файла одним из способов
#' ### Чтение из xlsx
library(readxl)
bird <- read_excel(path = "data/loyn.xlsx", sheet = 1)
#' ### Чтение из csv
bird <- read.table("data/loyn.csv", header = TRUE, sep = "\t")

#' ## Все ли правильно открылось?
str(bird) # Структура данных
head(bird)     # Первые несколько строк файла

#' ## Знакомимся с данными
sapply(bird, function(x)sum(is.na(x)))

#' Каков объем выборки?
nrow(bird)


#' ## Задача
#' - Подберите модель множественной линейной регрессии, чтобы описать, как зависит обилие птиц от характеристик леса
#' - Проверьте значимость ее коэффициентов при помощи t-критерия

#' - `abund` - Обилие птиц
#' - `l10area` - Площадь леса, га
#' - `l10dist` - Расстояние до ближайшего леса, км (логарифм)
#' - `l10ldist` - Расстояние до ближайшего леса большего размера,
#' км (логарифм)
#' - `yr.isol` - Год изоляции лесного массива


#' ## Задача
#' Запишите уравнение множественной линейной регрессии
#' В качестве подсказки:
coef(bird_lm)
bird_lm$call





#' ## Для сравнения влияния разных факторов --- стандартизованные коэффициенты
scaled_bird_lm <- lm(abund ~ scale(l10area) + scale(l10dist) + scale(l10ldist) + scale(yr.isol), data = bird)
coef(scaled_bird_lm)

#' ## Задача
#'
#' Определите по значениям стандартизованных коэффициентов, какие факторы сильнее всего влияют на обилие птиц



#' ## Оценка качества подгонки модели
summary(bird_lm)$adj.r.squared



#' # Проверка условий применимости линейной регрессии

## 1. Проверим, есть ли в этих данных колинеарность предикторов
library(car)
vif(bird_lm) # variance inflation factors
sqrt(vif(bird_lm)) > 2 # есть ли проблемы?
1/vif(bird_lm) # tolerance


#' ## Для анализа остатков выделим нужные данные в новый датафрейм
library(ggplot2) # там есть функция fortify()
bird_diag <- fortify(bird_lm)

#' 2. График остатков от предсказанных значений

#' ## Задача
#' Постройте график зависимости стандартизованных остатков от предсказанных значений









# Создаем логический вектор, где TRUE,
# если стандартизованный остаток больше 2
f_outlier <- abs(bird_diag$.stdresid) > 2
# Создаем будущие ярлыки
labs <- ifelse(test = f_outlier,
               yes = row(bird_diag), # Если test == TRUE
               no = "") # Если test == FALSE

gg_resid <- ggplot(data = bird_diag,
                   aes(x = .fitted, y = .stdresid)) +
  geom_point(aes(size = .cooksd)) + # расстояние Кука
  geom_hline(yintercept = 0) +   # горизонтальная линия y = 0
  geom_text(aes(label = labs), hjust = 2, colour = "blue",
            size = 2) # номера наблюдений с остатками больше 2SD
gg_resid



#' ## 3. Квантильный график стандартизованных остатков

mean_val <- mean(bird_diag$.stdresid)
sd_val <- sd(bird_diag$.stdresid)
ggplot(bird_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") +
geom_abline(intercept = mean_val, slope = sd_val) + # точки должны быть здесь
  labs(x = "Квантили стандартного нормального распределения", y = "Квантили набора данных")



