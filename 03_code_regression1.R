#' ---
#' title: "Регрессионный анализ, часть 1"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"
#' ## Пример: потеря влаги личинками мучных хрущаков
#' - 9 экспериментов, продолжительность 6 дней
#' - разная относительная влажность воздуха, %
#' - измерена потеря влаги, мг
#' Nelson, 1964; данные из Sokal, Rohlf, 1997, табл. 14.1 по Logan, 2010. глава 8, пример 8c; Данные в файлах nelson.xlsx и nelson.csv

#' ## Скачиваем данные с сайта

#' Не забудьте войти в вашу директорию для матметодов при помощи `setwd()`

library(downloader)

# в рабочем каталоге создаем суб-директорию для данных
if(!dir.exists("data")) dir.create("data")

# скачиваем файл в xlsx, либо в текстовом формате
if (!file.exists("data/nelson.xlsx")) {
  download(
    url = "https://varmara.github.io/mathmethr/data/nelson.xlsx",
    destfile = "data/nelson.xlsx")
}

if (!file.exists("data/nelson.csv")) {
  download(
    url = "https://varmara.github.io/mathmethr/data/nelson.xls",
    destfile = "data/nelson.csv")
}

#' ## Читаем данные из файла одним из способов
#'
#' ### Чтение из xlsx
library(readxl)
nelson <- read_excel(path = "data/nelson.xlsx", sheet = 1)

#' ### Чтение из csv
nelson <- read.table("data/nelson.csv", header = TRUE, sep = "\t")

#' ## Все ли правильно открылось?
str(nelson) # Структура данных
head(nelson)     # Первые несколько строк файла

#' ## Знакомимся с данными
#' Есть ли пропущенные значения?
sapply(nelson, function(x)sum(is.na(x)))

#' Каков объем выборки?
nrow(nelson)

#' ## Как зависит потеря веса от влажности?
library(ggplot2)
theme_set(theme_bw())
gg_nelson <- ggplot(data=nelson, aes(x = humidity, y = weightloss)) +
  geom_point() +
  labs(x = "Относительная влажность, %",
       y = "Потеря веса, мг")
gg_nelson


#' ## Можно расчитать значение коэффициента корреляции между потерей веса и влажностью
p_cor <- cor.test(nelson$humidity, nelson$weightloss,
         alternative = "two.sided", method = "pearson")
p_cor



#' # Линейная регрессия
gg_nelson + geom_smooth(method = "lm")


#' ## Подбираем параметры линейной модели
nelson_lm <- lm(weightloss ~ humidity, nelson)
summary(nelson_lm)

#' ## Находим доверительные интервалы коэффициентов
# оценки коэффициентов отдельно
coef(nelson_lm)
# доверительные интервалы коэффициентов
confint(nelson_lm)


#' ## Предсказываем Y при заданном X
newdata <- data.frame(humidity = c(50, 100)) # значения, для которых предсказываем
(pr1 <- predict(nelson_lm, newdata, interval = "confidence", se = TRUE))


#' ## Строим доверительную зону регрессии
# На выбор

gg_nelson + geom_smooth(method = "lm") +
  labs (title = "95% доверительная зона регрессии")

gg_nelson + geom_smooth(method = "lm", level = 0.99) +
  labs (title = "99% доверительная зона регрессии")



#' В каких пределах находится потеря веса у 95\% жуков при заданной влажности?
newdata <- data.frame(humidity = c(50, 100)) # новые данные для предсказания значений
(pr2 <- predict(nelson_lm, newdata, interval = "prediction", se = TRUE))


#' ## Данные для доверительной области значений
(pr_all <- predict(nelson_lm, interval = "prediction"))
nelson_with_pred <- data.frame(nelson, pr_all)


#' ## Строим доверительную область значений и доверительный интервал одновременно
gg_nelson +
  geom_smooth(method = "lm",
              aes(fill = "Доверительный \nинтервал"),
              alpha = 0.4) +
  geom_ribbon(data = nelson_with_pred,
              aes(y = fit, ymin = lwr, ymax = upr,
                  fill = "Доверительная \nобласть значений"),
              alpha = 0.2) +
  scale_fill_manual('Интервалы', values = c('green', 'blue'))



#' # Тестирование значимости модели и ее коэффициентов
#' ## Тестируем значимость коэффициентов с помощью t-критерия
summary(nelson_lm)
#' ## Проверяем значимость модели при помощи F-критерия
nelson_aov <- aov(nelson_lm)
summary(nelson_aov)


#' ## Коэффициент детерминации можно найти в сводке модели
summary(nelson_lm)

