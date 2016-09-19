# ---
# title: "Регрессионный анализ, часть 1"
# subtitle: Математические методы в зоологии - на R
# date: "осень 2015"
# author: "Марина Варфоломеева"
# institution: СПбГУ
# ---

install.packages("gridExtra")

### Вы сможете
# - посчитать и протестировать различные коэффициенты корреляции между переменными
# - подобрать модель линейной регрессии и записать ее в виде уравнения
# - проверить валидность модели при помощи t- или F-теста
# - оценить долю изменчивости, которую объясняет модель, при помощи \(R^2\)

# Описание зависимости между переменными

## Читаем данные из файла и знакомимся с ними
# setwd("C:/mathmethr/week2") # установите рабочую директорию,
# или используйте полный путь к файлу
library(readxl)
nelson <- read_excel("nelson.xlsx", sheet = 1)
## или из .csv
# nelson <- read.table(file="nelson.csv", header = TRUE, sep = "\t", dec = ".")
str(nelson)
head(nelson)

## Связана ли потеря веса со влажностью?
## Задача: оцените силу связи
# - Посчитайте разные коэффициенты корреляции между потерей веса и влажностью
# - Чем отличаются результаты функций `cor()`, `cor.test()`?
cor(nelson$humidity, nelson$weightloss)
cor.test(nelson$humidity, nelson$weightloss)

## Как зависит потеря веса от влажности?
library(ggplot2)
theme_set(theme_bw())
gg_nelson <- ggplot(data=nelson, aes(x = humidity, y = weightloss)) +
  geom_point() +
  labs(x = "Относительная влажность, %", y = "Потеря веса, мг")
gg_nelson


# Линейная регрессия
## Добавим линию регрессии на график
gg_nelson + geom_smooth(method = "lm")
## Задача: как вы думаете, что это за серая область вокруг линии регрессии?



## Подбираем параметры линейной модели
nelson_lm <- lm(formula = weightloss ~ humidity, data = nelson)
summary(nelson_lm)
## Задача: Назовите, чему равны коэффициенты линейной регрессии?



## Неопределенность оценок коэффициентов
## Находим доверительные интервалы коэффициентов
# оценки коэффициентов отдельно
coef(nelson_lm)

# доверительные интервалы коэффициентов
confint(nelson_lm)

## Предсказываем Y при заданном X
# Какова средняя потеря веса при заданной влажности?
newdata <- data.frame(humidity = c(50, 100)) # значения, для которых предсказываем
(pr1 <- predict(nelson_lm, newdata, interval = "confidence", se = TRUE))

## Строим доверительную зону регрессии
library(gridExtra)
grid.arrange(gg_nelson + geom_smooth(method = "lm") +
               labs (title = "95% доверительная зона регрессии"),
             gg_nelson + geom_smooth(method = "lm", level = 0.99) +
               labs (title = "99% доверительная зона регрессии"),
             ncol = 1)

## Неопределенность оценок предсказанных значений
## Предсказываем изменение Y для 95\% наблюдений при заданном X
# В каких пределах находится потеря веса у 95\% жуков при заданной влажности?
newdata <- data.frame(humidity = c(50, 100)) # новые данные для предсказания значений
(pr2 <- predict(nelson_lm, newdata, interval = "prediction", se = TRUE))

## Данные для доверительной области значений
# Предсказанные значения для исходных данных объединим с исходными данными в новом датафрейме - для графиков
(pr_all <- predict(nelson_lm, interval = "prediction"))
nelson_with_pred <- data.frame(nelson, pr_all)


## Строим доверительную область значений и доверительный интервал одновременно
gg_nelson + geom_smooth(method = "lm",
                       aes(fill = "Доверительный \nинтервал"),
                       alpha = 0.4) +
  geom_ribbon(data = nelson_with_pred,
              aes(y = fit, ymin = lwr, ymax = upr,
                  fill = "Доверительная \nобласть значений"),
              alpha = 0.2) +
  scale_fill_manual('Интервалы', values = c('green', 'blue'))
## Осторожно!
### Вне интервала значений \(X\) ничего предсказать нельзя!


# Проверка валидности модели
## Проверка коэффициентов с помощью t-критерия
summary(nelson_lm)

## Проверка при помощи F-критерия
nelson_aov <- aov(nelson_lm)
summary(nelson_aov)

# Оценка качества подгонки модели
## Коэффициент детерминации можно найти в сводке модели
summary(nelson_lm)
# Не сравнивайте \(R^2\) моделей с разным числом параметров, \linebreak для этого есть \(R^2 _{adjusted}\)
