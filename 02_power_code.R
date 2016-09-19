# ---
# title       : Анализ мощности, часть 1
# subtitle    : Математические методы в зоологии - на R, осень 2015
# author      : Марина Варфоломеева
# job         : Каф. Зоологии беспозвоночных, СПбГУ

# Инсталлируем нужные пакеты
# Это нужно делать один раз
install.packages(c("pwr", "readxl", "effsize", "ggplot2"))

# A priory анализ мощности

## Величина эффекта из общих соображений

# Загружаем пакет pwr
# Это нужно делать один раз в каждой новой R-сессии
library(pwr)
cohen.ES(test = "t", size = "large")

# # Рассчитайте
# # величину умеренных и слабых эффектов для t-критерия
# #   library()
# #   cohen.ES()
# # Подсказка: обозначения можно посмотреть в файлах справки
# #     help(cohen.ES)
# #     ?cohen.ES
# #   cohen.ES # курсор на слове, нажать F1


---
## Величина эффекта из пилотных данных

alpha <- 0.05
power <- 0.80
sigma <- 27.7 # варьирование плотности халиотисов
diff <- 23.2 # ожидаемые различия плотности халиотисов
effect <- diff/sigma # величина эффекта
effect

## Считаем объем выборки
pwr.t.test(n = NULL, d = effect, power = power, sig.level = alpha,
           type = "two.sample", alternative = "two.sided")

## Рассчитайте
# # сколько нужно обследовать мест, чтобы обнаружить слабый эффект
# # с вероятностью 0.8, при уровне значимости 0.01
# #     cohen.ES()
# #   pwr.t.test()


## Пример: Улитки на устрицах в мангровых зарослях *
# В каких зонах мангровых зарослей на устрицах предпочитают обитать улитки?
# Minchinton, Ross, 1999

## Читаем данные из файла

# Не забудте войти в вашу директорию для матметодов, например, так
# setwd("C:/Мои\ документы/mathmethR/) # в Windows
# setwd(/home/yourusername/mathmethR/) # в Linux

library(readxl)
minch <- read_excel("minch.xls", sheet = 1)

str(minch) # Структура данных

## Просмотреть, что получилось можно так:

head(minch)     # Первые несколько строк файла
minch$zone[1:3] # Первые три значения переменной zone
minch[2:3, c(1, 3, 5)] # 2-3 строки и 1, 3, 5 столбцы

## Гистограмма числа улиток

library(ggplot2)
ggplot(data = minch, aes(x = limpt100)) + geom_histogram(stat = "bin", binwidth = 3)

## Не нравится тема? Можно привинтить другую!

# ggplot(data = minch, aes(x = limpt100)) + geom_histogram(stat = "bin", binwidth = 3) + theme_classic()
theme_set(theme_bw())
ggplot(data = minch, aes(x = limpt100)) +
  geom_histogram(stat = "bin", binwidth = 3)

## Раскрашиваем гистограмму

hp <- ggplot(data = minch, aes(x = limpt100, fill = site)) +
  geom_histogram(stat = "bin", binwidth = 3, position = "dodge") +
  labs(x = "Число улиток на 100 устриц", y = "Число проб", fill = "Сайт")
hp # теперь гистограмму из этого объекта можно вызвать в любой момент

## Раскрасить иначе? Нет проблем!

# Чтобы не переписывать всеменяем только эстетику
hp + aes(fill = zone) +  labs(fill = "Зона литорали")

## График с панелями

hp + facet_wrap(~ zone)

## Поэкспериментируйте с панелями
# # Что происходит, если мы выбираем другие переменные? Почему?
# # Какие еще бывают варианты разбивки на панели?<br />
# # Автоподсказки: напишите `facet` и нажмите `Ctrl+Space`
# # Что будет если менять `fill` и `facet` одновременно?
# #     ggplot()
# #     aes()
# #     geom_histogram()
# #    facet_wrap()




## Боксплоты числа улиток
bp <- ggplot(data = minch, aes(x = site, y = limpt100)) +
  geom_boxplot()
bp

## Дома самостоятельно поэкспериментируйте
# # с панелями `facet` и с эстетиками `fill` и `colour`
# # Что будет, если мы выберем другие переменные?
# # Опишите форму и разброс распределения улиток в двух сайтах
# # Симметрично? Похоже ли на нормальное?
# #    ggplot()
# #     aes()
# #     geom_boxplot()
# #    facet_wrap()


## Постройте
# боксплот и гистограмму переменной __sqlim100__ (квадратный корень из численности улиток) для двух сайтов<br />
# # Подсказка: `x` и `y` это тоже эстетики, поэтому можно использовать предыдущие графики
# # Стало ли распределение больше походить на нормальное?
# #     ggplot()
# #     geom_histogram()
# #     geom_boxplot()
# #    aes()


# A priory анализ мощности по данным пилотного исследования
minch_smpl <- read_excel("minch_smpl.xls", sheet = 1)
ggplot(minch_smpl, aes(x = site, y = sqlim100)) +
  geom_boxplot(aes(fill = site))

## Величина эффекта по исходным данным

library(effsize)
effect <- cohen.d(minch_smpl$sqlim100, minch_smpl$site)
effect
## как добыть из нее значение величины эффекта?
## Обращении к переменным по имени - `$`
### Как называется в структуре объекта элемент, где записана величина эффекта?

str(effect) # effect$estimate
# Для pwr.t.test() эффект должен быть положительным, поэтому вычислим модуль
effect <- abs(effect$estimate)

## Рассчитайте
## объем выборки, чтобы показать различия плотности улиток между сайтами с вероятностью 0.8?
##     pwr.t.test()


# Post hoc анализ мощности

## Что получилось бы на самом деле?

# bp <- ggplot(data = minch, aes(x = site,
# y = limpt100)) + geom_boxplot()
bp + aes(y = sqlim100)

# по умолчанию t-критерий для неравных дисперсий
# (Модификация Велча)
t.test(sqlim100 ~ site, data = minch,
       var.equal = FALSE)

effect_real <- cohen.d(minch$sqlim100, minch$site)
effect_real <- abs(effect_real$estimate)
pwr.t.test(n = 20, d = effect_real,
           power = NULL, sig.level = 0.05,
           type = "two.sample",
           alternative = "two.sided")


