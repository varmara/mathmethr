# title       : Анализ главных компонент
# subtitle    : Математические методы в зоологии - на R, осень 2014
# author      : Марина Варфоломеева

# install.packages(c("DAAG", "vegan", "gtable", "devtools", "HSAUR"))
# library("devtools")
# install_github("jiho/autoplot")

## Пример: Морфометрия поссумов
# Данные Lindenmayer et al. (1995)

## Знакомимся с данными
library(DAAG)
data("possum")

colnames(possum)

sum(is.na(possum))

possum[!complete.cases(possum), ]

# поссумы из разных сайтов
table(possum$site)

# поссумы из 2 популяций
table(possum$Pop)

# половой состав выборок из разных сайтов
with(possum, table(sex, site, Pop))


# В исходных данных сайты закодированы цифрами
unique(possum$site)
# Добавим названия сайтов
possum$site <- factor(possum$site,
                      levels = 1:7,
                      labels = c("Cambarville","Bellbird", "Whian Whian", "Byrangery", "Conondale ", "Allyn River", "Bulburin"))

## Как связаны признаки между собой?
# цвета
library(RColorBrewer)
cols <- brewer.pal(n = length(levels(possum$site)), name = "Set1")
# график
pairs(possum[, c(6:14, 2)], col = cols[possum$site], pch =  as.numeric(possum$sex))


## Анализ главных компонент
library(vegan)
# возьмем только строки, где нет пропущенных значений
possum <- possum[complete.cases(possum), ]
# ординация, используем переменные с hdlngth по belly
ord <- rda(possum[, 6:14], scale = TRUE)

## Все результаты можно посмотреть при помощи функции `summary()` {.smaller}
summary(ord)

# 1. Сколько компонент нужно оставить?
# 2. Сколько общей изменчивости объясняют оставленные компоненты?
# 3. Что означают получившиеся компоненты?
# 4. Как располагаются объекты в пространстве главных компонент?

## 1А. Cколько компонент нужно оставить?

# Вариант А. Оставляем компоненты с соб. числами > 1 (правило Кайзера)
eigenvals(ord)
eigenvals(ord) > mean(eigenvals(ord))

## 1Б. Cколько компонент нужно оставить?
# Вариант Б. Оставляем компоненты, кот объясняют больше изменчивости, чем возможно случайно (по модели сломанной палки).
screeplot(ord, bstick = TRUE, type = "lines")
abline(h = 1, lty = 2)

## 2. Сколько изменчивости объясняют компоненты?
eigenvals(ord)/sum(eigenvals(ord))*100

## 3. Что означают получившиеся компоненты?
scores(ord, display = "species", choices = c(1, 2, 3), scaling = "species", correlation = TRUE)

# График факторных нагрузок
biplot(ord, main = "PCA - scaling species", display = "species", scaling = "species", correlation = TRUE)

## График факторных нагрузок в ggplot

# install.packages("devtools")
# library("devtools")
# install_github("jiho/autoplot")

library(autoplot)

# fortify(ord) # исходные данные, если нужно

ggloadings <- autoplot(ord, data = possum, type = "var", PC = c(1, 2))  +  labs (x = "PC1", y = "PC2") + xlim(c(-2, 2)) + ylim(c(-2, 2))
ggloadings


## Интерпретируем компоненты по графику факторных нагрузок



## 4. Значения факторов (= факторные координаты) - координаты объектов в пространстве главных компонент

# Координаты можно добыть так
scores(ord, display = "sites",  choices = c(1, 2, 3), scaling = "sites")

## График факторных координат (= график ординации)
biplot(ord, scaling = "sites", main = "PCA - scaling sites", display = "sites", type = "t")

## График факторных координат в ggplot
ggscores <- autoplot(ord, data = possum, type = "obs", PC = c(1, 2), aes(colour = site, shape = sex), size = 3)
ggscores

## Делаем красивый график ординации.
# Подписи можно удалить из объекта `autoplot` и вставить свои.
# Смотрим на слои графика, второй из них содержит geom_text и он нам не нужен
ggscores$layers
# удаляем слой с подписями
ggscores$layers <- ggscores$layers[-2]


## Интерпретируем сходство объектов по графику ординации


# Несколько графиков рядом
library(gtable)
g1 <- ggplotGrob(ggloadings)
g2 <- ggplotGrob(ggscores)
g <- gtable:::cbind_gtable(g1, g2, "first")
grid.newpage()
grid.draw(g)


## Факторные координаты можно использовать для снижения размерности данных
# Так можно экстрагировать компоненты с исходными данными
scrs <- scores(ord, display = "sites",  choices = c(1, 2, 3), scaling = "sites")
data_with_pc <- data.frame(possum, scrs)
head(data_with_pc)



## Пример: Морфометрия египетских черепов. Данные Thompson, Randall-Maciver (1905). Источник Manly (1994).
# Измерения 150 черепов в мм:
# - mb - максимальная ширина
# - bh - высота от основания до макушки
# - bl - расстояние от основания черепа до края в. челюсти
# - nh - высота носа
# Эпоха (epoch):
# - 1 - ранний прединастический период (ок. 4000 до н.э.)
# - 2 - поздний прединастический период (ок. 3300 до н.э.)
# - 3 - 12 и 13 династии (ок. 1850 до н.э.)
# - 4 - Птолемейский период (ок. 200 до н.э.)
# - 5 - Римский период (ок. 150 н.э.)


## Знакомимся с данными
library(HSAUR)
data("skulls")
str(skulls, vec.len = 2)
sum(is.na(skulls))
table(skulls$epoch)

# цвета
cols <- brewer.pal(n = length(levels(skulls$epoch)), name = "Set1")
# график
pairs(skulls[, -1], col = cols[skulls$epoch])

# ## Задание:
# Сделайте анализ главных компонент. Как менялась форма черепов в древнем египте в разные эпохи?





