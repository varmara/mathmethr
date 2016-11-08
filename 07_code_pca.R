#' ---
#' title: "Анализ главных компонент"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"

#' ## Пример: Морфометрия поссумов
#' {Данные Lindenmayer et al. (1995)}

#' ## Знакомимся с данными
library(DAAG)
data(possum)
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
labels = c("Cambarville","Bellbird",
           "Whian Whian", "Byrangery",
           "Conondale ","Allyn River",
           "Bulburin"))


#' ## Как связаны признаки между собой?
library(RColorBrewer)
# цвета из Брюеровской палитры
cols <- brewer.pal(n = length(levels(possum$site)), name = "Set2")
# график
pairs(possum[, c(6:14, 2)], col = cols[possum$site],
      pch =  as.numeric(possum$sex))


#' ## Анализ главных компонент
library(vegan)
# возьмем только строки, где нет пропущенных значений
possum <- possum[complete.cases(possum), ]
# ординация, используем переменные с hdlngth по belly
ord <- rda(possum[, 6:14], scale = TRUE)

summary(ord)


#' 1. Сколько компонент нужно оставить?
#' 2. Сколько общей изменчивости объясняют оставленные компоненты?
#' 3. Что означают получившиеся компоненты?
#' 4. Как располагаются объекты в пространстве главных компонент?

#' ## 1А. Cколько компонент нужно оставить?
eigenvals(ord)
eigenvals(ord) > mean(eigenvals(ord))

#' ## 1Б. Cколько компонент нужно оставить?
screeplot(ord, bstick = TRUE, type = "lines")
abline(h = 1, lty = 2)

#' ## 2. Сколько изменчивости объясняют компоненты?
eigenvals(ord)/sum(eigenvals(ord))*100

#' ## 3. Что означают получившиеся компоненты?
scores(ord, display = "species", choices = c(1, 2, 3),
       scaling = "species", correlation = TRUE)
#' ## Можно нарисовать факторные нагрузки на графике
biplot(ord, scaling = "species", correlation = TRUE,
       main = "PCA -  species scaling", display = "species")

#' ## График факторных нагрузок в ggplot2
library(ggplot2)
theme_set(theme_bw())
library(ggrepel) # для подписей
library(grid) # для стрелочек
ar <- arrow(length = unit(0.1, "cm"))

df_load <- data.frame(scores(ord, display = "species",
         choices = c(1, 2), scaling = "species"))

ggloadings <- ggplot(df_load) +
  geom_text_repel(aes(x = PC1, y = PC2,
    label = rownames(df_load)), segment.alpha = 0.5) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
    colour = "grey40", arrow = ar) +
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 2))
ggloadings

#' ## 4. Значения факторов (= факторные координаты) --- координаты объектов в пространстве главных компонент
scores(ord, display = "sites",  choices = c(1, 2, 3), scaling = "sites")

#' ## График факторных координат (= график ординации)
biplot(ord, scaling = "sites", display = "sites",
       type = "t", main = "PCA - sites scaling")

#' ## График факторных координат в ggplot2
df_scores <- data.frame(possum, scores(ord, display = "sites",
                                       choices = c(1, 2, 3),
                                       scaling = "sites"))

ggscores <- ggplot(df_scores, aes(x = PC1, y = PC2,
                        colour = site, shape = sex)) +
  geom_point(size = 2) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1))
ggscores


#' ## Несколько графиков рядом: `grid.arrange()`
library(gridExtra)
grid.arrange(ggloadings, ggscores, nrow = 1)

#' ## Несколько графиков рядом: `gtable`
library(gtable)
g1 <- ggplotGrob(ggloadings)
g2 <- ggplotGrob(ggscores)
g <- gtable:::cbind_gtable(g1, g2, size = "first")
grid.newpage()
grid.draw(g)


#' ## Так можно экстрагировать компоненты с исходными данными
scrs <- scores(ord, display = "sites",
               choices = c(1, 2, 3), scaling = "sites")
data_with_pc <- data.frame(possum, scrs)
head(data_with_pc)

#' ## Пример: Морфометрия египетских черепов
#' Измерения 150 черепов в мм:
#' - mb --- максимальная ширина
#' - bh --- высота от основания до макушки
#' - bl --- расстояние от основания черепа до края в. челюсти
#' - nh --- высота носа
#' Эпоха (epoch):
#' - 1 --- ранний прединастический период (ок. 4000 до н.э.)
#' - 2 --- поздний прединастический период (ок. 3300 до н.э.)
#' - 3 --- 12 и 13 династии (ок. 1850 до н.э.)
#' - 4 --- Птолемейский период (ок. 200 до н.э.)
#' - 5 --- Римский период (ок. 150 н.э.)
#' {Данные Thompson, Randall-Maciver (1905). Источник Manly (1994).}

#' ## Знакомимся с данными
library(HSAUR)
data("skulls")
str(skulls, vec.len = 2)
sum(is.na(skulls))
table(skulls$epoch)

library(RColorBrewer)
cols <- brewer.pal(n = length(levels(skulls$epoch)), name = "Set1")
# график
pairs(skulls[, -1], col = cols[skulls$epoch])

#' ## Задание:
#' Сделайте анализ главных компонент. Как менялась форма черепов в древнем египте в разные эпохи?


