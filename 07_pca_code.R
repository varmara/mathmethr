# title: "Анализ главных компонент"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# ## Пример: Морфометрия поссумов ##################
# Данные Lindenmayer et al. (1995)

# ## Знакомимся с данными
library(DAAG)
data(possum)
colnames(possum)

colSums(is.na(possum))
# оставим только строки с полными наблюдениями
pos <- possum[complete.cases(possum), ]

# поссумы из разных сайтов из 2 популяций
table(pos$site, pos$Pop)

# половой состав выборок из разных сайтов
table(pos$sex, pos$site, pos$Pop)

# ## Как связаны признаки между собой?

# Серия графиков с признаками во всех возможных комбинациях
pairs(pos[, 6:14],
      pch =  as.numeric(pos$sex))

# поссумы из разных популяций раскрашены разными цветами
pairs(pos[, 6:14],
      col = pos$Pop,
      pch =  as.numeric(pos$sex))

# поссумы из разных точек раскрашены разными цветами
pairs(pos[, 6:14],
      col = pos$site,
      pch =  as.numeric(pos$sex))


# цвета для каждого сайта из Брюеровской палитры "Set1"
library(RColorBrewer)
n_sites <- length(unique(pos$site))
cols <- brewer.pal(n = n_sites, name = "Set1")

# график морфометрических переменных
pairs(pos[, 6:14],
      col = cols[pos$site],
      pch =  as.numeric(pos$sex))


#### Анализ главных компонент ####################
library(vegan)
# ординация, используем морфометрические переменные (с hdlngth по belly)
ord <- rda(pos[, 6:14], scale = TRUE)

summary(ord)

# 1. Сколько компонент нужно оставить?
# 2. Сколько общей изменчивости объясняют оставленные компоненты?
# 3. Что означают получившиеся компоненты?
# 4. Как располагаются объекты в пространстве главных компонент?


#### 1. Cколько компонент нужно оставить? ############

# собственные числа
eigenvals(ord)

# График собственных чисел
screeplot(ord, bstick = TRUE, type = "lines")

#### 2. Сколько изменчивости объясняют компоненты? #########

# Изменчивость, объясненная каждой из компонент, в процентах
eigenvals(ord) / sum(eigenvals(ord)) * 100

#### 3. Что означают получившиеся компоненты? ############

# Факторные нагрузки
scores(ord, display = "species", choices = c(1, 2, 3),
       scaling = "species", correlation = TRUE)


# Можно нарисовать факторные нагрузки на графике
biplot(ord, scaling = "species", correlation = TRUE,
       main = "PCA -  species scaling", display = "species")

# График факторных нагрузок в ggplot2
library(ggplot2)
theme_set(theme_bw())
library(ggrepel) # для подписей (geom_text_repel)
library(grid) # для стрелочек
# параметры стрелочек
ar <- arrow(length = unit(0.1, "cm"))
# датафрейм с факторными нагрузками
df_load <- data.frame(scores(ord,
                             display = "species",
                             choices = c(1, 2),
                             scaling = "species",
                             correlation = TRUE))
# график
ggloadings <- ggplot(df_load) +
  geom_text_repel(aes(x = PC1, y = PC2,
                      label = rownames(df_load)), segment.alpha = 0.5) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey40", arrow = ar) +
  coord_equal(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8))
ggloadings


#### 4. Как располагаются объекты в пространстве главных компонент?

# Значения факторов (= факторные координаты) --- координаты объектов в пространстве главных компонент
scores(ord, display = "sites",  choices = c(1, 2, 3), scaling = "sites")

# График факторных координат (= график ординации)
biplot(ord, scaling = "sites", display = "sites",
       type = "t", main = "PCA - sites scaling")

# График ординации в ggplot2
# данные для графика: факторные координаты и исходные переменные
df_scores <- data.frame(pos, scores(ord, display = "sites", scaling = "sites", choices = c(1, 2, 3)))
# график ординации
ggscores <- ggplot(df_scores, aes(x = PC1, y = PC2, colour = Pop, shape = sex)) +
  geom_point(size = 2) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1))
ggscores



# Два графика рядом
library(cowplot)
plot_grid(ggloadings, ggscores, labels = 'AUTO', align = 'vh', axis = 'r')


#### Пример: Морфометрия египетских черепов ###############

# Данные Thompson, Randall-Maciver (1905). Источник Manly (1994).

# Измерения 150 черепов в мм:
# - mb --- максимальная ширина
# - bh --- высота от основания до макушки
# - bl --- расстояние от основания черепа до края в. челюсти
# - nh --- высота носа

# Эпоха (epoch):
# - 1 --- ранний прединастический период (ок. 4000 до н.э.)
# - 2 --- поздний прединастический период (ок. 3300 до н.э.)
# - 3 --- 12 и 13 династии (ок. 1850 до н.э.)
# - 4 --- Птолемейский период (ок. 200 до н.э.)
# - 5 --- Римский период (ок. 150 н.э.)


# ## Знакомимся с данными
library(HSAUR)
data("skulls")

str(skulls)
sum(is.na(skulls))
table(skulls$epoch)

# цвета
library(RColorBrewer)
cols <- brewer.pal(n = length(levels(skulls$epoch)), name = "Set1")
# график
pairs(skulls[, -1], col = cols[skulls$epoch])


#### Задание 1 ---------------------------------------------

# Сделайте анализ главных компонент:
# 1. Сколько компонент нужно оставить?
# 2. Сколько общей изменчивости объясняют оставленные компоненты?
# 3. Что означают получившиеся компоненты?
# 4. Как располагаются объекты в пространстве главных компонент?

# Как менялась форма черепов в древнем египте в разные эпохи?


