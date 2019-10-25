# title: "Ординация и классификация с использованием мер сходства-различия" ########
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"



## Пример: Морфометрия поссумов ##################

# Данные Lindenmayer et al. (1995)

## Знакомимся с данными
library(DAAG)
data(possum)
colnames(possum)

colSums(is.na(possum))
# оставим только строки с полными наблюдениями
pos <- possum[complete.cases(possum), ]

# поссумы из разных сайтов из 2 популяций
table(pos$Pop, pos$site)


## Неметрическое многомерное шкалирование ########

library(vegan)
ord_euclid <- metaMDS(pos[, 6:14], distance = "euclid", autotransform = FALSE)


## Качество подгонки модели
ord_euclid$stress

# Координаты наблюдений:
head(ord_euclid$points, 10)

# График ординации:
ordiplot(ord_euclid, type = "t", cex = 0.5)


## Задание 1 -------------------------------------
#
# При помощи `ggplot2` постройте график неметрического многомерного шкалирования.
# Для графика используйте координаты точек `ord_euclid$points` и исходные данные.
# Раскрасьте график по значениям переменной `Pop`.
# Сделайте так, чтобы особи разного пола были изображены на разных панелях
#
# Дополните код

library()
# Данные для графика
points_euclid <- data.frame( , )
# График nMDS ординации
gg_euclid <- ggplot(, aes(x = , y = )) +
  geom_point() +
  facet_wrap(~ sex)
gg_euclid

## Задание 2 -------------------------------------
#
# Постройте nMDS ординацию при помощи евклидова расстояния, по стандартизованным данным

# Дополните код

# Ординация
ord_scaled <- metaMDS( (pos), distance = , autotransform = )
# Качество ординации





## График ординации по матрице евклидовых расстояний, рассчитанных по стандартизованным данным
# Данные для графика
points_scaled <- data.frame(ord_scaled$points, pos)
# График nMDS-ординации
gg_scaled <- gg_euclid %+% points_scaled
gg_scaled


## Видно, что графики ординации, полученные разными методами, различаются
library(gridExtra)
grid.arrange(gg_euclid + aes(size = age),
             gg_scaled + aes(size = age),
             ncol = 1)


# # Кластерный анализ ############################
#
## Пример: Морфометрия самок поссумов ############

# library(DAAG)
data(fossum)


## Создаем "говорящие" названия строк

rownames(fossum) # Было

# Чтобы имена строк были более информативны, добавим к ним название популяции
rownames(fossum) <- paste(fossum$Pop,
                          rownames(fossum),
                          sep = "_")
rownames(fossum) # стало

## Отбираем только то, что понадобится для кластеризации
fos <- fossum[complete.cases(fossum), 6:14]


## Кластерный анализ начинается с расчета матрицы расстояний между объектами

d <- dist(x = fos, method = "euclidean")

## Методы кластеризации ##########################

## Метод ближайшего соседа #######################

hc_single <- hclust(d, method = "single")
library(ape)
ph_single <- as.phylo(hc_single)
# cex - относительный размер шрифта
plot(ph_single, type = "phylogram", direction = "downwards", cex = 0.7)
axisPhylo(side = 2)


## Метод отдаленного соседа ######################

ph_compl <- as.phylo(hclust(d, method = "complete"))
plot(ph_compl, type = "phylogram", direction = "downwards", cex = 0.8)
axisPhylo(side = 2)


## Метод невзвешенного попарного среднего ########

ph_avg <- as.phylo(hclust(d, method = "average"))
plot(ph_avg, type = "phylogram", direction = "downwards", cex = 0.8)
axisPhylo(side = 2)


## Метод Варда ###################################

ph_w2 <- as.phylo(hclust(d, method = "ward.D2"))
plot(ph_w2, type = "phylogram", direction = "downwards", cex = 0.8)
axisPhylo(side = 2)



# Оценка качества кластеризации ##################

## Кофенетическая корреляция #####################

# Кофенетические расстояния
c_single <- as.dist(cophenetic(ph_single))
c_compl <- as.dist(cophenetic(ph_compl))
c_avg <- as.dist(cophenetic(ph_avg))
c_w2 <- as.dist(cophenetic(ph_w2))

# Кофенетические корреляции
cor(d, c_single)
cor(d, c_compl)
cor(d, c_avg)
cor(d, c_w2)


## Бутстреп ######################################

library(pvclust)

system.time({
cl_boot <- pvclust(scale(t(fos)),
                   method.hclust = "average",
                   method.dist = "euclidean",
                   nboot = 100,
                   parallel = TRUE,
                   iseed = 42)
})


plot(cl_boot, cex.pv = 0.8, cex = 0.8)


### Построение деревьев по генетическим данным ####

## Пример: Митохондриальная ДНК приматов. ########

# Датасет собан Dr. Masami Hasegawa (Institute of Statistical Mathematics, Tokyo), по данным сиквенирования Kenji Hayasaka, Takashi Gojobori, Satoshi Horai (Molecular Biology and Evolution 5: 626-644, 1988).
#
# Исходный файл в формате PHYLIP можно загрузить по ссылке: http://evolution.genetics.washington.edu/book/primates.dna

webpage <-"http://evolution.genetics.washington.edu/book/primates.dna"
primates.dna <- read.dna(webpage)
d_pri <- dist.dna(primates.dna, model = "K80")
hc_pri <- hclust(d_pri, method = "average")
ph_pri <- as.phylo(hc_pri)
plot(ph_pri, cex = 0.8)
axisPhylo()
