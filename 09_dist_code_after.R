# title       : Ординация и классификация с использованием мер сходства-различия
# subtitle    : Математические методы в зоологии - на R, осень 2015
# author      : Марина Варфоломеева

## Пример: Морфометрия поссумов
# Данные Lindenmayer et al. (1995)

## Знакомимся с данными
library(DAAG)
data(possum)
colnames(possum)

sum(is.na(possum))
possum[!complete.cases(possum), ]

# Добавим названия сайтов
possum$site <- factor(possum$site,  levels = 1:7,
labels = c("Cambarville","Bellbird",
           "Whian Whian", "Byrangery",
           "Conondale ","Allyn River",
           "Bulburin"))


# Отберем переменные, с которыми будем работать
colnames(possum)
possumc <- possum[complete.cases(possum), c(3:4, 5:14)]


## Неметрическое многомерное шкалирование
library(vegan)
ord_euclid <- metaMDS(possumc[, 3:10], distance = "euclid")

## Качество подгонки модели
ord_euclid$stress

## Ординация
ordiplot(ord_euclid, type = "t")

head(ord_euclid$points, 10)

## Задание:
# При помощи `ggplot2` постройте график неметрического многомерного шкалирования.
# Для графика используйте координаты точек `ord_euclid$points` и исходные данные.
# Раскрасьте график по значениям переменных `Pop` и `age`
# Изобразите поссумов разного пола на разных панелях

## Решение:

# 1) создаем датафрейм
ord_euclid_points <- data.frame(ord_euclid$points, possumc)
colnames(ord_euclid_points)

# 2) создаем график
library(ggplot2)
update_geom_defaults(geom = "point", new = list(shape = 19, size = 4))
# gg <-

ggplot(data = ord_euclid_points, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = Pop))

ggplot(data = ord_euclid_points, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = age))

ggplot(data = ord_euclid_points, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = age)) + facet_wrap(~sex)

# 2) как можно оптимально организовать графики

gg <- ggplot(data = ord_euclid_points, aes(x = MDS1, y = MDS2)) + geom_point() + facet_wrap(~sex)

gg + aes(colour = Pop)
gg + aes(colour = age)

gg + aes(colour = taill)

## Задание:
# Постройте nMDS ординацию при помощи евклидова расстояния, **без стандартизации**
# Воспользуйтесь справкой к функции `metaMDS()`, чтобы узнать, какие аргументы потребуется изменить.
# Какая ординация лучше?

## Решение:
ord_raw <- metaMDS(possumc[, 3:10], distance = "euclid", autotransform = F)
# стало
ord_raw$stress
# было
ord_euclid$stress

# 1) создаем датафрейм с новыми данными
ord_raw_points <- data.frame(ord_raw$points, possumc)
# 2) заменяем старый датафрейм в графиках на новый
# было
gg  + aes(colour = Pop)
# стало
gg %+% ord_raw_points + aes(colour = Pop)

gg %+% ord_raw_points + aes(colour = age)




## Как изменилась сама ординация?
### Прокрустово преобразование
proc <- procrustes(ord_raw, ord_euclid)

proc

plot(proc)

library(gridExtra)
ord_euclid_points <- data.frame(ord_euclid$points, possumc)
grid.arrange(gg %+% ord_euclid_points + aes(colour = Pop),
gg %+% ord_euclid_points + aes(colour = age),
ncol = 1)



# Кластерный анализ

## Пример: поссумы
# Морфометрия самок поссумов
library(DAAG)
data(fossum)
# создадим "говорящие" имена строк
rownames(fossum) <- paste(fossum$Pop, rownames(fossum), sep = "_")
fossumc <- fossum[complete.cases(fossum), 5:14]



# евклидово расстояние, расчитанное по стандартизованным данным
d <- dist(x = scale(fossumc), method = "euclidean")


## Метод ближайшего соседа

hc_single <- hclust(d, method = "single")
plot(hc_single)
# install.packages("ape")
library(ape)
ph_single <- as.phylo(hc_single)
plot(ph_single, type = "phylogram", cex = 0.7)
axisPhylo()


## Метод отдаленного соседа

ph_compl <- as.phylo(hclust(d, method = "complete"))
plot(ph_compl, type = "phylogram", cex = 0.7)
axisPhylo()


## Метод невзвешенного попарного среднего

ph_avg <- as.phylo(hclust(d, method = "average"))
plot(ph_avg, type = "phylogram", cex = 0.7)
axisPhylo()


## Метод Варда

ph_w2<- as.phylo(hclust(d, method = "ward.D2"))
plot(ph_w2, type = "phylogram", cex = 0.7)
axisPhylo()


# Cравнение и интерпретация результатов кластеризации

## Кофенетическая корреляция

c_single <- cophenetic(ph_single)
c_compl <- cophenetic(ph_compl)
c_avg <- cophenetic(ph_avg)
c_w2 <- cophenetic(ph_w2)

cor(d, as.dist(c_single))
cor(d, as.dist(c_compl))
cor(d, as.dist(c_avg))
cor(d, as.dist(c_w2))


## На каком уровне нужно делить дендрограмму на кластеры?


## Бутстреп
# install.packages("pvclust")
library(pvclust)

# итераций должно быть 1000 и больше
# здесь мало для скорости
set.seed(42)
cl_boot <- pvclust(scale(t(fossumc)), method.hclust = "average", nboot = 50, method.dist = "euclidean")

plot(cl_boot)
pvrect(cl_boot)


## И небольшая демонстрация - дерево по генетическим данным
webpage <-"http://evolution.genetics.washington.edu/book/primates.dna"
primates.dna <- read.dna(webpage)
d_pri <- dist.dna(primates.dna)
hc_pri <- hclust(d_pri, method = "average")
ph_pri <- as.phylo(hc_pri)
plot(ph_pri)
axisPhylo()


