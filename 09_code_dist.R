#' ---
#' title: "Ординация и классификация с использованием мер сходства-различия"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"



#' ## Пример: Морфометрия поссумов
#' Данные Lindenmayer et al. (1995)
#'
#' ## Знакомимся с данными
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

#' Отберем переменные, с которыми будем работать
colnames(possum)
possumc <- possum[complete.cases(possum), c(3:14)]

#' ## Неметрическое многомерное шкалирование
library(vegan)
ord_euclid_wisk <- metaMDS(possumc[, 3:10], distance = "euclid", autotransform = TRUE)

#' ## Качество подгонки модели
ord_euclid_wisk$stress

#' ## Ординация
#' Координаты наблюдений:
head(ord_euclid_wisk$points, 10)

#' График ординации:
ordiplot(ord_euclid_wisk, type = "t")

#' ## Задание:
#' При помощи `ggplot2` постройте график неметрического многомерного шкалирования.
#' Для графика используйте координаты точек `ord_euclid_wisk$points` и исходные данные.
#' Раскрасьте график по значениям переменных `Pop` и `age`
#' Изобразите поссумов разного пола на разных панелях


#' ## Задание:
#'
#' Постройте nMDS ординацию при помощи евклидова расстояния, **без стандартизации**
#'
#' Воспользуйтесь справкой к функции `metaMDS()`, чтобы узнать, какие аргументы потребуется изменить.




#' ## Как изменилась сама ординация?
proc <- procrustes(ord_raw,
                   ord_euclid_wisk)
proc
plot(proc)




#' # Кластерный анализ

#' ## Пример: поссумы
#' Морфометрия самок поссумов
library(DAAG)
data(fossum)
# создадим "говорящие" имена строк
rownames(fossum) <- paste(fossum$Pop, rownames(fossum), sep = "_")
fossumc <- fossum[complete.cases(fossum), 5:14]



#' Евклидово расстояние

d <- dist(x = fossumc, method = "euclidean")

#' ## Метод ближайшего соседа
hc_single <- hclust(d, method = "single")
library(ape)
ph_single <- as.phylo(hc_single)
par(cex = 0.9) # уменьшаем размер шрифта
plot(ph_single, type = "phylogram", direction = "downwards")
axisPhylo(side = 2)

#' ## Метод отдаленного соседа
ph_compl <- as.phylo(hclust(d, method = "complete"))
plot(ph_compl, type = "phylogram", direction = "downwards")
axisPhylo(side = 2)

#' ## Метод невзвешенного попарного среднего
ph_avg <- as.phylo(hclust(d, method = "average"))
plot(ph_avg, type = "phylogram", direction = "downwards")
axisPhylo(side = 2)

#' ## Метод Варда
ph_w2 <- as.phylo(hclust(d, method = "ward.D2"))
plot(ph_w2, type = "phylogram", direction = "downwards")
axisPhylo(side = 2)

#' ## Кофенетическая корреляция
c_single <- cophenetic(ph_single)
c_compl <- cophenetic(ph_compl)
c_avg <- cophenetic(ph_avg)
c_w2 <- cophenetic(ph_w2)

cor(d, as.dist(c_single))
cor(d, as.dist(c_compl))
cor(d, as.dist(c_avg))
cor(d, as.dist(c_w2))







#' ## Бутстреп
library(pvclust)

# итераций должно быть 10000 и больше
# здесь мало для скорости
set.seed(42)
system.time({
cl_boot <- pvclust(scale(t(fossumc)), method.hclust = "average",
                   nboot = 1000, method.dist = "euclidean")
})

plot(cl_boot)


#' ## И небольшая демонстрация --- дерево по генетическим данным
webpage <-"http://evolution.genetics.washington.edu/book/primates.dna"
primates.dna <- read.dna(webpage)
d_pri <- dist.dna(primates.dna, model = "K80")
hc_pri <- hclust(d_pri, method = "average")
ph_pri <- as.phylo(hc_pri)
plot(ph_pri)
axisPhylo()
