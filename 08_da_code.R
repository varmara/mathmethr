# ---
# title: "Дискриминантный анализ"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# Пакеты и функции ======================================================
# Для дискриминантного анализа
library(MASS)
source("LDA_helper_functions.R")
# Графики
library(ggplot2)
# Чтение данных
library(readxl)

# ## Пример: Морфометрия ирисов ==========================================
# Сверхзадача --- научиться классифицировать ирисы по нескольким измерениям цветка
data(iris)
head(iris, 10)

# ## По каким переменным легче всего различить группы?
pairs(iris[, -5], col = iris$Species)

#I. Дискриминантный анализ на тренировочных и тестовых данных #############

# 1) Разделяем на тренировочные и тестовые данные -------------------------

# доля от объема выборки, которая пойдет в тренировочный датасет
smp_size <- floor(0.80 * nrow(iris))
# устанавливаем зерно для воспроизводимости результатов
set.seed(982)
# индексы строк, которые пойдут в тренировочный датасет
in_train <- sample(1:nrow(iris), size = smp_size)

# 2) На тренировочных данных получаем стандартизованные коэффициенты дискриминантных функций -----

lda_tr_scaled <- lda(scale(iris[in_train, -5]), iris$Species[in_train])
# коэффициенты дискриминантных функций
lda_tr_scaled$scaling

# Для следующего этапа понадобится
# функция, которая добавит функций классификации к результатам дискр. анализа
# lda.class() из файла LDA_helper_functions.R

# 3) На тренировочных данных получаем функции классификации ---------------

lda_tr <- lda.class(iris[in_train, -5], iris$Species[in_train])
# Коэф. функций классификации
lda_tr$class.funs

# 4) Оцениваем качество классификации на тренировочных данных -------------

lda_tr_pred <- predict(lda_tr)
table(iris$Species[in_train], lda_tr_pred$class)


# 5) График классификации тренировочных данных  --------------------------

class_df <- data.frame(lda_tr_pred$x,
                          gr = lda_tr_pred$class,
                          real_gr = iris$Species[in_train])
ggplot(data = class_df, aes(x = LD1, y = LD2, colour = gr)) +
  geom_text(size = 3, aes(label = real_gr)) +
  theme(legend.position = "none")

# 6) Оценка качества классификации на тестовых данных ---------------------

lda_test_pred <- predict(lda_tr, iris[-in_train, -5])
table(iris$Species[-in_train], lda_test_pred$class)

# 7) График классификации тестовых данных ---------------------------------

class_df <- data.frame(lda_test_pred$x,
                          new = lda_test_pred$class,
                          real = iris$Species[-in_train])
class_df$Group <- factor(paste(class_df$real, class_df$new, sep = " as "))

ggplot(data = class_df, aes(x = LD1, y = LD2)) +
  geom_point(aes(colour = Group))

# II. Дискриминантный анализ с кросс-валидацией ###########################

# Кросс-валидация
lda_cv <- lda(iris[, -5], iris$Species, CV = TRUE)
names(lda_cv)
table(iris$Species, lda_cv$class)

# График классификации
ggplot(data = iris, aes(x = Petal.Length,
                        y = Sepal.Width,
                        colour = Species,
                        shape = lda_cv$class)) +
  geom_point(size = 3) +
  scale_shape_discrete("Classified as")

# Проверка условий применимости ###########################################

# 1) Mногомерная нормальность
x <- as.matrix(iris[, -5])
d <- mahalanobis(x, colMeans(x), cov(x))
qqplot(x = qchisq(p = ppoints(nrow(x)), df = ncol(x)),
       y = d,
       main="QQ график для оценки многомерной нормальности",
       ylab="Расстояние Махаланобиса")
abline(a = 0, b = 1)

# 2) Гомогенность ковариационных матриц
# функция из файла LDA_helper_functions.R
BoxMTest(as.matrix(iris[, -5]), iris$Species)


# Задание: Пингвины --------------------------------------------------------

# Морфометрия пингвинов Адели, Генту и Чинстрап
# (данные `penguins`, Horst et al. 2020).
#
# - При помощи дискриминантного анализа классифицируйте виды пингвинов,
# используя морфометрические данные
# - Хорошо ли работает классификация?
# - Выполняются ли условия применимости?

# library(palmerpenguins)
# data(penguins)
penguins <- read_xlsx(path = "data/penguins.xlsx", sheet = "penguin data")
head(penguins, 2)
colnames(penguins)

