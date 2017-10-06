# title: "Регрессионный анализ, часть 1"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# ## Пример: потеря влаги личинками мучных хрущаков #####
# Как зависит потеря влаги личинками [малого мучного хрущака](http://ru.wikipedia.org/wiki/Хрущак_малый_мучной) _Tribolium confusum_ от влажности воздуха?
# - 9 экспериментов, продолжительность 6 дней
# - разная относительная влажность воздуха, %
# - измерена потеря влаги, мг
# Nelson, 1964; данные из Sokal, Rohlf, 1997, табл. 14.1 по Logan, 2010. глава 8, пример 8c; Данные в файле nelson.xlsx


# ## Читаем данные из файла ######################
library(readxl)
nelson <- read_excel(path = "data/nelson.xlsx", sheet = 1)

# ## Знакомимся с данными ########################

# ## Все ли правильно открылось?
str(nelson)      # Структура данных
head(nelson)     # Первые несколько строк файла

# Есть ли пропущенные значения?
colSums(is.na(nelson))

# Каков объем выборки?
# Поскольку пропущенных значений нет, можем просто посчитать число строк
nrow(nelson)


# # Графики средствами пакета ggplot2 ############

# ### Давайте поэтапно построим график
# - `library(ggplot2)` --- активирует пакет ggplot2 со всеми его функциями
# - `ggplot()` --- создает пустой "базовый" слой --- основу графика
library(ggplot2)
ggplot()

# ## Откуда брать данные?
ggplot(data = nelson)

# ## Какие переменные изображать на графике?
ggplot(data = nelson, aes(x = humidity, y = weightloss))

# ## В виде чего изображать?
ggplot(data = nelson, aes(x = humidity, y = weightloss)) +
  geom_point()

# ## В виде чего изображать?
ggplot(data = nelson, aes(x = humidity, y = weightloss)) +
  geom_line()

# ## Можно использовать несколько геомов одновременно
ggplot(data = nelson, aes(x = humidity, y = weightloss)) +
  geom_point() +
  geom_line()

# ## Подписи осей, заголовок и т.д.
ggplot(data = nelson, aes(x = humidity, y = weightloss)) +
  geom_point() +
  labs(x = "Относительная влажность, %", y = "Потеря веса, мг",
       title = "Потеря веса мучных хрущаков \nпри разной влажности воздуха")

# ## Графики ggplot можно сохранять в переменные
gg_nelson <- ggplot(data = nelson, aes(x = humidity, y = weightloss)) +
  geom_point() +
  labs(x = "Относительная влажность, %", y = "Потеря веса, мг")
gg_nelson

# ## Темы оформления графиков можно менять и настраивать
gg_nelson + theme_classic()

# ## Можно установить любимую тему для всех последующих графиков
theme_set(theme_bw())
gg_nelson

# ## Графики можно сохранять в файлы
ggsave(filename = "bugs_weightloss.png", plot = gg_nelson)
ggsave(filename = "bugs_weightloss.pdf", plot = gg_nelson)




# # Корреляция ##################################

# ### Коэффициент корреляции Пирсона
p_cor <- cor.test(nelson$humidity, nelson$weightloss,
         alternative = "two.sided", method = "pearson")
p_cor


# ## Линейная регрессия ##########################
# # Линейная регрессия в R

# ## Подбираем параметры линейной модели
nelson_lm <- lm(weightloss ~ humidity, nelson)
summary(nelson_lm)


# ## Записываем уравнение линейной регрессии
# Коэффициенты модели:
coef(nelson_lm)

# # Тестирование значимости модели и ее коэффициентов

# ## Тестируем значимость коэффициентов t-критерием
summary(nelson_lm)


# ## Тестируем значимость модели целиком при помощи F-критерия
library(car)
nelson_aov <- Anova(nelson_lm, type = 3)
summary(nelson_aov)


# # График линейной регрессии ####################

gg_nelson + geom_smooth(method = "lm") +
  labs (title = "95% доверительная зона регрессии")

gg_nelson + geom_smooth(method = "lm", level = 0.99) +
  labs (title = "99% доверительная зона регрессии")


# # Оценка качества подгонки модели ##############
#
# ## Коэффициент детерминации
summary(nelson_lm)

# ## Использование линейной регрессии для предсказаний ########

# Для конкретного значения предиктора мы можем сделать два типа предсказаний
# - предсказываем среднее значение отклика --- это оценка точности положения линии регрессии
# - предсказываем значение отклика у 95% наблюдений --- это оценка точности предсказаний

# ## Предсказываем Y при заданном X ##############

# Какова средняя потеря веса при заданной влажности?
newdata <- data.frame(humidity = c(50, 100)) # значения, для которых предсказываем
(pr1 <- predict(nelson_lm, newdata, interval = "confidence", se = TRUE))


# ## Предсказываем изменение Y для 95% наблюдений при заданном X ######

# В каких пределах находится потеря веса у 95% жуков при заданной влажности?

newdata <- data.frame(humidity = c(50, 100)) # новые данные для предсказания значений
(pr2 <- predict(nelson_lm, newdata, interval = "prediction", se = TRUE))


# ## Данные для доверительной области значений

# Предсказанные значения для исходных данных объединим с исходными данными в новом датафрейме - для графиков
pr_all <- predict(nelson_lm, interval = "prediction")
# head(pr_all)
nelson_with_pred <- data.frame(nelson, pr_all)
# head(nelson_with_pred)

# ## Строим доверительную область значений и доверительный интервал одновременно
gg_nelson +
  geom_smooth(method = "lm",
              aes(fill = "Доверительный \nинтервал"),
              alpha = 0.4) +
  geom_ribbon(data = nelson_with_pred,
              aes(y = fit,
                  ymin = lwr,
                  ymax = upr,
                  fill = "Доверительная \nобласть значений"),
              alpha = 0.2) +
  scale_fill_manual('Интервалы',
                    values = c('green', 'blue'))

