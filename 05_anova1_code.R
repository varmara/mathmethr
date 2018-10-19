# title: "Дисперсионный анализ, часть 1"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# ## Пример: сон у млекопитающих #################
#
# Известно, что у разных млекопитающих
# продолжительность сна сильно варьирует. Ленивцы
# спят , коалы спят, а  кому-то достаточно.
# Условия жизни у всех тоже разные. Давайте
# проверим, есть ли связь между продолжительностью
# сна и уровнем опасности среды.
#
# - `TotalSleep` - общая продолжительность сна. В
# нашем анализе это будет зависимая переменная
# - `Danger`  - уровень опасности среды для вида,
# пять градаций (1 - 5)
#
# Данные: Allison, Cicchetti (1976), электронная
# версия [Statlib database](http://lib.stat.cmu.edu)

# ## Читаем данные из файла одним из способов

# ### Чтение из xlsx
library(readxl)
sleep <- read_excel(path = "data/sleep.xlsx", sheet = 1)

# ### Чтение из csv
sleep <- read.table("data/sleep.csv", header = TRUE, sep = "\t")

# ## Все ли правильно открылось?
str(sleep) # Структура данных
head(sleep, 2)     # Первые несколько строк файла

# Сделаем sleep$Danger фактором
sleep$Danger <- factor(sleep$Danger, levels = 1:5, labels = c("очень низкий", "низкий", "средний", "высокий", "очень высокий"))

# Есть ли пропущенные значения (особенно, в
# переменных, которые нас интересуют)?
colSums(is.na(sleep))

# Каков объем выборки?
# В одной из переменных, которые нам интересны,
# есть пропущенные значения. Это нужно учесть при
# рассчете объема выборки.
#
# Удалим из датафрейма `sleep` строки, где
# `TotalSleep` принимает значение `NA`.
sl <- sleep[!is.na(sleep$TotalSleep), ]

# Дальше будем работать с датафреймом `sl`. В нем
# нет пропущенных значений в интересующих нас
# переменных
nrow(sl)

# Каков объем выборки в каждой группе?
table(sl$Danger)


# ## Задание 1 ------------------------------------
#
# Дополните код, чтобы построить точечный график
# зависимости общей продолжительности сна
# (`TotalSleep`) от уровня опасности среды
# (`Danger`).

library( )
theme_set(theme_bw())

ggplot(data = , aes(x = , y = )) +
  geom_ (position = position_jitter(width = 0.05))


# ## Задание 2 -----------------------------------
#
# Точечный график --- не самый удобный способ
# представления таких данных. Лучше было бы
# изобразить средние значения и их 95%
# доверительные интервалы.
#
# Замените в графике из прошлого задания геом на этот стат:

stat_summary(geom = 'pointrange', fun.data = mean_cl_normal)



# ## Задание 3 -----------------------------------
#
# Раскрасьте график в зависимости от уровня
# опасности среды (используйте эстетику `colour`)





# ## Задание 4 -----------------------------------
#
# Создайте подписи к осям и легенде, при помощи
# слоя подписей labs(). Отредактируйте этот код и
# добавьте его к графику с предыдущего шага

labs(x = ,  = "Продолжительность сна", colour = )






# # Дисперсионный анализ


# ## Дисперсионный анализ в R

library(car)

sl_mod <- lm(TotalSleep ~ Danger, data = sl)

sl_anova <- Anova(sl_mod)

sl_anova

# ## Задание: Проверьте условия применимости
#
# Проверьте условия применимости дисперсионного
# анализа для модели `sl_mod`, Дополните код,
# чтобы построить графики остатков

# Данные для анализа остатков
sl_diag <- fortify()
head(sl_diag)
# График расстояния Кука
ggplot(data = , aes(x = 1:nrow(   ), y = )) +
  geom_bar(stat = "identity")
# График остатков от предсказанных значений
ggplot(data = , aes(x = , y = .stdresid)) +
  geom_
# Квантильный график остатков
qqPlot



# Паттерны в остатках (графики остатков от переменных в модели и вне ее). ------

# Постройте графики, используя этот код. Какие из
# переменных хорошо было бы добавить в модель?

sl_diag_full <- data.frame(sl_diag, sl)

gg_other <- ggplot(sl_diag_full, aes(y = .stdresid)) +
  geom_hline(yintercept = 0)

gg_other + geom_point(aes(x = log(BodyWt)))
gg_other + geom_point(aes(x = log(BrainWt)))
gg_other + geom_point(aes(x = NonDreaming))
gg_other + geom_point(aes(x = Dreaming))
gg_other + geom_point(aes(x = log(LifeSpan)))
gg_other + geom_point(aes(x = Gestation))
gg_other + geom_point(aes(x = Predation))
gg_other + geom_point(aes(x = Exposure))



# # Post hoc тесты

# ## Пост-хок тест Тьюки в R

library(multcomp)
sl_pht <- glht(sl_mod, linfct = mcp(Danger = "Tukey"))

# ## Результаты попарных сравнений (тест Тьюки)
summary(sl_pht)


# График предсказаний модели ---------------------

# ## Данные для графика при помощи `predict()`
MyData <- data.frame(Danger = levels(sl$Danger))
MyData$Danger <- factor(MyData$Danger, levels = levels(sl$Danger),
                        labels = levels(sl$Danger))
Predictions <- predict(sl_mod, newdata = MyData,
                      interval = "confidence")
MyData <- data.frame(MyData, Predictions)
MyData

# ## Задание 5 -----------------------------------
#
# Используя данные из датафрейма MyData, дополните
# код и постройте график? где разными столбиками с
# заливкой оранжевого цвета будут показаны
# предсказанные средние значения, а усами будут
# показаны их доверительные интервалы.

gg_means <- ggplot(data = , aes(x = , y = )) +
  geom_bar(stat = "", width = 0.5) +
  geom_errorbar(aes(ymin = , ymax = ), width = 0.1) +
  labs()
gg_means




# ## Достоверно различающиеся по пост-хок тесту
# группы обозначим разными буквами
gg_means +
  geom_text(aes(label = c("A", "A", "A", "AB", "B"),
                vjust = -0.3, hjust = 1.5), size = 6)

