# title       : Дисперсионный анализ, часть 2
# subtitle    : Математические методы в зоологии - на R, осень 2015
# author      : Марина Варфоломеева

## Пример: Возраст и память
memory <- read.delim(file="eysenck.csv")
head(memory, 10)

##   Посмотрим на боксплот
library(ggplot2)
theme_set(theme_bw() + theme(legend.key = element_blank()))
ggplot(data = memory, aes(x = Age, y = Words)) +
  geom_boxplot(aes(fill = Process))

# переставляем уровни в порядке следования средних значений memory$Words
memory$Process <- reorder(memory$Process, memory$Words, FUN=mean)
mem_p <- ggplot(data = memory, aes(x = Age, y = Words)) +
  geom_boxplot(aes(fill = Process))
mem_p


table(memory$Process)
table(memory$Age)
sum(is.na(memory))

table(memory$Process, memory$Age)

with(memory,
     table(Process, Age)
     )

## Подбираем линейную модель
# Внимание: при использовании III типа сумм квадратов, нужно при подборе линейной модели __обязательно указывать тип контрастов для факторов__. В данном случае - `contrasts=list(Age=contr.sum, Process=contr.sum)`
contr = list(Age=contr.sum, Process=contr.sum)
memory_fit <- lm(formula = Words ~ Age * Process, data = memory, contrasts = contr)

## Задание: Проверьте условия применимости дисперсионного анализа
library(car)
qqPlot(memory_fit)
op <- par(mfrow = c(2, 2))
plot(memory_fit)
par(op)


## Результаты дисперсионного анализа
Anova(memory_fit, type = 3)


## Пост хок тест по взаимодействию факторов
# 1. создаем переменную-взаимодействие
memory$AgeProc <- interaction(memory$Age, memory$Process)
# 2. подбираем модель без intercept
cell_means <- lm(Words ~ AgeProc - 1, data = memory)
# 3. делаем пост хок тест
library(multcomp)
memory_tukey <- glht(cell_means, linfct = mcp(AgeProc = "Tukey"))
options(width = 90)
summary(memory_tukey)



## Данные для графиков
# __Статистика по столбцам и по группам__ одновременно (n, средние,
# стандартные отклонения)
library(dplyr)
memory_summary <- memory %>%
  group_by(Age, Process) %>%
  summarise(
    .n = sum(!is.na(Words)),
    .mean = mean(Words, na.rm = TRUE),
    .sd = sd(Words, na.rm = TRUE),
    upr = .mean + 1.98*.sd,
    lwr = .mean - 1.98*.sd)

## Графики для результатов: Столбчатый график
pos <- position_dodge(width = 1)
mem_barp <- ggplot(data = memory_summary, aes(x = Age, y = .mean, ymin = lwr, ymax = upr, fill = Process)) +
  geom_bar(stat = "identity", position = pos) +
  geom_errorbar(width = 0.3, position = pos)
mem_barp


## Графики для результатов: Линии с точками
pos <- position_dodge(width = 0.5)
mem_linep <- ggplot(data = memory_summary, aes(x = Age, y = .mean, ymin = lwr, ymax = upr, colour = Process, group = Process)) +
  geom_point(size = 3, position = pos) +
  geom_line(position = pos) +
  geom_errorbar(width = 0.3, position = pos)
mem_linep

## Какой график лучше выбрать?
library(gridExtra)
grid.arrange(mem_barp, mem_linep, ncol = 1)


