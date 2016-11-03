#' title: "Дисперсионный анализ, часть 2"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"

#' # Многофакторный дисперсионный анализ в R

#' ## Пример: Возраст и память
#' Почему пожилые не так хорошо запоминают? Может быть не так тщательно перерабатывают информацию? (Eysenck, 1974)
#' Факторы:
#' - `Age` - Возраст:
#'     - `Younger` - 50 молодых
#'     - `Older` - 50 пожилых (55-65 лет)
#' - `Process` - тип активности:
#'     - `Counting` - посчитать число букв
#'     - `Rhyming` - придумать рифму к слову
#'     - `Adjective` - придумать прилагательное
#'     - `Imagery` - представить образ
#'     - `Intentional` - запомнить слово
#' Зависимая переменная - `Words` - сколько вспомнили слов
#' Пример из http://www.statsci.org/data/general/eysenck.html

memory <- read.delim(file = "data/eysenck.csv")
head(memory, 10)

#' Задание:
#' Постройте боксплот для зависимости числа слов в зависимости от возраста и способа запоминания





# переставляем в порядке следования средних значений memory$Words
memory$Process <- reorder(memory$Process, memory$Words, FUN = mean)


#' ## Подбираем линейную модель
memory_fit <- lm(formula = Words ~ Age * Process, data = memory,
contrasts = list(Age = contr.sum, Process = contr.sum))

#' ## Задание
#' Проверьте условия применимости дисперсионного анализа
#' - Есть ли гомогенность дисперсий?
#' - Не видно ли паттернов в остатках?
#' - Нормальное ли у остатков распределение?




#' ## Результаты дисперсионного анализа
library(car)
Anova(memory_fit, type = 3)

#' ## Пост хок тест
memory$AgeProc <- interaction(memory$Age, memory$Process)
cell_means <- lm(Words ~ AgeProc - 1, data = memory)
library(multcomp)
memory_tukey <- glht(cell_means, linfct = mcp(AgeProc = "Tukey"))
summary(memory_tukey)


#' ## Данные для графиков
process <- levels(memory$Process)
fprocess <- factor(process, levels = process)
MyData <- expand.grid(Age = levels(memory$Age),
                      Process = fprocess)
MyData <- data.frame(MyData,
  predict(memory_fit, newdata = MyData,
          interval = "confidence"))

#' ## Графики для результатов: Столбчатый график
pos <- position_dodge(width = 0.3)
gg_barp <- ggplot(data = MyData, aes(x = Process, y = fit,
            ymin = lwr,  ymax = upr, fill = Age)) +
  geom_bar(stat = "identity", position = pos, width = 0.3) +
  geom_errorbar(width = 0.1, position = pos)
gg_barp

#' ## Графики для результатов: Точки
gg_pointp <- ggplot(data = MyData, aes(x = Process, y = fit,
              ymin = lwr,  ymax = upr, colour = Age)) +
  geom_point(aes(shape = Age), size = 3, position = pos) +
  # geom_line(aes(group = Age), position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_pointp

#' ## Какой график лучше выбрать?
library(gridExtra)
grid.arrange(gg_barp + theme(legend.position = "bottom"),
             gg_pointp + theme(legend.position = "bottom"),
             ncol = 2)


#' ## Приводим понравившийся график в приличный вид
#' Например, этот
gg_final <- gg_pointp +
  labs(y = "Число слов") +
   scale_colour_brewer(name = "Возраст", palette = "Dark2",
                      labels = c("Пожилые", "Молодые")) +
  scale_shape_discrete(name = "Возраст",
                      labels = c("Пожилые", "Молодые")) +
  scale_x_discrete(name = "Процесс", palette = "Dark2",
                      labels = c("Счет", "Рифма",
                                 "Прилагательное", "Образ", "Запоминание"))

gg_final


#' ## Задание: Примеры фиксированных и случайных факторов
#' Опишите ситуации, когда эти факторы будут фиксированными, а когда  случайными
#' - Несколько произвольно выбранных градаций плотности моллюсков в полевом эксперименте, где плотностью манипулировали.
#' - Фактор размер червяка (маленький, средний, большой) в выборке червей.
#' - Деление губы Чупа на зоны с разной степенью распреснения.
