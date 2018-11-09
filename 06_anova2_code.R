# ---
# title: "Дисперсионный анализ, часть 2"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# # Пример: Возраст и способы запоминания ############################
# Какие способы запоминания информации лучше
# работают для молодых и для пожилых? (Eysenck,
# 1974)
# Факторы:
# - `Age` - Возраст:
#     - `Younger` - 50 молодых
#     - `Older` - 50 пожилых (55-65 лет)
# - `Process` - тип активности:
#     - `Counting` - посчитать число букв
#     - `Rhyming` - придумать рифму к слову
#     - `Adjective` - придумать прилагательное
#     - `Imagery` - представить образ
#     - `Intentional` - запомнить слово
# Зависимая переменная - `Words` - сколько вспомнили слов
# Пример из http://www.statsci.org/data/general/eysenck.html

# ## Открываем данные
memory <- read.table(file = "data/eysenck.csv", header = TRUE, sep = "\t")
# Все ли правильно открылось?
str(memory) # Структура данных
head(memory, 2) # Первые несколько строк файла

# Есть ли пропущенные значения
# (особенно, в переменных, которые нас интересуют)?
colSums(is.na(memory))
# Каков объем выборки?
nrow(memory) # всего
table(memory$Age, memory$Process) # в группах



# ## Задание 1 -----------------------------------------------------------
#
# Дополните код, чтобы построить график, на
# котором приведено среднее число слов (`Words`)
# для каждого возраста (`Age`) и способа
# запоминания (`Process`).

library()
theme_set()
ggplot(data = , aes()) +
  stat_summary(geom = '', fun.data = ,
               position = position_dodge(width = 0.5))




# Порядок уровней в факторах ###########################################

# ## Изменим порядок уровней в факторе
# `memory$Process` так, чтобы он соответствовал
# возрастанию средних значений `memory$Words`

# "старый" порядок уровней
levels(memory$Process)
# переставляем уровни в порядке следования средних значений memory$Words
memory$Process <- reorder(x = memory$Process, X = memory$Words, FUN = mean)
# "новый" порядок уровней стал таким
levels(memory$Process)

# ## График с новым порядком уровней
ggplot(data = memory, aes(x = Age, y = Words, colour = Process)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal,
               position = position_dodge(width = 0.5))#



# # Двухфакторный дисперсионный анализ ###################################

# ## Задаем модель со взаимодействием в R
# Взаимодействие обозначается `:` --- двоеточием
# Если есть факторы A и B, то их взаимодействие A:B


# ## Задание 2 ----------------------------------------------------------
#
# Дополните этот код, чтобы подобрать линейную
# модель со взаимодействием факторов, в которой
# используется нужный тип кодирования для
# факторов: `contrasts = list(Age = contr.sum,
# Process = contr.sum)`

# Линейная модель дисперсионного анализа со взаимодействием факторов
mem_mod <- lm(formula = , data = ,
contrasts = list(Age = contr.sum, Process = contr.sum))



# ## Проверка условий применимости #####################
# - Есть ли гомогенность дисперсий?
# - Не видно ли паттернов в остатках?
# - Нормальное ли у остатков распределение?

# ## Данные для анализа остатков
mem_diag <- fortify(mem_mod)
head(mem_diag)

# ## График расстояния Кука
ggplot(data = mem_diag, aes(x = 1:nrow(mem_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# ## Графики остатков от предикторов в модели (и не в модели, если есть)
ggplot(data = mem_diag, aes(x = Age, y = .stdresid)) +
  geom_boxplot()
ggplot(data = mem_diag, aes(x = Process, y = .stdresid)) +
  geom_boxplot()
ggplot(data = mem_diag, aes(x = Process, y = .stdresid, colour = Age)) +
  geom_boxplot()

# ## Квантильный график остатков
library(car)
qqPlot(mem_mod)


# ## Результаты дисперсионного анализа
Anova(mem_mod, type = 3)




# Пост хок тест для взаимодействия факторов ##############################
# Делается легче всего "обходным путем".

# ## Задание 3 -----------------------------------------------------------
#
# Дополните этот код, чтобы посчитать пост хок
# тест Тьюки по взаимодействию факторов.

# 1. Создаем переменную-взаимодействие
memory$AgeProc <- interaction(memory$Age, memory$)

# 2. Подбираем линейную модель зависимости переменной-отклика
# от переменной-взаимодействия, но без свободного члена
cell_means <- (Words ~ AgeProc - 1, data = )

# 3. Делаем пост хок тест для этой модели
library(multcomp)
memory_tukey <- glht(model = , linfct = mcp())
summary(memory_tukey)


# График предсказаний модели #############################################

# ## Данные для графиков

# Создаем все сочетания значений факторов при помощи expand.grid()
MyData <- expand.grid(
  Age = levels(memory$Age),
  # т.к. мы меняли порядок уровней для фактора Process, нужно это сохранить:
  Process = factor(levels(memory$Process), levels = levels(memory$Process)))
# Получаем предсказания для всех сочетаний значений факторов:
MyData <- data.frame(
  MyData,
  predict(mem_mod, newdata = MyData, interval = "confidence"))
head(MyData)

# График предсказаний модели
pos <- position_dodge(width = 0.3)
gg_pointp <- ggplot(data = MyData, aes(x = Process, y = fit, colour = Age)) +
  geom_point(aes(shape = Age), size = 3, position = pos) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1, position = pos)
gg_pointp

# Приводим график в приличный вид
gg_final <- gg_pointp +
  scale_colour_brewer(name = "Возраст", palette = "Dark2", labels = c("Пожилые", "Молодые")) +
  scale_shape_discrete(name = "Возраст", labels = c("Пожилые", "Молодые")) +
  scale_x_discrete(name = "Процесс", labels = c("Счет", "Рифма", "Прилагательное", "Образ", "Запоминание")) +
  labs(y = "Число слов")
gg_final
