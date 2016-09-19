# title       : Дисперсионный анализ, часть 1
# subtitle    : Математические методы в зоологии - на R, осень 2015
# author      : Марина Варфоломеева



# install.packages(c("car", "multcomp", "dplyr"))

## Пример: сон у млекопитающих
library(readxl)
sleep <- read_excel("sleep.xlsx", sheet = 1)
head(sleep, 2)
# Сделаем sleep$Danger фактором
sleep$Danger <- factor(sleep$Danger, levels = 1:5, labels = c("очень низкий", "низкий", "средний", "высокий", "очень высокий"))

## Задание: Постройте график
# Постройте график зависимости общей продолжительности сна от уровня опасности среды. Какой геом лучше подойдет для изображения (`geom_point` или `geom_boxplot`)?
# Раскрасьте график в зависимости от уровня опасности среды (используйте эстетики `fill` или `colour`)
# Придумайте, каким образом посчитать, в какой группе животных общая продолжительность сна больше?







## Дисперсионный анализ в R
library(car)
mod <- lm(TotalSleep ~ Danger, data=sleep)
sleep_anova <- Anova(mod)
sleep_anova

## Вопрос:
# Назовите условия применимости дисперсионного анализа







## Задание: Проверьте условия применимости дисперсионного анализа








## Немного более удобный квантильный график
qqPlot(mod)




# Post hoc тесты
library(multcomp)
sleep_pht <- glht(mod, linfct = mcp(Danger = "Tukey"))
summary(sleep_pht)



# Графическое представление результатов пост-хок теста
## Посчитаем описательную статистику по группам
library(dplyr) # есть удобные функции для описания данных
sleep_summary <- sleep %>% # берем датафрейм sleep
  group_by(Danger) %>% # делим на группы по Danger
  # по каждой группе считаем разное
  summarise(
    .n = sum(!is.na(TotalSleep)),
    .mean = mean(TotalSleep, na.rm = TRUE),
    .sd = sd(TotalSleep, na.rm = TRUE),
    upper_cl = .mean + 1.98*.sd,
    lower_cl = .mean - 1.98*.sd
  )
sleep_summary


## Этот график можно использовать для представления результатов
gg_means <- ggplot(sleep_summary, aes(x = Danger, y = .mean)) +
  geom_bar(stat = "identity", fill = "turquoise3", colour = "black", width = 0.5) +
  geom_errorbar(aes(ymin = lower_cl, ymax = upper_cl), width = 0.1) +
  labs(x = "Обработка", y = "Вес, г") +
  geom_text(aes(label = c("A", "A", "A", "AB", "B"), vjust = -0.3, hjust = 1.5), size = 6)
gg_means


## Можно "опустить" прямоугольники на ось х
upperlimit <- max(sleep_summary$upper_cl + 1)
gg_means +
  scale_y_continuous(expand = c(0,0),
    limit = c(0, upperlimit))



## Сохраняем таблицу дисперсионного анализа в файл одним из способов

# 1) в csv
write.csv(sleep_anova, file = "medley_res.csv")

# 2) в xls или xlsx с помощью XLConnect
# install.packages("XLConnect")
# Для его установки нужна Java и пакет rJava
# library(XLConnect)
# writeWorksheetToFile(data = sleep_anova, file = "medley_res.xls", sheet = "anova_table")

# 3) отправляем в буфер обмена (только Windows) для вставки в Word-Excel
write.table(file = "clipboard", x = sleep_anova, sep = "\t")
