#' title: "Дисперсионный анализ, часть 1"
#' subtitle: "Математические методы в зоологии с использованием R"
#' author: "Марина Варфоломеева"

#' ## Пример: сон у млекопитающих
#' - `TotalSleep` - общая продолжительность сна. В нашем анализе это будет зависимая переменная
#' - `Danger`  - уровень опасности среды для вида, пять градаций (1 - 5)
#' Данные: Allison, Cicchetti (1976), электронная версия [Statlib database](http://lib.stat.cmu.edu)

#' ## Скачиваем данные с сайта
#' Не забудьте войти в вашу директорию для матметодов при помощи `setwd()`
# library(downloader)
#
# # в рабочем каталоге создаем суб-директорию для данных
# if(!dir.exists("data")) dir.create("data")
#
# # скачиваем файл в xlsx, либо в текстовом формате
# if (!file.exists("data/sleep.xlsx")) {
#   download(
#     url = "https://varmara.github.io/mathmethr/data/sleep.xlsx",
#     destfile = "data/sleep.xlsx")
# }
#
# if (!file.exists("data/sleep.csv")) {
#   download(
#     url = "https://varmara.github.io/mathmethr/data/sleep.xls",
#     destfile = "data/sleep.csv")
# }


#' ## Читаем данные из файла одним из способов
library(readxl)
sleep <- read_excel(path = "data/sleep.xlsx", sheet = 1)
sleep <- read.table("data/sleep.csv", header = TRUE, sep = "\t")

#' ## Все ли правильно открылось?
str(sleep) # Структура данных
head(sleep, 2)     # Первые несколько строк файла

# Сделаем sleep$Danger фактором
sleep$Danger <- factor(sleep$Danger, levels = 1:5, labels = c("очень низкий", "низкий", "средний", "высокий", "очень высокий"))


#' ## Знакомимся с данными
sapply(sleep, function(x)sum(is.na(x)))

#' ## Каков объем выборки?
flt <- ! is.na(sleep$TotalSleep)
sl <- sleep[flt, ]
nrow(sl)
table(sl$Danger)

#' ## Задание
#' Постройте график зависимости общей продолжительности сна (`TotalSleep`) от уровня опасности среды (`Danger`). Используйте `geom_boxplot`.
#' Раскрасьте график в зависимости от уровня опасности среды (используйте эстетики `fill` или `colour`)
#' Придумайте, каким образом посчитать, в какой группе животных общая продолжительность сна больше?
#' ### Дополнительное задание:
#' Попробуйте сменить палитру раскраски, используя `scale_colour_brewer` (варианты можно посмотреть в справке в подразделе примеров или в интернете [Colors (ggplot2): раздел RColorBrewer palette chart](http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/) )

#' # Дисперсионный анализ
library(car)
sl_mod <- lm(TotalSleep ~ Danger, data=sl)
sl_anova <- Anova(sl_mod)
sl_anova


#' ## Задание: Проверьте условия применимости
#' Проверьте условия применимости дисперсионного анализа, используя графики остатков


#' # Post hoc тесты

library(multcomp)
sl_pht <- glht(sl_mod, linfct = mcp(Danger = "Tukey"))
summary(sl_pht)


#' ## Данные для графика при помощи `predict()`
MyData <- data.frame(Danger = levels(sl$Danger))
MyData$Danger <- factor(
  MyData$Danger,
  levels = c("очень низкий", "низкий", "средний", "высокий", "очень высокий"),
  labels = c("очень низкий", "низкий", "средний", "высокий", "очень высокий"))
MyData <- data.frame(MyData,
                     predict(sl_mod, newdata = MyData,
                             interval = "confidence"))
MyData


gg_means <- ggplot(MyData, aes(x = Danger, y = fit)) +
  geom_bar(stat = "identity", fill = "turquoise3", colour = "black", width = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  labs(x = "Уровень опасности", y = "Продолжительность сна, ч")
gg_means


#' ## Достоверно различающиеся по пост-хок тесту группы обозначим разными буквами
gg_means +
  geom_text(aes(label = c("A", "A", "A", "AB", "B"),
                vjust = -0.3, hjust = 1.5), size = 6)
