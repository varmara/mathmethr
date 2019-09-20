# ---
# title: "Регрессионный анализ, часть 1"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# ## Пример: стерильность пыльцы гибридов
#
# Гибриды отдаленных видов часто бывают стерильны.
# Но насколько они должны быть разными для этого?
# Как зависит плодовитость гибридов
# [смолевок](http://ru.wikipedia.org/wiki/Смолёвка)
# _Silene vulgaris_ от генетической удаленности?

# - `proportionSterile` --- доля стерильных пыльцевых зерен
# - `geneticDistance` --- генетическая удаленность видов
# Moyle et al. 2004; данные из Whitlock, Schluter, 2015, глава 17, упр.10; Данные в файлах HybridPollenSterility.xlsx и HybridPollenSterility.csv


# ## Читаем данные из файла #######################################

# Чтение из xlsx:
library(readxl)
hybrid <- read_excel(path = 'data/HybridPollenSterility.xlsx', sheet = 1)
head(hybrid)     # Первые несколько строк файла

# Или чтение из csv:
hybrid <- read.table(file = 'data/HybridPollenSterility.csv', header = TRUE, sep = ',')
head(hybrid)     # Первые несколько строк файла

# ## Знакомимся с данными ###################################

str(hybrid)      # Структура данных

# ## Сделаем более короткие имена
# Сейчас переменные называются так:
colnames(hybrid)
# Сделаем более удобные короткие названия:
colnames(hybrid) <- c('Distance', 'Sterile')
# Теперь переменные стали называться так:
colnames(hybrid)


# Есть ли пропущенные значения?
colSums(is.na(hybrid))

# Каков объем выборки?
nrow(hybrid)


# # Графики средствами пакета ggplot2 ###############################
# ## Грамматика графиков

# 1. Откуда брать данные?
# 2. Какие переменные изображать на графике?
# 3. В виде чего изображать?
# 4. Какие подписи нужны?
# 5. Какую тему оформления нужно использовать?




# Давайте поэтапно построим график
library(ggplot2)


# - `ggplot()` --- создает пустой "базовый" слой --- основу графика
ggplot()

# 1. Откуда брать данные?
ggplot(data = hybrid)

# 2. Какие переменные изображать на графике?
# Эстетики --- это свойства будущих элементов графика, которые будут изображать данные

ggplot(data = hybrid, aes(x = Distance, y = Sterile))

# 3. В виде чего изображать?
# Геомы --- графические элементы

ggplot(data = hybrid, aes(x = Distance, y = Sterile)) +
  geom_point()

# 4. Какие подписи нужны?
ggplot(data = hybrid, aes(x = Distance, y = Sterile)) +
  geom_point() +
  labs(x = 'Генетическое расстояние', y = 'Доля стерильных \nпыльцевых зерен')

# Графики ggplot можно сохранять в переменные
gg_hybrid <- ggplot(data = hybrid, aes(x = Distance, y = Sterile)) +
  geom_point() +
  labs(x = 'Генетическое расстояние', y = 'Доля стерильных \nпыльцевых зерен')
gg_hybrid

# 5. Какую тему оформления нужно использовать?
# `theme()` --- меняет отдельные элементы (см. справку)
# `theme_bw()`, `theme_classic()` и т.д. --- стили оформления целиком
gg_hybrid + theme_classic()

# ## Можно установить любимую тему для всех последующих графиков
theme_set(theme_bw())
gg_hybrid

# ## Графики можно сохранять в файлы
ggsave(filename = 'hybrids_Sterile.png', plot = gg_hybrid)
ggsave(filename = 'hybrids_Sterile.pdf', plot = gg_hybrid)


# # Корреляция ####################################################


# ## Есть ли связь между переменными?
gg_hybrid

# ## Можно посчитать корреляцию между долей стерильной пыльцы и генетическим расстоянием
p_cor <- cor.test(hybrid$Distance, hybrid$Sterile)
p_cor




# # Линейная регрессия ##############################################


# ## Подбираем параметры линейной модели
hybrid_lm <- lm(Sterile ~ Distance, hybrid)
summary(hybrid_lm)

# ## Записываем уравнение линейной регрессии




# # Тестирование значимости модели и ее коэффициентов ###########################

# Два эквивалентных варианта (не надо использовать оба)

# ## Тестируем значимость коэффициентов t-критерием
summary(hybrid_lm)


# ## Тестируем значимость модели целиком при помощи F-критерия
library(car)
Anova(hybrid_lm)



# # График линейной регрессии ##################################

# ## Строим доверительную зону регрессии
gg_hybrid + geom_smooth(method = 'lm') +
  labs(title = '95% доверительная зона регрессии')

# # Оценка качества подгонки модели #############################

# ## Коэффициент детерминации $R^2$ можно найти в сводке модели
summary(hybrid_lm)


# # Использование линейной регрессии для предсказаний ####################


# ## Предсказываем Y при заданном X

# Какова доля стерильной пыльцы межвидового
# гибрида, если генетическое расстояние между
# родителями 0.07 или 0.055?
newdata <- data.frame(Distance = c(0.07, 0.055)) # значения, для которых предсказываем
(pr1 <- predict(hybrid_lm, newdata, interval = 'confidence', se = TRUE))


# ## Предсказываем изменение Y для 95% наблюдений при заданном X

# В каких пределах находится доля стерильной
# пыльцы, если генетическое расстояние между
# родителями 0.07 или 0.055?
newdata <- data.frame(Distance = c(50, 100)) # новые данные для предсказания значений
(pr2 <- predict(hybrid_lm, newdata, interval = 'prediction', se = TRUE))


# ## Данные для доверительной области значений
# Предсказанные значения для исходных данных
# объединим с исходными данными в новом датафрейме
# - для графиков
(pr_all <- predict(hybrid_lm, interval = 'prediction'))
hybrid_with_pred <- data.frame(hybrid, pr_all)


# ## Строим доверительную область значений и доверительный интервал одновременно
gg_hybrid +
  geom_smooth(method = 'lm',
              aes(fill = 'Доверительный \nинтервал'),
              alpha = 0.4) +
  geom_ribbon(data = hybrid_with_pred,   # внимание, в этом слое используются данные предсказаний.
              aes(y = fit, ymin = lwr, ymax = upr,
                  fill = 'Доверительная \nобласть значений'),
              alpha = 0.2) +
  scale_fill_manual('Интервалы', values = c('green', 'blue'))




#  ####################
# gg_hybrid +
#   geom_smooth(method = 'lm', aes(fill = 'Доверительный \nинтервал'), alpha = 0.4) +
#   geom_ribbon(data = hybrid_with_pred, aes(ymin = lwr, ymax = upr, fill = 'Доверительная \nобласть значений'), alpha = 0.2) +
#   scale_fill_manual('Интервалы', values = c('green', 'blue')) +
#   geom_hline(yintercept = 1, linetype = 'dashed', colour = 'red3') +
#   geom_hline(yintercept = 0, linetype = 'dashed', colour = 'red3') +
#   annotate('polygon', x = c(-Inf, -Inf, Inf, Inf), y = c(1, Inf, Inf, 1), alpha = 0.2, fill = 'red') +
#   annotate('polygon', x = c(-Inf, -Inf, Inf, Inf), y = c(0, -Inf, -Inf, 0), alpha = 0.2, fill = 'red') +
#   annotate('text', label = 'Так не бывает! Нужна другая модель.', x = 0.06, y = 1.25)
