# task 1
#1
library(ggplot2)
library(data.table)
library(mltools)
# загрузка данных
df = read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
head(df)

# есть столбeц id, который нам не дает никакой полезной информации в рамках 
# данной задачи
df = subset(df, select=-id)
dim(df) #200 записей

unique(df$num_awards) # 0 1 3 2 5 4 6
unique(df$prog)       # 3 1 2
# сделаем эту переменную категориальной
df$prog = factor(df$prog)
ggplot(df, aes(prog)) +
  geom_bar() +
  ggtitle("Гистограмма для признака 'тип программы'")

summary(df$math)      
# минимум - 33, максимум - 75, отрицательных значений нет

# строим обобщенную линейную модель
G = glm(num_awards ~.,data=df, family="poisson")

# проверим значимость факторов
summary(G)

# признак prog3 оказался незначимым. Правда, если просто удалить его, то
# может пропасть информация о первом признаке. 

# модель, где просто уберем столбец с признаком prog3
ohdf = one_hot(as.data.table(df), cols = 'prog')
ohdf = subset(ohdf, select=-prog_3)
G_trucated = glm(num_awards ~.,data=ohdf, family="poisson")
summary(G_trucated)

# убираем и prog_1
ohdf2 = subset(ohdf, select=-prog_1)
G_trucated2 = glm(num_awards ~.,data=ohdf2, family="poisson")
summary(G_trucated2)
# теперь все признаки значимые.

# сравним с моделью, где попробуем перенумеровать признаки
df$prog = as.numeric(as.character(df$prog))
df$prog[df$prog == 1] = 4
df$prog = factor(df$prog)
G_changed = glm(num_awards ~.,data=df, family="poisson")
summary(G_changed)
# теперь все признаки значимые. 
# осталось переопределить кодировку: 
# 4 - профессиональная (прикладная), 2 - общая, 3 - академическая

# показатели моделей практически идентичны.

#2
# Протестируем гипотезу о том, что модель на самом деле является тривиальной 
# (то есть с одинаковым значением параметра в каждой точке)

# берем показатели для нашей модели и вычитаем из них показатели для тривиальной 
# модели (null model). со степенями свободы наоборот.

# У нас есть 2 модели, сравним их результаты

pchisq(G_changed$null.deviance - G_changed$deviance, 
       df = G_changed$df.null - G_changed$df.residual)
# получили 1, то есть отклоняем гипотезу о том, что модель является тривиальной.

pchisq(G_trucated2$null.deviance - G_trucated2$deviance, 
       df = G_trucated2$df.null - G_trucated2$df.residual)
# получили 1, то есть отклоняем гипотезу о том, что модель является тривиальной.
# остановим выбор в итоге на усеченной модели, так как она проще, и AIC у нее 
# немного меньше. А в целом это прекрасно, что при помощи некоторой перестановки 
# можно сделать фактор значимым на некотором уровне.

#3
# функция, возвращающая по заданным значениям типа программы и балла за финальный 
# экзамен вероятности получения 0, 1, 2, ..., 6 наград (сколько наград получит
# студент с данной программы и данным баллом за финальный экзамен)

f = function(prog, math){
  data = data.frame(prog_2 = as.numeric(prog == 2), math = max(min(math, 100), 0))
  as.numeric(round(predict(G_trucated2, data, type = "response")))
}

f(1, 10)  # 0
f(2, 50)  # 1
f(3, 80)  # 2
f(1, 87)  # 3
f(2, 80)  # 4
f(2, 83)  # 5
f(3, 98)  # 6

# заодно проверим значения, которые прогнозирует модель на исходных данных
z = predict(G_trucated2, ohdf2, type="response")
res = as.data.frame(cbind(z, round(z), ohdf$num_awards))
res['diff'] = res$V2 - res$V3
length(which(res$diff !=0)) / length(df) 
# ошиблись с количеством наград в почти в 25% случаев

