#task2
install.packages('np')
install.packages('scatterplot3d')
install.packages('fANCOVA')
install.packages('Hmisc')
install.packages('corrplot')

library(np)

#1, 2.1
# датасет
head(LifeCycleSavings)

# построим матрицу значений среднеквадратичных ошибок для разных методов для 
# всех четырех объясняющих переменных
M = data.frame('var' = NULL, 'bw_ker' = NULL, 'MSE' = NULL)

y = LifeCycleSavings$sr
bws = c("cv.ls", "cv.aic")
kernels = c("gaussian","epanechnikov")

# заполняем матрицу

for (i in 2:5){
  x = LifeCycleSavings[, i]
  for (bwmethod in bws){
    for (ker in kernels){
      pair = sprintf("%s_%s", bwmethod, ker)
      colname = colnames(LifeCycleSavings[i])
      model = npreg(txdat = x, tydat = y, bwmethod = bwmethod, ker = ker)
      MSE = mean((y - fitted(model)) ** 2)
      M = rbind(M, data.frame('var' = colname, 'bw_ker' = pair, 'MSE' = MSE))
    }
  }
}
M['pair_number'] = rep(c(1, 2, 3, 4), 4)
M['color'] = c(rep('red', 4), rep('green', 4), rep('purple', 4), rep('pink', 4))

# визуализируем результаты

plot(M$pair_number, M$MSE, col = M$color, xaxp = c(-1, -1, 1),
     main = 'MSE for different model parameters', cex.main = 2, 
     xlab = 'bw and kernel combination', ylab = 'MSE', lwd = 12, cex.lab = 1.5)
for (v in M$var[c(4, 8, 12, 16)]){
  min_val = min(M[M$var == v, ]$MSE)
  M_min = M[M$MSE == min_val, 3:4]
  points(M_min$pair_number, M_min$MSE, col = 'black')
}

legend(col = c(M$color[c(4, 8, 12, 16)], 'black'), x = "center", cex = 1.5,
       legend = c(M$var[c(4, 8, 12, 16)], 'min value'), pch = c(rep(16, 4), 1))
axis(1, labels = M$bw_ker[1:4], at = 1:4, padj = 0.3)

# Как видно, метод выбора ядра не влияет на среднеквадратичную ошибку, в то время как 
# для 3 из 4 переменных лучшим в смысле СКО методом выбора параметра bandwidth оказался 
# обобщённый метод кросс-проверки cv.ls

#2.2
# Выберем 2 переменные, которые наилучшим образом объясняют коэффициент 
# персональных сбережений

# посмотрим на корреляцию величин в датасете
library(corrplot)
library(Hmisc)
library(dplyr)

matr = rcorr(as.matrix(LifeCycleSavings))
par(mfcol = c(1, 1), pty = 'm', mar = c(1, 1, 1, 1))
corrplot(matr$r, p.mat=as.matrix(mutate_all(as.data.frame(matr$P), ~if_else(is.na(.), 0, .))))

# Проверим вывод линейной модели для всех переменных
L_all = lm(sr ~ ., data = LifeCycleSavings)
summary(L_all)

# Выбираем pop15 и ddpi в качестве двух переменных, которые наилучшим образом объясняют 
# коэффициент персональных сбережений. Во-первых, это две значимые переменные в линейной 
# регрессии. Во-вторых, значимая корреляция есть между sr и тремя переменными (pop75, pop15 
# и ddpi), но две из них сами между собой очень сильно коррелируют (pop75, pop15), поэтому 
# из них выбираем более значимую (pop15) и оставшуюся ddpi. Также по результатам вычисления 
# MSE pop75 показала в среднем худший результат.

#3
library(fANCOVA)

set.seed(888)
V1 = 2
V2 = 5

dt = sort(sample(nrow(LifeCycleSavings), nrow(LifeCycleSavings) * 0.8))
train = LifeCycleSavings[dt, c(1, V1, V2)]
test = LifeCycleSavings[-dt, c(1, V1, V2)]

# построим модели
L_2 = lm(sr ~ ., data = train)
Loess = loess.as(train[, 2:3], train$sr, criterion = "gcv")

# оценим параметры модели на обучающем наборе данных

sprintf("MSE на обучающей выборке для линейной модели равен %.2f", 
        mean((train$sr - fitted(L_2)) ** 2))
sprintf("MSE на обучающей выборке для модели LOESS равен %.2f", 
        mean((train$sr - fitted(Loess)) ** 2))

# проверим качество моделей на тестовом наборе данных

sprintf("MSE на тестовой выборке для линейной модели равен %.2f", 
        mean((train$sr - predict(L_2, test[, 2:3], type = 'response')) ** 2))
colnames(test) = c('sr', 'x1', 'x2')
sprintf("MSE на тестовой выборке для модели LOESS равен %.2f", 
        mean((train$sr - predict(Loess, test[, 2:3], type = 'response')) ** 2))

# выясняем, какая из построенных моделей является более точной

# И на обучающей, и на тестовой выборке лучшее качество по MSE показала модель 
# LOESS. Однако стоит иметь в виду, что выборка у нас достаточно маленькая, и
# и для другого разбиения результат мог получиться совсем другой.