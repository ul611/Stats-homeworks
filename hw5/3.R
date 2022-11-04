#task 3
# линейная модель на всем датасете
L_all = lm(sr ~ ., data = LifeCycleSavings)
summary(L_all)
# p-value: 0.0007904

#a
# строим ящики с усами  отдельно для каждой из 5 переменных
for (i in 1:5){
  name = colnames(LifeCycleSavings[i])
  main = sprintf("Boxplot for %s variable", name)
  boxplot(LifeCycleSavings[, i], main=main, xlab=name)
}

# выбросы нашлись только для одной переменной - ddpi. находим индексы выбросов .
ddpi = LifeCycleSavings$ddpi
del_inds_bp = which(ddpi %in% boxplot(ddpi, plot=FALSE)$out)
del_inds_bp
# 47 49

# линейная модель на датасете без выбросов, удаленных при помощи боксплотов
L_bp = lm(sr ~ ., data = LifeCycleSavings[-del_inds_bp, ])
summary(L_bp)
# p-value: 0.0002821

#b
#  подсчёт параметров leverages (регрессия от 4-х факторов у нас уже построена)
sum(hatvalues(L_all))
# 5 - все верно

del_inds_l = which(hatvalues(L_all) > 2 * mean(hatvalues(L_all)))
del_inds_l
#      Ireland         Japan United States         Libya
#           21            23            44            49

# линейная модель на датасете без выбросов, удаленных при помощи leverages
L_l = lm(sr ~ ., data = LifeCycleSavings[-del_inds_l, ])
summary(L_l)
# p-value: 0.005315

# в итоге, лучшее качество (по p-уровням тестов для коэффициентов детерминации R^2)
# показала модель с удалением выбросов по диаграмме размаха. Второй стала модель
# на всех данных. На порядок больший p-value у модели с удалением наблюдений, для 
# которых leverages превосходят более чем в 2 раза среднее значение этого параметра 
# по всем наблюдениям.
# при этом Libya дважды попадала в список наблюдений-выбросов.
# интересно было бы предсказать значения sr для наблюдений-выбросов всеми тремя 
# моделями, и сравнить отклонение от реального значения (так как если модели будут
# использоваться для предсказания, то могут прийти и такие значения-выбросы).
del_inds = unique(c(del_inds_bp, del_inds_l))
L_all_pred = predict(L_all, LifeCycleSavings[del_inds, ], type = "response")
L_bp_pred = predict(L_bp, LifeCycleSavings[del_inds, ], type = "response")
L_l_pred = predict(L_l, LifeCycleSavings[del_inds, ], type = "response")
real_sr = LifeCycleSavings$sr[del_inds]

# отклонения
sqrt(sum((L_all_pred - real_sr) ** 2) / 5)
# 3.398448
sqrt(sum((L_bp_pred - real_sr) ** 2) / 5)
# 5.720109
sqrt(sum((L_l_pred - real_sr) ** 2) / 5)
# 4.168431

# вот тут уже интереснее. Общая модель показала самый хороший результат, и это
# логично, так как она обучалась на всех данных, в том числе на этих 5 объектах.
# теперь на 2 месте модель с удалением объектов по leverages, и это несмотря на 
# то, что она не видела 4 из 5 переданных значений. на последнем месте модель с
# удалением выбросов по по диаграмме размаха, она была построена с учетом 4 из 5
# значений из списка.
