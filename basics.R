library(AER)
library(Ecdat)

data(Kakadu)
modk
?Kakadu
head(Kakadu)

data(University)
?University
head(University)
data(Workinghours)
data(BudgetItaly)

#1)Построение модели по всем регрессорам (зависимую переменную выбираете Вы исходя из содержательного смысла переменных)
modk <- lm(upper ~ ., data = Kakadu)
summary(modk)

#2)Проверка на наличие мультиколлинеарности и удаление некоторых регрессоров в случае ее наличия
vif(modk)
# всё хорошо, всё меньше 5

#3)Проверка остатков на нормальность через QQ-plot
plot(modk, which = 2)
#остатки супер ненормальны

#4)Тест Бокса-Кокса и логарифмирование зависимой переменную при необходимости
boxCox(modk)
modk2 <- update(modk, log(upper) ~ .)
summary(modk2)
boxCox(modk2)
# логарифировать


#5)Тест Рамсея 
resettest(modk)
#H0: степени не пропущены
#p-value < 0.05 => гипотеза отвергается =>  пропущены
#modk2 <- update(modk, upper^2 ~ .)
#summary(modk2)
#boxCox(modk2)
resettest(modk2)
plot(modk2, which = 2)

#6)Подбор спецификации при помощи crPlots
crPlots(modk2)

#7)Добавление робастных стандартных ошибок и проверка значимости переменных через coeftest
V_new <- vcovHC(modk2, type = "HC0")
coeftest(modk2, V_new)
coeftest(modk)
#появились значимые переменные

#8)Удаление незначимых переменных (вручную или через stepAIC)
modk3 <- stepAIC(modk2)

#9)Сравнение модели до удаления и после удаления переменных через тест ≪Короткая против длинной≫ (тест Вальда)
waldtest(modk3, modk2)
#p-value = 0.786, больше 0.05 - нулевая гипотеза принимается
#коэффциенты длинной равны нулю

#10)Сравнение короткой и длинной модели по критерию Акаике (AIC)
AIC(modk2)
AIC(modk3)
#у modk3 меньше чем у modk2, лучше 

#11)Выгрузка выдачи (красивой ??? НЕ скрин из консоли) финальной модели в word с интерпретацией коэффициентов перед регрессорами
library(sjPlot)
tab_model(modk3, vcov.type = "HC0", 
          show.ci = FALSE, show.se = TRUE, p.style = "numeric")