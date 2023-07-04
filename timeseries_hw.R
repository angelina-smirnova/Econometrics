#### Загружаем пакеты ####
library(readxl)
library(timeSeries)
library(xts)
library(car)
library(dplyr)
# install.packages("ARDL")
# install.packages("aTSA")
install.packages("forecast")
library(forecast)
library(ARDL)
library(aTSA)
library(lmtest)
library(stargazer)
library(sandwich)
library(dynlm)
library(forecast)
library(stats)


#### Смотрим на даныне ####

# Загружаем данные
data <- read_xlsx("data_ecm.xlsx")

# Возьмем  нужные столбцы
data2 <- data[c("year", "real R_D", "real GDP")]

# Сделаем 2 ряда 
R_D <- ts(data2$`real R_D`, start = 1983, end = 2021, frequency = 1)
plot(R_D)

GDP <- ts(data2$`real GDP`, start = 1983, end = 2021, frequency = 1)
plot(GDP)

# Возьмем прирост ВВП
GDP_d <- GDP %>% diff()
plot(GDP_d, type = "l")

# Проверим на стационарность
adf.test(GDP_d)
# Ряд стационарен без константы и тренда на 5% уровне значимости

# То же самое для R_D
# Возьмем прирост 
R_D_d <- R_D %>% diff()
plot(R_D_d, type = "l")
# Не выглядит стационарно

# Проверим на стационарность
adf.test(R_D_d)
# Если только без константы, тренда и на 5% урвоне значимости

# Проведем тест Энгла-Грейнджера на коинтеграцию
coint.test(GDP_d, R_D_d)
# Коинтегрированы на 5% урвоне значимости без тренда

# Добавим ОПЖ
le <- data$le %>% as.numeric()
# Сделаем временной ряд
le <- ts(le, start = 1983, end = 2021, frequency = 1)

# Посмтрим на ряд
plot(le, type = 'l')
# Явно не стационарен
# Проверим
adf.test(le)
# Так и есть

# Посмотрим на первые разности
le_d <- le %>% diff()
plot(le_d, type = 'l')
adf.test(le_d)
# Ряд явно не стационарен, его нельзя использовать в модели


#### А если вторые разности? ####
GDP_d2 <- GDP %>% diff(diff = 2)
plot(GDP_d2)
# Лучше
adf.test(GDP_d2)
# Ряд стационарен

R_D_d2 <- R_D %>% diff(diff = 2)
plot(R_D_d2)
# Разная дисперсия

# Проверим на стационарность
adf.test(R_D_d2)
# Ряд стационарен

#### Вернемся к первым разностям ####


#построим уравнение

# Объединим данные
data <- data.frame(GDP_d, R_D_d)
data2 <-data.frame(GDP_d2, R_D_d2) 

#какой порядок брать?
mod_auto <- auto_ardl(GDP_d ~ R_D_d, data, max_order = 3, selection = "BIC")
mod_auto
# Лучший порядок - это ARDL(1,3)

#### Пытаемся построить другие модели ####
mod3 <- dynlm(GDP_d ~ R_D_d + stats::lag(R_D_d, 1)  , data = data)
summary(mod3)
mod4 <- dynlm(GDP_d ~ R_D_d + stats::lag(R_D_d, 1) + stats::lag(R_D_d, 2) , data = data)
summary(mod4)
mod5 <- dynlm(GDP_d ~ R_D_d + stats::lag(R_D_d, 1) + stats::lag(R_D_d, 2) +
                + stats::lag(R_D_d, 3), data = data)
summary(mod5)
# Лаги R_D делают только хуже

mod6 <- dynlm(GDP_d ~ stats::lag(GDP_d, 1) + R_D_d , data = data)
summary(mod6)
mod7 <- dynlm(GDP_d ~ stats::lag(GDP_d, 1) + R_D_d + stats::lag(R_D_d, 1) , data = data)
summary(mod7)
mod8 <- dynlm(GDP_d ~ stats::lag(GDP_d, 1)+ R_D_d + stats::lag(R_D_d, 1) + stats::lag(R_D_d, 2)
              , data = data)
summary(mod8)

mod9 <- dynlm(GDP_d ~ stats::lag(GDP_d, 1)+ R_D_d + stats::lag(R_D_d, 1) + stats::lag(R_D_d, 2) +
                stats::lag(R_D_d, 3)  
              , data = data)
summary(mod9)

# Сравним с лагом GDP и без
BIC(mod2) ;BIC(mod3) ;BIC(mod4) ;BIC(mod5) ;BIC(mod6) ;BIC(mod7) ;BIC(mod8) ;BIC(mod9)

# Сравним mod2 и mod5
summary(mod2)
summary(mod5)
# В итоге модель ARDL(1,3) - все равно лучше

#### Вернемся к построению модели ####

#возьмем порядок 1 и 3
mod2 <- ardl(GDP_d ~ R_D_d, data, order = c(1,3))
summary(mod2)
# Лаги не значимы

# Каковы же мультипликторы в модели?
multipliers(mod2, type = "lr")
multipliers(mod2, type = "sr")
# Получается в долгосрочном периоде влияния нет, а
# в краткосрочном есть

# Проверим на автокорреляцию
# Посмотрим на остатки
e <- mod2$residuals
plot(e)
# Выглядит стационарно

# Првоерим 
#тест Бреуша-Годфри
bgtest(mod2)
#автокорреляции нет

# Тест на гетероскедастичность в остатках
install.packages("FinTS")
library(FinTS)
ArchTest(e)
# Гетероскедастичности тоже нет, все хорошо

# Посмотрим на ACF и PACF
tsdisplay(e)
# Это точно белый шум, ничего не значимо


# Делаем с робастыми стандартными ошибками
coeftest(mod2, vcov = vcovHAC(mod2))
summary(mod2)

grangertest(x = R_D_d, y = GDP_d, order = 2)
#R_D не помогает предсказывать ВВП
grangertest(x = GDP_d, y = R_D_d, order = 2)
# ВВП не помогает предсказывать R_D
# Получается R_D не является причной ВВП по Грейнджеру

#### Модель с 10 лагами ####
mod_auto <- auto_ardl(GDP_d ~ R_D_d, data, max_order = 10, selection = "BIC")
mod_auto
# Возьмем все же ARDL(1,10)

mod4 <- ardl(GDP_d ~ R_D_d, data, order = c(10,10))
summary(mod4)

# Каковы же мультипликторы в модели?
multipliers(mod4, type = "lr")
multipliers(mod4, type = "sr")
# Мультипликаторы не значимы

# Проверим на автокорреляцию
# Посмотрим на остатки
e <- mod4$residuals
plot(e)
# Выглядит стационарно

# Првоерим 
#тест Бреуша-Годфри
bgtest(mod4)
#автокорреляция есть

# Тест на гетероскедастичность в остатках
# install.packages("FinTS")
library(FinTS)
ArchTest(e)
# Гетероскедастичности нет, все хорошо

# Посмотрим на ACF и PACF
tsdisplay(e)

# Делаем с робастыми стандартными ошибками
coeftest(mod4, vcov = vcovHAC(mod4))
summary(mod4)

grangertest(x = R_D_d, y = GDP_d, order = 10)
#R_D помогает предсказывать ВВП
grangertest(x = GDP_d, y = R_D_d, order = 10)
# ВВП не помогает предсказывать R_D
# Получается R_D является причной ВВП по Грейнджеру












