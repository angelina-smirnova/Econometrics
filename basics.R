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

#1)$B'1'`'c'd'b'`'V'_'Z'V(B $B'^'`'U'V']'Z(B $B'a'`(B $B'S'c'V'^(B $B'b'V'T'b'V'c'c'`'b'Q'^(B ($B'Y'Q'S'Z'c'Z'^'e'p(B $B'a'V'b'V'^'V'_'_'e'p(B $B'S'm'R'Z'b'Q'V'd'V(B $B'#'m(B $B'Z'c'g'`'U'q(B $B'Z'Y(B $B'c'`'U'V'b'X'Q'd'V']'n'_'`'T'`(B $B'c'^'m'c']'Q(B $B'a'V'b'V'^'V'_'_'m'g(B)
modk <- lm(upper ~ ., data = Kakadu)
summary(modk)

#2)$B'1'b'`'S'V'b'\'Q(B $B'_'Q(B $B'_'Q']'Z'i'Z'V(B $B'^'e']'n'd'Z'\'`']']'Z'_'V'Q'b'_'`'c'd'Z(B $B'Z(B $B'e'U'Q']'V'_'Z'V(B $B'_'V'\'`'d'`'b'm'g(B $B'b'V'T'b'V'c'c'`'b'`'S(B $B'S(B $B'c']'e'i'Q'V(B $B'V'V(B $B'_'Q']'Z'i'Z'q(B
vif(modk)
# $B'S'c'W(B $B'g'`'b'`'j'`(B, $B'S'c'W(B $B'^'V'_'n'j'V(B 5

#3)$B'1'b'`'S'V'b'\'Q(B $B'`'c'd'Q'd'\'`'S(B $B'_'Q(B $B'_'`'b'^'Q']'n'_'`'c'd'n(B $B'i'V'b'V'Y(B QQ-plot
plot(modk, which = 2)
#$B'`'c'd'Q'd'\'Z(B $B'c'e'a'V'b(B $B'_'V'_'`'b'^'Q']'n'_'m(B

#4)$B'4'V'c'd(B $B'"'`'\'c'Q(B-$B','`'\'c'Q(B $B'Z(B $B']'`'T'Q'b'Z'f'^'Z'b'`'S'Q'_'Z'V(B $B'Y'Q'S'Z'c'Z'^'`'[(B $B'a'V'b'V'^'V'_'_'e'p(B $B'a'b'Z(B $B'_'V'`'R'g'`'U'Z'^'`'c'd'Z(B
boxCox(modk)
modk2 <- update(modk, log(upper) ~ .)
summary(modk2)
boxCox(modk2)
# $B']'`'T'Q'b'Z'f'Z'b'`'S'Q'd'n(B


#5)$B'4'V'c'd(B $B'2'Q'^'c'V'q(B 
resettest(modk)
#H0: $B'c'd'V'a'V'_'Z(B $B'_'V(B $B'a'b'`'a'e'k'V'_'m(B
#p-value < 0.05 => $B'T'Z'a'`'d'V'Y'Q(B $B'`'d'S'V'b'T'Q'V'd'c'q(B =>  $B'a'b'`'a'e'k'V'_'m(B
#modk2 <- update(modk, upper^2 ~ .)
#summary(modk2)
#boxCox(modk2)
resettest(modk2)
plot(modk2, which = 2)

#6)$B'1'`'U'R'`'b(B $B'c'a'V'h'Z'f'Z'\'Q'h'Z'Z(B $B'a'b'Z(B $B'a'`'^'`'k'Z(B crPlots
crPlots(modk2)

#7)$B'%'`'R'Q'S']'V'_'Z'V(B $B'b'`'R'Q'c'd'_'m'g(B $B'c'd'Q'_'U'Q'b'd'_'m'g(B $B'`'j'Z'R'`'\(B $B'Z(B $B'a'b'`'S'V'b'\'Q(B $B'Y'_'Q'i'Z'^'`'c'd'Z(B $B'a'V'b'V'^'V'_'_'m'g(B $B'i'V'b'V'Y(B coeftest
V_new <- vcovHC(modk2, type = "HC0")
coeftest(modk2, V_new)
coeftest(modk)
#$B'a'`'q'S'Z']'Z'c'n(B $B'Y'_'Q'i'Z'^'m'V(B $B'a'V'b'V'^'V'_'_'m'V(B

#8)$B'5'U'Q']'V'_'Z'V(B $B'_'V'Y'_'Q'i'Z'^'m'g(B $B'a'V'b'V'^'V'_'_'m'g(B ($B'S'b'e'i'_'e'p(B $B'Z']'Z(B $B'i'V'b'V'Y(B stepAIC)
modk3 <- stepAIC(modk2)

#9)$B'3'b'Q'S'_'V'_'Z'V(B $B'^'`'U'V']'Z(B $B'U'`(B $B'e'U'Q']'V'_'Z'q(B $B'Z(B $B'a'`'c']'V(B $B'e'U'Q']'V'_'Z'q(B $B'a'V'b'V'^'V'_'_'m'g(B $B'i'V'b'V'Y(B $B'd'V'c'd(B $B"c','`'b'`'d'\'Q'q(B $B'a'b'`'d'Z'S(B $B'U']'Z'_'_'`'["d(B ($B'd'V'c'd(B $B'#'Q']'n'U'Q(B)
waldtest(modk3, modk2)
#p-value = 0.786, $B'R'`']'n'j'V(B 0.05 - $B'_'e']'V'S'Q'q(B $B'T'Z'a'`'d'V'Y'Q(B $B'a'b'Z'_'Z'^'Q'V'd'c'q(B
#$B'\'`'o'f'f'h'Z'V'_'d'm(B $B'U']'Z'_'_'`'[(B $B'b'Q'S'_'m(B $B'_'e']'p(B

#10)$B'3'b'Q'S'_'V'_'Z'V(B $B'\'`'b'`'d'\'`'[(B $B'Z(B $B'U']'Z'_'_'`'[(B $B'^'`'U'V']'Z(B $B'a'`(B $B'\'b'Z'd'V'b'Z'p(B $B'!'\'Q'Z'\'V(B (AIC)
AIC(modk2)
AIC(modk3)
#$B'e(B modk3 $B'^'V'_'n'j'V(B $B'i'V'^(B $B'e(B modk2, $B']'e'i'j'V(B 

#11)$B'#'m'T'b'e'Y'\'Q(B $B'S'm'U'Q'i'Z(B ($B'\'b'Q'c'Z'S'`'[(B ??? $B'/'&(B $B'c'\'b'Z'_(B $B'Z'Y(B $B'\'`'_'c'`']'Z(B) $B'f'Z'_'Q']'n'_'`'[(B $B'^'`'U'V']'Z(B $B'S(B word $B'c(B $B'Z'_'d'V'b'a'b'V'd'Q'h'Z'V'[(B $B'\'`'o'f'f'Z'h'Z'V'_'d'`'S(B $B'a'V'b'V'U(B $B'b'V'T'b'V'c'c'`'b'Q'^'Z(B
library(sjPlot)
tab_model(modk3, vcov.type = "HC0", 
          show.ci = FALSE, show.se = TRUE, p.style = "numeric")