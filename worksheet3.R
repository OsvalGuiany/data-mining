x<-seq(0, 8, by=0.1)
b<-c(3, 37, -12, 1)
sigma <- 5
y<-b[1]+b[2]*x+b[3]*x^2+b[4]*x^3 +rnorm(x, 0, sigma)
plot(x, y)

courbe_lin <- function(a) b[1]+b[2]*a+b[3]*a^2+b[4]*a^3 +rnorm(x, 0, sigma)
courbe_sans_sig <- function(a) b[1]+b[2]*a+b[3]*a^2+b[4]*a^3
curve(courbe_lin, from=0, to=8 ,n=81)

x3 = x^3
x2 <- x^2
x0 <- vector("numeric", 81)
x0<- x0+b[1]
#dim(x0) <- 81
myset = data.frame(x3, x2, x, x0)

#vals = x0+b[2]*x+b[3]*x^2+b[4]*x^3

lm_x = lm(y ~ ., myset)
coef(lm_x ~ res)

plot(x, y, col="red")
curve(courbe_lin, from=0, to=8 ,n=81, add=TRUE)
curve(courbe_sans_sig, from=0, to=8 ,n=81, add=TRUE, col="green")

# calculer la variance des residuels 
var(lm_x$residuals)

# compute confidence interval of significance level a = 5%
confint(lm_x, level=0.95)
