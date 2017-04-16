reg.data<-read.table("regression.txt",header=TRUE)
reg.data
attach(reg.data)

plot(tannin,growth,pch=16)
# To find the linear regression fit use 'lm'
# The response variable comes first (growth in our example), then the tilde ~, 
# then the name of the continuous explanatory variable (tannin).
fit <- lm(growth~tannin)

fit$coefficients

# draw the best fit line
abline(fit,col="red")

predt <- function(fit, x) {  # hand-made prediction function
  return (fit$coefficients[[1]] + x * fit$coefficients[[2]])
}

plot(tannin,growth,pch=16)
abline(fit,col="red")
segments(tannin,predt(fit,tannin),tannin,growth,col="red",lty=2)

# their specific values can also be accessed by this component:
fit$residuals

#Confidence Intervals
head(iris)
pl <- iris$Petal.Length
sl <- iris$Sepal.Length
model <- lm(pl ~ sl)
model

pred <- predict(model, data.frame(sl = sort(sl)), level = 0.95, interval = "confidence")
head(pred)
lower <- pred[,2]
upper <- pred[,3]

# make plot 
plot(sl, pl, pch=19, type="n")
grid()
polygon(c(sort(sl), rev(sort(sl))), c(upper, rev(lower)), col = "gray", border = "gray")
points(sl, pl, pch=19, cex=0.6)
abline(model, col="red", lwd=2)
