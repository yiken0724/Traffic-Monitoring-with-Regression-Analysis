setwd("C:/Users/enric/OneDrive/Desktop/MATH/MH3510/Project")
getwd()
# install.packages( "dplyr" )
library(dplyr)
library(caret)


# Graphical display of the observed data.
data_raw <- read.table('aadt.txt',header=FALSE)
df <-
  data.frame(y=data_raw$V1,x1=data_raw$V2,x2=data_raw$V3,x3=data_raw$V4,x4=data_raw$V5)
df$x4[df$x4 == 2]<-0
process <- preProcess(df, method=c("range"))
df_scaled <- predict(process, df)
plot(df)
plot(df_scaled)


# Fit a multiple linear regression model for both scaled and non-scaled data.
mlr <- lm(y ~ x1+x2+x3+x4, data=df)
summary(mlr)
mlr_scaled <- lm(y ~ x1+x2+x3+x4, data=df_scaled)
summary(mlr_scaled)
##  x3 has the least significance to the model in terms of t-test while x1, x2 and x4 are all highly significant  ##


# Normality checking.
qqnorm(residuals(mlr),ylab='Residuals')
qqline(residuals(mlr))
qqnorm(residuals(mlr_scaled),ylab='Residuals')
qqline(residuals(mlr_scaled))
##  QQplot observations:
##  points generally follow normal line on the qqplot except for the points on the upper tail, this suggest that
##  the data follow a normal distribution generally but have a few outliers at the upper tail


# Draw some plots of residuals.
par(mfrow=c(2,3))
plot(residuals(mlr),ylab='Residuals',xlab='Time')
plot(residuals(mlr),fitted(mlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(mlr),df$x1,ylab='Residuals',xlab='x1')
plot(residuals(mlr),df$x2,ylab='Residuals',xlab='x2')
plot(residuals(mlr),df$x3,ylab='Residuals',xlab='x3')
plot(residuals(mlr),df$x4,ylab='Residuals',xlab='x4')
par(mfrow=c(1,1))
# scaled data
par(mfrow=c(2,3))
plot(residuals(mlr_scaled),ylab='Residuals',xlab='Time')
plot(residuals(mlr_scaled),fitted(mlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(mlr_scaled),df_scaled$x1,ylab='Residuals',xlab='x1')
plot(residuals(mlr_scaled),df_scaled$x2,ylab='Residuals',xlab='x2')
plot(residuals(mlr_scaled),df_scaled$x3,ylab='Residuals',xlab='x3')
plot(residuals(mlr_scaled),df_scaled$x4,ylab='Residuals',xlab='x4')
par(mfrow=c(1,1))
## !!explain


# Durbin-Watson tests.
# install.packages( "lmtest" )
library(lmtest)
dwtest(y ~ x1+x2+x3+x4, data=df)
dwtest(y ~ x1+x2+x3+x4, data=df_scaled)


# Some F-tests.
mlr1 <- lm(y ~ x1+x2+x4,data=df)  #remove x3 as insignificant from above
anova(mlr1,mlr)
##  we can not reject the null hypothesis of B3^=0 even at the level of 0.1

mlr1_scaled <- lm(y ~ x1+x2+x4,data=df_scaled)  #remove x3 as insignificant from above
anova(mlr1_scaled,mlr_scaled)
##  we can not reject the null hypothesis of B3^=0 even at the level of 0.1

mlr2 <- lm(y ~ x1+x2+offset(100.3*x3)+x4,data=df)  # from MLR results, B3^=100.3
summary(mlr2)
anova(mlr2,mlr)
##  we can not reject the null hypothesis even at the level of 0.1

mlr2_scaled <- lm(y ~ x1+x2+offset(0.03*x3)+x4,data=df_scaled)  # from MLR(scaled) results, B3^=0.03163
summary(mlr2_scaled)
anova(mlr2_scaled,mlr_scaled)
##  we can not reject the null hypothesis even at the level of 0.1

# Predicting non-scaled inputs
con <- data.frame(x1=50000,x2=3,x3=60,x4=0)
predict(mlr,con,interval='confidence',level=0.95)
predict(mlr,con,interval='prediction',level=0.95)

# Predicting scaled inputs
# scaling new input
new_input <- data.frame(y=-1,x1=50000,x2=3,x3=60,x4=0)
df_input <- rbind(new_input,df)
process <- preProcess(df_input[c('x1','x2','x3','x4')], method=c("range"))
df_input_scaled <- predict(process, df_input)
## scaled input: x1=0.045286737 ,x2=0.1666667 ,x3=0.83673469  ,x4=0
to_predict <- data.frame(x1=0.045286737 ,x2=0.1666667 ,x3=0.83673469  ,x4=0)
predict(mlr_scaled,to_predict,interval='confidence',level=0.95)
predict(mlr_scaled,to_predict,interval='prediction',level=0.95)

predicted_val <- (0.05732971*(max(df['y'])-min(df['y'])))+min(df['y'])
predicted_lwr_conf <- (0.00543876*(max(df['y'])-min(df['y'])))+min(df['y'])
predicted_upr_conf <- (0.1092207*(max(df['y'])-min(df['y'])))+min(df['y'])
predicted_lwr_pred <- (-0.1444346 *(max(df['y'])-min(df['y'])))+min(df['y'])
predicted_upr_pred <- (0.259094*(max(df['y'])-min(df['y'])))+min(df['y'])
conf<-data.frame(fit=predicted_val,lwr=predicted_lwr_conf,upr=predicted_upr_conf)
pred<-data.frame(fit=predicted_val,lwr=predicted_lwr_pred,upr=predicted_upr_pred)
conf
pred