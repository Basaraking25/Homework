# Homework
library(dplyr)
library(MASS)
library(Matrix)
library(glmnet)
library(mgcv)
library(earth)
library(vip)

#Transformation
data <- claimdata 
claim_data <- filter(data,Code1==c(1,2,7))
log_cols<-c("CLAIM","DED","GRT","DWT","VALUE")
claim_data[log_cols] <- log(claim_data[log_cols])
claim_data["AGE"]<-log(claim_data["AGE"]+1)

linearMod <- lm(CLAIM~DED+AGE+GRT+DWT+VALUE+HP+Code1,claim_data )
summary(linearMod)
#Intercept,VALUE and HP are relevant to the CLAIM with the order from high to low.

###stepAIC
aic <- stepAIC(linearMod)
aic
summary(aic)
#uesing the backwise selection to select the cofficient and get the same result
AIC(linearMod)
AIC(aic)
#aic has the lower AIC than linearMod.Then it's better.

###glmnet
install.packages("glmnet")
x<- claim_data %>% select(-CLAIM)%>% as.matrix()
y<- claim_data %>% select(CLAIM)%>% as.matrix()
lasso <- glmnet(x,y,alpha = 1)
ridge <- glmnet(x,y,alpha = 0)
plot(lasso,xvar = "lambda")
plot(ridge,xvar = "lambda")

###gam model
gamMod <- gam(CLAIM~DED+AGE+GRT+DWT+VALUE+HP+Code1,data=claim_data)
summary(gamMod)
plot.gam(gamMod,all.terms = TRUE) #the figure margins too large

###MARS model
marsMod <- earth(CLAIM~DED+AGE+GRT+DWT+VALUE+HP+Code1,data=claim_data)
plotmo(marsMod)
summary(marsMod)
summary(marsMod, digits = 2, style = "pmax")#why we use this kind of summary

###vip packages
vip(linearMod)
vip(aic)
vip(marsMod)

###prediction
library(dplyr)
prediction <- slice(claimdata,c(100,200,300))
Log_col <- c("DED","GRT","DWT","VALUE")
prediction[Log_col] <- log(prediction[Log_col])
prediction["AGE"] <- log(prediction["AGE"]+1)

observation <- slice(claimdata,c(100,200,300))
observation["CLAIM"] <- log(observation["CLAIM"])

linear_prediction <- predict.lm(linearMod,prediction)
aic_prediction <- predict(aic,prediction)
gam_prediction <- predict.gam(gamMod,prediction)
mars_prediction <- predict(marsMod,prediction)
result <- data.frame(cbind(linear_prediction,aic_prediction,gam_prediction,mars_prediction,observation))
names(result)[names(result) == 'CLAIM'] <- 'mars_prediction'
names(result)[names(result) == 'CLAIM.1'] <- 'observed CLAIM'
result 
