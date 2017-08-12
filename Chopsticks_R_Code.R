require(ggplot2)
require(gridExtra)
require(knitr)
require(xtable)
require(MASS)
require(caret)
require(car)
require(ppcor)
require(plyr)
require(GGally)

#data load#
p <- function(x) {formatC(x, format="f", digits=2)}
df <- read.csv("Chopsticks_Data_Raw.csv", stringsAsFactors=FALSE)
df <- df[complete.cases(df[,c("Tip","Price","Gender","Race","Payment_Type","Miles","Time_Elapsed")]),]
for(i in c("Race","Gender","Type","Payment_Type","Age")){
  df[, i] <- as.factor(df[,i])
}
row.names(df) <- 1:nrow(df)
print(xtable(df[sample(nrow(df),10),c("Time_Elapsed","Price","Tip","Miles","Payment_Type","Gender","Race")],caption="Sample of the Data",label="tab:datahead"),latex.environments="center",floating=FALSE,include.rownames=FALSE)

#box plot demographics#
dff <- df[complete.cases(df[,c("Race","Age","Gender","Payment_Type")]),]
p1 <- ggplot(dff, aes(x = Race, y = Tip)) + geom_boxplot()
p2 <- ggplot(dff, aes(x = Age, y = Tip)) + geom_boxplot()
p3 <- ggplot(dff, aes(x = Gender, y = Tip)) + geom_boxplot()
p4 <- ggplot(dff, aes(x = Payment_Type, y = Tip)) + geom_boxplot() +scale_x_discrete(name="Payment Type")
grid.arrange(p1,p2, p3,p4, nrow=2)

#correlation matrix#
ggpairs(df[,c("Tip", "Price", "Miles", "Time_Elapsed")])
lr <- lm(Tip~Price,df)
summar <- summary(lr)
df4 <- mutate(df, Race_Color = ifelse(Race=="B",'darkgreen',
                                      ifelse(Race=="W",'darkgrey',
                                             ifelse(Race=='A','cyan','brown4'))))
#simple model#
print(xtable(summary(lr), caption="Summary statistics for simple model", label="tab:simpsummary"),floating=FALSE)
price <- summar$coefficients[2,]

#simple model graph#
p1 <- ggplot(df, aes(x=Price,y=Tip)) + geom_point(shape=1) + geom_smooth(method="glm",se=FALSE)
p1

#AIC results#
xtable(stepAIC(lr,scope=~Price+Gender+Race+Payment_Type+Miles+Time_Elapsed,direction="forward"))
lr <- update(lr, ~.+Gender+Race+Payment_Type+Miles+Time_Elapsed)
summar <- summary(lr)

#AIC Analysis#
avPlotss<- avPlots(lr)
av7 <- avPlotss[[7]]
av8 <- avPlotss[[8]]
p1 <- ggplot(data=data.frame(av7),aes(x=Miles,y=Tip)) + geom_point() + geom_smooth(method="glm",se=FALSE)
p2 <- ggplot(data=data.frame(av8),aes(x=Time_Elapsed,y=Tip)) + geom_point() + geom_smooth(method = "glm", se=FALSE)
grid.arrange(p1,p2,nrow=1)

#residual plots#
lr <- lm(Tip~Price+Payment_Type,df)
plot(lr)

#residual table#
source("printbold.R")
residTab <- data.frame("Residuals" = residuals(lr),"Hat_Values"=hatvalues(lr), "Standard" = rstandard(lr), "Jack-Knifed"=rstudent(lr), "Cooks_Distance"= cooks.distance(lr), "Predicted" = predict(lr))
attach(residTab)
indices = Standard>2 | Hat_Values > (32/nrow(residTab)) | Cooks_Distance >=1
log_matrix <- cbind(rep(FALSE,sum(indices)),Hat_Values[indices]>(4/nrow(residTab)),abs(Standard[indices])>2,abs(Jack.Knifed[indices])>2,rep(FALSE,sum(indices)),rep(FALSE,sum(indices)))
printbold(xtable(residTab[indices,]), which=log_matrix)

#logistic summary#
df2 <- mutate(df, Tipped=ifelse(Tip==0,0,1))
llr <- glm(Tipped~Price,family=binomial(link="logit"),df2)
xtable(summary(llr))