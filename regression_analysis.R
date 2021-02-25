## Calculate Group Trendlines

## Change working directory
setwd(dir = "/home/p2baghae/Documents/curiosity-notebook-master/analysis/")
library(ggplot2)
library(abind)
library(reshape2)

library(basicTrendline)

## Read in the data
data_5min <- read.csv(file = "output/ultimate.csv", header = T)
data_5min <- subset(data_5min,select = c(talkingTime.5m.,user_id,groupname,condition))


names(data_5min)[names(data_5min) == "talkingTime.5m."] <- "5"


# Tedline for Group
T4G = function(dfr, g) {
  
  testbt = subset(dfr,groupname == g)
  x <- testbt$variable
  m.ratio<-lm(formula = testbt$value ~ x ,na.action=na.exclude)
  
  coeff=coefficients(m.ratio)

  p <- plot(x,testbt$value,xaxt = "n", ylab = "Talking (s)",xlim=c(0,76),ylim=c(0,240))
  abline(m.ratio,col="red")

  grid(NA, 5, lwd = 2)
  my_list <- list("intercept" = round(coeff[1],3), "slope" = round(coeff[2],3))
  return(my_list)
}

data_5min <- data_5min[-c(29,30,13,14), ] 


newdata1 <- subset(data_5min, condition == 1) 
newdata2 <- subset(data_5min, condition == 3) 

data_5min2 <- data_5min
data_5min2[data_5min2 == 0] <- 1
for(i in 30:1) {
  data_5min2[2*i,][data_5min2[2*i,] == 0] <-1
  if (as.numeric(data_5min2[2*i,]) == 0){
      data_5min2[2*i,] <- 1
  }
  data_5min2[2*i-1,] <- as.numeric(data_5min2[2*i-1,])/as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,"groupname" ] <- data_5min2[2*i,"groupname"]
  data_5min2[2*i-1,"condition" ] <- data_5min2[2*i,"condition"]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}
data_5min2 <- data_5min2[-c(7,15), ]

data<-data_5min2
ggplot(data) +
  geom_boxplot(aes(x = as.factor(condition), y=talkingTime, fill = as.factor(condition))) +
  theme(axis.title=element_text(size=rel(1.5))) +
  theme(plot.title=element_text(size=rel(1.5)))
t.test(x = subset(data, condition == 1)$talkingTime , y = subset(data, condition == 3)$talkingTime , alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

data_5min2 <- subset(data_5min2,select = -c(talkingTime))
df3 <- subset(data_5min2, condition == 1) 
df4 <- subset(data_5min2, condition == 3) 
df3 <- subset(df3, select =-c(condition,user_id))
df4 <- subset(df4, select =-c(condition,user_id))
df33 <- melt(df3)  #the function melt reshapes it from wide to long
df33$rowid <- 1:15  #add a rowid identifying variable

df44 <- melt(df4)  #the function melt reshapes it from wide to long
df44$rowid <- 1:13  #add a rowid identifying variable

df44$variable <-as.numeric(df44$variable)*5
df33$variable <-as.numeric(df33$variable)*5

new.baskets <- data.frame()
x <- c("group", "intercept", "slope")
colnames(df) <- x
for(g in df3$groupname) {
  print(g)
  j <- data.frame(g, T4G(df33,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets <- rbind(new.baskets,j)
}
new.baskets$condition <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

new.baskets2 <- data.frame()
x <- c("group",  "intercept", "slope")
colnames(df) <- x
for(g in df4$groupname) {
  print(g)
  j <- data.frame(g, T4G(df44,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets2 <- rbind(new.baskets2,j)
}
new.baskets2$condition <- c(3,3,3,3,3,3,3,3,3,3,3,3,3)

wt = rbind(new.baskets,new.baskets2)
wt$condition <- as.factor(wt$condition)
p <- ggplot(wt, aes(x=condition, y=slope)) + geom_violin(trim=FALSE)
p
p + xlab("Condition") + ylab("Trend of Talking Time") + scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) 

mean(new.baskets$slope, na.rm = FALSE)
mean(new.baskets2$slope, na.rm = FALSE)

p <- plot(c(0,76), c(0,16), type="n", xlab="Interaction Duration (minutes)", ylab="Ratio of user1 to user2 (s)",panel.first = grid())
p <- points(df44$variable,df44$value,xaxt = "n", ylab = "Talking (s)",col="cyan3")
val <- mean(new.baskets2$slope, na.rm = FALSE)
val2 <- mean(new.baskets2$intercept, na.rm = FALSE)
p <- abline(val2,val,col="cyan3",lwd=3)
abline(h=1,col="blue",lwd=1)
p <- points(df33$variable,df33$value,xaxt = "n", ylab = "Talking (s)",col="coral2")
val <- mean(new.baskets$slope, na.rm = FALSE)
val2 <- mean(new.baskets$intercept, na.rm = FALSE)
abline(val2,val,col="coral2",lwd=3)
legend(60, 15, legend=c("Baseline", "Adaptive", "Ratio = 1"),col=c("cyan3", "coral2","blue" ),lty=1)

######## get each from the percentage perspective
for(i in 30:1) {
  data_5min2[2*i,][data_5min2[2*i,] == 0] <-1
  data_5min2[2*i-1,] <- as.numeric(data_5min2[2*i-1,])+as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,] <- as.numeric(data_5min2[2*i,])/as.numeric(data_5min2[2*i-1,])
  data_5min2[2*i-1,"groupname" ] <- data_5min2[2*i,"groupname"]
  data_5min2[2*i-1,"condition" ] <- data_5min2[2*i,"condition"]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}
data_5min2 <- data_5min2[-c(7,15), ] 

########### Dana 2nd 
data_5min2 <- data_5min
data_5min2 <- subset(data_5min2,select = -c(talkingTime))

data_5min2[data_5min2 == 0] <- 1
for(i in 30:1) {
  jj <- as.numeric(data_5min2[2*i-1,])+as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,] <- 100*abs(0.5 - as.numeric(data_5min2[2*i-1,])/jj)

  data_5min2[2*i-1,"groupname" ] <- data_5min2[2*i,"groupname"]
  data_5min2[2*i-1,"condition" ] <- data_5min2[2*i,"condition"]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}
data_5min2 <- data_5min2[-c(7,15), ] 

df3 <- subset(data_5min2, condition == 1) 
df4 <- subset(data_5min2, condition == 3) 
df3 <- subset(df3, select =-c(condition,user_id))
df4 <- subset(df4, select =-c(condition,user_id))
df33 <- melt(df3)  #the function melt reshapes it from wide to long
df33$rowid <- 1:15  #add a rowid identifying variable

df44 <- melt(df4)  #the function melt reshapes it from wide to long
df44$rowid <- 1:13  #add a rowid identifying variable

df44$variable <-as.numeric(df44$variable)*5
df33$variable <-as.numeric(df33$variable)*5

new.baskets <- data.frame()
x <- c("group", "intercept", "slope")


for(g in df3$groupname) {
  print(g)
  j <- data.frame(g, T4G(df33,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets <- rbind(new.baskets,j)
}
new.baskets$condition <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

new.baskets2 <- data.frame()
x <- c("group",  "intercept", "slope")
for(g in df4$groupname) {
  print(g)
  j <- data.frame(g, T4G(df44,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets2 <- rbind(new.baskets2,j)
}
new.baskets2$condition <- c(3,3,3,3,3,3,3,3,3,3,3,3,3)


wt = rbind(new.baskets,new.baskets2)
wt$condition <- as.factor(wt$condition)
p <- ggplot(wt, aes(x=condition, y=slope)) + geom_violin(trim=FALSE)
p
p + xlab("Condition") + ylab("Trend of Talking Time") + scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) 

mean(new.baskets$slope, na.rm = FALSE)
# rowMeans(new.baskets)
mean(new.baskets2$slope, na.rm = FALSE)

p <- plot(c(0,76), c(0,1), type="n", xlab="Interaction Duration (minutes)", ylab="Ratio of user1 to talking time (s)",panel.first = grid())
p <- points(df44$variable,df44$value,xaxt = "n", ylab = "Talking (s)",col="cyan3")
val <- mean(new.baskets2$slope, na.rm = FALSE)
val2 <- mean(new.baskets2$intercept, na.rm = FALSE)
p <- abline(val2,val,col="cyan3",lwd=3)
abline(h=0.5,col="blue",lwd=1)
p <- points(df33$variable,df33$value,xaxt = "n", ylab = "Talking (s)",col="coral2")
val <- mean(new.baskets$slope, na.rm = FALSE)
val2 <- mean(new.baskets$intercept, na.rm = FALSE)
abline(val2,val,col="coral2",lwd=3)
legend(60,1, legend=c("Baseline", "Adaptive", "0.5"),col=c("cyan3", "coral2","blue" ),lty=1)

p <- plot(c(0,76), c(0,50), type="n", xlab="Interaction Duration (minutes)", ylab="Percentage Distance from 50%",panel.first = grid(),cex.lab=1.25, cex.axis=1.25)
p <- points(df44$variable,df44$value,xaxt = "n", ylab = "Talking (s)",col="cyan3")
val <- mean(new.baskets2$slope, na.rm = FALSE)
val2 <- mean(new.baskets2$intercept, na.rm = FALSE)
p <- abline(val2,val,col="cyan3",lwd=3)
p <- points(df33$variable,df33$value,xaxt = "n", ylab = "Talking (s)",col="coral2",pch=4)
val <- mean(new.baskets$slope, na.rm = FALSE)
val2 <- mean(new.baskets$intercept, na.rm = FALSE)
abline(val2,val,col="coral2", lty=6,lwd=3)
legend(60,50, legend=c("Baseline", "Adaptive"),col=c("cyan3", "coral2"),lty=c(1,6),lwd=3)

cond1 <- new.baskets$slope
cond2 <- new.baskets2$slope
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Slope of Talking Time", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Baseline")
abline(v = mean(cond2), col = "red", lwd = 2)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)

t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

################# Are slopes significantly different? #############################
cond1 <- new.baskets$slope
cond2 <- new.baskets2$slope # adaptive
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Slope", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Baseline")
abline(v = mean(cond2), col = "red", lwd = 2)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)
var.test(x = cond1, y = cond2, ratio = 1, alternative = "two.sided", conf.level = 0.95)


data_5min2 <- subset(data_5min2, select =-c(user_id))
wt <- melt(data_5min2,id.vars = c("groupname","condition"))
wt$condition <- as.factor(wt$condition)
wt$variable <- as.numeric(as.character(wt$variable))
p <- ggplot(wt, aes(x=condition, y=value)) + geom_violin(trim=FALSE,fill = as.factor(wt$condition))
p
p + ylab("Talking Duration (S)") + scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Neutral")) +theme(text = element_text(size=16))

agg <- aggregate(wt$value,by=list(wt$condition,wt$variable), FUN=mean, na.rm=TRUE)
stds <- aggregate(wt$value,by=list(wt$condition,wt$variable), FUN=sd, na.rm=TRUE)

names(agg)[names(agg)=="Group.1"] <- "condition"
names(stds)[names(stds)=="Group.1"] <- "condition"
levels(agg$condition)[levels(agg$condition)=="1"] <- "adaptive"
levels(agg$condition)[levels(agg$condition)=="3"] <- "baseline"

ggplot(agg, aes(x = Group.2, y = x, fill = condition, shape=condition, colour=condition)) +
  geom_point(size = 4) + geom_hline(yintercept=1, linetype="dashed", color = "blue")+
  geom_errorbar(aes(ymax = x+stds$x, ymin = x-stds$x))+ ylab("average talking time (s)") + xlab("duration of experiment (minutes)")+
  guides(fill = guide_legend(reverse=FALSE)) + scale_fill_discrete(breaks=c("adaptive","baseline")) + theme(text = element_text(size=14))

ggplot(agg, aes(x = Group.2, y = x-1, fill = condition, shape=condition, colour=condition)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = x+stds$x-1, ymin = x-stds$x-1))+ ylab("average talking time (s)") + xlab("duration of experiment (minutes)")+
  guides(fill = guide_legend(reverse=FALSE)) + scale_fill_discrete(breaks=c("adaptive","baseline")) + theme(text = element_text(size=14))

shabshab = data.frame(x = agg$condition, y = agg$x-1)
newdata1 <- subset(shabshab, x =="baseline" ) 
newdata2 <- subset(shabshab, x =="adaptive") 

sum(newdata1$y^2)
sum(newdata2$y^2,na.rm=TRUE)


################# THE SLOPES PLOT ############### 10th attempt
data_5min2 <- data_5min
data_5min2 <- subset(data_5min2,select = -c(talkingTime))

for(i in 30:1) {
  data_5min2[2*i-1,] <- as.numeric(data_5min2[2*i-1,])+as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,"groupname" ] <- data_5min2[2*i,"groupname"]
  data_5min2[2*i-1,"condition" ] <- data_5min2[2*i,"condition"]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}
data_5min2 <- data_5min2[-c(7,15), ] 

df3 <- subset(data_5min2, condition == 1) 
df4 <- subset(data_5min2, condition == 3) 
df3 <- subset(df3, select =-c(condition,user_id))
df4 <- subset(df4, select =-c(condition,user_id))
df33 <- melt(df3)  #the function melt reshapes it from wide to long
df33$rowid <- 1:15  #add a rowid identifying variable

df44 <- melt(df4)  #the function melt reshapes it from wide to long
df44$rowid <- 1:13  #add a rowid identifying variable


df44$variable <-as.numeric(df44$variable)*5
df33$variable <-as.numeric(df33$variable)*5

new.baskets <- data.frame()
x <- c("group", "intercept", "slope")
colnames(df) <- x
for(g in df3$groupname) {
  print(g)
  j <- data.frame(g, T4G(df33,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets <- rbind(new.baskets,j)
}
new.baskets$condition <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

new.baskets2 <- data.frame()
x <- c("group",  "intercept", "slope")
colnames(df) <- x
for(g in df4$groupname) {
  print(g)
  j <- data.frame(g, T4G(df44,g))
  names(j) <- c("group",  "intercept", "slope")
  new.baskets2 <- rbind(new.baskets2,j)
}
new.baskets2$condition <- c(3,3,3,3,3,3,3,3,3,3,3,3,3)


wt = rbind(new.baskets,new.baskets2)
wt$condition <- as.factor(wt$condition)


mean(new.baskets$slope, na.rm = FALSE)
# rowMeans(new.baskets)
mean(new.baskets2$slope, na.rm = FALSE)

p <- plot(c(0,76), c(0,240), type="n", xlab="Interaction Duration (minutes)", ylab="Talking Time (s)",panel.first = grid(), cex.lab=1.25, cex.axis=1.25)
p <- points(df44$variable,df44$value,xaxt = "n", ylab = "Talking (s)",col="cyan3")
val <- mean(new.baskets2$slope, na.rm = FALSE) #neutral
val2 <- mean(new.baskets2$intercept, na.rm = FALSE)
p <- abline(val2,val,col="cyan3",lwd=3)
p <- points(df33$variable,df33$value,xaxt = "n", ylab = "Talking (s)",col="coral2",pch=4)
val11 <- mean(new.baskets$slope, na.rm = FALSE)
val22 <- mean(new.baskets$intercept, na.rm = FALSE)
abline(val22,val11,col="coral2",lty=6,lwd=3)
legend(60, 220, legend=c("Baseline", "Adaptive"),col=c("cyan3", "coral2"),lty=c(1,6),lwd=3)

dat <-  subset(df33,select = -c(rowid,groupname))
desired_length <- 210 # or whatever length you want
empty_vec <- rep(1, desired_length)
dat$condition <- empty_vec
desired_length <- 182 # or whatever length you want
empty_vec <- rep(3, desired_length)
dat2 <-subset(df44,select = -c(rowid,groupname))
dat2$condition <- empty_vec
dat <- rbind(dat,dat2)
dat$condition <- as.factor(dat$condition)

ggplot(dat, aes(x=variable, y=value, color=condition, linetype = condition)) +
  geom_point(shape=dat$condition,size=2) +
  geom_smooth(method=stats::lm,   # Add linear regression lines
              se=FALSE,size=2)+ # Don't add shaded confidence region
  geom_abline(intercept = 37, slope = -5)+
                 

### is the diff significant?
cond1 <- new.baskets$slope
cond2 <- new.baskets2$slope
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Slope of Talking Time", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Baseline")
abline(v = mean(cond2), col = "red", lwd = 2)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)

t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
