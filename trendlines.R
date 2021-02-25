## Linear Regression

## Change working directory
setwd(dir = "/home/p2baghae/Documents/curiosity-notebook-master/analysis/")
library(ggplot2)
library(abind)
require(gridExtra)
# library(ggpubr)
library(moments)
library(ff)
library(MASS)

# library(tidyverse)
library(viridis)

## Read the ultimate data points, includes everything
data <- read.csv(file = "output/ultimate.csv", header = T)

## Make it sum for each group instead of individual
data$age_english[is.na(data$age_english)] <- 0
levels(data$gender) <- c(levels(data$gender), "coed")
for(i in 30:1) {
  if (data[2*i-1,"gender"] == data[2*i,"gender"]){
    keep <-data[2*i,"gender"]
  } else {
    keep <- "coed"
  }
  data[2*i-1,] <- (as.numeric(data[2*i-1,]) + as.numeric(data[2*i,]))/2
  data[2*i-1,"groupname" ] <- data[2*i,"groupname"]
  data[2*i-1,"condition" ] <- data[2*i,"condition"]
  data[2*i-1,"totalTime" ] <- data[2*i-1,"totalTime"]/2
  data[2*i-1,"gender"] <- keep
  data <- data[rownames(data) != 2*i, ]
}
# excluding two groups
data <- data[-c(7,15), ] 

sts.ex.sat <- subset(data, select = c("condition", "age","gender","age_english","stem","ratio"))
summary(sts.ex.sat)

cor(sts.ex.sat) 

# Fit our regression model
sat.mod <- aov(ratio ~ condition+ age + gender + age_english, # regression formula
              data=sts.ex.sat) # data set

# Summarize and print the results
summary(sat.mod) # show regression coefficients table
confint(sat.mod)

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

sat.voting.mod <-  lm(ratio ~ condition + age + gender,
                      data = na.omit(sts.ex.sat))
sat.mod <- update(sat.mod, data=na.omit(sts.ex.sat))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)

########### TEST SCORE KNOWLEDGE #######################
data$pre_knowledge_rock_correct_points[is.na(data$pre_knowledge_rock_correct_points)] <- 0
data$post_knowledge_rock_correct_points[is.na(data$post_knowledge_rock_correct_points)] <- 0
attach(mtcars)
plot(data$pre_knowledge_rock_correct_points,col ="red",
     xlab="Group Indext", ylab="Test Score")
legend("topright",c("Post-Experiment","Pre-Experiment"), fill=c("green","red"))
points(data$post_knowledge_rock_correct_points,col="green",pch = 7)
abline(h=mean(data$post_knowledge_rock_correct_points),col="green",lwd = 4)
abline(h=mean(data$pre_knowledge_rock_correct_points),col="red",lwd = 4)
t.test(x = data$pre_knowledge_rock_correct_points, y = data$post_knowledge_rock_correct_points, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

m.ratio<-lm(data$post_knowledge_rock_correct_points-data$pre_knowledge_rock_correct_points~as.factor(data$condition)+data$pre_knowledge_rock_correct_points)
print(AIC(m.ratio))
print(summary(m.ratio))
plot(m.ratio)

library(dplyr)
df <- data %>%
  group_by(pre_knowledge_rock_correct_points) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = pre_knowledge_rock_correct_points, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) 


cond1 <- subset(data, condition == 1) 
cond2 <- subset(data, condition == 3) 
cond1 <- cond1$post_knowledge_rock_correct_points
cond2 <- cond2$post_knowledge_rock_correct_points

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Age", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Neutral")
abline(v = mean(cond2), col = "red", lwd = 2)

cond1 <- subset(data, condition == 1) 
cond2 <- subset(data, condition == 3) 
cond1 <- cond1$enjoyed_group
cond2 <- cond2$enjoyed_group

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2,na.rm = TRUE))
xmax <- max(max(cond1), max(B=cond2,na.rm = TRUE))
hist(cond1, xlim = c(xmin, xmax), main = "fariness", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Neutral")
abline(v = mean(cond2,na.rm = TRUE), col = "red", lwd = 2)

mean(cond1)
mean(cond2,na.rm = TRUE)
sd(cond1)
sd(cond2,na.rm = TRUE)

## Perform the test
## Ho: mu1 = mu2 vs. Ha: mu1 != mu2
t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

ada <- cond1$post_knowledge_rock_correct_points - cond1$pre_knowledge_rock_correct_points #adaptive
base <- cond2$post_knowledge_rock_correct_points- cond2$pre_knowledge_rock_correct_points
plot(ada,col ="coral2",
     xlab="Group Index", ylab="Test Score",panel.first = grid(),cex.lab=1.25, cex.axis=1.25)
legend("bottomleft",c("Diff Baseline","Diff Adaptive"), fill=c("cyan3","coral2"))
points(base,col="cyan3",pch = 7)
abline(h=mean(ada),col="coral2",lwd = 4)
abline(h=mean(base),col="cyan3",lwd = 4)

t.test(x = ada, y = base, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
mean(ada, na.rm = TRUE)
mean(base,na.rm = TRUE)
sd(ada,na.rm = TRUE)
sd(base,na.rm = TRUE)

m.ratio<-lm(data$post_knowledge_rock_correct_points-data$pre_knowledge_rock_correct_points~as.factor(data$condition)+data$know_rocks+data$pre_knowledge_rock_correct_points+data$interes_rocks)
print(summary(m.ratio))
print(AIC(m.ratio))
par(mfrow=c(2,2))
plot(m.ratio)
par(mfrow=c(1,1))
##########################################
# interest in rocks was more important
m.ratio<-lm(data$ratio ~ as.factor(data$condition)+data$interes_rocks, data = data)
m.ratio<-lm(data$liked_teaching~as.factor(data$condition)+data$gender+data$age+data$interest_robot+data$interact_ca+data$interes_rocks+data$interest_ca+data$interact_robot)


# postiesgood includes the column names desired to be included in analysis
postiesgood <- c("age","stem","native","age_english","condition","interes_rocks","Unkind_kind")

# check every column agains these 
for(g in postiesgood) {
  sink(paste(g,"2","txt",sep="."))
  f <- as.formula(paste(paste("data",g,sep="$"), "as.factor(data$condition)+ data$interes_rocks+ data$know_rocks+ data$interact_ca+ data$interest_ca", sep = " ~ "))
  print(g)
  m.ratio<-lm(f,data=data)
  print(summary(m.ratio))
  sink()
}

##################################### PLOTS Final Draft #######################################################
talkingData <- data[c("totalTime","talkingTime","groupname","condition","ratio")]
talkingData$condition <- as.factor(whateves$condition)
ggplot(whateves, aes(x = condition, y = talkingTime/totalTime)) + geom_point()+
  geom_boxplot() + ylab("Ratio of talking to experiment duration") + xlab("Condition")+
  scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) +theme(text = element_text(size=12))

ggplot(talkingData, aes(x = condition, y = ratio)) + geom_point()+
  geom_boxplot() + ylab("Ratio of talking to experiment duration") + xlab("Condition")+
  scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) +theme(text = element_text(size=12))

dp <- ggplot(talkingData, aes(x=condition, y=talkingTime/totalTime, fill=condition)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.4, fill="white")+ geom_point()+
  ylab("Ratio of talking to experiment duration") + xlab("Condition")+
  scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) +theme(text = element_text(size=18))
dp+ theme(legend.position = "none")
#######################################################################################################

inv.compete <- 1/log(combined.data[,6]+1) #data transformation
model.comb<- glm(t(complete30.new)~condition11.new+article+combined.data[,4]+combined.data[,5]+inv.compete,family = binomial(link=logit))

significantdiff <- c("contribution","encouraging","know_rocks","num_article_click")
for(g in significantdiff) {
  f <- as.formula(paste(paste("data",g,sep="$"), "as.factor(data$condition)", sep = " ~ "))
  m.earliest_correct_guess<-lm(f)
  anova(m.earliest_correct_guess)
  print(g)
  print(summary(m.earliest_correct_guess))
}

# enjoyed_group,num_button_click,talkingTime.20m.,talkingTime.25m.
m.ratio<-lm(totalTime ~ as.factor(data$condition),data=data)
print(summary(m.ratio))
hist(data$talkingTime)

# A small p-value (typically ≤ 0.05) indicates strong evidence against the null hypothesis
var.test(x = cond1, y = cond2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
t.test(x = cond1, y = cond2, alternative = "greater", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
t.test(x = cond1, y = cond2, alternative = "less", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

############## Do the difference between things to see if the are even different
cond1 <- subset(data, condition == 1) 
cond2 <- subset(data, condition == 3) 

cond11 <- cond1$ratio
cond22 <- cond2$ratio

par(mfrow=c(2,1))
xmin <- min(min(cond11), min(cond22))
xmax <- max(max(cond11), max(B=cond22))
hist(cond11, xlim = c(xmin, xmax), main = "enjoyed_group", xlab = "Adaptive")
abline(v = mean(cond11), col = "red", lwd = 2)
hist(cond22, xlim = c(xmin, xmax), main = "enjoyed_group", xlab = "Neutral")
abline(v = mean(cond22), col = "red", lwd = 2)

t.test(x = cond11, y = cond22, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

###### BOX PLOT everything to see what looks different ##############
postiesgood <- c("enjoyed_group","liked_teaching","teach_again","talkingTime","totalTime","ratio")
data2 <- subset(data, select = -c(condition))

nums <- unlist(lapply(data2, is.numeric))  
postiesgood <- colnames(data2[ , nums])
for(g in postiesgood) {
  sink(paste(g,"2","txt",sep="."))
  print(g)
  f <- as.formula(paste(paste("data",g,sep="$"), "as.factor(condition)", sep = " ~ "))
  # g <- boxplot(f, data=data)
  print(t.test(f, data))
  sink()
}

ggplot(data, aes(x=as.factor(condition), y=ratio, fill=as.factor(condition))) +
  geom_violin() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  ggtitle("Violin chart") +
  xlab("")

ggplot(data,aes(x=as.factor(condition), y=ratio, fill=as.factor(condition))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

##### BOX PLOT ############
ggplot(data) +
  geom_boxplot(aes(x = as.factor(condition), y=ratio, fill = as.factor(condition))) +
  theme(axis.title=element_text(size=rel(1.5))) +
  theme(plot.title=element_text(size=rel(1.5)))
t.test(x = subset(data, condition == 1)$ratio , y = subset(data, condition == 3)$ratio , alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

################## Check normality and T-test
hist(data$ratio)
a <- boxcox(data$ratio ~ as.factor(data$condition),data=data)
a$x[which.max(a$y)] 
hist((data$ratio)^(a$x[which.max(a$y)]))
data$ratio <-(data$ratio)^a$x[which.max(a$y)]
t.test(x = subset(data, condition == 1)$ratio , y = subset(data, condition == 3)$ratio , alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

######################### now check the model
g <- "num_teach_type_button_click"
hist(data$num_teach_type_button_click)
data$num_teach_type_button_click <- 1/(data$num_teach_type_button_click) # how to transform data that is not normal
hist(data$num_teach_type_button_click)
f <- as.formula(paste(paste("data",g,sep="$"), "as.factor(data$condition)+data$age+ data$interact_ca+data$interest_robot", sep = " ~ ")) # the assumptions kinda hold

m.ratio<-aov(f,data=data)
print(summary(m.ratio))
print(AIC(m.ratio))
par(mfrow=c(2,2))
plot(m.ratio)
par(mfrow=c(1,1))

# Further goodness of fit tests are done
g <- "ratio"
f <- as.formula(paste(paste("data",g,sep="$"), "as.factor(data$condition)+data$gender+ data$age+ data$interes_rocks+ data$know_rocks", sep = " ~ ")) # the assumptions kinda hold
m.ratio<-lm(f,data=data)
print(summary(m.ratio))
print(AIC(m.ratio))
par(mfrow=c(2,2))
plot(m.ratio)
par(mfrow=c(1,1))

### CHANGED contribution to calculate distance from 3
data$contribution <- data.frame(y = abs(data$contribution-6))
f <- as.formula(paste(paste("data",g,sep="$"),"as.factor(data$condition)+ data$age+ data$interes_rocks+ data$know_rocks", sep = " ~ ")) # the assumptions kinda hold
m.ratio<-lm(f,data=data)
print(summary(m.ratio))
print(AIC(m.ratio))
par(mfrow=c(2,2))
plot(m.ratio)
par(mfrow=c(1,1))
cond1 <- subset(data, condition == 1)$contribution
cond2 <- subset(data, condition == 3)$contribution
t.test(x = cond1 , y = cond2 , alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)

##### same thing for encouraging
g <- "encouraging"
f <- as.formula(paste(paste("data",g,sep="$"),"as.factor(data$condition)+ data$age+ data$interes_rocks+ data$know_rocks", sep = " ~ ")) # the assumptions kinda hold
m.ratio<-lm(f,data=data)
print(summary(m.ratio))
print(AIC(m.ratio))
par(mfrow=c(2,2))
plot(m.ratio)
par(mfrow=c(1,1))
cond1 <- subset(data, condition == 1)$encouraging
cond2 <- subset(data, condition == 3)$encouraging
t.test(x = cond1 , y = cond2 , alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

mean(cond1, na.rm = TRUE)
mean(cond2,na.rm = TRUE)
sd(cond1,na.rm = TRUE)
sd(cond2,na.rm = TRUE)

###################### VIOLIN BOX TOGETHER #########################################
dp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")
dp + theme_classic()

## Pie Chart for thesis
cond1 <- subset(data, condition == 1)
cond2 <- subset(data, condition == 3)
prestudy <- c("degree","faculty","stem","native","age_english","condition",)

ggplot(data=data, aes(x=degree,fill = condition)) +
  geom_bar() 

tmp <- sapply(data$degree, as.character)
data$degree <- tmp
data$condition<-replace(data$condition, data$condition == 1,'adaptive')
data$condition<-replace(data$condition, data$condition == 3,'baseline')

MnM <-c("High school diploma or the equivalent","Some college credit, no degree","Trade/technical/vocational training") 
p1 <- ggplot(data=data,aes(x=degree,fill=condition))+
  geom_bar(position="dodge") + scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8"), labels=MnM) +
  theme(text = element_text(size=12))+ylab("count")+xlab("Degree") + theme(legend.position = "top",axis.text.x = element_text(angle=90, size=10))+
  geom_text(stat='count', aes(label=..count..))
p1
