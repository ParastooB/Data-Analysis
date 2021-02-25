## Prestudy t-test Example

## Change working directory
setwd(dir = "/home/p2baghae/Documents/curiosity-notebook-master/analysis/")
library(ggplot2)
library(abind)
library(reshape2)

## Read in the data
data_5min <- read.csv(file = "output/ultimate.csv", header = T)
data_5min <- subset(data_5min,select = c(talkingTime.5m.,user_id,groupname,condition))

names(data_5min)[names(data_5min) == "talkingTime.5m."] <- "5"


data_5min <- data_5min[-c(29,30,13,14), ] 

################## FOR Qualitive resuslts ################

wt <- melt(data_5min,id.vars = c("user_id","groupname","condition"))
wt$condition <- as.factor(wt$condition)
p <- ggplot(wt, aes(x=condition, y=value)) + geom_violin(trim=FALSE,fill = as.factor(condition))
p + ylab("Talking Duration (S)") + scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Neutral")) +theme(text = element_text(size=16))

agg <- aggregate(wt$value,by=list(wt$condition,wt$variable), FUN=mean, na.rm=TRUE)
stds <- aggregate(wt$value,by=list(wt$condition,wt$variable), FUN=sd, na.rm=TRUE)

newdata1 <- subset(agg, Group.1 == 1) 
newdata1 <- subset(newdata1, select = -c(Group.1))
newdata2 <- subset(agg, Group.1 == 3) 
newdata2 <- subset(newdata2, select = -c(Group.1))

newdatastd1 <- subset(stds, Group.1 == 1)
newdatastd1 <- subset(newdatastd1, select = -c(Group.1))
newdatastd2 <- subset(stds, Group.1 == 3)
newdatastd2 <- subset(newdatastd2, select = -c(Group.1))

g <- plot(newdata2$Group.2,newdata2$x)
points(newdata1$Group.2,newdata1$x,pch=8)
g + xlab("talking time (s)") + ylab("duration of experiment (minutes)")

set.seed(0815)
ggplot(newdata1, aes(x = Group.2, y = x)) +
  geom_point(size = 4, color='blue') +
  geom_errorbar(aes(ymax = x+newdatastd1$x, ymin = x-newdatastd1$x), color='blue')+ ylab("average talking time (s)") + xlab("duration of experiment (minutes)")+
  geom_point(aes(x = Group.2, y = newdata2$x),size = 4, ,color = "red") + geom_errorbar(aes(ymax = newdata2$x+newdatastd2$x, ymin = newdata2$x-newdatastd2$x),color='red') + 
  guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(breaks=c("adaptive","neutral"))

names(agg)[names(agg)=="Group.1"] <- "condition"
names(stds)[names(stds)=="Group.1"] <- "condition"
levels(agg$condition)[levels(agg$condition)=="1"] <- "adaptive"
levels(agg$condition)[levels(agg$condition)=="3"] <- "baseline"

ggplot(agg, aes(x = Group.2, y = x, fill = condition, shape=condition, colour=condition)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = x+stds$x, ymin = x-stds$x))+ ylab("average talking time (s)") + xlab("duration of experiment (minutes)")+
  guides(fill = guide_legend(reverse=FALSE)) + scale_fill_discrete(breaks=c("adaptive","baseline")) + theme(text = element_text(size=14))

data_5min2 <- data_5min

for(i in 30:1) {
  data_5min2[2*i-1,] <-  as.numeric(data_5min2[2*i-1,]) +  as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,1 ] <- data_5min2[2*i,1 ]
  data_5min2[2*i-1,31 ] <- data_5min2[2*i,31 ]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}

for(i in 30:1) {
  data_5min2[2*i-1,] <- as.numeric(data_5min2[2*i-1,]) + as.numeric(data_5min2[2*i,])
  data_5min2[2*i-1,"groupname" ] <- data_5min2[2*i,"groupname"]
  data_5min2[2*i-1,"condition" ] <- data_5min2[2*i,"condition"]
  data_5min2 <- data_5min2[rownames(data_5min2) != 2*i, ]
}
data_5min2 <- data_5min2[-c(7,15), ] 

wt <- melt(data_5min2,id.vars = c("user_id","groupname","condition"))
wt$condition <- as.factor(wt$condition) 
p <- ggplot(wt, aes(x=condition, y=value)) + geom_violin(trim=FALSE) + geom_point()
p + geom_boxplot(width=0.4, fill="white") + ylab("Talking Time during 5-min blocks (S)") + scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) +theme(text = element_text(size=14))


shabshabi <- subset(data_5min2, select = -c(user_id))
whateves <- data.frame(ID=shabshabi[,15], CODN = shabshabi[,16],Means=rowMeans(shabshabi[,1:14],na.rm = TRUE))

whateves$CODN <- as.factor(whateves$CODN)
ggplot(whateves, aes(x = CODN, y = Means)) +  geom_violin(trim=FALSE) + geom_point()+
  geom_boxplot(width=0.4) + ylab("Average talking in 5-min blocks for each group (s)") + xlab("Condition")+
  scale_x_discrete(breaks=c("1", "3"), labels=c("Adaptive", "Baseline")) +theme(text = element_text(size=12))

wt <- wt[,-c(1) ] 
wt$variable <- as.numeric(as.character(wt$variable))
wta <- subset(wt, condition == 1)
wtn <- subset(wt, condition == 3)
wta$condition <-as.factor(wta$condition)
wtn$condition <-as.factor(wtn$condition)

fit <- lm(wta$value~wta$variable,data=wta)
fit2 <- lm(wtn$value~wtn$variable,data=wtn)
plot(wta$variable,wta$value,col ="coral2",panel.first = grid())
points(wtn$variable,wtn$value,col="cyan3",pch = 7)
abline(a=coef(fit)[1], b=coef(fit)[2],col = "coral2")
abline(a=coef(fit2)[1], b=coef(fit2)[2],col = "cyan3")


