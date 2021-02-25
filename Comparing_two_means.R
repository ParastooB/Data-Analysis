## Prestudy t-test Example

## Change working directory
setwd(dir = "/home/p2baghae/Documents/curiosity-notebook-master/analysis/")
library(ggplot2)

## Read in the data, everything
dataa <- read.csv(file = "output/ultimate.csv", header = T)
# dataa <- dataa[-c(33,34),]
# cond1 <- droplevels(cond1)

dataa <- dataa[-c(29,30,13,14), ] 
dataa$age_english[is.na(dataa$age_english)] <- 0

cond_a <- subset(dataa, dataa$condition == 1) 
cond_n <- subset(dataa, dataa$condition == 3) 

length(which(cond_n$gender == "man"))
length(which(cond_n$gender == "woman"))
length(which(cond_a$gender == "man"))
length(which(cond_a$gender == "woman"))

length(which(cond_n$stem == "0"))
length(which(cond_a$stem == "0"))

length(which(cond_n$native == "1"))
length(which(cond_a$native == "1"))

cond1 <- cond_a$know_rocks
cond2 <- cond_n$know_rocks
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)
t.test(x = cond1, y = cond2, alternative = "less", mu = 0, paired = F, var.equal = T, conf.level = 0.95)



ggplot(cond_n, aes(age)) + geom_bar(fill = "#0073C2FF")

data_audio <- read.csv(file = "output/audio_analysis.csv", header = T)
cond1 <- subset(data_audio, conditions == 1) 
# cond1 <- subset(cond1, select = -c(groupname, talkingTime, conditions, totalTime, user_id))
cond2 <- subset(data_audio, conditions == 3) 
cond1 <- cond1$ratio
cond2 <- cond2$ratio

# t.test(x = cond1, y = cond2)

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Age", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Neutral")
abline(v = mean(cond2), col = "red", lwd = 2)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)
## First test the equality of variances
## your null hypothesis will always be that the variances are equal.
var.test(x = cond1, y = cond2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
## F = 1.163, num df = 31, denom df = 25, p-value = 0.7049 therefore the variences are almost the same!

## Perform the test
## Ho: mu1 = mu2 vs. Ha: mu1 != mu2
t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Ho: mu1 <= mu2 vs. Ha: mu1 > mu2
t.test(x = cond1, y = cond2, alternative = "greater", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Ho: mu1 >= mu2 vs. Ha: mu1 < mu2
t.test(x = cond1, y = cond2, alternative = "less", mu = 0, paired = F, var.equal = T, conf.level = 0.95)

# check the ages and genders of groups and create mix gender groups
dataa$age_english[is.na(dataa$age_english)] <- 0
levels(dataa$gender) <- c(levels(dataa$gender), "coed")
for(i in 30:1) {
  if (dataa[2*i-1,"gender"] == dataa[2*i,"gender"]){
   keep <-dataa[2*i,"gender"]
  } else {
    keep <- "coed"
  }
  dataa[2*i-1,] <- dataa[2*i-1,] + dataa[2*i,]
  dataa[2*i-1,"groupname" ] <- dataa[2*i,"groupname"]
  dataa[2*i-1,"condition" ] <- dataa[2*i,"condition"]
  dataa[2*i-1,"totalTime" ] <- dataa[2*i,"totalTime"]
  dataa[2*i-1,"age"] <- abs(dataa[2*i-1,"age"] - 2*dataa[2*i,"age"])
  dataa[2*i-1,"gender"] <- keep
  dataa <- dataa[rownames(dataa) != 2*i, ]
}

sum(dataa$gender == 'man' & dataa$condition == 1)

dataa$good_student[is.na(dataa$good_student)] <- 2.5

cond_a <- subset(dataa, dataa$condition == 1) 
cond_n <- subset(dataa, dataa$condition == 3) 

cond1 <- cond_a$good_student
cond2 <- cond_n$good_student

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(B=cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Good Student", xlab = "Adaptive")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Neutral")
abline(v = mean(cond2), col = "red", lwd = 2)
mean(cond1)
mean(cond2)
sd(cond1)
sd(cond2)

t.test(x = cond1, y = cond2, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95)
