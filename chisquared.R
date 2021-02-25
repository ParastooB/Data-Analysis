library(MASS)

smoke <- matrix(c(2,5,8,2,4,7),ncol=2,byrow=FALSE)
colnames(smoke) <- c("adaptive","neutral")
rownames(smoke) <- c("men","women","mix")
smoke <- as.table(smoke)
smoke
chisq.test(smoke) 