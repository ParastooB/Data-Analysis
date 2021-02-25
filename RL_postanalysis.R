## Prestudy t-test Example

## Change working directory
setwd(dir = "/RL_tables/")
library(ggplot2)

files = list.files(path = "/RL_tables")
dataa <- read.csv(file = "ZS2.csv", header = T)


genPDF = function(input) {
  ## Read in the data, everything
  dataa <- read.csv(file = input, header = T)
  
  dataa$random <- c(NA, head(dataa['random'], dim(dataa)[1] - 1)[[1]])
  dataa = dataa[-1,]
  
  ########### PLOT #######################
  # if row numbers are integers (most likely!)
  dataa$interaction <- as.numeric(row.names(dataa))
  
  sp<-ggplot(dataa, aes(x=interaction, y=reward, color=random)) + geom_point() +geom_text(aes(label=interaction),hjust=0, vjust=0) +
    geom_hline(aes(yintercept= mean(reward[random=="True"])),color="green", linetype="dashed") +
    geom_hline(aes(yintercept= mean(reward[random=="False"])),color="red", linetype="dashed") 
  sp 
  
  index = which(files == input)
  file_name =paste('A', strsplit(input, ".",fixed = TRUE)[[1]][1], '.tiff', sep = "")
  # setEPS()
  # postscript()
  tiff(filename = file_name,width = 800, height = 480)
  print(sp)
  print(file_name)
  cond_rand <- subset(dataa, dataa$random == "True") 
  cond_adapt <- subset(dataa, dataa$random == "False") 
  print(t.test(x = cond_rand$reward, y = cond_adapt$reward, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.95))
  dev.off()
}

for (i in 1:length(files)) {
  genPDF(files[i])
}
