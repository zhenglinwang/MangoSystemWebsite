library(ggplot2)
library(scales)


dir<-"C:/Users/anderstn/Desktop/RStudio/MangoApp/DM" #change this to your directory

temp <- list.files(pattern="*.csv", path = dir, full.names = TRUE)
importDM<-do.call("rbind", lapply(temp, read.csv, header = TRUE))
rawDM <- as.data.frame(importDM)

dmScat <- data.frame(sort(rawDM$Prediction.1.Value))
colnames(dmScat) <- c("DM")
numb <- nrow(dmScat)


dmScat$nValue <- c(1:numb)
dmScat$percent <- (dmScat$nValue/numb)*100


cumGraph <- ggplot(dmScat, aes(x = dmScat$DM, y = percent)) +
  geom_point(color = "orange", size = 5, shape = 1) +
  labs(x = "DM", y = "Cumulative %")+
  theme(panel.background = element_rect(fill = "black"))

cumGraph


