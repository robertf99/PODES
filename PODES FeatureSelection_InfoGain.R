setwd("D:/Monash Study/Winter Research/R codes")
library(XLConnect)

###################################
# Correct type for 08 and 11 data
###################################

#11 first#

basePath = file.path("D:/Monash Study/Winter Research/")
input = file.path(basePath, "source/2011_trim.xlsx")
data.class = readWorksheetFromFile(input,sheet = 2)

# Retrive class information for vaiable types
class=sapply(data.class,class)
input = file.path(basePath, "source/connect2011.csv")
data.11 = read.csv(input, header=TRUE, colClass=class,na.strings=c("NA", ""))

# Add population
data.11$pop = data.11$R401A + data.11$R401B


#08 data second# 

# Treatment to be done in excel to convert all connect08.csv into numerical

input = file.path(basePath, "source/2008_trim.xlsx")
data.class = readWorksheetFromFile(input,sheet = 1)

class=sapply(data.class,class)
input = file.path(basePath, "source/connect2008.csv")
data.08 = read.csv(input, header=TRUE, colClass=class,na.strings=c("NA", ""))

data.08$pop = data.08$R401A + data.08$R401B

data.11=filter(data.11,data.11$connect != "PLN")
data.08=filter(data.08,data.08$connect != "PLN")

#####################################
# Information gain for 08 and 11 data
######################################

library(doMC)
library(glmnet)
library(dplyr)
registerDoMC(cores=2)

# Information gain for 11
library(FSelector)

info.connect_11 = information.gain(connect~., data.11)
info.connect_11 = info.connect_11[order(-info.connect_11$attr_importance),,drop=FALSE]

# info.gain for 08


info.connect_08 = information.gain(connect~., data.08)
info.connect_08 = info.connect_08[order(-info.connect_08$attr_importance),,drop=FALSE]

infogain08 = data.frame(rownames(info.connect_08),info.connect_08)
infogain11 = data.frame(rownames(info.connect_11),info.connect_11)

write.csv(infogain08,"infogain08.csv")
write.csv(infogain11,"infogain11.csv")


