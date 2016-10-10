######################
# Pre-processing
######################

#install.packages("XLConnect")
options(java.parameters = "-Xmx7000m")
library(XLConnect)

basePath = file.path("D:/Monash Study/Winter Research/")
input = file.path(basePath, "source/Podes_Survey_2014trim.xlsx")
data.raw = readWorksheetFromFile(input,sheet = 1)

# Retrive class information for vaiable types
class=sapply(data.raw,class)
input = file.path(basePath, "source/Podes_Survey_2014.csv")
data.raw= read.csv(input, header=TRUE, colClass=class,na.strings=c("NA", ""))

# Edit electricit and PLN variable
data.raw$electricity = 0
data.raw$electricity[which(data.raw$R501A1 !=0)] = "PLN"
data.raw$electricity[which(data.raw$R501A2 !=0 & data.raw$R501A1 ==0)] = "nonPLN"
data.raw$electricity[which(data.raw$R501A2 ==0 & data.raw$R501A1 ==0)] = "noE"

data.raw$PLN = 0
data.raw$PLN[which(data.raw$electricity == "PLN")] = 1
data.raw$PLN = as.factor(data.raw$PLN)

data.14 = data.raw
rm(data.raw)

######################################################
# find out villages that changed PLN status from 11-14
######################################################

#install.packages("dplyr")
library(dplyr)
input = file.path(basePath, "source/podes_desa_11.csv")
data.11=read.csv(input)
data.08=read.csv("D:/Monash Study/Winter Research/Source/PODES08.csv")

data.08=tbl_df(data.08)
data.11=tbl_df(data.11)
data.2014=tbl_df(data.2014)

# edit PLN variable for 11 survey data, no info provided for villages that have no power supply
data.08=mutate(data.08,PLN = ifelse(R501A==2,0,ifelse(R501B1 %in%c(0,NA),0,1)))
data.11=mutate(data.11,PLN = ifelse(R501A!=0,1,0))

# prepare to inner-merge with 2014 data
data.2014$id2013=as.numeric(data.2014$id2013)

nonPLN11= filter(data.11,PLN==0)
yesPLN2014= filter(data.2014,PLN==1)

nonPLN08 =filter(data.08,PLN==0)
yesPLN11 = filter(data.11,PLN==1)

data.join11_14 = inner_join(nonPLN11,yesPLN2014,by=c("id11"="id2013"))
data.join08_11 = inner_join(nonPLN08,yesPLN11,by=c("id08"="id11"))

village_id11 = select(data.join11_14, id11)
village_id08 =select(data.join08_11,id08)

# update data.2014 to specify which villages were recently connected

data.2014 =mutate(data.2014, connect = ifelse((data.2014$id2013 %in% village_id11$id11),1,
                                              ifelse((PLN ==1 ),"PLN",0)))

data.11 =mutate(data.11, connect = ifelse((data.11$id11 %in% village_id11$id11),1,
                                              ifelse((PLN ==1 ),"PLN",0)))

data.08 =mutate(data.08, connect = ifelse((data.08$id08 %in% village_id08$id08),1,
                                              ifelse((PLN ==1 ),"PLN",0)))
table(data.08$connect,useNA = "always")

# ouput csv file for Tableau

write.csv(data.11,"connect2011.csv")
write.csv(data.08,"connect2008.csv")

