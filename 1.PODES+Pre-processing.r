
######################
# Pre-processing 2014
######################

#install.packages("XLConnect")
options(java.parameters = "-Xmx7000m")
library(XLConnect)

basePath = file.path("D:/Monash Study/Winter Research/")
input = file.path(basePath, "source/Podes_Survey_2014trim.xlsx")
data.raw = readWorksheetFromFile(input,sheet = 1)

# Retrive class information for vaiable types
class=sapply(data.raw,class)
input = file.path(basePath, "source/Podes_Survey_2014.csv") #saved from original xlsx file with population added
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

#######################
# Read in 2011 and 2008
#######################

#11 data first# 
input = file.path(basePath, "source/2011_trim.xlsx")
data.class = readWorksheetFromFile(input,sheet = 1)

# Retrive class information for vaiable types
class=sapply(data.class,class)
input = file.path(basePath, "source/podes_desa_2011.csv") #saved from original dataset file with villageID added
data.11 = read.csv(input, header=TRUE, colClass=class,na.strings=c("NA", ""))

#08 data second# 
# Treatment done in excel to correct all connect2008.csv data type

input = file.path(basePath, "source/2008_trim.xlsx")
data.class = readWorksheetFromFile(input,sheet = 1)

class=sapply(data.class,class)
input = file.path(basePath, "source/PODES2008.csv") #saved from original dataset file with villageID added
data.08 = read.csv(input, header=TRUE, colClass=class,na.strings=c("NA", ""))


dim(data.11)
dim(data.08)

######################################################
# find out villages that changed PLN status from 11-14
######################################################
library(dplyr)

data.08=tbl_df(data.08)
data.11=tbl_df(data.11)
data.14=tbl_df(data.14)

# edit PLN variable for 11 survey data, no info provided for villages that have no power supply
data.08=mutate(data.08,PLN = ifelse(R501A==2,0,ifelse(R501B1 %in%c(0,NA),0,1)))  #R501B1 asks for number of PLN-connected families
data.11=mutate(data.11,PLN = ifelse(R501A!=0,1,0))

nonPLN11= filter(data.11,PLN==0)
yesPLN14= filter(data.14,PLN==1)

nonPLN08 =filter(data.08,PLN==0)
yesPLN11 = filter(data.11,PLN==1)

data.join11_14 = inner_join(nonPLN11,yesPLN14,by=c("id2011"="id2013"))
data.join08_11 = inner_join(nonPLN08,yesPLN11,by=c("id2008"="id2011"))

village_id11 = select(data.join11_14, id2011)
village_id08 =select(data.join08_11,id2008)

# update data.11 and 08 to specify which villages were recently connected

data.11 =mutate(data.11, connect = ifelse((data.11$id2011 %in% village_id11$id2011),1,
                                              ifelse((PLN ==1 ),"PLN",0)))

data.08 =mutate(data.08, connect = ifelse((data.08$id2008 %in% village_id08$id2008),1,
                                              ifelse((PLN ==1 ),"PLN",0)))

# Add population
data.11$POP = data.11$R401A + data.11$R401B
data.08$POP = data.08$R401A + data.08$R401B

table(data.08$connect,useNA = "always")
table(data.11$connect,useNA = "always")
dim(data.11)
dim(data.08)
#rm(data.join11_14, data.join08_11, nonPLN11, yesPLN14, village_id11, village_id08, nonPLN08, yesPLN11)

na = sort(sapply(data.11,function(x){mean(is.na(x)==TRUE)}),decreasing = TRUE)
na = na[na!=0]

na = as.data.frame(na)
na = na[order(row.names(na)),,drop=FALSE]
write.csv(na, "na.csv")

#################
# Imputation 2011
#################

varName = sapply(data.11, is.numeric)
nominalVar=colnames(data.11)[!varName]
numericVar=colnames(data.11)[varName]

# All nominial variables with NAs equal to unique level "NA"
na.nom=sapply(data.11[,nominalVar],function(df){sum(is.na(df))})
na.nom=na.nom[na.nom!=0]
data.11[,nominalVar][is.na(data.11[,nominalVar])==TRUE]="NA"       

# Recode all numerical NAs
na.num=sapply(data.11[,numericVar],function(df){sum(is.na(df))})
na.num=na.num[na.num!=0]
data.11[,numericVar][is.na(data.11[,numericVar])==TRUE]=0

# Village Head Age should not be 0 when NA
data.11$R1501AK3[data.11$R1501AK3==0] = NA
data.11$R1501BK3[data.11$R1501BK3==0] = NA

#################
# Imputation 2008
#################

varName = sapply(data.08, is.numeric)
nominalVar=colnames(data.08)[!varName]
numericVar=colnames(data.08)[varName]

# All nominial variables with NAs equal to unique level "NA"
na.nom=sapply(data.08[,nominalVar],function(df){sum(is.na(df))})
na.nom=na.nom[na.nom!=0]
data.08[,nominalVar][is.na(data.08[,nominalVar])==TRUE]="NA"

# Recode all numerical NAs
na.num=sapply(data.08[,numericVar],function(df){sum(is.na(df))})
na.num=na.num[na.num!=0]
data.08[,numericVar][is.na(data.08[,numericVar])==TRUE]=0

data.08$R1401A_3[data.08$R1401A_3==0] = NA
data.08$R1401B_3[data.08$R1401B_3==0] = NA

#################
# Imputation 2014
#################

varName = sapply(data.14, is.numeric)
nominalVar=colnames(data.14)[!varName]
numericVar=colnames(data.14)[varName]

# All nominial variables with NAs equal to unique level "NA"
na.nom=sapply(data.14[,nominalVar],function(df){sum(is.na(df))})
na.nom=na.nom[na.nom!=0]
data.14[,nominalVar][is.na(data.14[,nominalVar])==TRUE]="NA"

# Recode all numerical NAs
na.num=sapply(data.14[,numericVar],function(df){sum(is.na(df))})
na.num=na.num[na.num!=0]
data.14[,numericVar][is.na(data.14[,numericVar])==TRUE]=0

data.14$R1601A_K3[data.14$R1601A_K3==0] = NA
data.14$R1601B_K3[data.14$R1601B_K3==0] = NA

######################
# Save to connect file
######################

write.csv(data.11,"connect2011.csv",row.names=FALSE)
write.csv(data.08,"connect2008.csv",row.names=FALSE)
write.csv(data.14,"connect2014.csv",row.names=FALSE)

save(data.11,file = "data11.RData")
save(data.08,file = "data08.RData")
save(data.14,file = "data14.RData")

env = new.env()
load("data11.RData",env )
data.11 = env$data.11

env = new.env()
load("data08.RData",env )
data.08 = env$data.08

dim(data.11)
dim(data.08)

data.11=filter(data.11,data.11$connect != "PLN")
data.08=filter(data.08,data.08$connect != "PLN")

dim(data.11)
dim(data.08)

#####################################
# Information gain for 08 and 11 data
######################################

library(glmnet)
library(dplyr)

# Information gain for 11
library(FSelector)

info.connect_11 = information.gain(connect~., data.11)
info.connect_11 = info.connect_11[order(-info.connect_11$attr_importance),,drop=FALSE]

# info.gain for 08


info.connect_08 = information.gain(connect~., data.08)
info.connect_08 = info.connect_08[order(-info.connect_08$attr_importance),,drop=FALSE]

infogain08 = data.frame(rownames(info.connect_08),info.connect_08)
infogain11 = data.frame(rownames(info.connect_11),info.connect_11)

head(info.connect_08,10)
head(info.connect_11,10)

write.csv(infogain08,"infogain08.csv", row.names = FALSE)
write.csv(infogain11,"infogain11.csv", row.names = FALSE)

table(data.11$R1501AK3,useNA = 'always')

##########
#LASSO
##########

library(glmnet)
library(doMC)
registerDoMC(cores=2)

# Delete all unessary variables
data.11=data.11[,!names(data.11) %in% 
                        c('X',"id2011","KODE_PROV",  "NAMA_PROV",  "KODE_KAB",   "NAMA_KAB",  
                          "KODE_KEC",   "NAMA_KEC",   "KODE_DESA",  "NAMA_DESA",  "R106",
                          "R301", "R302A" ,"R303B","NAMA_PULAU")]

#Problematic R1004A(B,C)K3, R1401A(B)K4, R1402A(B,C)K4(5), 
#they have abnormal levels as factor variables
data.11=data.11[,!names(data.11) %in% 
                  c('R1004AK3', 'R1004BK3', 'R1004CK3', 'R1401AK4',
                    'R1401B1K4','R1401B2K4','R1401B3K4','R1401B4K4','R1401B5K4','R1401B6K4',
                    'R1402A1K4','R1402A2K4','R1402A3K4','R1402A4K4','R1402A1K5','R1402A2K5',
                    'R1402A3K5','R1402A4K5','R1402B1K5','R1402B2K5','R1402B3K5','R1402C1K5',
                    'R1402C1K5','R1402C2K5','R1402C3K5')]

# R807(Majority ethnithity) contains three variables, delete two of them
data.11 = data.11[,!names(data.11) %in% c('R807','R807_2')]

# Delete all level-1 variables to avoid complete separation
level1 = sapply(data.11,function(x){length(unique(x))})
level1 = level1[level1==1]
level1
data.11=data.11[,!names(data.11) %in% c('R501A','PLN','R60108K4')]

# There should be no NAs except for R1501AK3 and R1501BK3 (age of village head and secretary)
# We delete these two variables as retaining the rows is more important than retaining these columns
data.11=data.11[,!names(data.11) %in% c('R1501AK3','R1501BK3')]

dim(data.11)

# check if there are NAs, not allowed for regression!
sapply(data.11,function(df){mean(is.na(df))})

# LASSO Model
options(warn=-1)

x.11 = sparse.model.matrix(connect~.,data.11)[,-1]
y.11 = data.11$connect

fit.11 = cv.glmnet(x.11,y.11,alpha = 1, type.measure = "class", family = "binomial", parallel=TRUE)

plot(fit.11)
print (fit.11)

library(AUC)
pred.11 = predict(fit.11, x.11, type='response')

auc(roc(pred.11, factor(y.11)))
plot(roc(pred.11, factor(y.11)), main = "ROC",col="red")

coef.11 = predict(fit.11, newx = x.11, s = "lambda.1se", type = "coefficients",exact = TRUE)
coef.11 = as.matrix(coef.11)
coef.11 = coef.11[coef.11!=0,,drop=FALSE]

write.csv(coef.11,"coef11.csv")

coef.11
length(coef.11)
