setwd("D:/Monash Study/Winter Research/R codes")
library(XLConnect)
library(dplyr)

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

##########
#LASSO
##########

library(glmnet)

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

# Delete all level-1 variables

level1 = sapply(data.11,function(x){length(unique(x))})
level1 = level1[level1==1]
level1
data.11=data.11[,!names(data.11) %in% c('R501A','PLN','R60108K4')]

# Recode all categorical NAs
varName = sapply(data.11, is.numeric)
table(varName)
nominalVar=colnames(data.11)[!varName]
numericVar=colnames(data.11)[varName]

na.nom=sapply(data.11[,nominalVar],function(df){sum(is.na(df))})
na.nom=na.nom[na.nom!=0]
data.11[,nominalVar][is.na(data.11[,nominalVar])==TRUE]="NA"

# Recode all numerical NAs
na.num=sapply(data.11[,numericVar],function(df){sum(is.na(df))})
na.num=na.num[na.num!=0]
data.11[,numericVar][is.na(data.11[,numericVar])==TRUE]=0

#Model

x.11 = sparse.model.matrix(connect~.,data.11)[,-1]
y.11 = data.11$connect

fit.11 = cv.glmnet(x.11,y.11,alpha = 1, family = "binomial")

plot(fit.11)
print (fit.11)
#43.5% deviation explained
lamb.min = fit.11$lambda.min
coef.11 = coef(fit.11,s="lambda.min") #1se

coef.11 = as.matrix(coef.11)

coef.11 = coef.11[coef.11!=0,,drop=FALSE]

write.csv(coef.11,"coef11.csv")


data.14=read.csv("connect2014.csv")
