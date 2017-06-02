
library(dplyr)

env = new.env()
load("data11.RData",env )
data.11 = env$data.11

env = new.env()
load("data14.RData",env )
data.14 = env$data.14

data.14=filter(data.14,data.14$PLN != 1)
data.11=filter(data.11,data.11$connect != "PLN")

dim(data.14)
dim(data.11)

comp = function(x,y){
  a = unique(x)[!(unique(x) %in% y)]
  b = unique(y)[!(unique(y) %in% x)]
  cat("uniqueA: ")
  for (i in a){
      cat(i,sum(x==i),", ")
  }
  cat ('\n')
  cat("uniqueB: ")
  for (j in b){
      cat(j,sum(y==j),", ")
  }
  cat ('\n')
}

table(data.11$R903EK2)
table(data.14$R903E_K2)

sapply(data.14,class)

colnames(data.14)[which(colnames(data.14)== 'id2013')] = "villageID"
colnames(data.11)[which(colnames(data.11)== 'id2011')] = "villageID"

comp(data.14$R101,data.11$KODE_PROV)

unique(data.frame(data.14$R101,data.14$R101N))
unique(data.frame(data.11$KODE_PROV,data.11$NAMA_PROV))

# Make new Province varaible
colnames(data.14)[which(colnames(data.14)== 'R101')] = "provinceID"
colnames(data.11)[which(colnames(data.11)== 'KODE_PROV')] = "provinceID"
data.14$provinceID[data.14$provinceID ==65] = 64
data.11 = filter(data.11,data.11$provinceID != 31)
data.11 = filter(data.11,data.11$provinceID != 36)
dim(data.14)
dim(data.11)

unique(data.frame(data.14$provinceID,data.14$R101N))

comp(data.14$R102,data.11$KODE_KAB)

# Make new District varaible
colnames(data.14)[which(colnames(data.14)== 'R102')] = "districtID"
colnames(data.11)[which(colnames(data.11)== 'KODE_KAB')] = "districtID"

comp(data.14$R103,data.11$KODE_KEC)

unique(data.14[data.14$R103 %in% c(84,115,118,119,124,184,185,194,195,611,612,614,68,551,552,541,545),c('R103','R101N')])
dim(data.14[data.14$R103 %in% c(84,115,118,119,124,184,185,194,195,611,612,614,68,551,552,541,545),])

colnames(data.14)[which(colnames(data.14)== 'R103')] = "subDisID"
colnames(data.11)[which(colnames(data.11)== 'KODE_KEC')] = "subDisID"
data.14 = filter(data.14, !(data.14$subDisID %in% c(84,115,118,119,124,184,185,194,195,611,612,614,68,551,552,541,545)))
data.11 = filter(data.11, !(data.11$subDisID %in% c(300,114,133,104,145,172,229,232,233,234,235,236,237,238,239,242,243,244,245,246,247,248)))
dim(data.14)
dim(data.11)

comp(data.14$R301,data.11$R301)
table(data.14$R301)
table(data.11$R301)

# update 2014 R301 value
colnames(data.14)[which(colnames(data.14)== 'R301')] = "villageStatus"
colnames(data.11)[which(colnames(data.11)== 'R301')] = "villageStatus"
data.14$villageStatus = recode(data.14$villageStatus, '5'='3')
comp(data.14$villageStatus,data.11$villageStatus)
table(data.14$villageStatus)
table(data.11$villageStatus)

#304A
table(data.11$R304A)
table(data.14$R304A)

# Update 304A
data.11 = mutate(data.11, SLS = ifelse(data.11$R304A == 0,"2","1"))
data.14 = mutate(data.14, SLS = ifelse(data.14$R304A == 2,"2","1"))
table(data.11$SLS)
table(data.14$SLS)

#304C
table(data.11$R304C)
table(data.14$R304BA_K4+data.14$R304BB_K4+data.14$R304BC_K4+data.14$R304BD_K4)

# Update 304C as SLS_N
colnames(data.11)[which(colnames(data.11)== 'R304C')] = "SLS_N"
data.14 = mutate(data.14, SLS_N = data.14$R304BA_K4+data.14$R304BB_K4+data.14$R304BC_K4+data.14$R304BD_K4)
table(data.11$SLS_N)
table(data.14$SLS_N)

table(data.11$R305A)
table(data.14$R305B)

# update 305A as Slope
colnames(data.11)[which(colnames(data.11)== 'R305A')] = "slope"
colnames(data.14)[which(colnames(data.14)== 'R305B')] = "slope"
data.11$slope = recode(data.11$slope, "2" = "1", "3" = "2", "4" = "3" )
table(data.11$slope)
table(data.14$slope)

table(data.11$R305D)
table(data.14$R307A)

# Update AtSea, AtSeaUse and AtSeaMangrove
colnames(data.11)[which(colnames(data.11)== 'R305D')] = "atSea"
colnames(data.14)[which(colnames(data.14)== 'R307A')] = "atSea"

colnames(data.11)[which(colnames(data.11)== 'R305E2A')] = "atSeaUse_1"
colnames(data.11)[which(colnames(data.11)== 'R305E2B')] = "atSeaUse_2"
colnames(data.11)[which(colnames(data.11)== 'R305E2C')] = "atSeaUse_3"
colnames(data.11)[which(colnames(data.11)== 'R305E2D')] = "atSeaUse_4"
colnames(data.11)[which(colnames(data.11)== 'R305E2E')] = "atSeaUse_5"

colnames(data.14)[which(colnames(data.14)== 'R307B1A')] = "atSeaUse_1"
colnames(data.14)[which(colnames(data.14)== 'R307B1B')] = "atSeaUse_2"
colnames(data.14)[which(colnames(data.14)== 'R307B1C')] = "atSeaUse_3"
colnames(data.14)[which(colnames(data.14)== 'R307B1D')] = "atSeaUse_4"
colnames(data.14)[which(colnames(data.14)== 'R307B1E')] = "atSeaUse_5"

colnames(data.11)[which(colnames(data.11)== 'R305E3')] = "atSeaMangrove"
colnames(data.14)[which(colnames(data.14)== 'R307B2')] = "atSeaMangrove"

table(data.11$R306A)
table(data.14$R308A)

colnames(data.11)[which(colnames(data.11)== 'R306A')] = "byForest"
colnames(data.14)[which(colnames(data.14)== 'R308A')] = "byForest"
table(data.11$byForest)
table(data.14$byForest)

table(data.11$R306B)
table(data.14$R308B)

colnames(data.11)[which(colnames(data.11)== 'R306B')] = "byForestUse"
colnames(data.14)[which(colnames(data.14)== 'R308B')] = "byForestUse"
table(data.11$byForestUse)
table(data.14$byForestUse)

colnames(data.11)[which(colnames(data.11)== 'R402A')] = "migrantM"
colnames(data.11)[which(colnames(data.11)== 'R402B')] = "migrantF"
colnames(data.14)[which(colnames(data.14)== 'R403B1')] = "migrantM"
colnames(data.14)[which(colnames(data.14)== 'R403B2')] = "migrantF"

# match!
comp(data.11$R403A, data.14$R404A)
comp(data.11$R403B, data.14$R404B1) 

colnames(data.11)[which(colnames(data.11)== 'R403A')] = "incomeSource"
colnames(data.14)[which(colnames(data.14)== 'R404A')] = "incomeSource"
colnames(data.11)[which(colnames(data.11)== 'R403B')] = "agriType"
colnames(data.14)[which(colnames(data.14)== 'R404B1')] = "agriType"

colnames(data.11)[which(colnames(data.11)== 'R502A')] = "strLight"
colnames(data.14)[which(colnames(data.14)== 'R502A')] = "strLight"
colnames(data.11)[which(colnames(data.11)== 'R502B')] = "strLightEngy"
colnames(data.14)[which(colnames(data.14)== 'R502B')] = "strLightEngy"

# match!
comp(data.11$R503, data.14$R503)
comp(data.11$R504, data.14$R504)
comp(data.11$R505A, data.14$R505A)
comp(data.11$R505B, data.14$R505B)

colnames(data.11)[which(colnames(data.11)== 'R503')] = "fuelCook"
colnames(data.14)[which(colnames(data.14)== 'R503')] = "fuelCook"
colnames(data.11)[which(colnames(data.11)== 'R504')] = "toilet"
colnames(data.14)[which(colnames(data.14)== 'R504')] = "toilet"
colnames(data.11)[which(colnames(data.11)== 'R505A')] = "litter"
colnames(data.14)[which(colnames(data.14)== 'R505A')] = "litter"
colnames(data.11)[which(colnames(data.11)== 'R505B')] = "pubTrash"
colnames(data.14)[which(colnames(data.14)== 'R505B')] = "pubTrash"

comp(data.11$R506AK2, data.14$R508A_K2)
comp(data.11$R506AK3, data.14$R508A_K3)
comp(data.11$R506AK4, data.14$R508A_K4)

colnames(data.11)[which(colnames(data.11)== 'R506AK2')] = "river"
colnames(data.11)[which(colnames(data.11)== 'R506AK3')] = "irrigation"
colnames(data.11)[which(colnames(data.11)== 'R506AK4')] = "lake"
colnames(data.14)[which(colnames(data.14)== 'R508A_K2')] = "river"
colnames(data.14)[which(colnames(data.14)== 'R508A_K3')] = "irrigation"
colnames(data.14)[which(colnames(data.14)== 'R508A_K4')] = "lake"

colnames(data.11)[which(colnames(data.11)== 'R506B1K2')] = "riverUse_1"
colnames(data.11)[which(colnames(data.11)== 'R506B2K2')] = "riverUse_2"
colnames(data.11)[which(colnames(data.11)== 'R506B3K2')] = "riverUse_3"
colnames(data.11)[which(colnames(data.11)== 'R506B4K2')] = "riverUse_4"
colnames(data.11)[which(colnames(data.11)== 'R506B5K2')] = "riverUse_5"
colnames(data.11)[which(colnames(data.11)== 'R506B1K3')] = "irrigationUse_1"
colnames(data.11)[which(colnames(data.11)== 'R506B2K3')] = "irrigationUse_2"
colnames(data.11)[which(colnames(data.11)== 'R506B3K3')] = "irrigationUse_3"
colnames(data.11)[which(colnames(data.11)== 'R506B4K3')] = "irrigationUse_4"
colnames(data.11)[which(colnames(data.11)== 'R506B1K4')] = "lakeUse_1"
colnames(data.11)[which(colnames(data.11)== 'R506B2K4')] = "lakeUse_2"
colnames(data.11)[which(colnames(data.11)== 'R506B3K4')] = "lakeUse_3"
colnames(data.11)[which(colnames(data.11)== 'R506B4K4')] = "lakeUse_4"
colnames(data.11)[which(colnames(data.11)== 'R506B5K4')] = "lakeUse_5"

colnames(data.14)[which(colnames(data.14)== 'R508B1_K2')] = "riverUse_1"
colnames(data.14)[which(colnames(data.14)== 'R508B2_K2')] = "riverUse_2"
colnames(data.14)[which(colnames(data.14)== 'R508B3_K2')] = "riverUse_3"
colnames(data.14)[which(colnames(data.14)== 'R508B4_K2')] = "riverUse_4"
colnames(data.14)[which(colnames(data.14)== 'R508B6_K2')] = "riverUse_5"
colnames(data.14)[which(colnames(data.14)== 'R508B1_K3')] = "irrigationUse_1"
colnames(data.14)[which(colnames(data.14)== 'R508B2_K3')] = "irrigationUse_2"
colnames(data.14)[which(colnames(data.14)== 'R508B3_K3')] = "irrigationUse_3"
colnames(data.14)[which(colnames(data.14)== 'R508B4_K3')] = "irrigationUse_4"
colnames(data.14)[which(colnames(data.14)== 'R508B1_K4')] = "lakeUse_1"
colnames(data.14)[which(colnames(data.14)== 'R508B2_K4')] = "lakeUse_2"
colnames(data.14)[which(colnames(data.14)== 'R508B3_K4')] = "lakeUse_3"
colnames(data.14)[which(colnames(data.14)== 'R508B4_K4')] = "lakeUse_4"
colnames(data.14)[which(colnames(data.14)== 'R508B6_K4')] = "lakeUse_5"

comp(data.11$R508A,data.14$R509B)

colnames(data.11)[which(colnames(data.11)== 'R508A')] = "byRiver"
colnames(data.14)[which(colnames(data.14)== 'R509B')] = "byRiver"
colnames(data.11)[which(colnames(data.11)== 'R508B')] = "byRiver_village"
colnames(data.14)[which(colnames(data.14)== 'R509C1')] = "byRiver_village"
colnames(data.11)[which(colnames(data.11)== 'R508C')] = "byRiver_house"
colnames(data.14)[which(colnames(data.14)== 'R509C2')] = "byRiver_house"
colnames(data.11)[which(colnames(data.11)== 'R508D')] = "byRiver_family"
colnames(data.14)[which(colnames(data.14)== 'R509C3')] = "byRiver_family"

comp(data.11$R509A,data.14$R510A)

colnames(data.11)[which(colnames(data.11)== 'R509A')] = "bySUTET"
colnames(data.14)[which(colnames(data.14)== 'R510A')] = "bySUTET"
colnames(data.11)[which(colnames(data.11)== 'R509B1')] = "bySUTET_village"
colnames(data.14)[which(colnames(data.14)== 'R510C1')] = "bySUTET_village"
colnames(data.11)[which(colnames(data.11)== 'R509B2')] = "bySUTET_house"
colnames(data.14)[which(colnames(data.14)== 'R510C2')] = "bySUTET_house"
colnames(data.11)[which(colnames(data.11)== 'R509B3')] = "bySUTET_family"
colnames(data.14)[which(colnames(data.14)== 'R510C3')] = "bySUTET_family"

comp(data.11$R510A,data.14$R511A)

colnames(data.11)[which(colnames(data.11)== 'R510A')] = "bySlum"
colnames(data.14)[which(colnames(data.14)== 'R511A')] = "bySlum"
colnames(data.11)[which(colnames(data.11)== 'R510B1')] = "bySlum_village"
colnames(data.14)[which(colnames(data.14)== 'R511B1')] = "bySlum_village"
colnames(data.11)[which(colnames(data.11)== 'R510B2')] = "bySlum_house"
colnames(data.14)[which(colnames(data.14)== 'R511B2')] = "bySlum_house"
colnames(data.11)[which(colnames(data.11)== 'R510B3')] = "bySlum_family"
colnames(data.14)[which(colnames(data.14)== 'R511B3')] = "bySlum_family"

comp(data.11$R511AK3,data.14$R512A1_K3)
comp(data.11$R511BK3,data.14$R512B1_K3)
comp(data.11$R511CK3,data.14$R512C1_K3)

colnames(data.11)[which(colnames(data.11)== 'R511AK2')] = "pollution_1"
colnames(data.14)[which(colnames(data.14)== 'R512A_K2')] = "pollution_1"
colnames(data.11)[which(colnames(data.11)== 'R511AK3')] = "pollution_1Src"
colnames(data.14)[which(colnames(data.14)== 'R512A1_K3')] = "pollution_1Src"
colnames(data.11)[which(colnames(data.11)== 'R511AK4')] = "pollution_1Cmplt"
colnames(data.14)[which(colnames(data.14)== 'R512A_K4')] = "pollution_1Cmplt"

colnames(data.11)[which(colnames(data.11)== 'R511BK2')] = "pollution_2"
colnames(data.14)[which(colnames(data.14)== 'R512B_K2')] = "pollution_2"
colnames(data.11)[which(colnames(data.11)== 'R511BK3')] = "pollution_2Src"
colnames(data.14)[which(colnames(data.14)== 'R512B1_K3')] = "pollution_2Src"
colnames(data.11)[which(colnames(data.11)== 'R511BK4')] = "pollution_2Cmplt"
colnames(data.14)[which(colnames(data.14)== 'R512B_K4')] = "pollution_2Cmplt"

colnames(data.11)[which(colnames(data.11)== 'R511CK2')] = "pollution_3"
colnames(data.14)[which(colnames(data.14)== 'R512C_K2')] = "pollution_3"
colnames(data.11)[which(colnames(data.11)== 'R511CK3')] = "pollution_3Src"
colnames(data.14)[which(colnames(data.14)== 'R512C1_K3')] = "pollution_3Src"
colnames(data.11)[which(colnames(data.11)== 'R511CK4')] = "pollution_3Cmplt"
colnames(data.14)[which(colnames(data.14)== 'R512C_K4')] = "pollution_3Cmplt"

table(data.11$R512A)
table(data.14$R513)

colnames(data.11)[which(colnames(data.11)== 'R512A')] = "burnField"
colnames(data.14)[which(colnames(data.14)== 'R513')] = "burnField"

colnames(data.11)[which(colnames(data.11)== 'R513')] = "excavation"
colnames(data.14)[which(colnames(data.14)== 'R514')] = "excavation"

table(data.11$R60101K3)
table(data.14$R601A_K3+data.14$R601A_K5+data.14$R601A_K7)

colnames(data.11)[which(colnames(data.11)== 'R60101K2')] = "disaster_1"
colnames(data.14)[which(colnames(data.14)== 'R601A_K2')] = "disaster_1"
colnames(data.11)[which(colnames(data.11)== 'R60101K3')] = "disaster_1N"
data.14$disaster_1N = data.14$R601A_K3+data.14$R601A_K5+data.14$R601A_K7
colnames(data.11)[which(colnames(data.11)== 'R60101K4')] = "disaster_1Dead"
data.14$disaster_1Dead = data.14$R601A_K4 + data.14$R601A_K6 + data.14$R601A_K8

colnames(data.11)[which(colnames(data.11)== 'R60102K2')] = "disaster_2"
colnames(data.14)[which(colnames(data.14)== 'R601B_K2')] = "disaster_2"
colnames(data.11)[which(colnames(data.11)== 'R60102K3')] = "disaster_2N"
data.14$disaster_2N = data.14$R601B_K3+data.14$R601B_K5+data.14$R601B_K7
colnames(data.11)[which(colnames(data.11)== 'R60102K4')] = "disaster_2Dead"
data.14$disaster_2Dead = data.14$R601B_K4 + data.14$R601B_K6 + data.14$R601B_K8

colnames(data.11)[which(colnames(data.11)== 'R60103K2')] = "disaster_3"
colnames(data.14)[which(colnames(data.14)== 'R601C_K2')] = "disaster_3"
colnames(data.11)[which(colnames(data.11)== 'R60103K3')] = "disaster_3N"
data.14$disaster_3N = data.14$R601C_K3+data.14$R601C_K5+data.14$R601C_K7
colnames(data.11)[which(colnames(data.11)== 'R60103K4')] = "disaster_3Dead"
data.14$disaster_3Dead = data.14$R601C_K4 + data.14$R601C_K6 + data.14$R601C_K8

colnames(data.11)[which(colnames(data.11)== 'R60104K2')] = "disaster_4"
colnames(data.14)[which(colnames(data.14)== 'R601D_K2')] = "disaster_4"
colnames(data.11)[which(colnames(data.11)== 'R60104K3')] = "disaster_4N"
data.14$disaster_4N = data.14$R601D_K3+data.14$R601D_K5+data.14$R601D_K7
colnames(data.11)[which(colnames(data.11)== 'R60104K4')] = "disaster_4Dead"
data.14$disaster_4Dead = data.14$R601D_K4 + data.14$R601D_K6 + data.14$R601D_K8

colnames(data.11)[which(colnames(data.11)== 'R60105K2')] = "disaster_5"
colnames(data.14)[which(colnames(data.14)== 'R601E_K2')] = "disaster_5"
colnames(data.11)[which(colnames(data.11)== 'R60105K3')] = "disaster_5N"
data.14$disaster_5N = data.14$R601E_K3+data.14$R601E_K5+data.14$R601E_K7
colnames(data.11)[which(colnames(data.11)== 'R60105K4')] = "disaster_5Dead"
data.14$disaster_5Dead = data.14$R601E_K4 + data.14$R601E_K6 + data.14$R601E_K8

colnames(data.11)[which(colnames(data.11)== 'R60106K2')] = "disaster_6"
colnames(data.14)[which(colnames(data.14)== 'R601F_K2')] = "disaster_6"
colnames(data.11)[which(colnames(data.11)== 'R60106K3')] = "disaster_6N"
data.14$disaster_6N = data.14$R601F_K3+data.14$R601F_K5+data.14$R601F_K7
colnames(data.11)[which(colnames(data.11)== 'R60106K4')] = "disaster_6Dead"
data.14$disaster_6Dead = data.14$R601F_K4 + data.14$R601F_K6 + data.14$R601F_K8

colnames(data.11)[which(colnames(data.11)== 'R60107K2')] = "disaster_7"
colnames(data.14)[which(colnames(data.14)== 'R601G_K2')] = "disaster_7"
colnames(data.11)[which(colnames(data.11)== 'R60107K3')] = "disaster_7N"
data.14$disaster_7N = data.14$R601G_K3+data.14$R601G_K5+data.14$R601G_K7
colnames(data.11)[which(colnames(data.11)== 'R60107K4')] = "disaster_7Dead"
data.14$disaster_7Dead = data.14$R601G_K4 + data.14$R601G_K6 + data.14$R601G_K8

colnames(data.11)[which(colnames(data.11)== 'R60108K2')] = "disaster_8"
colnames(data.14)[which(colnames(data.14)== 'R601H_K2')] = "disaster_8"
colnames(data.11)[which(colnames(data.11)== 'R60108K3')] = "disaster_8N"
data.14$disaster_8N = data.14$R601H_K3+data.14$R601H_K5+data.14$R601H_K7
colnames(data.11)[which(colnames(data.11)== 'R60108K4')] = "disaster_8Dead"
data.14$disaster_8Dead = data.14$R601H_K4 + data.14$R601H_K6 + data.14$R601H_K8

colnames(data.11)[which(colnames(data.11)== 'R60109K2')] = "disaster_9"
colnames(data.14)[which(colnames(data.14)== 'R601I_K2')] = "disaster_9"
colnames(data.11)[which(colnames(data.11)== 'R60109K3')] = "disaster_9N"
data.14$disaster_9N = data.14$R601I_K3+data.14$R601I_K5+data.14$R601I_K7
colnames(data.11)[which(colnames(data.11)== 'R60109K4')] = "disaster_9Dead"
data.14$disaster_9Dead = data.14$R601I_K4 + data.14$R601I_K6 + data.14$R601I_K8

colnames(data.11)[which(colnames(data.11)== 'R60110K2')] = "disaster_10"
colnames(data.14)[which(colnames(data.14)== 'R601J_K2')] = "disaster_10"
colnames(data.11)[which(colnames(data.11)== 'R60110K3')] = "disaster_10N"
data.14$disaster_10N = data.14$R601J_K3+data.14$R601J_K5+data.14$R601J_K7
colnames(data.11)[which(colnames(data.11)== 'R60110K4')] = "disaster_10Dead"
data.14$disaster_10Dead = data.14$R601J_K4 + data.14$R601J_K6 + data.14$R601J_K8

table(data.11$disaster_1N)
table(data.14$disaster_1N)

colnames(data.11)[which(colnames(data.11)== 'R701AK2')] = "edu_1State"
colnames(data.14)[which(colnames(data.14)== 'R701A_K2')] = "edu_1State"
colnames(data.11)[which(colnames(data.11)== 'R701AK3')] = "edu_1Private"
colnames(data.14)[which(colnames(data.14)== 'R701A_K3')] = "edu_1Private"
colnames(data.11)[which(colnames(data.11)== 'R701AK4')] = "edu_1Distance"
colnames(data.14)[which(colnames(data.14)== 'R701A_K4')] = "edu_1Distance"

colnames(data.11)[which(colnames(data.11)== 'R701BK2')] = "edu_2State"
colnames(data.14)[which(colnames(data.14)== 'R701B_K2')] = "edu_2State"
colnames(data.11)[which(colnames(data.11)== 'R701BK3')] = "edu_2Private"
colnames(data.14)[which(colnames(data.14)== 'R701B_K3')] = "edu_2Private"
colnames(data.11)[which(colnames(data.11)== 'R701BK4')] = "edu_2Distance"
colnames(data.14)[which(colnames(data.14)== 'R701B_K4')] = "edu_2Distance"

colnames(data.11)[which(colnames(data.11)== 'R701CK2')] = "edu_3State"
colnames(data.14)[which(colnames(data.14)== 'R701C_K2')] = "edu_3State"
colnames(data.11)[which(colnames(data.11)== 'R701CK3')] = "edu_3Private"
colnames(data.14)[which(colnames(data.14)== 'R701C_K3')] = "edu_3Private"
colnames(data.11)[which(colnames(data.11)== 'R701CK4')] = "edu_3Distance"
colnames(data.14)[which(colnames(data.14)== 'R701C_K4')] = "edu_3Distance"

colnames(data.11)[which(colnames(data.11)== 'R701DK2')] = "edu_4State"
colnames(data.14)[which(colnames(data.14)== 'R701D_K2')] = "edu_4State"
colnames(data.11)[which(colnames(data.11)== 'R701DK3')] = "edu_4Private"
colnames(data.14)[which(colnames(data.14)== 'R701D_K3')] = "edu_4Private"
colnames(data.11)[which(colnames(data.11)== 'R701DK4')] = "edu_4Distance"
colnames(data.14)[which(colnames(data.14)== 'R701D_K4')] = "edu_4Distance"

colnames(data.11)[which(colnames(data.11)== 'R701EK2')] = "edu_5State"
colnames(data.14)[which(colnames(data.14)== 'R701E_K2')] = "edu_5State"
colnames(data.11)[which(colnames(data.11)== 'R701EK3')] = "edu_5Private"
colnames(data.14)[which(colnames(data.14)== 'R701E_K3')] = "edu_5Private"
colnames(data.11)[which(colnames(data.11)== 'R701EK4')] = "edu_5Distance"
colnames(data.14)[which(colnames(data.14)== 'R701E_K4')] = "edu_5Distance"

colnames(data.11)[which(colnames(data.11)== 'R701FK2')] = "edu_6State"
colnames(data.14)[which(colnames(data.14)== 'R701F_K2')] = "edu_6State"
colnames(data.11)[which(colnames(data.11)== 'R701FK3')] = "edu_6Private"
colnames(data.14)[which(colnames(data.14)== 'R701F_K3')] = "edu_6Private"

colnames(data.11)[which(colnames(data.11)== 'R701GK2')] = "edu_7State"
colnames(data.14)[which(colnames(data.14)== 'R701G_K2')] = "edu_7State"
colnames(data.11)[which(colnames(data.11)== 'R701GK3')] = "edu_7Private"
colnames(data.14)[which(colnames(data.14)== 'R701G_K3')] = "edu_7Private"

colnames(data.11)[which(colnames(data.11)== 'R701HK3')] = "edu_8Private"
colnames(data.14)[which(colnames(data.14)== 'R701H_K3')] = "edu_8Private"

colnames(data.11)[which(colnames(data.11)== 'R701IK3')] = "edu_9Private"
colnames(data.14)[which(colnames(data.14)== 'R701I_K3')] = "edu_9Private"

colnames(data.11)[which(colnames(data.11)== 'R701JK3')] = "edu_10Private"
colnames(data.14)[which(colnames(data.14)== 'R701J_K3')] = "edu_10Private"

colnames(data.11)[which(colnames(data.11)== 'R702A')] = "voc_1N"
colnames(data.14)[which(colnames(data.14)== 'R703A')] = "voc_1N"
colnames(data.11)[which(colnames(data.11)== 'R702B')] = "voc_2N"
colnames(data.14)[which(colnames(data.14)== 'R703B')] = "voc_2N"
colnames(data.11)[which(colnames(data.11)== 'R702C')] = "voc_3N"
colnames(data.14)[which(colnames(data.14)== 'R703C')] = "voc_3N"
colnames(data.11)[which(colnames(data.11)== 'R702D')] = "voc_4N"
colnames(data.14)[which(colnames(data.14)== 'R703D')] = "voc_4N"
colnames(data.11)[which(colnames(data.11)== 'R702E')] = "voc_5N"
colnames(data.14)[which(colnames(data.14)== 'R703E')] = "voc_5N"
colnames(data.11)[which(colnames(data.11)== 'R702F')] = "voc_6N"
colnames(data.14)[which(colnames(data.14)== 'R703F')] = "voc_6N"
colnames(data.11)[which(colnames(data.11)== 'R702G')] = "voc_7N"
colnames(data.14)[which(colnames(data.14)== 'R703G')] = "voc_7N"

table(data.14$R702F,useNA = 'always')
table(data.11$R703E)

colnames(data.11)[which(colnames(data.11)== 'R703A')] = "otherEdu_1"
colnames(data.14)[which(colnames(data.14)== 'R702A')] = "otherEdu_1"
colnames(data.11)[which(colnames(data.11)== 'R703B')] = "otherEdu_2"
colnames(data.14)[which(colnames(data.14)== 'R702B')] = "otherEdu_2"
colnames(data.11)[which(colnames(data.11)== 'R703C')] = "otherEdu_3"
colnames(data.14)[which(colnames(data.14)== 'R702C')] = "otherEdu_3"
colnames(data.11)[which(colnames(data.11)== 'R703D')] = "otherEdu_4"
colnames(data.14)[which(colnames(data.14)== 'R702D')] = "otherEdu_4"
colnames(data.11)[which(colnames(data.11)== 'R703E')] = "otherEdu_5"
colnames(data.14)[which(colnames(data.14)== 'R702F')] = "otherEdu_5"
data.14$otherEdu_5 = ifelse(data.14$otherEdu_5 == "3","1","2") #there is no NA

table(data.14$otherEdu_5,useNA = 'always')
table(data.11$otherEdu_5)

colnames(data.11)[which(colnames(data.11)== 'R704AK2')] = "health_1"
colnames(data.14)[which(colnames(data.14)== 'R704A_K2')] = "health_1"
colnames(data.11)[which(colnames(data.11)== 'R704AK3')] = "health_1N"
colnames(data.14)[which(colnames(data.14)== 'R704A_K3')] = "health_1N"
colnames(data.11)[which(colnames(data.11)== 'R704AK4')] = "health_1Distance"
colnames(data.14)[which(colnames(data.14)== 'R704A_K4')] = "health_1Distance"
colnames(data.11)[which(colnames(data.11)== 'R704AK5')] = "health_1EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704A_K5')] = "health_1EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704BK2')] = "health_2"
colnames(data.14)[which(colnames(data.14)== 'R704B_K2')] = "health_2"
colnames(data.11)[which(colnames(data.11)== 'R704BK3')] = "health_2N"
colnames(data.14)[which(colnames(data.14)== 'R704B_K3')] = "health_2N"
colnames(data.11)[which(colnames(data.11)== 'R704BK4')] = "health_2Distance"
colnames(data.14)[which(colnames(data.14)== 'R704B_K4')] = "health_2Distance"
colnames(data.11)[which(colnames(data.11)== 'R704BK5')] = "health_2EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704B_K5')] = "health_2EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704CK2')] = "health_3"
colnames(data.14)[which(colnames(data.14)== 'R704F_K2')] = "health_3"
colnames(data.11)[which(colnames(data.11)== 'R704CK3')] = "health_3N"
colnames(data.14)[which(colnames(data.14)== 'R704F_K3')] = "health_3N"
colnames(data.11)[which(colnames(data.11)== 'R704CK4')] = "health_3Distance"
colnames(data.14)[which(colnames(data.14)== 'R704F_K4')] = "health_3Distance"
colnames(data.11)[which(colnames(data.11)== 'R704CK5')] = "health_3EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704F_K5')] = "health_3EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704DK2')] = "health_4"
data.14$health_4 = ifelse(as.numeric(data.14$R704C_K2) * as.numeric(data.14$R704D_K2)== 4, "2" ,"1")
colnames(data.11)[which(colnames(data.11)== 'R704DK3')] = "health_4N"
data.14$health_4N = data.14$R704C_K3 + data.14$R704D_K3
colnames(data.11)[which(colnames(data.11)== 'R704DK4')] = "health_4Distance"
data.14$health_4Distance = apply(cbind(data.14$R704C_K4, data.14$R704D_K4),MARGIN = 1,FUN = min)
colnames(data.11)[which(colnames(data.11)== 'R704DK5')] = "health_4EasytoTravel"
data.14$health_4EasytoTravel = as.character(apply(cbind(as.numeric(data.14$R704C_K5),as.numeric(data.14$R704D_K5)),
                                                  MARGIN = 1, 
                                                  FUN = function(x){round(mean(x,na.rm=TRUE),0)}))
data.14$health_4EasytoTravel = recode(data.14$health_4EasytoTravel,'NaN'="NA" )

colnames(data.11)[which(colnames(data.11)== 'R704EK2')] = "health_5"
colnames(data.14)[which(colnames(data.14)== 'R704E_K2')] = "health_5"
colnames(data.11)[which(colnames(data.11)== 'R704EK3')] = "health_5N"
colnames(data.14)[which(colnames(data.14)== 'R704E_K3')] = "health_5N"
colnames(data.11)[which(colnames(data.11)== 'R704EK4')] = "health_5Distance"
colnames(data.14)[which(colnames(data.14)== 'R704E_K4')] = "health_5Distance"
colnames(data.11)[which(colnames(data.11)== 'R704EK5')] = "health_5EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704E_K5')] = "health_5EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704FK2')] = "health_6"
colnames(data.14)[which(colnames(data.14)== 'R704G_K2')] = "health_6"
colnames(data.11)[which(colnames(data.11)== 'R704FK3')] = "health_6N"
colnames(data.14)[which(colnames(data.14)== 'R704G_K3')] = "health_6N"
colnames(data.11)[which(colnames(data.11)== 'R704FK4')] = "health_6Distance"
colnames(data.14)[which(colnames(data.14)== 'R704G_K4')] = "health_6Distance"
colnames(data.11)[which(colnames(data.11)== 'R704FK5')] = "health_6EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704G_K5')] = "health_6EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704GK2')] = "health_7"
colnames(data.14)[which(colnames(data.14)== 'R704H_K2')] = "health_7"
colnames(data.11)[which(colnames(data.11)== 'R704GK3')] = "health_7N"
colnames(data.14)[which(colnames(data.14)== 'R704H_K3')] = "health_7N"
colnames(data.11)[which(colnames(data.11)== 'R704GK4')] = "health_7Distance"
colnames(data.14)[which(colnames(data.14)== 'R704H_K4')] = "health_7Distance"
colnames(data.11)[which(colnames(data.11)== 'R704GK5')] = "health_7EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704H_K5')] = "health_7EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704HK2')] = "health_8"
colnames(data.14)[which(colnames(data.14)== 'R704I_K2')] = "health_8"
colnames(data.11)[which(colnames(data.11)== 'R704HK3')] = "health_8N"
colnames(data.14)[which(colnames(data.14)== 'R704I_K3')] = "health_8N"
colnames(data.11)[which(colnames(data.11)== 'R704HK4')] = "health_8Distance"
colnames(data.14)[which(colnames(data.14)== 'R704I_K4')] = "health_8Distance"
colnames(data.11)[which(colnames(data.11)== 'R704HK5')] = "health_8EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704I_K5')] = "health_8EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704IK2')] = "health_9"
colnames(data.14)[which(colnames(data.14)== 'R704J_K2')] = "health_9"
colnames(data.11)[which(colnames(data.11)== 'R704IK3')] = "health_9N"
colnames(data.14)[which(colnames(data.14)== 'R704J_K3')] = "health_9N"
colnames(data.11)[which(colnames(data.11)== 'R704IK4')] = "health_9Distance"
colnames(data.14)[which(colnames(data.14)== 'R704J_K4')] = "health_9Distance"
colnames(data.11)[which(colnames(data.11)== 'R704IK5')] = "health_9EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704J_K5')] = "health_9EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704JK2')] = "health_10"
colnames(data.14)[which(colnames(data.14)== 'R704K_K2')] = "health_10"
colnames(data.11)[which(colnames(data.11)== 'R704JK3')] = "health_10N"
colnames(data.14)[which(colnames(data.14)== 'R704K_K3')] = "health_10N"

colnames(data.11)[which(colnames(data.11)== 'R704KK2')] = "health_11"
colnames(data.14)[which(colnames(data.14)== 'R704L_K2')] = "health_11"
colnames(data.11)[which(colnames(data.11)== 'R704KK3')] = "health_11N"
colnames(data.14)[which(colnames(data.14)== 'R704L_K3')] = "health_11N"
colnames(data.11)[which(colnames(data.11)== 'R704KK4')] = "health_11Distance"
colnames(data.14)[which(colnames(data.14)== 'R704L_K4')] = "health_11Distance"
colnames(data.11)[which(colnames(data.11)== 'R704KK5')] = "health_11EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704L_K5')] = "health_11EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R704LK2')] = "health_12"
colnames(data.14)[which(colnames(data.14)== 'R704M_K2')] = "health_12"
colnames(data.11)[which(colnames(data.11)== 'R704LK4')] = "health_12Distance"
colnames(data.14)[which(colnames(data.14)== 'R704M_K4')] = "health_12Distance"
colnames(data.11)[which(colnames(data.11)== 'R704LK5')] = "health_12EasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R704M_K5')] = "health_12EasytoTravel"

colnames(data.11)[which(colnames(data.11)== 'R705B')] = "posyandu_N"
colnames(data.14)[which(colnames(data.14)== 'R705A')] = "posyandu_N"
colnames(data.11)[which(colnames(data.11)== 'R705C')] = "posyandu_N2"
colnames(data.14)[which(colnames(data.14)== 'R705B')] = "posyandu_N2"

colnames(data.11)[which(colnames(data.11)== 'R707A1')] = "doctorM_N"
colnames(data.14)[which(colnames(data.14)== 'R706A1')] = "doctorM_N"
colnames(data.11)[which(colnames(data.11)== 'R707A2')] = "doctorF_N"
colnames(data.14)[which(colnames(data.14)== 'R706A2')] = "doctorF_N"

colnames(data.11)[which(colnames(data.11)== 'R707B')] = "dentist_N"
colnames(data.14)[which(colnames(data.14)== 'R706B')] = "dentist_N"
colnames(data.11)[which(colnames(data.11)== 'R707C')] = "midwife_N"
colnames(data.14)[which(colnames(data.14)== 'R706C')] = "midwife_N"
colnames(data.11)[which(colnames(data.11)== 'R707D')] = "otherHealth_N"
colnames(data.14)[which(colnames(data.14)== 'R706D')] = "otherHealth_N"
colnames(data.11)[which(colnames(data.11)== 'R707E')] = "TBA_N"
colnames(data.14)[which(colnames(data.14)== 'R708')] = "TBA_N"

colnames(data.11)[which(colnames(data.11)== 'R708AK2')] = "disease_1"
colnames(data.14)[which(colnames(data.14)== 'R709A_K2')] = "disease_1"
colnames(data.11)[which(colnames(data.11)== 'R708AK3')] = "disease_1N"
colnames(data.14)[which(colnames(data.14)== 'R709A_K3')] = "disease_1N"
colnames(data.11)[which(colnames(data.11)== 'R708AK4')] = "disease_1Died"
colnames(data.14)[which(colnames(data.14)== 'R709A_K4')] = "disease_1Died"

colnames(data.11)[which(colnames(data.11)== 'R708BK2')] = "disease_2"
colnames(data.14)[which(colnames(data.14)== 'R709B_K2')] = "disease_2"
colnames(data.11)[which(colnames(data.11)== 'R708BK3')] = "disease_2N"
colnames(data.14)[which(colnames(data.14)== 'R709B_K3')] = "disease_2N"
colnames(data.11)[which(colnames(data.11)== 'R708BK4')] = "disease_2Died"
colnames(data.14)[which(colnames(data.14)== 'R709B_K4')] = "disease_2Died"

colnames(data.11)[which(colnames(data.11)== 'R708CK2')] = "disease_3"
colnames(data.14)[which(colnames(data.14)== 'R709C_K2')] = "disease_3"
colnames(data.11)[which(colnames(data.11)== 'R708CK3')] = "disease_3N"
colnames(data.14)[which(colnames(data.14)== 'R709C_K3')] = "disease_3N"
colnames(data.11)[which(colnames(data.11)== 'R708CK4')] = "disease_3Died"
colnames(data.14)[which(colnames(data.14)== 'R709C_K4')] = "disease_3Died"

colnames(data.11)[which(colnames(data.11)== 'R708EK2')] = "disease_4"
colnames(data.14)[which(colnames(data.14)== 'R709D_K2')] = "disease_4"
colnames(data.11)[which(colnames(data.11)== 'R708EK3')] = "disease_4N"
colnames(data.14)[which(colnames(data.14)== 'R709D_K3')] = "disease_4N"
colnames(data.11)[which(colnames(data.11)== 'R708EK4')] = "disease_4Died"
colnames(data.14)[which(colnames(data.14)== 'R709D_K4')] = "disease_4Died"

colnames(data.11)[which(colnames(data.11)== 'R708FK2')] = "disease_5"
colnames(data.14)[which(colnames(data.14)== 'R709E_K2')] = "disease_5"
colnames(data.11)[which(colnames(data.11)== 'R708FK3')] = "disease_5N"
colnames(data.14)[which(colnames(data.14)== 'R709E_K3')] = "disease_5N"
colnames(data.11)[which(colnames(data.11)== 'R708FK4')] = "disease_5Died"
colnames(data.14)[which(colnames(data.14)== 'R709E_K4')] = "disease_5Died"

colnames(data.11)[which(colnames(data.11)== 'R708HK2')] = "disease_6"
colnames(data.14)[which(colnames(data.14)== 'R709H_K2')] = "disease_6"
colnames(data.11)[which(colnames(data.11)== 'R708HK3')] = "disease_6N"
colnames(data.14)[which(colnames(data.14)== 'R709H_K3')] = "disease_6N"
colnames(data.11)[which(colnames(data.11)== 'R708HK4')] = "disease_6Died"
colnames(data.14)[which(colnames(data.14)== 'R709H_K4')] = "disease_6Died"

colnames(data.11)[which(colnames(data.11)== 'R709')] = "mulnutrition_N"
colnames(data.14)[which(colnames(data.14)== 'R710')] = "mulnutrition_N"
colnames(data.11)[which(colnames(data.11)== 'R711')] = "JAMKESMAS_N"
colnames(data.14)[which(colnames(data.14)== 'R711A')] = "JAMKESMAS_N"
colnames(data.11)[which(colnames(data.11)== 'R712')] = "SKTM_N"
colnames(data.14)[which(colnames(data.14)== 'R711B')] = "SKTM_N"

table(data.11$R802)
table(data.14$R802)

colnames(data.11)[which(colnames(data.11)== 'R80101')] = "religion_1"
colnames(data.14)[which(colnames(data.14)== 'R80101')] = "religion_1"
colnames(data.11)[which(colnames(data.11)== 'R80102')] = "religion_2"
colnames(data.14)[which(colnames(data.14)== 'R80102')] = "religion_2"
colnames(data.11)[which(colnames(data.11)== 'R80103')] = "religion_3"
colnames(data.14)[which(colnames(data.14)== 'R80103')] = "religion_3"
colnames(data.11)[which(colnames(data.11)== 'R80104')] = "religion_4"
colnames(data.14)[which(colnames(data.14)== 'R80104')] = "religion_4"
colnames(data.11)[which(colnames(data.11)== 'R80105')] = "religion_5"
colnames(data.14)[which(colnames(data.14)== 'R80105')] = "religion_5"
colnames(data.11)[which(colnames(data.11)== 'R80106')] = "religion_6"
colnames(data.14)[which(colnames(data.14)== 'R80106')] = "religion_6"
colnames(data.11)[which(colnames(data.11)== 'R80107')] = "religion_7"
colnames(data.14)[which(colnames(data.14)== 'R80107')] = "religion_7"

colnames(data.11)[which(colnames(data.11)== 'R802')] = "mostReligion"
data.11$mostReligion = recode(data.11$mostReligion, "6" = "7")
colnames(data.14)[which(colnames(data.14)== 'R802')] = "mostReligion"

colnames(data.11)[which(colnames(data.11)== 'R803A')] = "worship_1"
colnames(data.14)[which(colnames(data.14)== 'R803A')] = "worship_1"
colnames(data.11)[which(colnames(data.11)== 'R803B')] = "worship_2"
colnames(data.14)[which(colnames(data.14)== 'R803B')] = "worship_2"
colnames(data.11)[which(colnames(data.11)== 'R803C')] = "worship_3"
colnames(data.14)[which(colnames(data.14)== 'R803C')] = "worship_3"
colnames(data.11)[which(colnames(data.11)== 'R803D')] = "worship_4"
colnames(data.14)[which(colnames(data.14)== 'R803D')] = "worship_4"
colnames(data.11)[which(colnames(data.11)== 'R803E')] = "worship_5"
colnames(data.14)[which(colnames(data.14)== 'R803E')] = "worship_5"
colnames(data.11)[which(colnames(data.11)== 'R803F')] = "worship_6"
colnames(data.14)[which(colnames(data.14)== 'R803F')] = "worship_6"
colnames(data.11)[which(colnames(data.11)== 'R803G')] = "worship_7"
colnames(data.14)[which(colnames(data.14)== 'R803G')] = "worship_7"
colnames(data.11)[which(colnames(data.11)== 'R803H')] = "worship_8"
colnames(data.14)[which(colnames(data.14)== 'R803H')] = "worship_8"

colnames(data.11)[which(colnames(data.11)== 'R805A')] = "disabled_1"
colnames(data.14)[which(colnames(data.14)== 'R805A')] = "disabled_1"
colnames(data.11)[which(colnames(data.11)== 'R805B')] = "disabled_2"
colnames(data.14)[which(colnames(data.14)== 'R805B')] = "disabled_2"
colnames(data.11)[which(colnames(data.11)== 'R805C')] = "disabled_3"
colnames(data.14)[which(colnames(data.14)== 'R805C')] = "disabled_3"
colnames(data.11)[which(colnames(data.11)== 'R805D')] = "disabled_4"
colnames(data.14)[which(colnames(data.14)== 'R805D')] = "disabled_4"
colnames(data.11)[which(colnames(data.11)== 'R805E')] = "disabled_5"
colnames(data.14)[which(colnames(data.14)== 'R805E')] = "disabled_5"
colnames(data.11)[which(colnames(data.11)== 'R805F')] = "disabled_6"
colnames(data.14)[which(colnames(data.14)== 'R805F')] = "disabled_6"
colnames(data.11)[which(colnames(data.11)== 'R805G')] = "disabled_7"
colnames(data.14)[which(colnames(data.14)== 'R805G')] = "disabled_7"
colnames(data.11)[which(colnames(data.11)== 'R805H')] = "disabled_8"
colnames(data.14)[which(colnames(data.14)== 'R805H')] = "disabled_8"
colnames(data.11)[which(colnames(data.11)== 'R805I')] = "disabled_9"
colnames(data.14)[which(colnames(data.14)== 'R805I')] = "disabled_9"

colnames(data.11)[which(colnames(data.11)== 'R806')] = "multiEthnicity"
colnames(data.14)[which(colnames(data.14)== 'R804A1')] = "multiEthnicity"

colnames(data.11)[which(colnames(data.11)== 'R808')] = "commService"
colnames(data.14)[which(colnames(data.14)== 'R807B')] = "commService"
data.14$commService = recode(data.14$commService, "3" = "1", "4" = "2")

colnames(data.11)[which(colnames(data.11)== 'R901A')] = "cinema"
colnames(data.14)[which(colnames(data.14)== 'R902A1')] = "cinema"
colnames(data.11)[which(colnames(data.11)== 'R901B')] = "cinemaDistance"
colnames(data.14)[which(colnames(data.14)== 'R902A2')] = "cinemaDistance"
colnames(data.11)[which(colnames(data.11)== 'R902A')] = "pub"
colnames(data.14)[which(colnames(data.14)== 'R902B1')] = "pub"
colnames(data.11)[which(colnames(data.11)== 'R902B')] = "pubDistance"
colnames(data.14)[which(colnames(data.14)== 'R902B2')] = "pubDistance"

colnames(data.11)[which(colnames(data.11)== 'R903AK2')] = "sport_1"
colnames(data.14)[which(colnames(data.14)== 'R903A_K2')] = "sport_1"
colnames(data.11)[which(colnames(data.11)== 'R903AK3')] = "sport_1Group"
colnames(data.14)[which(colnames(data.14)== 'R903A_K3')] = "sport_1Group"

colnames(data.11)[which(colnames(data.11)== 'R903BK2')] = "sport_2"
colnames(data.14)[which(colnames(data.14)== 'R903B_K2')] = "sport_2"
colnames(data.11)[which(colnames(data.11)== 'R903BK3')] = "sport_2Group"
colnames(data.14)[which(colnames(data.14)== 'R903B_K3')] = "sport_2Group"

colnames(data.11)[which(colnames(data.11)== 'R903CK2')] = "sport_3"
colnames(data.14)[which(colnames(data.14)== 'R903C_K2')] = "sport_3"
colnames(data.11)[which(colnames(data.11)== 'R903CK3')] = "sport_3Group"
colnames(data.14)[which(colnames(data.14)== 'R903C_K3')] = "sport_3Group"

colnames(data.11)[which(colnames(data.11)== 'R903DK2')] = "sport_4"
colnames(data.14)[which(colnames(data.14)== 'R903D_K2')] = "sport_4"
colnames(data.11)[which(colnames(data.11)== 'R903DK3')] = "sport_4Group"
colnames(data.14)[which(colnames(data.14)== 'R903D_K3')] = "sport_4Group"

colnames(data.11)[which(colnames(data.11)== 'R903EK2')] = "sport_5"
colnames(data.14)[which(colnames(data.14)== 'R903E_K2')] = "sport_5"
colnames(data.11)[which(colnames(data.11)== 'R903EK3')] = "sport_5Group"
colnames(data.14)[which(colnames(data.14)== 'R903E_K3')] = "sport_5Group"

colnames(data.11)[which(colnames(data.11)== 'R903FK2')] = "sport_6"
colnames(data.14)[which(colnames(data.14)== 'R903G_K2')] = "sport_6"
colnames(data.11)[which(colnames(data.11)== 'R903FK3')] = "sport_6Group"
colnames(data.14)[which(colnames(data.14)== 'R903G_K3')] = "sport_6Group"

colnames(data.11)[which(colnames(data.11)== 'R903GK2')] = "sport_7"
colnames(data.14)[which(colnames(data.14)== 'R903H_K2')] = "sport_7"
colnames(data.11)[which(colnames(data.11)== 'R903GK3')] = "sport_7Group"
colnames(data.14)[which(colnames(data.14)== 'R903H_K3')] = "sport_7Group"

colnames(data.11)[which(colnames(data.11)== 'R903HK3')] = "sport_8Group"
colnames(data.14)[which(colnames(data.14)== 'R903F_K3')] = "sport_8Group"

colnames(data.11)[which(colnames(data.11)== 'R903IK3')] = "sport_9Group"
colnames(data.14)[which(colnames(data.14)== 'R903I_K3')] = "sport_9Group"

colnames(data.11)[which(colnames(data.11)== 'R903JK3')] = "sport_10Group"
colnames(data.14)[which(colnames(data.14)== 'R903J_K3')] = "sport_10Group"

table(data.11$R1001B2)
table(data.14$R1001B2)

colnames(data.11)[which(colnames(data.11)== 'R1001A')] = "infrastructure"
colnames(data.14)[which(colnames(data.14)== 'R1001A')] = "infrastructure"
colnames(data.11)[which(colnames(data.11)== 'R1001B1')] = "roadType"
colnames(data.14)[which(colnames(data.14)== 'R1001B1')] = "roadType"
colnames(data.11)[which(colnames(data.11)== 'R1001B2')] = "roadThrough"
colnames(data.14)[which(colnames(data.14)== 'R1001B2')] = "roadThrough"
data.14$roadThrough = recode(data.14$roadThrough, '2' = '1', '3'='2','4' ='2')
table(data.14$roadThrough)

table(data.11$roadType)

table(data.11$R1004CK5)
table(data.14$R1002D_K6)

colnames(data.11)[which(colnames(data.11)== 'R1004AK2')] = "distance_1"
colnames(data.14)[which(colnames(data.14)== 'R1002A_K2')] = "distance_1"
colnames(data.11)[which(colnames(data.11)== 'R1004AK5')] = "distance_1Transport"
colnames(data.14)[which(colnames(data.14)== 'R1002A_K6')] = "distance_1Transport"
data.11$distance_1Transport = recode(data.11$distance_1Transport, 
                                     '0'='NA','1'='16','2'='1','4'='2','8'='2','16'='4','32'='4','64'='8','128'='16')

colnames(data.11)[which(colnames(data.11)== 'R1004BK2')] = "distance_2"
colnames(data.14)[which(colnames(data.14)== 'R1002B_K2')] = "distance_2"
colnames(data.11)[which(colnames(data.11)== 'R1004BK5')] = "distance_2Transport"
colnames(data.14)[which(colnames(data.14)== 'R1002B_K6')] = "distance_2Transport"
data.11$distance_2Transport = recode(data.11$distance_2Transport, 
                                      '0'='NA','1'='16','2'='1','4'='2','8'='2','16'='4','32'='4','64'='8','128'='16')

colnames(data.11)[which(colnames(data.11)== 'R1004CK2')] = "distance_3"
colnames(data.14)[which(colnames(data.14)== 'R1002D_K2')] = "distance_3"
colnames(data.11)[which(colnames(data.11)== 'R1004CK5')] = "distance_3Transport"
colnames(data.14)[which(colnames(data.14)== 'R1002D_K6')] = "distance_3Transport"
data.11$distance_3Transport = recode(data.11$distance_3Transport, 
                                      '0'='NA','1'='16','2'='1','4'='2','8'='2','16'='4','32'='4','64'='8','128'='16')

colnames(data.11)[which(colnames(data.11)== 'R1005A')] = "landPhone"
colnames(data.14)[which(colnames(data.14)== 'R1003A')] = "landPhone"
colnames(data.11)[which(colnames(data.11)== 'R1005B')] = "landPhone_N"
colnames(data.14)[which(colnames(data.14)== 'R1003B')] = "landPhone_N"
colnames(data.11)[which(colnames(data.11)== 'R1006')] = "payPhone"
colnames(data.14)[which(colnames(data.14)== 'R1004A')] = "payPhone"
colnames(data.11)[which(colnames(data.11)== 'R1007A')] = "BTS"
colnames(data.14)[which(colnames(data.14)== 'R1005A')] = "BTS"
colnames(data.11)[which(colnames(data.11)== 'R1007B')] = "cellPhone"
colnames(data.14)[which(colnames(data.14)== 'R1005B')] = "cellPhone"
colnames(data.11)[which(colnames(data.11)== 'R1008')] = "telecomm"
colnames(data.14)[which(colnames(data.14)== 'R1006')] = "telecomm"
colnames(data.11)[which(colnames(data.11)== 'R1009')] = "internet"
colnames(data.14)[which(colnames(data.14)== 'R1007B')] = "internet"
data.14$internet = recode(data.14$internet,"3"="1","4"="2" )

colnames(data.11)[which(colnames(data.11)== 'R1010A')] = "postOffice"
colnames(data.14)[which(colnames(data.14)== 'R1008A')] = "postOffice"
colnames(data.11)[which(colnames(data.11)== 'R1011')] = "mobilePO"
colnames(data.14)[which(colnames(data.14)== 'R1008B')] = "mobilePO"
data.14$mobilePO = recode(data.14$mobilePO,"3"="1","4"="2" )

colnames(data.11)[which(colnames(data.11)== 'R1012A')] = "TV_1"
colnames(data.14)[which(colnames(data.14)== 'R1009B_K2')] = "TV_1"
colnames(data.11)[which(colnames(data.11)== 'R1012B')] = "TV_2"
data.11$TV_2 = recode(data.11$TV_2,"3"="1","4"="2" )
colnames(data.14)[which(colnames(data.14)== 'R1009C_K2')] = "TV_2"
colnames(data.11)[which(colnames(data.11)== 'R1012C')] = "TV_3"
data.11$TV_3 = recode(data.11$TV_3,"5"="1","6"="2" )
colnames(data.14)[which(colnames(data.14)== 'R1009A_K2')] = "TV_3"
colnames(data.11)[which(colnames(data.11)== 'R1012D')] = "TV_4"
data.11$TV_4 = recode(data.11$TV_4,"7"="1","8"="2" )
colnames(data.14)[which(colnames(data.14)== 'R1009D_K2')] = "TV_4"

colnames(data.11)[which(colnames(data.11)== 'R1103AK4')] = "landChg_1"
colnames(data.14)[which(colnames(data.14)== 'R1103A_K4')] = "landChg_1"
colnames(data.11)[which(colnames(data.11)== 'R1103AK5')] = "landChg_1pct"
colnames(data.14)[which(colnames(data.14)== 'R1103A_K5')] = "landChg_1pct"

colnames(data.11)[which(colnames(data.11)== 'R1103AK6')] = "landChg_2"
colnames(data.14)[which(colnames(data.14)== 'R1103A_K6')] = "landChg_2"
colnames(data.11)[which(colnames(data.11)== 'R1103AK7')] = "landChg_2pct"
colnames(data.14)[which(colnames(data.14)== 'R1103A_K7')] = "landChg_2pct"

colnames(data.11)[which(colnames(data.11)== 'R1103BK2')] = "landChg_3"
colnames(data.14)[which(colnames(data.14)== 'R1103B_K2')] = "landChg_3"
colnames(data.11)[which(colnames(data.11)== 'R1103BK3')] = "landChg_3pct"
colnames(data.14)[which(colnames(data.14)== 'R1103B_K3')] = "landChg_3pct"

colnames(data.11)[which(colnames(data.11)== 'R1103BK6')] = "landChg_4"
colnames(data.14)[which(colnames(data.14)== 'R1103B_K6')] = "landChg_4"
colnames(data.11)[which(colnames(data.11)== 'R1103BK7')] = "landChg_4pct"
colnames(data.14)[which(colnames(data.14)== 'R1103B_K7')] = "landChg_4pct"

colnames(data.11)[which(colnames(data.11)== 'R1103CK2')] = "landChg_5"
colnames(data.14)[which(colnames(data.14)== 'R1103C_K2')] = "landChg_5"
colnames(data.11)[which(colnames(data.11)== 'R1103CK3')] = "landChg_5pct"
colnames(data.14)[which(colnames(data.14)== 'R1103C_K3')] = "landChg_5pct"

colnames(data.11)[which(colnames(data.11)== 'R1103CK4')] = "landChg_6"
colnames(data.14)[which(colnames(data.14)== 'R1103C_K4')] = "landChg_6"
colnames(data.11)[which(colnames(data.11)== 'R1103CK5')] = "landChg_6pct"
colnames(data.14)[which(colnames(data.14)== 'R1103C_K5')] = "landChg_6pct"

colnames(data.11)[which(colnames(data.11)== 'R1202A')] = "industry_1"
colnames(data.14)[which(colnames(data.14)== 'R1201A')] = "industry_1"
colnames(data.11)[which(colnames(data.11)== 'R1202B')] = "industry_2"
colnames(data.14)[which(colnames(data.14)== 'R1201B')] = "industry_2"
colnames(data.11)[which(colnames(data.11)== 'R1202C')] = "industry_3"
colnames(data.14)[which(colnames(data.14)== 'R1201C')] = "industry_3"
colnames(data.11)[which(colnames(data.11)== 'R1202D')] = "industry_4"
colnames(data.14)[which(colnames(data.14)== 'R1201D')] = "industry_4"
colnames(data.11)[which(colnames(data.11)== 'R1202E')] = "industry_5"
colnames(data.14)[which(colnames(data.14)== 'R1201E')] = "industry_5"
colnames(data.11)[which(colnames(data.11)== 'R1202F')] = "industry_6"
colnames(data.14)[which(colnames(data.14)== 'R1201F')] = "industry_6"
colnames(data.11)[which(colnames(data.11)== 'R1202G')] = "industry_7"
colnames(data.14)[which(colnames(data.14)== 'R1201G')] = "industry_7"
colnames(data.11)[which(colnames(data.11)== 'R1202H')] = "industry_8"
colnames(data.14)[which(colnames(data.14)== 'R1201H')] = "industry_8"

colnames(data.11)[which(colnames(data.11)== 'R1201A')] = "KUD"
colnames(data.14)[which(colnames(data.14)== 'R1213A')] = "KUD"
colnames(data.11)[which(colnames(data.11)== 'R1201B')] = "nonKUD"
colnames(data.14)[which(colnames(data.14)== 'R1213B')] = "nonKUD"

colnames(data.11)[which(colnames(data.11)== 'R1203A')] = "gpShop"
colnames(data.14)[which(colnames(data.14)== 'R1202A')] = "gpShop"
colnames(data.11)[which(colnames(data.11)== 'R1203B')] = "gpShopDis"
colnames(data.14)[which(colnames(data.14)== 'R1202B')] = "gpShopDis"

colnames(data.11)[which(colnames(data.11)== 'R1204A')] = "kerosene"
colnames(data.14)[which(colnames(data.14)== 'R1203A')] = "kerosene"
colnames(data.11)[which(colnames(data.11)== 'R1204B')] = "LPG"
colnames(data.14)[which(colnames(data.14)== 'R1203B')] = "LPG"

colnames(data.11)[which(colnames(data.11)== 'R1205A')] = "prmntMkt"
data.14$prmntMkt = as.character(ifelse(data.14$R1204A + data.14$R1204B == 0, "2", "1"))
colnames(data.11)[which(colnames(data.11)== 'R1205B')] = "prmntMktDis"
colnames(data.14)[which(colnames(data.14)== 'R1204C')] = "prmntMktDis"

colnames(data.11)[which(colnames(data.11)== 'R1206')] = "fltMkt"
colnames(data.14)[which(colnames(data.14)== 'R1205')] = "fltMkt"
colnames(data.11)[which(colnames(data.11)== 'R1207')] = "miniMkt"
colnames(data.14)[which(colnames(data.14)== 'R1206')] = "miniMkt"

colnames(data.11)[which(colnames(data.11)== 'R1208')] = "grocery"
colnames(data.14)[which(colnames(data.14)== 'R1207')] = "grocery"
colnames(data.11)[which(colnames(data.11)== 'R1209')] = "tavern"
colnames(data.14)[which(colnames(data.14)== 'R1208')] = "tavern"

colnames(data.11)[which(colnames(data.11)== 'R1210')] = "restaurant"
colnames(data.14)[which(colnames(data.14)== 'R1209')] = "restaurant"
colnames(data.11)[which(colnames(data.11)== 'R1211')] = "hotel"
colnames(data.14)[which(colnames(data.14)== 'R1210')] = "hotel"
colnames(data.11)[which(colnames(data.11)== 'R1212')] = "lodge"
colnames(data.14)[which(colnames(data.14)== 'R1211')] = "lodge"

colnames(data.11)[which(colnames(data.11)== 'R1213A')] = "cooperative_1"
colnames(data.14)[which(colnames(data.14)== 'R1212A')] = "cooperative_1"
colnames(data.11)[which(colnames(data.11)== 'R1213B')] = "cooperative_2"
colnames(data.14)[which(colnames(data.14)== 'R1212B')] = "cooperative_2"
colnames(data.11)[which(colnames(data.11)== 'R1213C')] = "cooperative_3"
colnames(data.14)[which(colnames(data.14)== 'R1212C')] = "cooperative_3"
colnames(data.11)[which(colnames(data.11)== 'R1213D')] = "cooperative_4"
colnames(data.14)[which(colnames(data.14)== 'R1212D')] = "cooperative_4"

colnames(data.11)[which(colnames(data.11)== 'R1214A')] = "creditFclty_1"
colnames(data.14)[which(colnames(data.14)== 'R1214A')] = "creditFclty_1"
colnames(data.11)[which(colnames(data.11)== 'R1214C')] = "creditFclty_2"
colnames(data.14)[which(colnames(data.14)== 'R1214C')] = "creditFclty_2"

colnames(data.11)[which(colnames(data.11)== 'R1215AK2')] = "bank_1"
data.14$bank_1 = ifelse(as.numeric(data.14$R1215A_K2) * as.numeric(data.14$R1215B_K2)==4,"2","1")
colnames(data.11)[which(colnames(data.11)== 'R1215AK3')] = "bank_1N"
data.14$bank_1N = data.14$R1215A_K3 + data.14$R1215B_K3

colnames(data.11)[which(colnames(data.11)== 'R1215BK2')] = "bank_2"
colnames(data.14)[which(colnames(data.14)== 'R1215C_K2')] = "bank_2"
colnames(data.11)[which(colnames(data.11)== 'R1215BK3')] = "bank_2N"
colnames(data.14)[which(colnames(data.14)== 'R1215C_K3')] = "bank_2N"

colnames(data.11)[which(colnames(data.11)== 'R1301A')] = "massFght"
colnames(data.14)[which(colnames(data.14)== 'R1301A')] = "massFght"

colnames(data.11)[which(colnames(data.11)== 'R1301B1K2')] = "fght_1N"
colnames(data.14)[which(colnames(data.14)== 'R1301B1_K2')] = "fght_1N"
colnames(data.11)[which(colnames(data.11)== 'R1301B1K3')] = "fght_1Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B1_K3')] = "fght_1Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B1K4')] = "fght_1Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B1_K4')] = "fght_1Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B2K2')] = "fght_2N"
colnames(data.14)[which(colnames(data.14)== 'R1301B2_K2')] = "fght_2N"
colnames(data.11)[which(colnames(data.11)== 'R1301B2K3')] = "fght_2Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B2_K3')] = "fght_2Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B2K4')] = "fght_2Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B2_K4')] = "fght_2Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B3K2')] = "fght_3N"
colnames(data.14)[which(colnames(data.14)== 'R1301B3_K2')] = "fght_3N"
colnames(data.11)[which(colnames(data.11)== 'R1301B3K3')] = "fght_3Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B3_K3')] = "fght_3Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B3K4')] = "fght_3Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B3_K4')] = "fght_3Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B4K2')] = "fght_4N"
colnames(data.14)[which(colnames(data.14)== 'R1301B4_K2')] = "fght_4N"
colnames(data.11)[which(colnames(data.11)== 'R1301B4K3')] = "fght_4Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B4_K3')] = "fght_4Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B4K4')] = "fght_4Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B4_K4')] = "fght_4Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B5K2')] = "fght_5N"
colnames(data.14)[which(colnames(data.14)== 'R1301B5_K2')] = "fght_5N"
colnames(data.11)[which(colnames(data.11)== 'R1301B5K3')] = "fght_5Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B5_K3')] = "fght_5Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B5K4')] = "fght_5Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B5_K4')] = "fght_5Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B6K2')] = "fght_6N"
colnames(data.14)[which(colnames(data.14)== 'R1301B6_K2')] = "fght_6N"
colnames(data.11)[which(colnames(data.11)== 'R1301B6K3')] = "fght_6Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B6_K3')] = "fght_6Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B6K4')] = "fght_6Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B6_K4')] = "fght_6Injured"

colnames(data.11)[which(colnames(data.11)== 'R1301B7K2')] = "fght_7N"
colnames(data.14)[which(colnames(data.14)== 'R1301B7_K2')] = "fght_7N"
colnames(data.11)[which(colnames(data.11)== 'R1301B7K3')] = "fght_7Dead"
colnames(data.14)[which(colnames(data.14)== 'R1301B7_K3')] = "fght_7Dead"
colnames(data.11)[which(colnames(data.11)== 'R1301B7K4')] = "fght_7Injured"
colnames(data.14)[which(colnames(data.14)== 'R1301B7_K4')] = "fght_7Injured"

colnames(data.11)[which(colnames(data.11)== 'R130301K2')] = "criminal_1"
colnames(data.14)[which(colnames(data.14)== 'R1303A01K3')] = "criminal_1"
colnames(data.11)[which(colnames(data.11)== 'R130301K3')] = "criminal_1Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A01K4')] = "criminal_1Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130302K2')] = "criminal_2"
colnames(data.14)[which(colnames(data.14)== 'R1303A02K3')] = "criminal_2"
colnames(data.11)[which(colnames(data.11)== 'R130302K3')] = "criminal_2Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A02K4')] = "criminal_2Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130303K2')] = "criminal_3"
colnames(data.14)[which(colnames(data.14)== 'R1303A03K3')] = "criminal_3"
colnames(data.11)[which(colnames(data.11)== 'R130303K3')] = "criminal_3Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A03K4')] = "criminal_3Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130304K2')] = "criminal_4"
colnames(data.14)[which(colnames(data.14)== 'R1303A04K3')] = "criminal_4"
colnames(data.11)[which(colnames(data.11)== 'R130304K3')] = "criminal_4Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A04K4')] = "criminal_4Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130305K2')] = "criminal_5"
colnames(data.14)[which(colnames(data.14)== 'R1303A05K3')] = "criminal_5"
colnames(data.11)[which(colnames(data.11)== 'R130305K3')] = "criminal_5Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A05K4')] = "criminal_5Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130306K2')] = "criminal_6"
colnames(data.14)[which(colnames(data.14)== 'R1303A06K3')] = "criminal_6"
colnames(data.11)[which(colnames(data.11)== 'R130306K3')] = "criminal_6Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A06K4')] = "criminal_6Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130307K2')] = "criminal_7"
colnames(data.14)[which(colnames(data.14)== 'R1303A07K3')] = "criminal_7"
colnames(data.11)[which(colnames(data.11)== 'R130307K3')] = "criminal_7Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A07K4')] = "criminal_7Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130308K2')] = "criminal_8"
colnames(data.14)[which(colnames(data.14)== 'R1303A08K3')] = "criminal_8"
colnames(data.11)[which(colnames(data.11)== 'R130308K3')] = "criminal_8Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A08K4')] = "criminal_8Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130309K2')] = "criminal_9"
colnames(data.14)[which(colnames(data.14)== 'R1303A09K3')] = "criminal_9"
colnames(data.11)[which(colnames(data.11)== 'R130309K3')] = "criminal_9Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A09K4')] = "criminal_9Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R130310K2')] = "criminal_10"
colnames(data.14)[which(colnames(data.14)== 'R1303A10K3')] = "criminal_10"
colnames(data.11)[which(colnames(data.11)== 'R130310K3')] = "criminal_10Tndcy"
colnames(data.14)[which(colnames(data.14)== 'R1303A10K4')] = "criminal_10Tndcy"

colnames(data.11)[which(colnames(data.11)== 'R1308A')] = "secActivities_1"
colnames(data.14)[which(colnames(data.14)== 'R1304A')] = "secActivities_1"
colnames(data.11)[which(colnames(data.11)== 'R1308B')] = "secActivities_2"
colnames(data.14)[which(colnames(data.14)== 'R1304B')] = "secActivities_2"
colnames(data.11)[which(colnames(data.11)== 'R1308C')] = "secActivities_3"
colnames(data.14)[which(colnames(data.14)== 'R1304C')] = "secActivities_3"
colnames(data.11)[which(colnames(data.11)== 'R1308D')] = "secActivities_4"
colnames(data.14)[which(colnames(data.14)== 'R1304D')] = "secActivities_4"

colnames(data.11)[which(colnames(data.11)== 'R1309BK2')] = "police"
colnames(data.14)[which(colnames(data.14)== 'R1306A')] = "police"
colnames(data.11)[which(colnames(data.11)== 'R1309BK3')] = "policeDis"
colnames(data.14)[which(colnames(data.14)== 'R1306B1')] = "policeDis"
colnames(data.11)[which(colnames(data.11)== 'R1309BK4')] = "policeEasytoTravel"
colnames(data.14)[which(colnames(data.14)== 'R1306B2')] = "policeEasytoTravel"
colnames(data.11)[which(colnames(data.11)== 'R1310')] = "guards"
colnames(data.14)[which(colnames(data.14)== 'R1305')] = "guards"

colnames(data.11)[which(colnames(data.11)== 'R1304')] = "suicide"
colnames(data.14)[which(colnames(data.14)== 'R1307')] = "suicide"
colnames(data.11)[which(colnames(data.11)== 'R1306')] = "streetChild"
data.11$streetChild = ifelse(data.11$streetChild == 0, "2", "1")
colnames(data.14)[which(colnames(data.14)== 'R1308A')] = "streetChild"
colnames(data.11)[which(colnames(data.11)== 'R1307')] = "sexWorker"
colnames(data.14)[which(colnames(data.14)== 'R1309')] = "sexWorker"

colnames(data.11)[which(colnames(data.11)== 'R1401AK2')] = "income_1Form"
colnames(data.14)[which(colnames(data.14)== 'R1501A_K2')] = "income_1Form"
colnames(data.11)[which(colnames(data.11)== 'R1401AK3')] = "income_1"
colnames(data.14)[which(colnames(data.14)== 'R1501A_K3')] = "income_1"

colnames(data.11)[which(colnames(data.11)== 'R1401B1K2')] = "income_2Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C1_K2')] = "income_2Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B1K3')] = "income_2"
colnames(data.14)[which(colnames(data.14)== 'R1501C1_K3')] = "income_2"

colnames(data.11)[which(colnames(data.11)== 'R1401B2K2')] = "income_3Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C2_K2')] = "income_3Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B2K3')] = "income_3"
colnames(data.14)[which(colnames(data.14)== 'R1501C2_K3')] = "income_3"

colnames(data.11)[which(colnames(data.11)== 'R1401B3K2')] = "income_4Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C3_K2')] = "income_4Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B3K3')] = "income_4"
colnames(data.14)[which(colnames(data.14)== 'R1501C3_K3')] = "income_4"

colnames(data.11)[which(colnames(data.11)== 'R1401B4K2')] = "income_5Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C4_K2')] = "income_5Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B4K3')] = "income_5"
colnames(data.14)[which(colnames(data.14)== 'R1501C4_K3')] = "income_5"

colnames(data.11)[which(colnames(data.11)== 'R1401B5K2')] = "income_6Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C5_K2')] = "income_6Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B5K3')] = "income_6"
colnames(data.14)[which(colnames(data.14)== 'R1501C5_K3')] = "income_6"

colnames(data.11)[which(colnames(data.11)== 'R1401B6K2')] = "income_7Form"
colnames(data.14)[which(colnames(data.14)== 'R1501C6_K2')] = "income_7Form"
colnames(data.11)[which(colnames(data.11)== 'R1401B6K3')] = "income_7"
colnames(data.14)[which(colnames(data.14)== 'R1501C6_K3')] = "income_7"

colnames(data.11)[which(colnames(data.11)== 'R1402A1K2')] = "dev_1"
colnames(data.14)[which(colnames(data.14)== 'R1401A1_K2')] = "dev_1"
colnames(data.11)[which(colnames(data.11)== 'R1402A1K3')] = "dev_1Source"
colnames(data.14)[which(colnames(data.14)== 'R1401A1_K3')] = "dev_1Source"

colnames(data.11)[which(colnames(data.11)== 'R1402A2K2')] = "dev_2"
colnames(data.14)[which(colnames(data.14)== 'R1401A2_K2')] = "dev_2"
colnames(data.11)[which(colnames(data.11)== 'R1402A2K3')] = "dev_2Source"
colnames(data.14)[which(colnames(data.14)== 'R1401A2_K3')] = "dev_2Source"

colnames(data.11)[which(colnames(data.11)== 'R1402A3K2')] = "dev_3"
colnames(data.14)[which(colnames(data.14)== 'R1401A3_K2')] = "dev_3"
colnames(data.11)[which(colnames(data.11)== 'R1402A3K3')] = "dev_3Source"
colnames(data.14)[which(colnames(data.14)== 'R1401A3_K3')] = "dev_3Source"

colnames(data.11)[which(colnames(data.11)== 'R1402A4K2')] = "dev_4"
colnames(data.14)[which(colnames(data.14)== 'R1401A4_K2')] = "dev_4"
colnames(data.11)[which(colnames(data.11)== 'R1402A4K3')] = "dev_4Source"
colnames(data.14)[which(colnames(data.14)== 'R1401A4_K3')] = "dev_4Source"

colnames(data.11)[which(colnames(data.11)== 'R1402B1K2')] = "fund_1"
colnames(data.14)[which(colnames(data.14)== 'R1401B1_K2')] = "fund_1"
colnames(data.11)[which(colnames(data.11)== 'R1402B1K3')] = "fund_1Source"
colnames(data.14)[which(colnames(data.14)== 'R1401B1_K3')] = "fund_1Source"

colnames(data.11)[which(colnames(data.11)== 'R1402B2K2')] = "fund_2"
colnames(data.14)[which(colnames(data.14)== 'R1401B2_K2')] = "fund_2"
colnames(data.11)[which(colnames(data.11)== 'R1402B2K3')] = "fund_2Source"
colnames(data.14)[which(colnames(data.14)== 'R1401B2_K3')] = "fund_2Source"

colnames(data.11)[which(colnames(data.11)== 'R1402B3K2')] = "fund_3"
colnames(data.14)[which(colnames(data.14)== 'R1401B3_K2')] = "fund_3"
colnames(data.11)[which(colnames(data.11)== 'R1402B3K3')] = "fund_3Source"
colnames(data.14)[which(colnames(data.14)== 'R1401B3_K3')] = "fund_3Source"

colnames(data.11)[which(colnames(data.11)== 'R1402C1K2')] = "progm_1"
colnames(data.14)[which(colnames(data.14)== 'R1401C1_K2')] = "progm_1"
colnames(data.11)[which(colnames(data.11)== 'R1402C1K3')] = "progm_1Source"
colnames(data.14)[which(colnames(data.14)== 'R1401C1_K3')] = "progm_1Source"

colnames(data.11)[which(colnames(data.11)== 'R1402C2K2')] = "progm_2"
colnames(data.14)[which(colnames(data.14)== 'R1401C2_K2')] = "progm_2"
colnames(data.11)[which(colnames(data.11)== 'R1402C2K3')] = "progm_2Source"
colnames(data.14)[which(colnames(data.14)== 'R1401C2_K3')] = "progm_2Source"

colnames(data.11)[which(colnames(data.11)== 'R1402C3K2')] = "progm_3"
colnames(data.14)[which(colnames(data.14)== 'R1401C3_K2')] = "progm_3"
colnames(data.11)[which(colnames(data.11)== 'R1402C3K3')] = "progm_3Source"
colnames(data.14)[which(colnames(data.14)== 'R1401C3_K3')] = "progm_3Source"

colnames(data.11)[which(colnames(data.11)== 'R1501AK2')] = "head"
colnames(data.14)[which(colnames(data.14)== 'R1601A_K2')] = "head"
colnames(data.11)[which(colnames(data.11)== 'R1501AK3')] = "headAge"
colnames(data.14)[which(colnames(data.14)== 'R1601A_K3')] = "headAge"
colnames(data.11)[which(colnames(data.11)== 'R1501AK4')] = "headSex"
colnames(data.14)[which(colnames(data.14)== 'R1601A_K4')] = "headSex"
colnames(data.11)[which(colnames(data.11)== 'R1501AK5')] = "headEdu"
colnames(data.14)[which(colnames(data.14)== 'R1601A_K5')] = "headEdu"
data.14$headEdu = recode(data.14$headEdu, '8'= '7', '9'='7')

colnames(data.11)[which(colnames(data.11)== 'R1501BK2')] = "sec"
colnames(data.14)[which(colnames(data.14)== 'R1601B_K2')] = "sec"
colnames(data.11)[which(colnames(data.11)== 'R1501BK3')] = "secAge"
colnames(data.14)[which(colnames(data.14)== 'R1601B_K3')] = "secAge"
colnames(data.11)[which(colnames(data.11)== 'R1501BK4')] = "secSex"
colnames(data.14)[which(colnames(data.14)== 'R1601B_K4')] = "secSex"
colnames(data.11)[which(colnames(data.11)== 'R1501BK5')] = "secEdu"
colnames(data.14)[which(colnames(data.14)== 'R1601B_K5')] = "secEdu"
data.14$secEdu = recode(data.14$secEdu, '8'= '7', '9'='7')

table(data.11$connect)

data.11$connect = ifelse(data.11$connect == 0,0,1)

table(data.11$connect)

schema11 = c('villageID', 'provinceID', 'districtID', 'subDisID', 'villageStatus', 'SLS', 'SLS_N', 'slope', 'atSea', 'atSeaUse_1', 
                 'atSeaUse_2', 'atSeaUse_3', 'atSeaUse_4', 'atSeaUse_5', 'atSeaMangrove', 'byForest', 'byForestUse', 'migrantM', 
                 'migrantF', 'incomeSource', 'agriType', 'strLight', 'strLightEngy', 'fuelCook', 'toilet', 'litter', 'pubTrash', 
                 'river', 'riverUse_1', 'riverUse_2', 'riverUse_3', 'riverUse_4', 'riverUse_5', 'irrigation', 'lake', 'irrigationUse_1', 
                 'lakeUse_1', 'irrigationUse_2', 'lakeUse_2', 'irrigationUse_3', 'lakeUse_3', 'irrigationUse_4', 'lakeUse_4', 'lakeUse_5', 
                 'byRiver', 'byRiver_village', 'byRiver_house', 'byRiver_family', 'bySUTET', 'bySUTET_village', 'bySUTET_house', 
                 'bySUTET_family', 'bySlum', 'bySlum_village', 'bySlum_house', 'bySlum_family', 'pollution_1', 'pollution_1Src', 
                 'pollution_1Cmplt', 'pollution_2', 'pollution_2Src', 'pollution_2Cmplt', 'pollution_3', 'pollution_3Src', 
                 'pollution_3Cmplt', 'burnField', 'excavation', 'disaster_1', 'disaster_1N', 'disaster_1Dead', 'disaster_2', 
                 'disaster_2N', 'disaster_2Dead', 'disaster_3', 'disaster_3N', 'disaster_3Dead', 'disaster_4', 'disaster_4N', 
                 'disaster_4Dead', 'disaster_5', 'disaster_5N', 'disaster_5Dead', 'disaster_6', 'disaster_6N', 'disaster_6Dead', 
                 'disaster_7', 'disaster_7N', 'disaster_7Dead', 'disaster_8', 'disaster_8N', 'disaster_8Dead', 'disaster_9', 
                 'disaster_9N', 'disaster_9Dead', 'disaster_10', 'disaster_10N', 'disaster_10Dead', 'edu_1State', 'edu_1Private', 
                 'edu_1Distance', 'edu_2State', 'edu_2Private', 'edu_2Distance', 'edu_3State', 'edu_3Private', 'edu_3Distance', 
                 'edu_4State', 'edu_4Private', 'edu_4Distance', 'edu_5State', 'edu_5Private', 'edu_5Distance', 'edu_6State', 
                 'edu_6Private', 'edu_7State', 'edu_7Private', 'edu_8Private', 'edu_9Private', 'edu_10Private', 
                 'voc_1N', 'voc_2N', 'voc_3N', 'voc_4N', 'voc_5N', 'voc_6N', 'voc_7N', 'otherEdu_1', 'otherEdu_2', 'otherEdu_3', 
                 'otherEdu_4', 'otherEdu_5', 'health_1', 'health_1N', 'health_1Distance', 'health_1EasytoTravel', 'health_2', 
                 'health_2N', 'health_2Distance', 'health_2EasytoTravel', 'health_3', 'health_3N', 'health_3Distance', 
                 'health_3EasytoTravel', 'health_4', 'health_4N', 'health_4Distance', 'health_4EasytoTravel', 'health_5', 'health_5N', 
                 'health_5Distance', 'health_5EasytoTravel', 'health_6', 'health_6N', 'health_6Distance', 'health_6EasytoTravel', 
                 'health_7', 'health_7N', 'health_7Distance', 'health_7EasytoTravel', 'health_8', 'health_8N', 'health_8Distance', 
                 'health_8EasytoTravel', 'health_9', 'health_9N', 'health_9Distance', 'health_9EasytoTravel', 'health_10', 'health_10N', 
                 'health_11', 'health_11N', 'health_11Distance', 'health_11EasytoTravel', 'health_12', 'health_12Distance', 
                 'health_12EasytoTravel', 'posyandu_N', 'posyandu_N2', 'doctorM_N', 'doctorF_N', 'dentist_N', 'midwife_N', 
                 'otherHealth_N', 'TBA_N', 'disease_1', 'disease_1N', 'disease_1Died', 'disease_2', 'disease_2N', 'disease_2Died', 
                 'disease_3', 'disease_3N', 'disease_3Died', 'disease_4', 'disease_4N', 'disease_4Died', 'disease_5', 'disease_5N', 
                 'disease_5Died', 'disease_6', 'disease_6N', 'disease_6Died', 'mulnutrition_N', 'JAMKESMAS_N', 'SKTM_N', 'religion_1', 
                 'religion_2', 'religion_3', 'religion_4', 'religion_5', 'religion_6', 'religion_7', 'mostReligion', 'worship_1', 'worship_2',
                 'worship_3', 'worship_4', 'worship_5', 'worship_6', 'worship_7', 'worship_8', 'disabled_1', 'disabled_2', 'disabled_3', 
                 'disabled_4', 'disabled_5', 'disabled_6', 'disabled_7', 'disabled_8', 'disabled_9', 'multiEthnicity', 'commService', 'cinema', 
                 'cinemaDistance', 'pub', 'pubDistance', 'sport_1', 'sport_1Group', 'sport_2', 'sport_2Group', 'sport_3', 'sport_3Group', 
                 'sport_4', 'sport_4Group', 'sport_5', 'sport_5Group', 'sport_6', 'sport_6Group', 'sport_7', 'sport_7Group', 
                 'sport_8Group', 'sport_9Group', 'sport_10Group', 'infrastructure', 'roadType', 'roadThrough', 'distance_1', 
                 'distance_1Transport', 'distance_2', 'distance_2Transport', 'distance_3', 'distance_3Transport', 'landPhone', 
                 'landPhone_N', 'payPhone', 'BTS', 'cellPhone', 'telecomm', 'internet', 'postOffice', 'mobilePO', 'TV_1', 'TV_2', 
                 'TV_3', 'TV_4', 'landChg_1', 'landChg_1pct', 'landChg_2', 'landChg_2pct', 'landChg_3', 'landChg_3pct', 'landChg_4', 
                 'landChg_4pct', 'landChg_5', 'landChg_5pct', 'landChg_6', 'landChg_6pct', 'KUD', 'nonKUD', 'industry_1', 
                 'industry_2', 'industry_3', 'industry_4', 'industry_5', 'industry_6', 'industry_7', 'industry_8', 'gpShop', 
                 'gpShopDis', 'kerosene', 'LPG', 'prmntMkt', 'prmntMktDis', 'fltMkt', 'miniMkt', 'grocery', 'tavern', 'restaurant', 
                 'hotel', 'lodge', 'cooperative_1', 'cooperative_2', 'cooperative_3', 'cooperative_4', 'creditFclty_1',
                 'creditFclty_2', 'bank_1', 'bank_1N', 'bank_2', 'bank_2N', 'massFght', 'fght_1N', 'fght_1Dead', 
                 'fght_1Injured', 'fght_2N', 'fght_2Dead', 'fght_2Injured', 'fght_3N', 'fght_3Dead', 'fght_3Injured', 'fght_4N', 
                 'fght_4Dead', 'fght_4Injured', 'fght_5N', 'fght_5Dead', 'fght_5Injured', 'fght_6N', 'fght_6Dead', 'fght_6Injured', 
                 'fght_7N', 'fght_7Dead', 'fght_7Injured', 'criminal_1', 'criminal_1Tndcy', 'criminal_2', 'criminal_2Tndcy', 
                 'criminal_3', 'criminal_3Tndcy', 'criminal_4', 'criminal_4Tndcy', 'criminal_5', 'criminal_5Tndcy', 'criminal_6', 
                 'criminal_6Tndcy', 'criminal_7', 'criminal_7Tndcy', 'criminal_8', 'criminal_8Tndcy', 'criminal_9', 'criminal_9Tndcy', 
                 'criminal_10', 'criminal_10Tndcy', 'suicide', 'streetChild', 'sexWorker', 'secActivities_1', 'secActivities_2', 
                 'secActivities_3', 'secActivities_4', 'police', 'policeDis', 'policeEasytoTravel', 'guards', 'income_1Form', 
                 'income_1', 'income_2Form', 'income_2', 'income_3Form', 'income_3', 'income_4Form', 'income_4', 'income_5Form', 
                 'income_5', 'income_6Form', 'income_6', 'income_7Form', 'income_7', 'dev_1', 'dev_1Source', 'dev_2', 'dev_2Source', 
                 'dev_3', 'dev_3Source', 'dev_4', 'dev_4Source', 'fund_1', 'fund_1Source', 'fund_2', 'fund_2Source', 'fund_3', 
                 'fund_3Source', 'progm_1', 'progm_1Source', 'progm_2', 'progm_2Source', 'progm_3', 'progm_3Source', 'head', 
                 'headAge', 'headSex', 'headEdu', 'sec', 'secAge', 'secSex', 'secEdu', 'POP','connect')

schema14 = c('villageID', 'provinceID', 'districtID', 'subDisID', 'villageStatus', 'SLS', 'SLS_N', 'slope', 'atSea', 'atSeaUse_1', 
                 'atSeaUse_2', 'atSeaUse_3', 'atSeaUse_4', 'atSeaUse_5', 'atSeaMangrove', 'byForest', 'byForestUse', 'migrantM', 
                 'migrantF', 'incomeSource', 'agriType', 'strLight', 'strLightEngy', 'fuelCook', 'toilet', 'litter', 'pubTrash', 
                 'river', 'riverUse_1', 'riverUse_2', 'riverUse_3', 'riverUse_4', 'riverUse_5', 'irrigation', 'lake', 'irrigationUse_1', 
                 'lakeUse_1', 'irrigationUse_2', 'lakeUse_2', 'irrigationUse_3', 'lakeUse_3', 'irrigationUse_4', 'lakeUse_4', 'lakeUse_5', 
                 'byRiver', 'byRiver_village', 'byRiver_house', 'byRiver_family', 'bySUTET', 'bySUTET_village', 'bySUTET_house', 
                 'bySUTET_family', 'bySlum', 'bySlum_village', 'bySlum_house', 'bySlum_family', 'pollution_1', 'pollution_1Src', 
                 'pollution_1Cmplt', 'pollution_2', 'pollution_2Src', 'pollution_2Cmplt', 'pollution_3', 'pollution_3Src', 
                 'pollution_3Cmplt', 'burnField', 'excavation', 'disaster_1', 'disaster_1N', 'disaster_1Dead', 'disaster_2', 
                 'disaster_2N', 'disaster_2Dead', 'disaster_3', 'disaster_3N', 'disaster_3Dead', 'disaster_4', 'disaster_4N', 
                 'disaster_4Dead', 'disaster_5', 'disaster_5N', 'disaster_5Dead', 'disaster_6', 'disaster_6N', 'disaster_6Dead', 
                 'disaster_7', 'disaster_7N', 'disaster_7Dead', 'disaster_8', 'disaster_8N', 'disaster_8Dead', 'disaster_9', 
                 'disaster_9N', 'disaster_9Dead', 'disaster_10', 'disaster_10N', 'disaster_10Dead', 'edu_1State', 'edu_1Private', 
                 'edu_1Distance', 'edu_2State', 'edu_2Private', 'edu_2Distance', 'edu_3State', 'edu_3Private', 'edu_3Distance', 
                 'edu_4State', 'edu_4Private', 'edu_4Distance', 'edu_5State', 'edu_5Private', 'edu_5Distance', 'edu_6State', 
                 'edu_6Private', 'edu_7State', 'edu_7Private', 'edu_8Private', 'edu_9Private', 'edu_10Private', 
                 'voc_1N', 'voc_2N', 'voc_3N', 'voc_4N', 'voc_5N', 'voc_6N', 'voc_7N', 'otherEdu_1', 'otherEdu_2', 'otherEdu_3', 
                 'otherEdu_4', 'otherEdu_5', 'health_1', 'health_1N', 'health_1Distance', 'health_1EasytoTravel', 'health_2', 
                 'health_2N', 'health_2Distance', 'health_2EasytoTravel', 'health_3', 'health_3N', 'health_3Distance', 
                 'health_3EasytoTravel', 'health_4', 'health_4N', 'health_4Distance', 'health_4EasytoTravel', 'health_5', 'health_5N', 
                 'health_5Distance', 'health_5EasytoTravel', 'health_6', 'health_6N', 'health_6Distance', 'health_6EasytoTravel', 
                 'health_7', 'health_7N', 'health_7Distance', 'health_7EasytoTravel', 'health_8', 'health_8N', 'health_8Distance', 
                 'health_8EasytoTravel', 'health_9', 'health_9N', 'health_9Distance', 'health_9EasytoTravel', 'health_10', 'health_10N', 
                 'health_11', 'health_11N', 'health_11Distance', 'health_11EasytoTravel', 'health_12', 'health_12Distance', 
                 'health_12EasytoTravel', 'posyandu_N', 'posyandu_N2', 'doctorM_N', 'doctorF_N', 'dentist_N', 'midwife_N', 
                 'otherHealth_N', 'TBA_N', 'disease_1', 'disease_1N', 'disease_1Died', 'disease_2', 'disease_2N', 'disease_2Died', 
                 'disease_3', 'disease_3N', 'disease_3Died', 'disease_4', 'disease_4N', 'disease_4Died', 'disease_5', 'disease_5N', 
                 'disease_5Died', 'disease_6', 'disease_6N', 'disease_6Died', 'mulnutrition_N', 'JAMKESMAS_N', 'SKTM_N', 'religion_1', 
                 'religion_2', 'religion_3', 'religion_4', 'religion_5', 'religion_6', 'religion_7', 'mostReligion', 'worship_1', 'worship_2',
                 'worship_3', 'worship_4', 'worship_5', 'worship_6', 'worship_7', 'worship_8', 'disabled_1', 'disabled_2', 'disabled_3', 
                 'disabled_4', 'disabled_5', 'disabled_6', 'disabled_7', 'disabled_8', 'disabled_9', 'multiEthnicity', 'commService', 'cinema', 
                 'cinemaDistance', 'pub', 'pubDistance', 'sport_1', 'sport_1Group', 'sport_2', 'sport_2Group', 'sport_3', 'sport_3Group', 
                 'sport_4', 'sport_4Group', 'sport_5', 'sport_5Group', 'sport_6', 'sport_6Group', 'sport_7', 'sport_7Group', 
                 'sport_8Group', 'sport_9Group', 'sport_10Group', 'infrastructure', 'roadType', 'roadThrough', 'distance_1', 
                 'distance_1Transport', 'distance_2', 'distance_2Transport', 'distance_3', 'distance_3Transport', 'landPhone', 
                 'landPhone_N', 'payPhone', 'BTS', 'cellPhone', 'telecomm', 'internet', 'postOffice', 'mobilePO', 'TV_1', 'TV_2', 
                 'TV_3', 'TV_4', 'landChg_1', 'landChg_1pct', 'landChg_2', 'landChg_2pct', 'landChg_3', 'landChg_3pct', 'landChg_4', 
                 'landChg_4pct', 'landChg_5', 'landChg_5pct', 'landChg_6', 'landChg_6pct', 'KUD', 'nonKUD', 'industry_1', 
                 'industry_2', 'industry_3', 'industry_4', 'industry_5', 'industry_6', 'industry_7', 'industry_8', 'gpShop', 
                 'gpShopDis', 'kerosene', 'LPG', 'prmntMkt', 'prmntMktDis', 'fltMkt', 'miniMkt', 'grocery', 'tavern', 'restaurant', 
                 'hotel', 'lodge', 'cooperative_1', 'cooperative_2', 'cooperative_3', 'cooperative_4', 'creditFclty_1',
                 'creditFclty_2', 'bank_1', 'bank_1N', 'bank_2', 'bank_2N', 'massFght', 'fght_1N', 'fght_1Dead', 
                 'fght_1Injured', 'fght_2N', 'fght_2Dead', 'fght_2Injured', 'fght_3N', 'fght_3Dead', 'fght_3Injured', 'fght_4N', 
                 'fght_4Dead', 'fght_4Injured', 'fght_5N', 'fght_5Dead', 'fght_5Injured', 'fght_6N', 'fght_6Dead', 'fght_6Injured', 
                 'fght_7N', 'fght_7Dead', 'fght_7Injured', 'criminal_1', 'criminal_1Tndcy', 'criminal_2', 'criminal_2Tndcy', 
                 'criminal_3', 'criminal_3Tndcy', 'criminal_4', 'criminal_4Tndcy', 'criminal_5', 'criminal_5Tndcy', 'criminal_6', 
                 'criminal_6Tndcy', 'criminal_7', 'criminal_7Tndcy', 'criminal_8', 'criminal_8Tndcy', 'criminal_9', 'criminal_9Tndcy', 
                 'criminal_10', 'criminal_10Tndcy', 'suicide', 'streetChild', 'sexWorker', 'secActivities_1', 'secActivities_2', 
                 'secActivities_3', 'secActivities_4', 'police', 'policeDis', 'policeEasytoTravel', 'guards', 'income_1Form', 
                 'income_1', 'income_2Form', 'income_2', 'income_3Form', 'income_3', 'income_4Form', 'income_4', 'income_5Form', 
                 'income_5', 'income_6Form', 'income_6', 'income_7Form', 'income_7', 'dev_1', 'dev_1Source', 'dev_2', 'dev_2Source', 
                 'dev_3', 'dev_3Source', 'dev_4', 'dev_4Source', 'fund_1', 'fund_1Source', 'fund_2', 'fund_2Source', 'fund_3', 
                 'fund_3Source', 'progm_1', 'progm_1Source', 'progm_2', 'progm_2Source', 'progm_3', 'progm_3Source', 'head', 
                 'headAge', 'headSex', 'headEdu', 'sec', 'secAge', 'secSex', 'secEdu', 'POP')

data.N14 = data.14[, schema14]
data.N11 = data.11[, schema11] # only difference in "connect"

dim(data.N14)
dim(data.N11)

# check if variables are in same order
sum(names(data.N11)==names(data.N14))

save(data.N11, file = "dataN11.RData")
save(data.N14, file = "dataN14.RData")
