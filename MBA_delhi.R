#removing history
rm(list=ls(all=TRUE))

#loading data

all_store_1_15Nov <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all store_1-15Nov.csv")
all_store_16_30nov <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all store_16-30nov.csv")
all_stores_01_15july <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_01-15july.csv")
all_stores_16_31july <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_16-31july.csv")
all_stores_1_15aug <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_1-15aug.csv")
all_stores_16_31aug <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_16-31aug.csv")
all_stores_1_15oct <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_1-15oct.csv")
all_stores_16_30oct <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_16-30oct.csv")
all_stores_1_15sep <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_1-15sep.csv")
all_stores_16_30sep <- read.csv("C:/Users/Subhraneel.c/Desktop/ghialtxn/Delhi T1/all stores_16-30sep.csv")

library(dplyr)
data_all <- bind_rows(all_stores_01_15july,all_stores_16_31july,all_stores_1_15aug,all_stores_16_31aug,all_stores_1_15sep,all_stores_16_30sep,all_stores_1_15oct,all_stores_16_30oct,all_store_1_15Nov,all_store_16_30nov)

glimpse(data_all)

write.csv(data_all,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\data_all.csv")

#GAURDIAN_T1D

G<-data_all %>% filter(Name=="Gaurdian_T1D")

write.csv(G,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Gaurdian.csv")

#MBA
#supp=0.00008
library(arules)

trans_G <- as(split(Gaurdian$Item.No_,Gaurdian$Transaction.No_),"transactions")

r<-apriori(trans_G,parameter = list(supp=0.00008,conf=0.5,target="rules"))

inspect(head(sort(r, by="confidence")))

library(arulesViz)

plot(r, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)

#Chokola_T1D
#supp=0.0007

C<- data_all %>% filter(Name=="Chokola_T1D")



write.csv(C,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Chokola.csv")

trans_C <-as(split(chokola_updt$Description,chokola_updt$Receipt_No),"transactions")

r1<-apriori(trans_C,parameter = list(supp=0.002,conf=0.5,target="rules"))

inspect(head(sort(r1, by="confidence"),10))

plot(r1, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)


#The Cellar _T1D
#supp=0.00007

Ce <- data_all %>% filter(Name=="The Cellar _T1D")

write.csv(Ce,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Cellar.csv")

trans_Ce <-as(split(new_ce$Item.No_,new_ce$Transaction.No_),"transactions")

r2<-apriori(trans_Ce,parameter = list(supp=0.00007,conf=0.5,target="rules"))

inspect(head(sort(r2, by="confidence")))

plot(r2, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)


## RELIANCE HAMLEYS ##

RH <- data_all %>% filter(Name=="RELIANCE HAMLEYS")

write.csv(RH,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Reliance.csv")

trans_RH <-as(split(RH$Item.No_,RH$Transaction.No_),"transactions")

r3<-apriori(trans_RH,parameter = list(supp=0.003,conf=0.5,target="rules"))

inspect(head(sort(r3, by="confidence")))

plot(r3, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)



subset.matrix <- is.subset(r3, r3)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- r3[!redundant]
rules_pr<-rules.pruned
inspect(rules_pr)
inspect(rules.pruned)

inspect(head(sort(rules.pruned, by="confidence")))

plot(rules.pruned, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)



## Croma_T1D ##

CR <- data_all %>% filter(Name=="Croma_T1D")

write.csv(CR,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Croma.csv")

trans_CR <-as(split(CR$Item.No_,CR$Transaction.No_),"transactions")

r4<-apriori(trans_CR,parameter = list(supp=0.001,conf=0.5,target="rules"))

inspect(head(sort(r4, by="confidence")))

subset.matrix <- is.subset(r4, r4)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned1 <- r4[!redundant]

inspect(head(sort(rules.pruned1, by="confidence")))

plot(rules.pruned1, method="graph",control=list(type="items",main=""),interactive = TRUE,shading = NA)


## United Colors of Benetton _T1D ##

UCB <- data_all %>% filter(Name=="United Colors of Benetton _T1D")

write.csv(UCB,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\UCB.csv")

trans_UCB <-as(split(UCB$Item.No_,UCB$Transaction.No_),"transactions")

r5<-apriori(trans_UCB,parameter = list(supp=0.0001,conf=0.5,target="rules"))

inspect(head(sort(r5, by="confidence")))

subset.matrix <- is.subset(r5, r5)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned2 <- r5[!redundant]

inspect(head(sort(rules.pruned2, by="confidence")))


## FABINDIA_T1D ##

FAB <- data_all %>% filter(Name=="FABINDIA_T1D")

write.csv(FAB,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\FAB_IND.csv")

trans_FAB <-as(split(FAB$Item.No_,FAB$Transaction.No_),"transactions")

r6<-apriori(trans_FAB,parameter = list(supp=0.001,conf=0.5,target="rules"))

inspect(head(sort(r6, by="confidence")))


subset.matrix <- is.subset(r6, r6)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned3 <- r6[!redundant]

inspect(head(sort(rules.pruned3, by="confidence")))

## Relays ##

Rel<- data_all %>% filter(Name=="Relay T1C Chocolate store")

write.csv(Rel,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Relay.csv")

trans_rel <-as(split(Rel$Item.No_,Rel$Transaction.No_),"transactions")

r7<-apriori(trans_rel,parameter = list(supp=0.01,conf=0.5,target="rules"))

inspect(head(sort(r7, by="confidence")))

## Accesories ##

Acc<-data_all %>% filter(Name=="Accessorize_T1D")

write.csv(Acc,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Accesories.csv")

trans_acc <-as(split(Acc$Item.No_,Acc$Transaction.No_),"transactions")

r7<-apriori(trans_acc,parameter = list(supp=0.001,conf=0.5,target="rules"))

inspect(head(sort(r7, by="confidence")))

## Flight based Analysis ##

#6E-105
#6E-107

p<- aggregate(Chokola$Gross.Amount,by=list(Chokola$Time),FUN=sum)
p %>% group_by(Group.1) %>% arrange(desc(x))-> chokola_time
#07:40:07
#16:42:19
#15:47:48
#12:13:47
#08:07:44
#08:28:21


#count based best time

# 07:40:07  
# 20:59:44
# 16:19:56
# 04:51:51
# 19:40:31

# MBA #

flight_6E_chokola<- Chokola %>% filter(Flight.No_=="6E-105")

write.csv(Rel,file = "C:\\Users\\Subhraneel.c\\Desktop\\ghialtxn\\Delhi T1\\Relay.csv")

trans_Fl_ch <-as(split(flight_chokola$Item.No_,flight_chokola$Transaction.No_),"transactions")

r8<-apriori(trans_Fl_ch,parameter = list(supp=0.002,conf=0.5,target="rules"))

inspect(head(sort(r8, by="confidence")))

# best date

#2016-10-23
#2016-10-28 #friday
#2016-10-29 # saturday
#2016-10-22

# MBA 

trans_dt_Fl_ch_ <-as(split(best_date_6E_105$Item.No_,best_date_6E_105$Transaction.No_),"transactions")

r8<-apriori(trans_dt_Fl_ch_,parameter = list(supp=0.01,conf=0.5,target="rules"))

inspect(head(sort(r8, by="confidence")))


