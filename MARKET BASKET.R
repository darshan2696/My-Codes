library(arules)
library(plyr)
library(dplyr)
library(readxl)

getwd()
setwd("C:\\Users\\ethic\\Desktop\\test")
data<-read_excel("D:\\Downloads\\Online Retail Corrected.xlsx")

View(data)

str(data)

data$InvoiceDate <- as.Date(data$InvoiceDate)
trans_data = ddply(data,c("InvoiceDate","InvoiceNo"),
                   function(df)paste(df$Description,collapse=","))
View(trans_data)

trans_data$InvoiceDate = NULL
trans_data$InvoiceNo = NULL
colnames(trans_data) = c("items")
write.csv(trans_data,"items.csv",quote = FALSE, row.names = FALSE)
items = read.transactions("items.csv",format = c("basket"),sep = ",")

summary(items)

rules<-apriori(items,parameter = list(support = 0.01, conf = 0.8),appearance = list(lhs="COFFEE",rhs="SUGAR"))

summary(rules)
inspect(rules)


#==========================================================
#ASSINGMENT
#==========================================================

#Ans

#1a) 0.01017227
#1b) 0.8008753 
#2) {WOBBLY CHICKEN}          => {METAL}
# {WOBBLY RABBIT}             => {METAL} 
# {DECORATION}                => {METAL} 
# {DECORATION,WOBBLY CHICKEN} => {METAL} 
# {DECORATION,WOBBLY RABBIT}  => {METAL}
#3){COFFEE} => {SET 3 RETROSPOT TEA} 
#  {COFFEE} => {SUGAR}   



