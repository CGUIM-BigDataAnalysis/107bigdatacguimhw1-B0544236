#HW Q3


library(readr)
library(dplyr)

X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))

X106Compare<-X106edu
X106Compare$`研究所及以上-薪資`<-gsub("—|…","",X106Compare$`研究所及以上-薪資`)
X106Compare$`研究所及以上-薪資`<-as.numeric(X106Compare$`研究所及以上-薪資`)
X106Compare$`大學-薪資`<-gsub("—|…","",X106Compare$`大學-薪資`)
X106Compare$`大學-薪資`<-as.numeric(X106Compare$`大學-薪資`)

X106Compare$percent<-(X106Compare$`研究所及以上-薪資`/X106Compare$`大學-薪資`)
X106Compare<-arrange(X106Compare,desc(percent))
head(X106Compare,10)
