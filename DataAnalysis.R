local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org"
options(repos=r)})
install.packages("jsonlite")
install.packages("dplyr")
install.packages("readr")

library(jsonlite)
library(readr)
library(dplyr)

X103edu <- read_csv("C:/Users/user/Desktop/RHW/103edu.csv", 
                    locale = locale(encoding = "BIG5"))
X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
#gsub("—|…","",X103edu)
#X103EDU<-gsub("—|…","",X103edu)
X103edu$`大學-薪資`<-as.numeric(X103edu$`大學-薪資`)
#X106edu$`大學-薪資`<-as.numeric(X106edu$`大學-薪資`)

join_103_106edu<-inner_join(X106edu,X103edu,by="大職業別")
join_103_106edu$`大學-薪資.x`<-gsub("—|…","",join_103_106edu$`大學-薪資.x`)
join_103_106edu$`大學-薪資.x`<-as.numeric(join_103_106edu$`大學-薪資.x`)
join_103_106edu$`大學-薪資.y`<-gsub("—|…","",join_103_106edu$`大學-薪資.y`)
join_103_106edu$`大學-薪資.y`<-as.numeric(join_103_106edu$`大學-薪資.y`)


join_103_106edu$percent<-(join_103_106edu$`大學-薪資.x`/join_103_106edu$`大學-薪資.y`)
result<-arrange(join_103_106edu,desc(percent))
filter(result,percent>1)
result<-filter(result,percent>1.05)
#head(result,10)
job<-strsplit(result$大職業別,"-")
#num<-1:length(job)
#job[[1:58]][1] #error
tmp<-lapply(job,"[",1)
tmp2<-unlist(tmp)
table(tmp2)
#table(job)
#new_edu<-rbind(X106_edu,X103_edu)
