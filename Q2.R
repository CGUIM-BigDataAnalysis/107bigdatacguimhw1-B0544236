#HW Q2

library(readr)
library(dplyr)
X103edu <- read_csv("C:/Users/user/Desktop/RHW/103edu.csv", 
                    locale = locale(encoding = "BIG5"))
X104edu <- read_csv("C:/Users/user/Desktop/RHW/104edu.csv", 
                    locale = locale(encoding = "BIG5"))
X105edu <- read_csv("C:/Users/user/Desktop/RHW/105edu.csv", 
                    locale = locale(encoding = "BIG5"))
X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))

X103To106edu<-rbind(X103edu,X104edu)
X103To106edu<-rbind(X103To106edu,X105edu)
X103To106edu<-rbind(X103To106edu,X106edu)

#X103To106edu<-inner_join(X103edu,X104edu, by="大職業別")
#X103To106edu<-inner_join(X103To106edu,X105edu, by="大職業別")
#X103To106edu<-inner_join(X103To106edu,X106edu, by="大職業別")

class(X103To106edu$`大學-女/男`)
X103To106edu$`大學-女/男`<-gsub("—|…","",X103To106edu$`大學-女/男`)
X103To106edu$`大學-女/男`<-as.numeric(X103To106edu$`大學-女/男`)
X103To106<-arrange(X103To106edu, `大學-女/男`)
X103To106Desc<-arrange(X103To106edu, desc(`大學-女/男`))

Q2result<-filter(X103To106,`大學-女/男`<100)
Q2result2<-filter(X103To106Desc,`大學-女/男`>100)
