library(readr)
library(dplyr)

X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
want<-c(127,128,80,121)
X106edu$大職業別[want]
X106edu$`大學-薪資`[want]
X106edu$`研究所及以上-薪資`[want]
#這薪資比我想像中低...沒有一個到4萬以上，落差蠻大QQ
#大學跟研究所薪資 差1500~4000左右
#但就算這樣我還是不會唸研究所，因為這畢竟只是起薪，如果我不念研究所
#而是花這兩年去工作，那搞不好我薪水會比他們起薪高
#就算沒有，他們花錢去學校讀了兩年，付錢，而且沒有收入
#但我不但不用付錢，還賺錢，多賺了兩年，怎麼樣都比去讀研究所划算
