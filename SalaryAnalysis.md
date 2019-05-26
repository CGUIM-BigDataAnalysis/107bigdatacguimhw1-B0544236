107-2 大數據分析方法 作業一
================
put your name here

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
#install.packages("weatherData")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("readr")
#library(jsonlite)
#.libPaths("D:/R-3.5.2/library")
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.5.3

``` r
X103edu <- read_csv("C:/Users/user/Desktop/RHW/103edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#這邊單純就是讀取103、106csv檔案，但因為亂碼問題
#多了一行BIG5編碼處理
#X103edu$`大學-薪資`<-as.numeric(X103edu$`大學-薪資`)


join_103_106edu<-inner_join(X106edu,X103edu,by="大職業別")
#用兩者共通的大職業別來做inner_join
join_103_106edu$`大學-薪資.x`<-gsub("—|…","",join_103_106edu$`大學-薪資.x`)
#先刪掉大學-薪資.x(106年)的-和...我們用gsub用空字串取代他們
join_103_106edu$`大學-薪資.x`<-as.numeric(join_103_106edu$`大學-薪資.x`)
#轉數字型態
join_103_106edu$`大學-薪資.y`<-gsub("—|…","",join_103_106edu$`大學-薪資.y`)
join_103_106edu$`大學-薪資.y`<-as.numeric(join_103_106edu$`大學-薪資.y`)
```

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
#這是R Code Chunk
join_103_106edu$percent<-(join_103_106edu$`大學-薪資.x`/join_103_106edu$`大學-薪資.y`)
#先新增一個percent欄位，存的是106大學薪資/103
result<-arrange(join_103_106edu,desc(percent))
#我們用result存由大到小排列的資料表
filter(result,percent>1)
```

    ## # A tibble: 104 x 28
    ##    年度.x 大職業別 `經常性薪資-薪資.x`~ `經常性薪資-女/男.x`~ `國中及以下-薪資.x`~
    ##     <dbl> <chr>               <dbl> <chr>            <chr>           
    ##  1   2017 其他服務業-技~            27270 98.31            —               
    ##  2   2017 住宿及餐飲業-~            24468 99.76            23639           
    ##  3   2017 用水供應及污染~            31505 98.77            —               
    ##  4   2017 專業_科學及技~            35538 98.8             —               
    ##  5   2017 其他服務業-技~            24938 98.2             —               
    ##  6   2017 營造業-服務及~            27308 96.68            24667           
    ##  7   2017 其他服務業-專~            32250 100              —               
    ##  8   2017 資訊及通訊傳播~            33646 99.03            —               
    ##  9   2017 不動產業-專業~            34237 100              —               
    ## 10   2017 教育服務業-事~            24326 98.42            —               
    ## # ... with 94 more rows, and 23 more variables:
    ## #   `國中及以下-女/男.x` <chr>, `高中或高職-薪資.x` <chr>,
    ## #   `高中或高職-女/男.x` <chr>, `專科-薪資.x` <chr>, `專科-女/男.x` <chr>,
    ## #   `大學-薪資.x` <dbl>, `大學-女/男.x` <chr>,
    ## #   `研究所及以上-薪資.x` <chr>, `研究所及以上-女/男.x` <chr>,
    ## #   年度.y <dbl>, `經常性薪資-薪資.y` <dbl>, `經常性薪資-女/男.y` <dbl>,
    ## #   `國中及以下-薪資.y` <chr>, `國中及以下-女/男.y` <chr>,
    ## #   `高中或高職-薪資.y` <chr>, `高中或高職-女/男.y` <chr>,
    ## #   `專科-薪資.y` <chr>, `專科-女/男.y` <chr>, `大學-薪資.y` <dbl>,
    ## #   `大學-女/男.y` <chr>, `研究所及以上-薪資.y` <chr>,
    ## #   `研究所及以上-女/男.y` <chr>, percent <dbl>

``` r
#選取percent大於1，表示106薪水比較多的row
```

### 提高超過5%的的職業有哪些?

``` r
#這是R Code Chunk
result<-filter(result,percent>1.05)
#高出5%，因此塞選percent>1.05的欄位
```

### 主要的職業種別是哪些種類呢?

``` r
#這是R Code Chunk
job<-strsplit(result$大職業別,"-")
#因為我們需要-前面的主職業出現次數，因此先做切割
tmp<-lapply(job,"[",1)
#因為切割完，有可能每個List有1~2個字串，但我們只要每一列的第一個
#運用lappy來取
tmp2<-unlist(tmp)
#解除list格式
table(tmp2)
```

    ## tmp2
    ##      工業及服務業部門              工業部門              不動產業 
    ##                     1                     1                     1 
    ##            支援服務業  用水供應及污染整治業          住宿及餐飲業 
    ##                     3                     6                     4 
    ##            其他服務業            服務業部門          金融及保險業 
    ##                     5                     5                     1 
    ## 專業_科學及技術服務業            教育服務業      資訊及通訊傳播業 
    ##                     5                     5                     5 
    ##          運輸及倉儲業      電力及燃氣供應業                製造業 
    ##                     4                     2                     1 
    ##                營造業        醫療保健服務業 藝術_娛樂及休閒服務業 
    ##                     3                     2                     3 
    ##      礦業及土石採取業 
    ##                     1

``` r
#用table來記出現次數
```

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#這是R Code Chunk
library(readr)
library(dplyr)
X103edu <- read_csv("C:/Users/user/Desktop/RHW/103edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X104edu <- read_csv("C:/Users/user/Desktop/RHW/104edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X105edu <- read_csv("C:/Users/user/Desktop/RHW/105edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#前面就單純讀取資料
X103To106edu<-rbind(X103edu,X104edu)
X103To106edu<-rbind(X103To106edu,X105edu)
X103To106edu<-rbind(X103To106edu,X106edu)
#這邊不用inner_join用rbind因為我們不想新增多的欄位，而是103~106的資料都擺在同個欄位

#X103To106edu<-inner_join(X103edu,X104edu, by="大職業別")
#X103To106edu<-inner_join(X103To106edu,X105edu, by="大職業別")
#X103To106edu<-inner_join(X103To106edu,X106edu, by="大職業別")

class(X103To106edu$`大學-女/男`)
```

    ## [1] "character"

``` r
#先檢查類別，發現是字串
X103To106edu$`大學-女/男`<-gsub("—|…","",X103To106edu$`大學-女/男`)
#一樣先用gsub空字串取代-和...
X103To106edu$`大學-女/男`<-as.numeric(X103To106edu$`大學-女/男`)
#轉數字
X103To106<-arrange(X103To106edu, `大學-女/男`)
#由小到大排序
X103To106Desc<-arrange(X103To106edu, desc(`大學-女/男`))
#由大到小排序

Q2result<-filter(X103To106,`大學-女/男`<100)
#相除<1表示男生薪水比女生多(這邊用100是因為資料表內他是用1~100表示)
#No1.礦業及土石採取業-技藝_機械設備操作及組裝人員 #NO2.教育服務業-技藝_機械設備操作及組裝人員...no.17營造業-技藝、機械設備操作及組#裝人員
#我從最後這幾名(女生薪水少男生比較多)的工作來看幾乎都是技術、設備、勞力工作人員
#這樣看結果蠻合理的，可能這些工作比較需要男生，而比較不願意用女生，因此給女生薪水較低
```

### 哪些行業女生薪資比男生薪資多?

``` r
#這是R Code Chunk
Q2result2<-filter(X103To106Desc,`大學-女/男`>100)
#>1(100)表示女生薪水多於男生
#我們看結果，只有發現三個職業女生薪水比男生多一些
#no.1資訊及通訊傳播業-服務及銷售工作人員
#no.2專業、科學及技術服務業-技藝、機械設備操作及組裝人員
#no.3金融及保險業-專業人員
#從結果來看第1和3名感覺還蠻合理，服務業負責服務、專業、銷售
#這種工作很多女生反而更擅長，而且用不到太多的勞力，比較需要的是口才，和專業能力
#但是第2名蠻驚訝的，這算是勞力型的工作，但也需要具備專業知識
#我猜想，可能專業知識需求佔這個工作內容比重比較高，而不是勞力
#也可能女生比較細心，這個工作又需要細心操作
```

研究所薪資差異
--------------

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#這是R Code Chunk
library(readr)
library(dplyr)

X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X106Compare<-X106edu
X106Compare$`研究所及以上-薪資`<-gsub("—|…","",X106Compare$`研究所及以上-薪資`)
X106Compare$`研究所及以上-薪資`<-as.numeric(X106Compare$`研究所及以上-薪資`)
#以上步驟都和前幾題一樣
X106Compare$`大學-薪資`<-gsub("—|…","",X106Compare$`大學-薪資`)
X106Compare$`大學-薪資`<-as.numeric(X106Compare$`大學-薪資`)

X106Compare$percent<-(X106Compare$`研究所及以上-薪資`/X106Compare$`大學-薪資`)
#我們新增percent欄位來儲存研究所學歷薪資/大學學歷薪資
X106Compare<-arrange(X106Compare,desc(percent))
#由大到小排列
head(X106Compare,10)
```

    ## # A tibble: 10 x 15
    ##     年度 大職業別 `經常性薪資-薪資`~ `經常性薪資-女/男`~ `國中及以下-薪資`~
    ##    <dbl> <chr>               <dbl> <chr>            <chr>           
    ##  1  2017 礦業及土石採取~            23441 97.21            —               
    ##  2  2017 專業_科學及技~            29345 97.86            23556           
    ##  3  2017 其他服務業-技~            27270 98.31            —               
    ##  4  2017 專業_科學及技~            26712 98.51            —               
    ##  5  2017 批發及零售業~            26457 98.54            23376           
    ##  6  2017 製造業              26782 98.23            22563           
    ##  7  2017 藝術_娛樂及休~            24602 97.67            —               
    ##  8  2017 工業部門            26860 98.11            22669           
    ##  9  2017 工業及服務業部~            27055 98.31            22841           
    ## 10  2017 服務業部門~            27363 98.56            23045           
    ## # ... with 10 more variables: `國中及以下-女/男` <chr>,
    ## #   `高中或高職-薪資` <chr>, `高中或高職-女/男` <chr>, `專科-薪資` <chr>,
    ## #   `專科-女/男` <chr>, `大學-薪資` <dbl>, `大學-女/男` <chr>,
    ## #   `研究所及以上-薪資` <dbl>, `研究所及以上-女/男` <chr>, percent <dbl>

``` r
#既然要看最划算的職業，那我們來看no.1礦業及土石採取業-事務支援人員
#percent:1.208946 研究所學歷薪資大概多了20%
#我們再直接薪水   研究所以上:30000  大學:24815
#整整多了5185元
#所以想讀研究所，可以考慮讀這個職業相關科系
```

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

``` r
#這是R Code Chunk
library(readr)
library(dplyr)

X106edu <- read_csv("C:/Users/user/Desktop/RHW/106edu.csv", 
                    locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
want<-c(127,128,80,121)
#上面這是我有興趣的職業，在的列數

X106edu$大職業別[want]
```

    ## [1] "藝術_娛樂及休閒服務業"                
    ## [2] "藝術_娛樂及休閒服務業-專業人員"       
    ## [3] "資訊及通訊傳播業-技術員及助理專業人員"
    ## [4] "醫療保健服務業-專業人員"

``` r
#我直接列出來
#[1] "藝術_娛樂及休閒服務業"                 "藝術_娛樂及休閒服務業-專業人員"       
#[3] "資訊及通訊傳播業-技術員及助理專業人員" "醫療保健服務業-專業人員"  
X106edu$`大學-薪資`[want]
```

    ## [1] "26614" "30071" "28902" "34733"

``` r
#"26614" "30071" "28902" "34733"
X106edu$`研究所及以上-薪資`[want]
```

    ## [1] "30160" "31711" "32354" "38153"

``` r
#"30160" "31711" "32354" "38153"

#以上的薪資和我想的落差蠻大，沒有一個有辦法到4萬QQ
#看完覺得失落，比想像中低好多
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
#這是R Code Chunk
X106edu$`大學-薪資`[want]
```

    ## [1] "26614" "30071" "28902" "34733"

``` r
#"26614" "30071" "28902" "34733"
X106edu$`研究所及以上-薪資`[want]
```

    ## [1] "30160" "31711" "32354" "38153"

``` r
#"30160" "31711" "32354" "38153"

#分別差了
# 3546    1640    3452    3420
#雖然的確薪水都有差
#但我還是不會去讀研究所
#第一點，只多了幾千，卻要花兩年
#而且這兩年不但要算上時間成本、沒有進帳，反而還要支出
#第二點，如果我大學畢業就去工作，這兩年搞不好我提昇薪水，還比研究所生剛畢業高
#綜合以上兩點，我還是不會讀研究所
```
