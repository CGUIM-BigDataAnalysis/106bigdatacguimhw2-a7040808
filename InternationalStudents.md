106-2 大數據分析方法 作業二
================
陳威廷

作業完整說明[連結](https://docs.google.com/document/d/1aLGSsGXhgOVgwzSg9JdaNz2qGPQJSoupDAQownkGf_I/edit?usp=sharing)

學習再也不限定在自己出生的國家，台灣每年有許多學生選擇就讀國外的大專院校，同時也有人多國外的學生來台灣就讀，透過分析[大專校院境外學生人數統計](https://data.gov.tw/dataset/6289)、[大專校院本國學生出國進修交流數](https://data.gov.tw/dataset/24730)、[世界各主要國家之我國留學生人數統計表](https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv)可以了解103年以後各大專院校國際交流的情形。請同學分析以下議題，並以視覺化的方式呈現分析結果，呈現103年以後大專院校國際交流的情形。

來台境外生分析
--------------

### 資料匯入與處理

``` r
library(readr)
library(knitr)
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(tidyr)
Statisticscon103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
Statisticscon104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
Statisticscon105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
Statisticscon106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
Statisticscol103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
Statisticscol104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
Statisticscol105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
Statisticscol106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
ComPop<-read_csv("Student_RPT_07.csv")
IS<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
IS<-IS[,1:3]
ComPop<-ComPop[3:35031,c(1,2,6,9,10,12:15)]
names(ComPop)<-c("學年度","學期","學校名稱","學制","國別","英文名稱","小計","男","女")
```

### 哪些國家來台灣唸書的學生最多呢？

``` r
Q1_103<-data.frame(Statisticscon103$國別,rowSums(Statisticscon103[3:11]))
names(Q1_103)<-c("國別","total")
Q1_103$total<-as.numeric(Q1_103$total)
Q1_104<-data.frame(Statisticscon104$國別,rowSums(Statisticscon104[3:11]))
names(Q1_104)<-c("國別","total")
Q1_104$total<-as.numeric(Q1_104$total)
Q1_105<-data.frame(Statisticscon105$國別,rowSums(Statisticscon105[3:11]))
names(Q1_105)<-c("國別","total")
Q1_105$total<-as.numeric(Q1_105$total)
Q1_106<-data.frame(Statisticscon106$國別,rowSums(Statisticscon106[3:11]))
names(Q1_106)<-c("國別","total")
Q1_106$total<-as.numeric(Q1_106$total)
Q1<-full_join(Q1_103,Q1_104,by ="國別")
Q1<-full_join(Q1,Q1_105,by ="國別")
Q1<-full_join(Q1,Q1_106,by ="國別")
Q1[is.na(Q1)]<-0
Q1<-data.frame(Q1$國別,rowSums(Q1[,2:5]))
names(Q1)<-c("國別","total")
kable(head(Q1[order(Q1$total,decreasing = TRUE),],10))
```

|     | 國別     |   total|
|-----|:---------|-------:|
| 1   | 中國大陸 |  152524|
| 2   | 馬來西亞 |   62031|
| 3   | 香港     |   31940|
| 4   | 日本     |   28200|
| 6   | 越南     |   21670|
| 5   | 澳門     |   20302|
| 8   | 印尼     |   19620|
| 7   | 南韓     |   16948|
| 121 | 美國     |   14846|
| 9   | 泰國     |    7035|

### 哪間大學的境外生最多呢？

``` r
Statisticscol103<-Statisticscol103[,-10]
Q2_103<-data.frame(Statisticscol103$學校名稱,rowSums(Statisticscol103[4:11]))
names(Q2_103)<-c("學校名稱","total")
Statisticscol104<-Statisticscol104[,-10]
Q2_104<-data.frame(Statisticscol104$學校名稱,rowSums(Statisticscol104[4:11]))
names(Q2_104)<-c("學校名稱","total")
Statisticscol105<-Statisticscol105[,-10]
Q2_105<-data.frame(Statisticscol105$學校名稱,rowSums(Statisticscol105[4:11]))
names(Q2_105)<-c("學校名稱","total")
Statisticscol106<-Statisticscol106[,-10]
Q2_106<-data.frame(Statisticscol106$學校名稱,rowSums(Statisticscol106[4:11]))
names(Q2_106)<-c("學校名稱","total")
Q2<-full_join(Q2_103,Q2_104,by ="學校名稱")
Q2<-full_join(Q2,Q2_105,by ="學校名稱")
Q2<-full_join(Q2,Q2_106,by ="學校名稱")
Q2[is.na(Q2)]<-0
Q2<-data.frame(Q2$學校名稱,rowSums(Q2[,2:5]))
names(Q2)<-c("學校名稱","total")
kable(head(Q2[order(Q2$total,decreasing = TRUE),],10))
```

|     | 學校名稱         |  total|
|-----|:-----------------|------:|
| 4   | 國立臺灣師範大學 |  22113|
| 3   | 國立臺灣大學     |  18199|
| 55  | 中國文化大學     |  16074|
| 65  | 銘傳大學         |  16057|
| 54  | 淡江大學         |  13887|
| 1   | 國立政治大學     |  11626|
| 6   | 國立成功大學     |  10982|
| 51  | 輔仁大學         |   9499|
| 56  | 逢甲大學         |   9474|
| 53  | 中原大學         |   7662|

### 各個國家來台灣唸書的學生人數條狀圖

``` r
others<-c("其他",sum(filter(Q1,total<3000)$total))
Q1$國別<-as.character(Q1$國別)
Q3<-rbind(filter(Q1,total>=3000),others)
Q3$total<-as.numeric(Q3$total)
Q3<-Q3[order(Q3$total,decreasing = TRUE),]
ggplot(data=Q3)+
  geom_bar(aes(x=國別,y=total),stat="identity")
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryBar-1.png)

### 各個國家來台灣唸書的學生人數面量圖

``` r
code<-read_csv("CODE.csv",locale = locale(encoding = "BIG5"))
code<-code[,c(1,4)]
names(code)<-c("國別","ISO")
Q4<-left_join(Q1,code,by="國別")
Q4<-Q4[order(Q4$ISO),]
row.names(Q4)<-c(1:179)
Q4$ISO[162:179]<-c("CHN","KOR","UAR","AUS","MHL","COD","SSD","COM","GHA","SRB","VCT","KNA","FSM","SLE","SOM","XKS","TTO","SML")
data("country.map")
country.map<-country.map[,c(1,2,6,47)]
names(country.map)<-c("long","lat","Group","ISO")
Q4<-left_join(country.map,Q4,by="ISO")
Q4[is.na(Q4)]<-0
Q4map<-ggplot() +
  geom_polygon(data = Q4, 
               aes(x = long, y = lat, 
                   group = Group, 
                   fill = total), 
               color = "black", 
               size = 0.25,
               na.rm = T) + 
  coord_cartesian(xlim = c(-180, 180) ,ylim = c(-90, 90))+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(7,"Reds"))+
  theme_void()+
  labs(fill="單位(人)",title = "各國來台留學生面量圖")
Q4map
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryMap-1.png)

台灣學生國際交流分析
--------------------

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？

``` r
Q5<-ComPop
Q5$小計<-as.numeric(Q5$小計)
a<-group_by(Q5,`國別`) %>% 
   summarise(total=sum(小計))
Q5<-full_join(a,code,by="國別")
Q5<-Q5[order(Q5$total,decreasing = T),]
Q5[grep("[A-Z]{3}",Q5$ISO,invert=T),]
```

    ## # A tibble: 75 x 3
    ##    國別              total ISO  
    ##    <chr>             <dbl> <chr>
    ##  1 中國大陸         10429. <NA> 
    ##  2 大陸地區          5996. <NA> 
    ##  3 南韓              2498. <NA> 
    ##  4 大韓民國(南韓)    2131. <NA> 
    ##  5 德意志聯邦共和國  1458. <NA> 
    ##  6 澳大利亞           926. <NA> 
    ##  7 泰王國(泰國)       567. <NA> 
    ##  8 新加坡共和國       479. <NA> 
    ##  9 西班牙王國         478. <NA> 
    ## 10 荷蘭王國           349. <NA> 
    ## # ... with 65 more rows

``` r
Q5$ISO[grep("[A-Z]{3}",Q5$ISO,invert=T)]<-c("CHN","CHN","KOR","KOR","DEU","AUS","THA","SGP","ESP","NLD",
                                            "CZE","AUT","RUS","SWE","PHL","BEL","IDN","VNM","FIN","ITA")
Q5<-group_by(Q5,ISO) %>% summarise(total = sum(total,na.rm=T))
kable(head(Q5[order(Q5$total,decreasing = TRUE),],10))
```

| ISO |  total|
|:----|------:|
| CHN |  16621|
| JPN |  12430|
| USA |   8916|
| KOR |   4771|
| DEU |   3211|
| FRA |   2415|
| FXX |   2415|
| GBR |   1416|
| ESP |   1282|
| SGP |   1188|

### 哪間大學的出國交流學生數最多呢？

``` r
ComPop$小計<-as.numeric(ComPop$小計)
Q6<-group_by(ComPop,學校名稱) %>% 
  summarise(total=sum(小計))
kable(head(Q6[order(Q6$total,decreasing = TRUE),],10))
```

| 學校名稱     |  total|
|:-------------|------:|
| 國立臺灣大學 |   4719|
| 淡江大學     |   3794|
| 國立政治大學 |   3479|
| 逢甲大學     |   2646|
| 東海大學     |   1881|
| 元智大學     |   1864|
| 國立交通大學 |   1513|
| 東吳大學     |   1457|
| 國立成功大學 |   1397|
| 國立臺北大學 |   1397|

### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖

``` r
others2<-c("其他",sum(filter(Q5,total<200)$total))
Q6b<-rbind(filter(Q5,total>=200),others2)
Q6b$total<-as.numeric(Q6b$total)
ggplot(data=Q6b)+
  geom_bar(aes(x=ISO,y=total),stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryBar-1.png)

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖

``` r
Q7<-left_join(country.map,Q5,by="ISO")
Q7[is.na(Q7)]<-0
Q7map<-ggplot() +
  geom_polygon(data = Q7, 
               aes(x = long, y = lat, 
                   group = Group, 
                   fill = total), 
               color = "black", 
               size = 0.25,
               na.rm = T) + 
  coord_cartesian(xlim = c(-180, 180) ,ylim = c(-90, 90))+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(fill="單位(人)",title = "台灣學生進修交流國家")
Q7map
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryMap-1.png)

台灣學生出國留學分析
--------------------

### 台灣學生最喜歡去哪些國家留學呢？

``` r
kable(head(IS[order(IS$總人數,decreasing = TRUE),],10))
```

| 洲別   | 國別     | 總人數 |
|:-------|:---------|:------:|
| 美洲   | 美國     |  21127 |
| 大洋洲 | 澳大利亞 |  13582 |
| 亞洲   | 日本     |  8444  |
| 美洲   | 加拿大   |  4827  |
| 歐洲   | 英國     |  3815  |
| 歐洲   | 德國     |  1488  |
| 大洋洲 | 紐西蘭   |  1106  |
| 歐洲   | 波蘭     |   561  |
| 亞洲   | 馬來西亞 |   502  |
| 歐洲   | 奧地利   |   419  |

### 台灣學生最喜歡去哪些國家留學面量圖

``` r
Q8<-left_join(IS,code,by="國別")
Q8<-left_join(country.map,Q8,by="ISO")
Q8[is.na(Q8)]<-0
Q8map<-ggplot() +
  geom_polygon(data = Q8, 
               aes(x = long, y = lat, 
                   group = Group, 
                   fill = 總人數), 
               color = "black", 
               size = 0.25,
               na.rm = T) + 
  coord_cartesian(xlim = c(-180, 180) ,ylim = c(-90, 90))+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(7,"Reds"))+
  theme_void()+
  labs(fill="單位(人)",title = "各國來台留學生面量圖")
Q8map
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAbMap-1.png)

綜合分析
--------

請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。

``` r
Q1_105a<-data.frame(Statisticscon105$國別,rowSums(Statisticscon105[3:5]))
names(Q1_105a)<-c("國別","total")
Q1_105a$total<-as.numeric(Q1_105a$total)
Q9a<-Q1_105a[order(Q1_105a$total,decreasing = TRUE),]
names(Q9a)<-c("國別","外國人來台")
Q9a$國別<-as.character(Q9a$國別)
Q9a[9,1]<-"韓國"
Q9b<-IS[order(IS$總人數,decreasing = TRUE),]
Q9b<-Q9b[,-1]
names(Q9b)<-c("國別","台灣人出國")
Q9<-left_join(Q9b,Q9a,by="國別")
Q9$國別 <- factor(Q9$國別, levels = Q9$國別[order(desc(Q9$台灣人出國))])
Q9<-gather(Q9,
           key=Type,value=total,台灣人出國,外國人來台)
ggplot(data=Q9)+
  geom_bar(aes(x=國別,y=total,fill=Type),stat="identity",position=position_dodge(width = 0.8))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
```

![](InternationalStudents_files/figure-markdown_github/unnamed-chunk-1-1.png)

此圖分別展示出了台灣人出國留學以及外國人來台之情況，由左至右以台灣人出國的人數作為排序。由圖中資訊可知，台灣人主要留學的國家為歐美地區，而來台的外國人主要為東南亞以及東北亞，由此看出趨勢並不相同。
