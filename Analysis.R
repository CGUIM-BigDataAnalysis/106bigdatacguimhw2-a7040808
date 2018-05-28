#資料匯入與處理
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
ComPop<-ComPop[3:35022,c(1,2,6,9,10,12:15)]
names(ComPop)<-c("學年度","學期","學校名稱","學制","國別","英文名稱","小計","男","女")

#哪些國家來台灣唸書的學生最多呢？
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

### 哪間大學的境外生最多呢？
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

### 各個國家來台灣唸書的學生人數條狀圖
others<-c("其他",sum(filter(Q1,total<3000)$total))
Q1$國別<-as.character(Q1$國別)
Q3<-rbind(filter(Q1,total>=3000),others)
Q3$total<-as.numeric(Q3$total)
Q3<-Q3[order(Q3$total,decreasing = TRUE),]
ggplot(data=Q3)+
  geom_bar(aes(x=國別,y=total),stat="identity")

### 各個國家來台灣唸書的學生人數面量圖
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

## 台灣學生國際交流分析

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？
Q5<-ComPop
Q5$小計<-as.numeric(Q5$小計)
Q5$國別<-gsub("中國大陸|大陸地區","中國",Q5$國別)
Q5$國別[grepl("大韓民國",Q5$國別)]<-"韓國"
Q5$國別<-gsub("南韓","韓國",Q5$國別)
Q5$國別<-gsub("澳大利亞","澳洲",Q5$國別)
Q5$國別<-gsub("德意志聯邦共和國","德國",Q5$國別)
Q5$國別<-gsub("甘比亞共和國","甘比亞",Q5$國別)
Q5$國別<-gsub("菲律賓共和國","菲律賓",Q5$國別)
Q5$國別<-gsub("印度尼西亞共和國","印尼",Q5$國別)
Q5$國別<-gsub("荷蘭王國","荷蘭",Q5$國別)
Q5$國別<-gsub("捷克共和國","捷克",Q5$國別)
Q5$國別[grepl("泰王國",Q5$國別)]<-"泰國"
Q5$國別<-gsub("俄羅斯聯邦","俄羅斯",Q5$國別)
Q5$國別<-gsub("西班牙王國","西班牙",Q5$國別)
Q5$國別<-gsub("新加坡共和國","新加坡",Q5$國別)
Q5$國別<-gsub("奧地利共和國","奧地利",Q5$國別)
Q5$國別<-gsub("瑞典王國","瑞典",Q5$國別)
a<-group_by(Q5,`國別`) %>% 
   summarise(total=sum(小計))

kable(head(Q5[order(Q5$total,decreasing = TRUE),],10))

### 哪間大學的出國交流學生數最多呢？
Q6<-group_by(Q5,學校名稱) %>% 
  summarise(total=sum(小計))
kable(head(Q6[order(Q6$total,decreasing = TRUE),],10))


### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖
ggplot(data=a)+
  geom_bar(aes(x=國別,y=total),stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖
Q7<-left_join(a,code,by="國別")
Q7<-left_join(country.map,Q7,by="ISO")
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

### 台灣學生最喜歡去哪些國家留學呢？
kable(head(IS[order(IS$總人數,decreasing = TRUE),],10))

### 台灣學生最喜歡去哪些國家留學面量圖
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

## 綜合分析
Q9a<-Q1_105[order(Q1_105$total,decreasing = TRUE),]
names(Q9a)<-c("國別","外國人來台")
Q9a$國別<-as.character(Q9a$國別)
Q9a[8,1]<-"韓國"
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
