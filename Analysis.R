#資料匯入與處理
library(readr)
library(knitr)
library(ggplot2)
library(dplyr)
Statisticscon103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
Statisticscon104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
Statisticscon105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
Statisticscon106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
Statisticscol103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
Statisticscol104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
Statisticscol105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
Statisticscol106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
ComPop<-read_csv("Student_RPT_07.csv")
IS<-read.csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
ComPop<-ComPop[3:35031,c(1,2,6,10,12:15)]
names(ComPop)<-c("學年度","學期","學校名稱","國別","英文名稱","小計","男","女")

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
others<-c("其他",sum(filter(Q1,total<1000)$total))
Q1$國別<-as.character(Q1$國別)
Q3<-rbind(filter(Q1,total>=3000),others)
Q3$total<-as.numeric(Q3$total)
Q3<-Q3[order(Q3$total,decreasing = TRUE),]
ggplot(data=Q3)+
  geom_bar(aes(x=國別,y=total),stat="identity")
