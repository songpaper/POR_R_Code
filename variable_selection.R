rm(list=ls())
# install.packages("readr") install.packages("stringi")
# library(stringi, readr)
# setwd("/Users/hhi_a507249/Desktop/업무/과거 소요자재 POR 관련 데이터 수집 및 분석/")
# guess_encoding("por_data.csv")

setwd("/Users/hhi_a507249/Rcode/por explore/")
library(dplyr)
library(ggplot2)
library(reshape2)

par(family="NanumGothic")

csv<-"/Users/hhi_a507249/Desktop/업무/과거 소요자재 POR 관련 데이터 수집 및 분석/data/por_data.csv"
d1<-read.csv(csv)
d1<-as.data.frame(d1)
colnames(d1)
str(d1)

###############################################################

## variable select(remove null)
na<-apply(d1, 2, function(x) sum(is.na(x)))
d2<-d1%>%
  select(names(na)[which(na<1600)])
#colnames(d1[,which(na>1600)])
dim(d2); dim(d1)

## factor type 인것 같은 변수 형변환 
change1<-c("구매요청번호", "품목번호", "공급업체", "플랜트", "저장위치", "품목번호.1",
           "예산통제코드", "예약행번", "영업문서", "영업문서행번", "예약번호")
change2<-"환율"
for(i in 1:length(change1)) d2[,which(colnames(d2)==change1[i])]<-as.factor(d2[,which(colnames(d2)==change1[i])])
for(i in 1:length(change2)) d2[,which(colnames(d2)==change2[i])]<-as.numeric(d2[,which(colnames(d2)==change2[i])])


## type
type<-sapply(d2,class)
type

## factor levels 개수
n=dim(d2)[2]
levels<-rep(0, len=n)
for(i in 1:n) levels[i]<-length(levels(d2[,i]))
levels
## null 개수
nul<-apply(d2, 2, function(x) sum(x==""))
nul
## 분산값
var<-apply(d2, 2, var)
var

## info table 
name<-colnames(d2)
info<-data.frame(name=name , levels=levels, nul=nul, type=type, var=var)
info[,which(colnames(info)=="name")]<-as.character(info$name)
## type(integer, factor)별 변수 현황 파악 + 분산0 & 요소수 적은 데이터 삭제
info_int<-info %>%
  filter(type=="integer" | type=="numeric" ) %>%
  filter(var!=0)

info_fac<-info %>%
  filter(nul<1600) %>%
  filter(type=="factor" & levels > 4)
which(nul>1600)
info_int
info_fac

info_int$name
info_fac$name

### 위의 과정을 통해서 필터링 된 데이터셋 d3 생성 
d3<-d2 %>%
  select(c(info_int$name, info_fac$name)) 
dim(d3)
colnames(d3)

save.image()


