rm(list=ls())
# setwd("/Users/hhi_a507249/Rcode/por explore/")

library(dplyr)
library(ggplot2)
library(plot3D)
# library(rgl)
library(dummies)
library(arules)

par(family="NanumGothic")
theme_update(text=element_text(family="NanumGothic"))

## pairs plot...................
d3 %>%
  select(info_int$name) %>% 
  pairs(cex.axis=2, cex.labels=2)

colnames(d3)

td<-d3 %>%
  select(POR생성일, 호선번호, 자재내역) %>%
  group_by(호선번호, POR생성일)

td<-as.data.frame(td)
head(td, 100)


## 2. 연관성 분석
# POR생성일
# 호선번호
# 자재내역

dum<-dummy(d3$자재내역)
colnames(dum)<-gsub("자재내역", "", colnames(dum))

d4<-data.frame("POR생성일"=d3$POR생성일, "호선번호"=d3$호선번호, dum)
# colnames(d4)

d5<- d4 %>%
  group_by_if(is.factor) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE)

d6<-d5[,-c(1,2)]
d6<-as.matrix(d6)
d6[d6>1]<-1

dim(d6)

d7<-d6[-which(apply(d6,1,sum)==1),]
dim(d7)
rownames(d7)<-1:dim(d7)[1]

tran <- as(d7, "transactions")

# association rule analysis
asso<-apriori(tran, parameter = list(supp = 0.05, conf = 0.05, target = "rule"))
summary(asso)
inspect(asso)



## 3. 업체간 유사도 파악 
# 업체명
# 자재내역


dum<-dummy(d3$자재내역)
dd<-data.frame("업체명"=d3$업체명, dum)
# dd[1:10, 1:4]
dd1<-rowsum(dd[,-1], dd[,1])
# dim(dd1)
# dd1[1:10, 1:4]
# dummy화 확인 
# name<-"(주)윈윈"
# v<-dd1[which(rownames(dd1)==name),]
# dd[which(dd[,1]==name),(which(as.numeric(v)>1)+1)]

distdd1<-dist(dd1)
fit<-cmdscale(distdd1, eig=T, k=3)
fit
plot(fit$points[,1],fit$points[,2])
plot3d(fit$points[,1],fit$points[,2],fit$points[,3])



## 4. 선급 정보 시각화
# 선급1
# 선급2

d3 %>%
  group_by(선급1) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(x=선급1, y=n)) + geom_bar(stat="identity")+
  labs(title="선급1 COUNT",x="선급1", y = "Count")

d3 %>%
  group_by(선급2) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(x=선급2, y=n)) + geom_bar(stat="identity")+
  labs(title="선급2 COUNT",x="선급2", y = "Count")


## 5. 업체별 주문 처리 능력 파악 plot 
# 업체명
# 구매오더.발주일
# 최종입고일

d4<-data.frame(d3, diff=as.numeric(as.Date(as.character(d3$최종입고일), "%Y.%m.%d")-as.Date(as.character(d3$구매오더.발주일), "%Y.%m.%d")))
g_date<-d4 %>%
  group_by(업체명) %>%
  summarise(mean=mean(diff),
            sum=sum(diff))
head(g_date)
g_date<-as.data.frame(g_date)

# theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
# theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경

ggplot(data=g_date, aes(x=g_date[,1], y=g_date[,2], group=1)) +
  geom_point(size=1, col="red")+
  geom_line()+
  labs(title="업체별 발주일-입고일 기간",x="업체명", y = "구매오더 발주일-최종입고일") + 
  theme(axis.title.x = theme.ax, axis.title.y = theme.ax, plot.title = theme.ti)

g_date %>%
  filter(mean>75) 


## 6. 전체 호선에 대한 발주 간격 및 분포 파악
# 호선번호
# 구매오더.발주일

d4<-d3 %>%
  select(호선번호, 구매오더.발주일)
head(d4)
# d3[c(21,61),]
i<-7
n1<-length(levels(d4$호선번호))
glist<-list()
result<-NULL
for(i in 1:n1) {
  n2<-which(d4[,1]==levels(d4$호선번호)[i])
  if(length(n2) > 1){
    glist[[i]]<-d4[n2,]
    print(nrow(glist[[i]]))
    collect<-as.numeric(diff(sort(as.Date(as.character(glist[[i]][,2]), "%Y.%m.%d"))))
    names(collect)<-rep(as.character(glist[[i]][1,1]), len=length(collect))
    result<-append(result, collect)
  }
}
plot(density(result[which(result!=0)]))
plot(density(result))

head(result, 12)

which(result==83)
result[2]

i<-0
j<-7
n<-0
count<-NULL
temp<-result
while(n<max(result)){
  if(i==0){
    t1<-which(temp<=0)
    count<-append(count, length(temp[t1]))
    temp<-temp[-t1]
    i<-i+1
  }else{
    n<-i*j
    t1<-which(temp<=n)
    # if(length(which(t1==2))>0) break;
    count<-append(count, length(temp[t1]))
    if(length(t1)>0){
      temp<-temp[-t1]
    }
    i<-i+1
    print(n)
  }
}
period<-NULL
for(i in 0:(length(count)-1)) period<-append(period, paste0(i,"주"))

gresult<-data.frame(period=period, count=count)

ggplot(data=gresult[-1,], aes(x=period, y=count)) +
  geom_bar(stat="identity", color="blue", fill="blue")+
  labs(title="업체별 발주간격 그룹화(주 단위)",x="주(week)", y = "Count")

##################################################################################
#################      기타 연습 코딩....             ############################
##################################################################################
## 구매접수일-요청수량
g_date<-d3%>%
  group_by(구매.접수일) %>%
  summarise(요청수량=sum(요청수량))
g_date<-as.data.frame(g_date)
g_date[,1]<-as.Date(as.character(g_date[,1]), "%Y.%m.%d")
theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경

ggplot(data=g_date, aes(x=g_date[,1], y=g_date[,2], group=1)) +
  geom_point(size=1, col="red")+
  geom_line()+
  labs(title="Plot of Por",x="구매 접수일", y = "요청수량") + 
  theme(axis.title.x = theme.ax, axis.title.y = theme.ax, plot.title = theme.ti)

## 구매접수일-출고일
a1<-d3[,which(colnames(d3)=="구매.접수일")]
a1<-as.character(a1)
a1<-as.Date(a1, "%Y.%m.%d")
a1
a2<-d3[,which(colnames(d3)=="출고.불출.일")]
a2<-as.character(a2)
a2<-as.Date(a2, "%Y.%m.%d")

n<-length(a1)
diff<-rep(0, len=n)
for(i in 1:n) diff[i]<-as.integer(a2[i]-a1[i])

## 구매접수일-출고일 plot
ggplot() + geom_line(aes(x=seq_along(diff), y=diff))+
  labs(title="Plot of Por",x="index", y = "구매접수일-출고일") + 
  theme(axis.title.x = theme.ax, axis.title.y = theme.ax, plot.title = theme.ti)

d4<-cbind(d3, diff)

g_date<-d3%>%
  group_by(최종입고일) %>%
  summarise(입고수량.누적.=sum(입고수량.누적.))
g_date<-as.data.frame(g_date)

ggplot(d4, aes(프로젝트명, diff))+geom_bar(stat="identity")  ## 몇가지 비이상적 기간 발생

## 비용 관련 정보
p1<-d3[,which(colnames(d3)=="구매금액")]
ggplot() + geom_line(aes(x=seq_along(p1), y=p1))+
  labs(title="Plot of Por",x="index", y = "구매금액") + 
  theme(axis.title.x = theme.ax, axis.title.y = theme.ax, plot.title = theme.ti)


p2<-d3[,which(colnames(d3)=="제품타입")]
p3<-d3[,which(colnames(d3)=="구매담당")]
p4<-d3[,which(colnames(d3)=="납품예정정보")]

boxplot(p1~p2)
boxplot(p1~p3)
boxplot(p1~p4)