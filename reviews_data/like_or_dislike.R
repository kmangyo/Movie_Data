library(reshape2)
library(ggplot2)
library(MASS)

#데이터는 다음 영화평 데이터를 수집
#수집 방법은 아래 코드 참고
#https://github.com/kmangyo/Movie_Data/blob/master/reviews_data/ReviewDataScraping.R 

#일반적 영화 평점 형태
with(movie.sales_open.review.all,hist(star.y))
with(subset(movie.sales_open.review.all, name==c('암살')),hist(star.y))
with(subset(movie.sales_open.review.all, name==c('내부자들')),hist(star.y))

#호불호 갈리는 영화는 있는가? 아래 영화는 평점이 극단으로 나누어짐
with(subset(movie.sales_open.review.all, name==c('연평해전')),hist(star.y))
with(subset(movie.sales_open.review.all, name==c('쎄시봉')),hist(star.y))

#어떻게 찾아낼까?
#beta distribution parameter estimation 사용
#예시, beta distribution은 유연하게 분포의 형태를 표현할 수 있음. 극단으로 나누어지는 별점 분포도 추정 가능할 것
beta.a<-data.frame(value=rbeta(1000, .5,.5),name=c('shape1=0.5, shape2=0.5'))
beta.b<-data.frame(value=rbeta(1000, .5,1),name=c('shape1=0.5, shape2=1'))
beta.c<-data.frame(value=rbeta(1000, 1,.5),name=c('shape1=1, shape2=0.5'))
beta.d<-data.frame(value=rbeta(1000, 1,1),name=c('shape1=1, shape2=1'))
beta.e<-data.frame(value=rbeta(1000, 5,5),name=c('shape1=5, shape2=5'))
beta.df<-rbind(beta.a, beta.b, beta.c, beta.d, beta.e)
ggplot(beta.df, aes(x = value)) +geom_histogram(binwidth = .05)+ facet_wrap(~ name)

#어떤 영화일까?
#평점이 100개 이상인 영화만 분석에 활용
#10점은 9점으로 0점은 1점에 포함. 기존 평점에 /10 적용 (0~1사이의 값으로 추정해야 하기 때문)
likedislike<-subset(movie.sales_open.review.all,numberofstar>=100)
likedislike$star.sub<-with(likedislike, ifelse(star.y==10,9,ifelse(star.y==0,1,star.y)))
likedislike_split<-split(likedislike, likedislike$name)

beta<-list()

for(i in 1:length(likedislike_split)) {
  star<-likedislike_split[i]
  star<-data.frame(star)
  star<-star[42]/10
  beta[i]<-fitdistr(star[1:nrow(star),1], dbeta, list(shape1=1.63,shape2=0.85))
}

#list형으로 저장된 값을 data.frame으로 변경
beta.df <- data.frame(matrix(unlist(beta), nrow=length(beta), byrow=T))
names(beta.df)<-c('shape1','shape2')
beta.df$no<-1
beta.df$no<-cumsum(beta.df$no)

#shape1과2가 1미만인 영화만 확인
like_dislike_subset<-subset(beta.df, shape1<1&shape2<1)
like_dislike_subset$no

for(i in like_dislike_subset$no){
  print(names(likedislike_split[i]))
}

#위에서 분류된 영화의 분포를 확인
like.dislike.name<-c("나의절친악당들","도리화가","드래곤블레이드","신은죽지않았다","쎄시봉", "어우동:주인없는꽃", "연평해전", "워킹걸", "위험한상견례2")
like.dislike.movie<-subset(movie.sales_open.review.all, name %in% like.dislike.name)

#히스토그램과 density plot으로 보기
ggplot(like.dislike.movie, aes(x = star.y)) +geom_histogram(binwidth = 1)+ facet_wrap(~ name)+xlab("평점")
ggplot(like.dislike.movie, aes(x = star.y)) +geom_density()+ facet_wrap(~ name)+xlab("평점")

