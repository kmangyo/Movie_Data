# MS cognitive service computer vision api 

library(rvest)
library(stringr)
library(rjson)
library(httr)
library(dplyr)
library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))

key <-'XXXX'
visionURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceAttributes=age,gender,smile"
imgae<-list()
con<-list()

# memory error
for(i in 1:nrow(movie.name.image.spec_500)) {
  image[i]<-movie.name.image.spec_500[i,6]
  mybody = list(url = image[i])
  visionResponse = POST(
    url = visionURL, 
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
    body = mybody,
    encode = 'json')
  con[[i]] <- content(visionResponse)
  if(!is.null(con[[i]]$error$code)){
    Sys.sleep(60)
    image[i]<-movie.name.image.spec_500[i,6]
    mybody = list(url = image[i])
    visionResponse = POST(
      url = visionURL, 
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
      body = mybody,
      encode = 'json')
    con[[i]] <- content(visionResponse)
  }
}

face<-list()
for (i in 1:nrow(movie.name.image.spec_500)) {
  if (is.null(unlist(con[[i]]))){
    face[[i]]<-'na'
  } else {
    face[[i]]<-data.frame(t(unlist(con[[i]][[1]]$faceAttributes)))
  }
}

face<-melt(face)

table(face$gender)
hist(as.numeric(as.character(face$age)))

ggplot(face, aes(x = as.numeric(as.character(age)))) + geom_histogram(binwidth = 1) + facet_wrap(~ gender)

movie.name.image.face<-cbind(movie.name.image.spec_500[1:2644,1:ncol(movie.name.image.spec_500)], face)
movie.name.image.face <- movie.name.image.face[complete.cases(movie.name.image.face$gender), ]
movie.name.image.face <- movie.name.image.face[complete.cases(movie.name.image.face$role), ]
movie.name.image.face <- subset(movie.name.image.face, rank<=295)

table(movie.name.image.face$gender)
table(movie.name.image.face$gender)/sum(table(movie.name.image.face$gender))

table(movie.name.image.face$gender, movie.name.image.face$role)
table(movie.name.image.face$gender, movie.name.image.face$role)/sum(table(movie.name.image.face$gender, movie.name.image.face$role))

ggplot(movie.name.image.face, aes(x = as.numeric(as.character(age)))) + geom_histogram(binwidth = 1) + facet_wrap(~ gender+role) + xlab("Age")
ggplot(movie.name.image.face, aes(x = as.numeric(as.character(age)))) + geom_histogram(binwidth = 1) + facet_wrap(~ gender) + xlab("Age")
ggplot(movie.name.image.face, aes(x = as.numeric(as.character(age)))) + geom_density() + facet_wrap(~ gender) + xlab("Age")
ggplot(movie.name.image.face, aes(x = as.numeric(as.character(age)))) + geom_density() + facet_wrap(~ gender+role) + xlab("Age")

library(BayesFactor)
movie.name.image.face$age<-as.numeric(as.character(movie.name.image.face$age))
bf<-anovaBF(age ~ gender*role, data=movie.name.image.face)
plot(bf)
plot(bf[3:4] / bf[2])

movie.name.image.face.genre<- strsplit(as.character(movie.name.image.face$genre),',')
genre.list<-list()

for (i in 1:length(movie.name.image.face.genre)){
  genre.list[i]<-movie.name.image.face.genre[[i]][1]
}

movie.name.image.face<-cbind(movie.name.image.face, data.frame(genre.list=unlist(genre.list)))
qplot(factor(genre.list, levels = movie.name.image.face.genre.df.sum$genre.list), data=movie.name.image.face, geom="bar", fill=factor(gender)) + xlab('Genre') + scale_fill_discrete(name="Group")
ggplot(movie.name.image.face, aes(x = factor(genre.list, levels = movie.name.image.face.genre.df.sum$genre.list), fill = as.factor(gender))) + geom_bar(position='dodge') + xlab('Genre') + scale_fill_discrete(name="Group")

movie.name.image.face.genre.df<- movie.name.image.face %>% group_by(genre.list,gender) %>% summarise(n=n())
movie.name.image.face.genre.df.sum<-movie.name.image.face %>% group_by(genre.list) %>% summarise(n=n())
movie.name.image.face.genre.df.sum <- movie.name.image.face.genre.df.sum %>% arrange(-n)
movie.name.image.face.genre.df<-merge(movie.name.image.face.genre.df, movie.name.image.face.genre.df.sum, c('genre.list'),all.x=T)
movie.name.image.face.genre.df$ratio<-with(movie.name.image.face.genre.df, n.x/n.y)
ggplot(movie.name.image.face.genre.df,aes(x = factor(genre.list, levels = movie.name.image.face.genre.df.sum$genre.list), y = ratio, fill = as.factor(gender))) + geom_bar(stat = "identity") + xlab('Genre') + scale_fill_discrete(name="Group")

movie.name.image.face.country<- strsplit(as.character(movie.name.image.face$country),'\r\n\t\t\t\r\n\t\t\t')
country.list<-list()

for (i in 1:length(movie.name.image.face.country)){
  country.list[i]<-movie.name.image.face.country[[i]][1]
}

country.list<-unlist(country.list)
country.list<-gsub(",| ", "", country.list)

movie.name.image.face<-cbind(movie.name.image.face, data.frame(country.list=country.list))
qplot(factor(country.list, levels = movie.name.image.face.country.df.sum$country.list ), data=movie.name.image.face, geom="bar", fill=factor(gender)) + xlab('Country') + scale_fill_discrete(name="Group")

ggplot(movie.name.image.face, aes(x = factor(country.list, levels = movie.name.image.face.country.df.sum$country.list), fill = as.factor(gender))) + geom_bar(position='dodge') + xlab('Country') + scale_fill_discrete(name="Group")

movie.name.image.face.country.df<- movie.name.image.face %>% group_by(country.list,gender) %>% summarise(n=n())
movie.name.image.face.country.df.sum<-movie.name.image.face %>% group_by(country.list) %>% summarise(n=n())
movie.name.image.face.country.df.sum <- movie.name.image.face.country.df.sum %>% arrange(-n)

movie.name.image.face.country.df<-merge(movie.name.image.face.country.df, movie.name.image.face.country.df.sum, c('country.list'),all.x=T)
movie.name.image.face.country.df$ratio<-with(movie.name.image.face.country.df, n.x/n.y)
ggplot(movie.name.image.face.country.df,aes(x = country.list, y = ratio, fill = as.factor(gender))) + geom_bar(stat = "identity")

with(subset(movie.name.image.face, country.list==c('일본')), table(genre.list))
