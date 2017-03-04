library(rvest)
library(stringr)
library(rjson)
library(httr)
library(dplyr)
library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))

# naver movie
url<-'http://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20170206&tg=0&page='
page<-c(1:20)
url<-paste0(url,page)

title<-list()
point<-list()
title.url<-list()

for (i in 1:length(url)) {
  title[[i]]<-read_html(url[i]) %>% html_nodes('.title') %>% html_text()
  point[[i]]<-read_html(url[i]) %>% html_nodes('.point') %>% html_text()
  title.url[[i]]<-read_html(url[i]) %>% html_nodes('.title') %>% as.character()
}

title<-unlist(title)
point<-unlist(point)
title.url<-unlist(title.url)

title.url<-data.frame(do.call('rbind', strsplit(title.url,'<a href=\"/',fixed=TRUE)))
title.url<-title.url[2]
title.url<-data.frame(do.call('rbind', strsplit(as.character(title.url$X2),'title=',fixed=TRUE)))
title.url<-title.url[1]
title.url<-str_sub(as.character(title.url$X1),1,nchar(as.character(title.url$X1))-2)

title.url<-paste0('http://movie.naver.com/',title.url)
title.url<-gsub("http://movie.naver.com/movie/bi/mi/basic.", "http://movie.naver.com/movie/bi/mi/detail.", title.url)

role<-list()
name<-list()
movie.spec<-list()

for(i in 1:length(title.url)){
#role
  role[[i]]<-read_html(title.url[i]) %>% html_nodes('.p_part') %>% html_text()
#name
  name[[i]]<-read_html(title.url[i]) %>% html_nodes('.k_name') %>% html_text()
#spec
  movie.spec[[i]]<-read_html(title.url[i]) %>% html_nodes('.info_spec') %>% html_text()
}

pic<-list()

#pic
for(i in 1:length(title.url)){
  pic[[i]]<-read_html(title.url[i]) %>% html_nodes('a img') %>% html_attr('src')
  pic[[i]]<-data.frame(pic=pic[[i]])
  pic[[i]]$num<-stri_count_regex(pic[[i]]$pic, "&quality=95|dft_img111x139.png")
  pic[[i]]<-subset(pic[[i]], num>0)
}

library(reshape2)

role<-melt(role)
name<-melt(name)
role$seq<-1
name$seq<-1
role$seq<- with(role, ave(seq,L1,FUN=cumsum))
name$seq<- with(name, ave(seq,L1,FUN=cumsum))
movie.name<-merge(role, name, c('L1','seq'),all.y=T)
names(movie.name)<-c('rank','seq','role','name')

pic<-melt(pic)
pic<-pic[c(1,4)]
pic$seq<-1
pic$seq<- with(pic, ave(seq,L1,FUN=cumsum))
names(pic)<-c('pic','rank','seq')

movie.name<-merge(movie.name, pic, c('rank','seq'),all.x=T)

movie.name.url<-data.frame(do.call('rbind', strsplit(as.character(movie.name$pic),'.jpg&',fixed=TRUE)))
movie.name.url<-movie.name.url[c(1)]
names(movie.name.url)<-c('pic.url')
movie.name.url$pic.url<-paste0(movie.name.url$pic.url,c('.jpg'))

movie.name<-cbind(movie.name,movie.name.url)
movie.name$image<-stri_count_regex(movie.name$pic, "dft_img111x139.png")
movie.name.image<-subset(movie.name, image==0)

title.point<-data.frame(title=title,point=point,rank=c(1:length(title)))

movie.name.image<-merge(movie.name.image, title.point, c('rank'),all.x=T)

movie.spec.list<-list()

for (i in 1:length(movie.spec)){
  movie.spec.list[i]<-movie.spec[[i]][1]
}

movie.spec.list<-unlist(movie.spec.list)
movie.spec.list<-data.frame(spec=movie.spec.list, rank=c(1:length(movie.spec.list)))

movie.spec.list<-strsplit(as.character(movie.spec.list$spec),'\r\n\t\t\t\r\n\t\t\t\r\n\t\t\t\t')

movie.spec.list.genre<-list()
movie.spec.list.country<-list()

for (i in 1:length(movie.spec.list)){
  movie.spec.list.genre[i]<-movie.spec.list[[i]][1]
  movie.spec.list.country[i]<-movie.spec.list[[i]][2]
}

movie.spec.list.genre<-unlist(movie.spec.list.genre)
movie.spec.list.country<-unlist(movie.spec.list.country)

movie.spec.list.gen.cty<-data.frame(genre=movie.spec.list.genre,country=movie.spec.list.country,rank=c(1:length(movie.spec.list.genre)))

movie.name.image.spec<-merge(movie.name.image, movie.spec.list.gen.cty, c('rank'),all.x = T)
