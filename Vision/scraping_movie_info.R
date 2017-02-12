# naver movie
library(rvest)
library(stringr)
library(reshape2)

url<-'http://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20170206&tg=0'
title<-read_html(url) %>% html_nodes('.title') %>% html_text()
point<-read_html(url) %>% html_nodes('.point') %>% html_text()
title.url<-read_html(url) %>% html_nodes('.title') %>% as.character()

title.url<-data.frame(do.call('rbind', strsplit(title.url,'<a href=\"/',fixed=TRUE)))
title.url<-title.url[2]
title.url<-data.frame(do.call('rbind', strsplit(as.character(title.url$X2),'title=',fixed=TRUE)))
title.url<-title.url[1]
title.url<-str_sub(as.character(title.url$X1),1,nchar(as.character(title.url$X1))-2)

title.url<-paste0('http://movie.naver.com/',title.url)
title.url<-gsub("http://movie.naver.com/movie/bi/mi/basic.", "http://movie.naver.com/movie/bi/mi/detail.", title.url)

role<-list()
name<-list()

for(i in 1:length(title.url)){
#role
  role[[i]]<-read_html(title.url[i]) %>% html_nodes('.p_part') %>% html_text()
#name
  name[[i]]<-read_html(title.url[i]) %>% html_nodes('.k_name') %>% html_text()
}

pic<-list()

#pic
for(i in 1:length(title.url)){
  pic[[i]]<-read_html(title.url[i]) %>% html_nodes('a img') %>% html_attr('src')
  pic[[i]]<-data.frame(pic=pic[[i]])
  pic[[i]]$num<-stri_count_regex(pic[[i]]$pic, "&quality=95|dft_img111x139.png")
  pic[[i]]<-subset(pic[[i]], num>0)
}

role<-melt(role)
name<-melt(name)
role$rank<-1
name$rank<-1
role$rank<- with(role, ave(rank,L1,FUN=cumsum))
name$rank<- with(name, ave(rank,L1,FUN=cumsum))
movie.name<-merge(role, name, c('L1','rank'),all.y=T)
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
