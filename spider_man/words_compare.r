install.packages('tidyr')
library(tidytext)
library(tidyr)
library(reshape2)
library(dplyr)
library(rvest)
library(httr)
devtools::install_github('haven-jeon/KoNLP')
install.packages('KoNLP')
library(KoNLP)
useNIADic()
library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))
# ref http://varianceexplained.org/r/tidytext-gender-plots/

# spider man home coming

url_2017<-'http://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=135874&target=after&page='
no <- c(1:1000)
url_2017 <-paste0(url_2017, no)

# amazing spider man
url_2012<-'http://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=66823&target=after&page='
no <- c(1:881)
url_2012 <- paste0(url_2012, no)

point_17<-list()
comments_17<-list()
date_17<-list()

for (i in 751:1000) {
  point_17[[i]]<-read_html(iconv(url_2017[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.point') %>% html_text()
  comments_17[[i]]<-read_html(iconv(url_2017[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.title') %>% html_text()
  date_17[[i]]<-read_html(iconv(url_2017[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.num') %>% html_text()
}

point_12<-list()
comments_12<-list()
date_12<-list()

for (i in 751:881) {
  point_12[[i]]<-read_html(iconv(url_2012[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.point') %>% html_text()
  comments_12[[i]]<-read_html(iconv(url_2012[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.title') %>% html_text()
  date_12[[i]]<-read_html(iconv(url_2012[i], from = "euc-kr", to = "CP949"), encoding = "CP949") %>% html_nodes('.num') %>% html_text()
}

comments_12_df <- melt(comments_12)
comments_17_df <- melt(comments_17)

comments_17_df$value<-as.character(comments_17_df$value)
comments_12_df$value<-as.character(comments_12_df$value)

#
text.noun_12<- list()
for (i in 1:nrow(comments_12_df) ) {
  text.noun_12[i]<-melt(extractNoun(comments_12_df[i,1]))
}

text.noun_17<- list()
for (i in 1:nrow(comments_17_df) ) {
  text.noun_17[i]<-melt(extractNoun(comments_17_df[i,1]))
}

text.noun_12_df <-melt(text.noun_12)
text.noun_17_df <-melt(text.noun_17)

text.noun_12_df$group<-'amazing'
text.noun_17_df$group<-'home'

text.noun_df<-rbind(text.noun_12_df,text.noun_17_df)

text.noun_df_hist<- text.noun_df %>% count(group, value)
text.noun_df_hist<- text.noun_df_hist %>% arrange(-n)

text.noun_df_count <- text.noun_df %>%
  count(group, value) %>% filter(n>100) %>% 
  spread(group, n, fill = 0) %>%
  mutate(total = amazing + home,
         amazing = (amazing + 1) / sum(amazing + 1),
         home = (home + 1) / sum(home + 1),
         log_ratio = log2(home / amazing),
         abs_ratio = abs(log_ratio)) %>%
  arrange(desc(log_ratio))

head(text.noun_df_count, 20)
tail(text.noun_df_count, 20)

ggplot(head(text.noun_df_count, 20), aes(x=reorder(value, log_ratio), y=log_ratio)) + geom_bar(stat = "identity") + coord_flip()
ggplot(tail(text.noun_df_count, 20), aes(x=reorder(value, -log_ratio), y=log_ratio)) + geom_bar(stat = "identity") + coord_flip()


