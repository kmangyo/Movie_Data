# MS cognitive service computer vision api 

library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))

key <-'XXXX'
visionURL = "https://westus.api.cognitive.microsoft.com/vision/v1.0/analyze?visualFeatures=Faces"

imgae<-list()
con<-list()

for(i in 1:467) {
image[i]<-movie.name.image[i,6]
mybody = list(url = image[i])
visionResponse = POST(
  url = visionURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json')
con[[i]] <- content(visionResponse)
  if(is.null(con[[i]]$requestId)){
  Sys.sleep(60)
  image[i]<-movie.name.image[i,6]
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
for (i in 1:467) {
  if (is.null(unlist(con[[i]]$faces))){
   face[[i]]<-'na'
  } else {
  face[[i]]<-data.frame(t(unlist(con[[i]]$faces)))
  }
}

face<-melt(face)

table(face$gender)

movie.name.image.face<-cbind(movie.name.image, face)
ggplot(movie.name.image.face[complete.cases(movie.name.image.face$gender), ] , aes(x = as.numeric(as.character(age)))) + geom_histogram(binwidth = 1) + facet_wrap(~ gender) + xlab("Age")
