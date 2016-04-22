cleanText <- function(textVector){
  require(tm)
  require(SnowballC)
  options(mc.cores=1)
  
  profanity<-VectorSource(readLines("bad-words.txt"))
  corpus <- Corpus(VectorSource(textVector))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, tolower)
  # corpus <- tm_map(corpus, stemDocument, lazy=TRUE)
  #corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, profanity$content)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus
}

loadNgram <- function() {
  require(data.table)
  ngram.model<<-readRDS("ngram.rds")
  lm.ngram <<- length(ngram.model)
}
backoff <- function(str) {
  require(data.table)
  str <- unlist(strsplit(str, " "))
  str.length <- length(str)
  stopifnot(str.length <= lm.ngram)
  
  grepTerm<-function() {
    offset<-0
    for (n in rev(seq(str.length))) {
      offset<- offset + 1
      if (n == 1)
        phase <- gsub("<s>", "", sprintf("^%s", paste(str[offset:str.length], collapse = '')))
      else
        phase <- gsub("<s>", "", sprintf("^%s", paste(str[offset:str.length], collapse = ' ')))
      
      print(c(n,offset,phase))
      
      if(phase == "^")
        test <- suggestWord()
      else
        test<-ngram.model[[n]][grep(phase, term)]
      if(nrow(test)>0)
        return(test)
    }
  }
  
  grepTerm()
}

calcProb<-function(str) {
  # scrub <- cleanText(sprintf("%s", str))
  # str <- unlist(strsplit(scrub[[1]]$content, " "))
  str <- unlist(strsplit(str, " "))
  str.length <- length(str)
  stopifnot(str.length <= lm.ngram)
  # lambda =  c(0.33, 0.33, 0.33)
  # lambda = c(0.4182835, 0.2679848, 0.3137317) #20pc ngram tweet+news
  # lambda = c(0.3556900, 0.3544992, 0.2898108) #20pc ngram tweet+blogs
  # lambda = c(0.3737955, 0.3274974, 0.2987071) #5pc ngram
  lambda = c(0.3291919, 0.3184344, 0.1961350, 0.1562387) #4gram tweet+blogs
  sum(unlist(sapply(rev(seq(str.length)), function(x) {
    offset<- str.length - x + 1
    #     print(paste(str[offset:str.length], collapse = ' '))
    #     print(lambda[x]*exp(ngram.model[[x]][term==paste(str[offset:str.length], collapse = ' '), tp]))
    lambda[x]*exp(ngram.model[[x]][term==paste(str[offset:str.length], collapse = ' '), tp])
  })), na.rm = T)
}

suggestWord<-function(freq=5, size=10){
  hiFreqWord <- ngram.model[[1]][tf>freq]
  hiFreqWord[sample(nrow(hiFreqWord), size=size)]
}
predict <- function(str, show=1) {
  str <- gsub("\\s+$", "", str)
  scrub <- cleanText(sprintf("%s", str))
  scrub <- sprintf("%s <s>", scrub[[1]]$content)
  scrub <- unlist(strsplit(scrub, " "))
  str.length <- length(scrub)
  scrub <- paste(scrub[max(1, str.length-lm.ngram+1):str.length], collapse = ' ')
  print(c(str.length, scrub))
  test<-backoff(scrub)
  glmtp<-numeric()
  if(nrow(test) > 10 ){
    test<- test[order(tf, decreasing = T)][1:10]
  }
  glmtp<-sapply(test$term, calcProb)
  test$glmtp<-glmtp
  test[order(glmtp, decreasing = T)][1:min(show, nrow(test))]
}

### Sample to run
# saveNgram(0.05)  #default 0.01, and run once to build TDM
# loadNgram()
# predict("and make me the")