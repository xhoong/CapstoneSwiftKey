source("./NgramApp/ngram_model.R")

ngramToken <-function(c, ngram=1) {
  require(tm)
  require(RWeka)
  options(mc.cores=1)
  TermDocumentMatrix(c, control=list(tokenize=function(x){
    RWeka::NGramTokenizer(x, Weka_control(min = ngram, max = ngram))}))
}

sampleAndToken <- function (c, ratio, ngram) {
  require(tm)
  require(RWeka)
  sampleCorpus <- sample(c, ratio*length(c))
  ngramToken(sampleCorpus, ngram=ngram)
}

saveNgram<- function(percent=0.01){
  options( java.parameters = "-Xmx10g" ) 
  require(parallel)
  require(foreach)
  require(doParallel)
  require(tm)
  require(RWeka)
  require(slam)
  require(data.table)
  
  if (!file.exists("./twitter.RData")) {
   twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE, warn=F)
   twitter <- iconv(twitter, 'UTF-8', 'ASCII', "byte")
   save(twitter, file="twitter.RData")
  }
  if (!file.exists("./blogs.RData")) {
    blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn=F)
    blogs <- iconv(blogs, 'UTF-8', 'ASCII', "byte")
    save(blogs, file="blogs.RData")
  }
  if (!file.exists("./news.RData")) {
    news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = F)
    news <- iconv(news, 'UTF-8', 'ASCII', "byte")
    save(news, file="news.RData")
  }
  
  load("twitter.RData")
  # load("blogs.RData")
  load("news.RData")
  
  ## Remove special character in file to enable tm package to process
  
  twitSmpl <- sample(twitter, percent*length(twitter))
  # blogSmpl <- sample(blogs, percent*length(blogs))
  newsSmpl <- sample(news, percent*length(news))
  # twitSmpl <- sample(twitter, 1000)
  # blogSmpl <- sample(blogs, 1000)
  # newsSmpl <- sample(news, 1000)
  message("Running corpus text cleaning job")

  # allSmpl <- list(twitSmpl,blogSmpl,newsSmpl)
  allSmpl <- list(twitSmpl,newsSmpl)
  cl<-makeCluster(detectCores())
  registerDoParallel(cl)
  allSmpl <- foreach(s=1:length(allSmpl), 
                     .combine = list, 
                     .multicombine = T, 
                     .export = "cleanText") %dopar%
    cleanText(allSmpl[s])
  
  stopCluster(cl)

  allCorpus <- Corpus(VectorSource(unlist(allSmpl)))
  # allCorpus <- cleanText(c(twitSmpl,blogSmpl,newsSmpl))
  #Reference: http://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
  message("Running NGram tokenized job")
  cl<-makeCluster(detectCores())
  registerDoParallel(cl)
  ngram<-foreach(ng=1:lm.ngram, 
                 .combine = list,
                 .multicombine = T,
                 .export = c("lm.ngram", "ngramToken")) %dopar% {
                   ngramToken(allCorpus, ngram=ng)
                 }
  stopCluster(cl)
  message("Calculating term frequency")
  cores<-detectCores()
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  ngramDF<-foreach(ng=1:lm.ngram,
                   .combine = list,
                   .multicombine = T,
                   .export = c("pruneNgram")) %dopar% {
                     require(data.table)
                     ngramFreq<-slam::row_sums(ngram[[ng]])
                     pruneNgram(data.table(term=names(ngramFreq), tf=ngramFreq), ng)[, !"V1", with=F]
                   }
  stopCluster(cl)
  cl<-makeCluster(cores)
  registerDoParallel(cl)  
  message("Calculating term probability")
  ngramDF2<-foreach(ng=1:lm.ngram,
                   .combine = list,
                   .multicombine = T,
                   .export = c("findNgramFreq", "mcmapply")) %dopar% {
                     require(data.table)
                     ngram_ <- ngramDF[[ng]]
                     if(ng == 1)
                       ngram_$tp <- log(ngram_$tf/sum(ngram_$tf))
                     else {
                       ngram_$tp <-do.call(function(term, tf) 
                         log(tf/unlist(mcmapply(function(x, y) 
                                         findNgramFreq(ngramDF[[ng-1]], 
                                                       paste(unlist(
                                                         strsplit(as.character(x), ' '))[seq(ng-1)], 
                                                         collapse=' '), y), term, tf, mc.cores=cores/2))), 
                         ngram_[, list(term, tf)])
                     }
                     ngram_
                   }  
  stopCluster(cl)
  
  saveRDS(ngramDF2,file="ngram.rds")
  
  # save(ngram,file="ngram.RData")
}

findNgramFreq<-function(ngram, term_, tf) {
  utf<-ngram[term==term_, tf]
  if(length(utf) == 0) 
    tf
  else
    utf
}

pruneNgram <- function(ngram, n) {
  if (n==1) {
    return(ngram[tf>5])
  }
  if (n>2) {
    ngram<-ngram[tf>1]
  }
  ngram[order(term, -tf), paste0(strsplit(term, " ")[[1]][1:(n-1)], collapse = ' '), by="term,tf"][,head(.SD, 10), by=V1]
}


#ShinyApp reactive delay
#http://stackoverflow.com/questions/31051133/how-do-i-make-sure-that-a-shiny-reactive-plot-only-changes-once-all-other-reacti

lm.ngram=4
### Sample to run
# saveNgram(0.05)  #default 0.01, and run once to build TDM
# loadNgram()
# predict("and make me the")