library(tm)
library(slam)
library(RWeka)
require(tau)
dirName <- 'F:/btp/test/final/en_US'
blog <- readLines(paste0(dirName,"/en_US.blogs.txt"),skipNul=TRUE,encoding='latin1')
nb <- length(blog)
sampleSize <- 20000
set.seed(1340)
sblog <- blog[sample(1:nb,sampleSize)]
writeLines(sblog,"F:/btp/test/final/en_US_sample/en_US.blog.sample.txt")

dirName <- 'F:/btp/test/final/en_US_sample'
dCorpus <- Corpus(DirSource(dirName))
clean.text <- function(x, lowercase=TRUE, numbers=TRUE, punctuation=TRUE, spaces=TRUE){
  # x: character string
  
  # lower case
  if (lowercase)
    x = tolower(x)
  # remove numbers
  if (numbers)
    x = gsub("[[:digit:]]", "", x)
  # remove punctuation symbols
  if (punctuation)
    x = gsub("[[:punct:]]", "", x)
  # remove extra white spaces
  if (spaces) {
    x = gsub("[ \t]{2,}", " ", x)
    x = gsub("^\\s+|\\s+$", "", x)
  }
  # return
  x
}

cleanedCorpus <- clean.text(dCorpus)

Tokenizer <- function(min, max){ 
  function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
}
dTDM <- TermDocumentMatrix(dCorpus, control=list(tokenize=Tokenizer(2,6)))

Terms <- Terms(dTDM)
Ngrams <- data.frame(terms=sapply(Terms,function(t) paste(head(strsplit(t," ")[[1]],length(strsplit(t," ")[[1]])-1),collapse = " ")),
                     N=sapply(Terms,function(t) length(unlist(strsplit(t," ")))),
                     count=row_sums(dTDM),
                     last=sapply(Terms,function(t) tail(strsplit(t," ")[[1]],1)),
                     stringsAsFactors=FALSE
)
##ngram_model <- rollup(dTDM, 2, na.rm = TRUE, FUN = sum)
##ngram_model_dt <- data.table(token = ngram_model$dimnames$Terms, count = ngram_model$v)

Ngrams <- Ngrams[order(Ngrams$count,decreasing=TRUE),]
Ngrams <- Ngrams[order(Ngrams$N,decreasing=TRUE),]

predict_word<-function(x){
  temp<- strsplit(x, " ")[[1]]
  k <- length(temp) #Number of n-gram models in "ndictlist" minus one
  
  while(k){
    temp <- tail(temp, k) #Reducing the string
    k<-k-1
    ans<-subset(Ngrams,terms==paste(temp,collapse=" "))
    if(sum(ans$N!=0)){
      print(head(ans,3));
    }
##    ans<-match(paste(temp,collapse=" "),Ngrams[[1]])
##    if(!is.na(ans)){
  ##    print(Ngrams[[4]][ans]);
##      break;
##    }
  }
}
## when was the last
## i am a big fan of
## i am reading the
string <- tolower("i am a big fan of")
predict_word(string)
