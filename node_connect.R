library(KoNLP)
library(rJava)
library(dplyr)
library(tm)
library(wordcloud)
library(igraph)




########

# 주어진 train data 전처리 한 후 csv로 
traintext <- read.csv('C:/Users/USER/Desktop/traintext.csv')

text <- as.character(traintext[[1]])
author <- traintext[[2]]

TermDocumentMatrix(cbind(text,author)[,1])
class(cbind(text,author)[,1]) # character

CORPUS <- Corpus(VectorSource(cbind(text,author)[,1]))
TDM <- TermDocumentMatrix(CORPUS)
TDM
dim(TDM) # [1] 33474 54879 / 54879 문장에서 33474개 단어 추출 
inspect(TDM)
head(TDM.m,20)
TDM.m <- as.matrix(TDM) #TDM matrix
term.freq <- sort(rowSums(TDM.m),decreasing=T)
head(term.freq,50)  # 전체 Top 50
head(term.freq,100) # 전체 Top 100 # minimum > 1377

# word cloud
wordcloud(words=names(term.freq),freq=term.freq,
          min.freq=500,random.order=F,colors=brewer.pal(8,'Dark2'))


# 빈도수 1300 이상인 단어들만 
my.Term <- TDM.m[rownames(TDM.m) %in% names(term.freq[term.freq]>1300),]
my.Term[1,]

my.Term[my.Term >=1] <- 1
termMatrix <- my.Term %*% t(my.Term)
termMatrix[1:5,1:5]


g <- graph.adjacency(termMatrix[1:75,1:75],weight=T,mode='undirected')
g <- simplify(g)
###
V(g)$label <-V(g)$name
head(V(g)$label,5)
head(V(g)$degree,5)

layout1 <- layout.fruchterman.reingold(g)
plot(g)
###

V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree)+0.2
V(g)$label.color <- rgb(0,0,0.2,0.8)
V(g)$frame.color <- NA

egam <- (log(E(g)$weight)+0.4) / max(log(E(g)$weight)+0.4)
E(g)$width <- egam
E(g)$color <- rgb(0.5,0.5,0,egam)

plot(g,layout= layout1)
