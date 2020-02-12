library(tidyverse)
library(wordcloud)
library(tm)

data.df <- read_csv("../../stat.101.data.quiz.1.csv")
docs <- Corpus(VectorSource(data.df$stat.word))
tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()

tdm2 <- rowSums(tdm)
word.mat <- unique(word.mat) 
word.mat <- na.omit(word.mat) %>% as.vector()

tdm2 <- as.matrix(tdm2)
rownames(tdm2) <- word.mat
word.cloud <- as.matrix(tdm2[order(tdm2, decreasing=TRUE),])

wordcloud(rownames(tdm2), tdm2, min.freq =1, scale=c(3, 0.7), random.order = FALSE, random.color = FALSE, colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))


data.df2 <- data.df %>% filter(fav.col != "no") %>%
  filter(!is.na(fav.col))



pie.df <- data.df2 %>% select(fav.col) %>% group_by(fav.col) %>% summarise(count = n())

bp<- ggplot(pie.df, aes(x="", y=count, fill=fav.col))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + blank_theme +
  theme(axis.text.x=element_blank())
  geom_text(aes(y = count/10 + c(0, cumsum(count)[-length(count)]), 
                label = percent(count/100)), size=2)
