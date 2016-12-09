library(ggplot2)
library(reshape2)

years <- read.table("years.txt")
rownames(years) <- years[,1]
years <- years[,-1]


years$begin <- years[,1] #Technology begins in first year in row
years$end <- apply(years, 1, max)  #Technology ends in the last (highest) year in row
years$Technology <- rownames(years)

#Reshape data in long format for ggplot
reshape <- melt(years, id.vars=c("Technology", "begin", "end"))

p <- ggplot(data=reshape, aes(y=Technology)) + 
  labs(x="Conference Year", y="Technology") + 
  xlim(1968,2015) +
  theme(legend.position="none") +
  geom_segment(aes(x = begin, xend = end, y = reorder(Technology,-end), 
                   yend = reorder(Technology,-end), group=Technology), size = 5)  +
  geom_point(aes(x=value, color="blue"), size=2, shape=15)

p
