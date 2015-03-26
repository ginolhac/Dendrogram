library("ggplot2")
library("tidyr")
library("magrittr")
library("ggdendro")
library("RColorBrewer")

tab <- read.table("PhysioParam00.txt", header = TRUE, sep = "\t")
tab_comp <- tab %>% select(NodeID1 = NodeID2, NodeID2 = NodeID1, CE)
tab %<>% bind_rows(tab_comp)
physioa<-as.matrix(spread(tab, NodeID2, CE))
physioa[is.na(physioa)] <- 1

group<-read.table("Groups.txt", header = TRUE, sep = "\t")
physioa2 <- left_join(as.data.frame(physioa), group, by=c("NodeID1" = "NodeID"))
physioa2<-as.matrix(physioa2)


# must discard first line
plot(hclust(dist(t(physioa)[-1,])))

matdist <- dist(t(physioa)[-1,])

ggdendrogram(hclust(matdist),rotate = TRUE, size = 4, theme_dendro = FALSE)+theme_classic()

hc <- hclust(matdist)
#hcdata <- hclust(dist(physioa))
hcdata <- dendro_data(hc, type="rectangle")

hcdata$labels$label=colnames(physioa)[as.numeric(hc$order)+1]
leaf <- data.frame(NodeID = colnames(physioa)[as.numeric(hc$order)+1],
                   x = hcdata$labels$x,
                   y = hcdata$labels$y-0.2)
leaf  <- left_join(leaf, group)
leaf$NodeID <- with(leaf, factor(NodeID, levels = colnames(physioa)[as.numeric(hc$order)+1]))

ggdendrogram(hcdata)
ggplot() + 
  geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_text(data=leaf, aes(x=x, y=y, label=NodeID, colour = Group, hjust=0), size=4) +
  coord_flip() + 
  theme_classic()+
  scale_colour_brewer("function", type = "qual", palette = 2)+
  scale_y_reverse(expand=c(0.2, 0))

