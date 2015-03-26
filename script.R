setwd("D:/R/MirMet//Dendrogram")
library("tidyr")
library("dplyr")
library("ggplot2")
library("ggdendro")
library("magrittr")


tab <- read.table("PhysioParam00.txt", header = TRUE, sep = "\t")
tab_comp <- tab %>% select(NodeID1 = NodeID2, NodeID2 = NodeID1, CE)
tab %<>% bind_rows(tab_comp)

physioa<-as.matrix(spread(tab, NodeID2, CE))
physioa[is.na(physioa)] <- 1

group <- read.table("Groups.txt", header = TRUE, sep="\t")
physioa2 <- left_join(as.data.frame(physioa), group, by=c("NodeID1" = "NodeID"))
physioa2<-as.matrix(physioa2)

# Must discard first line
plot(hclust(dist(t(physioa)[-1,])))

matdist <- dist(t(physioa)[-1,])


ggdendrogram(hclust(dist(matdist)),rotate = TRUE, size = 4, theme_dendro = FALSE) + theme_classic()

hc <- hclust(matdist)
hcdata <- dendro_data(hc, type="rectangle")

hcdata$labels$label=colnames(physioa)[as.numeric(hc$order)+1]
leaf <- data.frame(NodeID = colnames(physioa)[as.numeric(hc$order)+1],
                   x = hcdata$labels$x,
                   y = hcdata$labels$y-0.2)
leaf <- left_join(leaf,group)
leaf$NodeID <- with(leaf,factor(NodeID, levels = colnames(physioa)[as.numeric(hc$order)+1]))

ggdendrogram(hcdata)
cbPalette <- c("#56B4E9", "#0072B2","#E69F00", "#D55E00","#999999", "#CC79A7")
p <- ggplot() + 
     geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
     geom_text(data=leaf, aes(x=x, y=y, label=NodeID, hjust=0, colour=Group), size=4, fontface ="bold") +
     coord_flip() + 
    #theme_classic() +
    #scale_color_brewer("function", type = "qual", palette = "Set1") +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme(legend.justification=c(0,1), legend.position=c(0, 1)) +
  geom_hline(aes(yintercept=1.5), colour="#BB0000", linetype="dashed") +
     scale_y_reverse(expand=c(0.2, 0))
pdf("PhysioDendro.pdf",width=8.3, height=11.7, onefile=T)
plot(p)
dev.off()
