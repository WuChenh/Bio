# PLOT #
#xz -9vk --threads=20 rice.origin.RData
#
library(ggplot2)
source("~/geom_split_violin.R")
load("~/rice.origin.RData")

############################## Plot Subpopulation #############################
num_ratio_subp <- count_subp()
p1 <- cbind(num_ratio_subp$origin, 'Original')
p2 <- cbind(num_ratio_subp$no_NA, 'Missing values removed')
colnames(p1) <- c('Subpopulation', 'Count', 'Ratio', 'Dataset')
colnames(p2) <- c('Subpopulation', 'Count', 'Ratio', 'Dataset')
num_ratio_subp <- rbind(p1, p2)
rm(p1, p2)
#
plot_subp <- ggplot(num_ratio_subp, aes(x=Subpopulation, y=Count, fill=Dataset, group=Dataset)) +
  ylim(c(0,110)) +
  geom_bar(position="dodge", width = .5, stat="identity") +
  #labs(title='Count Subpopulations') +
  theme_bw() +
  theme(legend.position = c(.22, .85),
        legend.background = element_blank(), panel.grid=element_blank()) +
  geom_text(show.legend = FALSE, 
            aes(alpha=.7, label=paste(format(round(Ratio*100, 2), nsmall=2), "% \n(", Count, ')', "\n\n", sep="")), 
            position=position_dodge2(.6), size=2.6, hjust=.4)
#720x450

############################### PLOT MAP ###############################
library(ggmap)
library(sp)
library(maptools)
library(maps)
map_posi <- rice.compl$splm[,1:8]
map_posi[,6] <- map_posi[,6] |> as.character() |> as.numeric()
map_posi[,7] <- map_posi[,7] |> as.character() |> as.numeric()
map_posi[,8] <- map_posi[,8] |> as.character()
colnames(map_posi)[8] <- "Subpopulation"
mp <- NULL
mapworld <- borders("world", colour="gray75", fill="gray75")
plot_mp <- ggplot(map_posi) +
  mapworld + #ylim(-60,90) +
  geom_point(aes(x=Longitude, y=Latitude), color="white", size=1.6) +
  geom_point(aes(x=Longitude, y=Latitude, color=Subpopulation), size=1.5, alpha=.4) +
  geom_point(aes(x=Longitude, y=Latitude), shape=1, size=1.6) +
  #xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  theme(legend.position=c(.07, .2), panel.grid=element_blank(),
        legend.background=element_blank(), axis.title=element_blank()) + #NoAxisTitles
  scale_x_continuous(breaks=c(-180,-120,-60,0,60,120,180),
                     expand=expansion(mult=c(0,0), add=c(-50,0)),
                     labels=c('180°', "120° W", "60° W", "0°", "60° E", "120° E", "180°")) +
  scale_y_continuous(breaks=c(-60,-30,0,30,60,90), limits=c(-60, 90),
                     expand=expansion(mult=c(0,0), add=c(-10,-27)),
                     labels=c("60° S", "30° S", "0°", "30° N", "60° N", "90° N"))
# output 900x450

################################# PLOT BGLR ###################################
load("~/rslt.rep30.bglr.RData")
#bglr by chr

#plot1: compare k
plot_bglr <- ggplot(rslt.rep30.bglr, aes(x=bayes, y=corr, fill=k)) +
  xlab('Bayes') +
  ylab('Correlation') +
  geom_split_violin(color=NA) +
  facet_wrap(~trait, nrow=2)
#Rplot_bglr_k 900x400

################################# PLOT RR-BLUP ###############################
load("~/rslt.rep30.rrblup.RData")
plot_rrblup <- ggplot(rslt.rep30.rrblup, aes(x=trait, y=corr, fill=k)) +
  theme_bw() +
  theme(legend.background=element_blank()) + #panel.grid=element_blank(), 
  xlab('Trait') +
  ylab('Correlation') +
  geom_split_violin(trim=T, color=NA) +
  geom_point(stat='summary', fun=median, position=position_dodge(width=.3), color='grey40') +
  stat_summary(fun.min=function(x){quantile(x)[2]}, fun.max=function(x){quantile(x)[4]},
               geom='errorbar', color='grey40', width=.03, size=.5,
               position=position_dodge(width=.3))
  #geom_boxplot(width=.2, outlier.colour = NA,color='grey30')
#Rplot_rrblup 600x400
