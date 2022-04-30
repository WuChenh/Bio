# PLOT #
#xz -9vk --threads=20 rice.origin.RData
#
library(ggplot2)
library(latex2exp)
source("~/geom_split_violin.R")
load("~/rice_origin.RData")

############################## Plot Subpopulation ##############################
num_ratio_subp <- count_subp(rice.compl.p11$splmd, rice.origin$SD1)
p1 <- cbind(num_ratio_subp$origin, 'Original')
p2 <- cbind(num_ratio_subp$compl, 'With missing values removed')
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
  theme(legend.position = c(.17, .85),
        legend.background = element_blank(), panel.grid=element_blank()) +
  geom_text(show.legend = FALSE, 
            aes(alpha=.7, label=paste(format(round(Ratio*100, 2), nsmall=2), "% \n(", Count, ')', "\n\n", sep="")), 
            position=position_dodge2(.6), size=2.6, hjust=.4)
#720x450

##################################### PLOT MAP #################################
library(ggmap)
library(sp)
library(maptools)
library(maps)
map_posi <- rice.compl.p11$splmd[,1:8]
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

################################# PLOT BGLR ####################################
#load("~/rslt.rep30.bglr.RData")
load("~/rslt.randrep10.bglr.RData")
#################bglr by chr##############(Developing)

#plot1: compare k by Corr
plot_bglr_cor <- ggplot(rslt.randrep10.bglr, aes(x=Bayes, y=Corr, fill=TrnPerc)) +
  theme_bw() +
  theme(legend.position=c(0.9, 0.1), #legend.position='bottom',legend.justification="right"
        legend.background=element_blank(),
        axis.text.x=element_text(angle=30, vjust=0.5, hjust=0.5)) + #panel.grid=element_blank(), 
  labs(fill="Training set\nproportion") +
  xlab('Bayes') +
  ylab("Mean Pearson\'s correlation") + #TeX("Mean $R^{\\2}$")
  geom_split_violin(color=NA) +
  geom_point(size=.9, stat='summary', fun=median, position=position_dodge(width=.4), color='grey40') +
  stat_summary(fun.min=function(x){quantile(x)[2]}, fun.max=function(x){quantile(x)[4]},
               geom='errorbar', color='grey40', width=.04, size=.3,
               position=position_dodge(width=.4)) +
  facet_wrap(~Trait, nrow=3)
#Rplot_bglr 900x450 / 800x500

#plot_bglr_nIter
load("~/bglr/iter/nIter.t4.bI49.RData")
rslt.t4.bI49 <- rslt.t4.bI49[-1,]
rslt.t4.bI49 <- as.data.frame(rslt.t4.bI49)

#1 MSE ##############
plot_bglr_nIter12000_MSE <- ggplot(rslt.t4.bI49, aes(x=nIter, y=MSE_mean)) +
  scale_x_continuous(breaks=c(100,2500,5000,7500,10000,12000)) +
  theme_bw() +
  theme(legend.background=element_blank()) + #panel.grid=element_blank(), 
  xlab('Number of iterations') +
  ylab('Mean MSE') + #TeX("$R^{\\2}$")
  geom_line() #+ geom_smooth(method='loess')
# output 700x400

plot_bglr_nIter2550_MSE <- ggplot(rslt.t4.bI49[1:50,], aes(x=nIter, y=MSE_mean)) + #color=MSE_mean
  scale_x_continuous(breaks=c(100,500,1000,1500,2000,2550)) +
  theme_bw() +
  theme(legend.background=element_blank(), legend.title=element_blank(), legend.position = 'bottom') + #panel.grid=element_blank(), 
  xlab('Number of iterations') +
  ylab('Mean MSE') + #TeX("$R^{\\2}$")
  geom_line() + geom_smooth(method='gam') #gam/loess
# output 700x400, 420x450

#2 Corr ##############
plot_bglr_nIter12000_Corr <- ggplot(rslt.t4.bI49, aes(x=nIter, y=Corr_mean)) +
  scale_x_continuous(breaks=c(100,2500,5000,7500,10000,12000)) +
  theme_bw() +
  theme(legend.background=element_blank()) + #panel.grid=element_blank(), 
  xlab('Number of iterations') +
  ylab("Mean Pearson\'s correlation") +
  geom_line() #+ geom_smooth(method='loess')
# output 700x400

plot_bglr_nIter2550_Corr <- ggplot(rslt.t4.bI49[1:50,], aes(x=nIter, y=Corr_mean)) + #color=Corr_mean
  scale_x_continuous(breaks=c(100,500,1000,1500,2000,2550)) +
  theme_bw() +
  theme(legend.background=element_blank(), legend.title=element_blank(), legend.position = 'bottom') + #panel.grid=element_blank(), 
  xlab('Number of iterations') +
  ylab("Mean Pearson\'s correlation") +
  geom_line() + geom_smooth(method='gam')
# output 700x400, 420x450

################################# PLOT RR-BLUP #################################
load("~/rslt.randrep10.rrblup.RData")
plot_rrblup_cor <- ggplot(rslt.randrep10.rrblup, aes(x=Trait, y=Corr, fill=TrnPerc)) +
  theme_bw() +
  theme(legend.position='bottom', legend.background=element_blank()) + #panel.grid=element_blank(), 
  labs(fill="Training set\nproportion") +
  xlab('Trait') +
  ylab("Mean Pearson\'s correlation") + #TeX("Mean $R^{\\2}$")
  geom_split_violin(trim=T, color=NA) +
  geom_point(stat='summary', fun=median, position=position_dodge(width=.3), color='grey40') +
  stat_summary(fun.min=function(x){quantile(x)[2]}, fun.max=function(x){quantile(x)[4]},
               geom='errorbar', color='grey40', width=.03, size=.5,
               position=position_dodge(width=.3))
#geom_boxplot(width=.2, outlier.colour = NA,color='grey30')
#Rplot_rrblup 600x400 / 700x450



############################## PLOT phenotypes' Cor ############################
trn.p.scale <- scale(as.matrix(datain$trn.p))
cor_P_mx <- t(trn.p.scale) %*% trn.p.scale
cor_P_mx <- cor(trn.p.scale)

library(corrplot)
corrplot(corr = cor_P_mx, method = "shade",type = "upper", order = 'alphabet', tl.pos = "lt", tl.col = 'black')
corrplot(corr = cor_P_mx, add = TRUE, type = "lower", method = "number", order = 'alphabet',
         diag = FALSE, tl.pos = "n", cl.pos="n")
# 600x480



################################ PLOT NN #######################################
library(ggplot2)
library(keras)

############################# Compare 2 traits combn ##########################
ggplot(NN_MT_combn_wt_plot, aes(Trait, MSE, fill=Combn))+
  theme_bw()+
  theme(legend.position='none')+
  scale_fill_grey()+
  geom_col(position='dodge')+
  geom_col(data=NN_ST_Wt_bst, aes(Trait, MSE, fill='grey', alpha=.2))
# Rplot_combn2_wt_MSE 800x450

ggplot(NN_MT_combn_wt_plot, aes(Trait, Corr, fill=Combn))+
  theme_bw()+
  theme(legend.position='none')+
  ylab("Mean Pearson\'s correlation")+
  scale_fill_grey()+
  geom_col(position='dodge')+
  geom_col(data=NN_ST_Wt_bst, aes(Trait, Corr, fill='grey', alpha=.2))
# Rplot_combn2_wt_Cor 800x450

#======================================ALL=====================================#
#######color
plot_ALL_MSE_colorful <- ggplot(allM1, aes(Trait, MSE, fill=allMethods)) +
  theme_bw() +
  theme(legend.position='bottom', legend.title=element_blank()) +
  geom_col(position='dodge', width=.7) +
  scale_fill_manual(values=c('#53868B', '#7AC5CD',
                             '#9BCD9B', '#B4EEB4',
                             '#2E8B57', '#3CB371',
                             '#FF4500', '#FF7F50',    #'#1c1c1c', '#4F4F4F',
                             '#FF7F24', '#FFA54F',
                             '#4F94CD', '#36648B'),
                    breaks=c('3','4','1','2','7','8','9','10','12','11','5','6'),
                    labels=c('ST,weighted', 'ST,unweighted',
                             'MT(11),weighted', 'MT(11),unweighted',
                             'MT(11),weighted,climate', 'MT(11),unweighted,climate',
                             'MT(2),weighted', 'MT(2),unweighted',
                             'MT(2),weighted,climate', 'MT(2),unweighted,climate',
                             'RR-BLUP', 'BLR'))
#######bw
plot_ALL_MSE_bw <- ggplot(allM1, aes(Trait, MSE, fill=allMethods)) +
  theme_bw() +
  theme(legend.position='bottom', legend.title=element_blank()) +
  geom_col(position='dodge', width=.7) +
  scale_fill_manual(values=c('#53868B', '#7AC5CD',
                             '#9BCD9B', '#B4EEB4',
                             '#2E8B57', '#3CB371',
                             '#1c1c1c', '#d2d2d2',
                             '#4f4f4f', '#b5b5b5',
                             '#4F94CD', '#36648B'),
                    breaks=c('3','4','1','2','7','8','9','10','12','11','5','6'),
                    labels=c('ST,weighted', 'ST,unweighted',
                             'MT(11),weighted', 'MT(11),unweighted',
                             'MT(11),weighted,climate', 'MT(11),unweighted,climate',
                             'MT(2),weighted', 'MT(2),unweighted',
                             'MT(2),weighted,climate', 'MT(2),unweighted,climate',
                             'RR-BLUP', 'BLR'))
# Rplot_ALL_MSE 900x500 850x550

#######color
plot_ALL_Cor_colorful <- ggplot(allM1, aes(Trait, Corr, fill=allMethods)) +
  theme_bw() +
  theme(legend.position='bottom', legend.title=element_blank()) +
  ylab("Mean Pearson\'s correlation") +
  geom_col(position='dodge', width=.7) +
  scale_fill_manual(values=c('#53868B', '#7AC5CD',
                             '#9BCD9B', '#B4EEB4',
                             '#2E8B57', '#3CB371',
                             '#FF4500', '#FF7F50',
                             '#FF7F24', '#FFA54F',
                             '#4F94CD', '#36648B'),
                    breaks=c('3','4','1','2','7','8','9','10','12','11','5','6'),
                    labels=c('ST,weighted', 'ST,unweighted',
                             'MT(11),weighted', 'MT(11),unweighted',
                             'MT(11),weighted,climate', 'MT(11),unweighted,climate',
                             'MT(2),weighted', 'MT(2),unweighted',
                             'MT(2),weighted,climate', 'MT(2),unweighted,climate',
                             'RR-BLUP', 'BLR'))
#######bw
plot_ALL_Cor_bw <- ggplot(allM1, aes(Trait, Corr, fill=allMethods)) +
  theme_bw() +
  theme(legend.position='bottom', legend.title=element_blank()) +
  ylab("Mean Pearson\'s correlation") +
  geom_col(position='dodge', width=.7) +
  scale_fill_manual(values=c('#53868B', '#7AC5CD',
                             '#9BCD9B', '#B4EEB4',
                             '#2E8B57', '#3CB371',
                             '#1c1c1c', '#d2d2d2',
                             '#4f4f4f', '#b5b5b5',
                             '#4F94CD', '#36648B'),
                    breaks=c('3','4','1','2','7','8','9','10','12','11','5','6'),
                    labels=c('ST,weighted', 'ST,unweighted',
                             'MT(11),weighted', 'MT(11),unweighted',
                             'MT(11),weighted,climate', 'MT(11),unweighted,climate',
                             'MT(2),weighted', 'MT(2),unweighted',
                             'MT(2),weighted,climate', 'MT(2),unweighted,climate',
                             'RR-BLUP', 'BLR'))
# Rplot_ALL_Cor 900x500 850x550


if(F){
  plot_comp_bz_mse <- ggplot(compare_b, aes(Trait, MSE, fill=batch_size)) +
    theme_bw() +
    scale_fill_grey() +
    theme(legend.position='bottom') +
    labs(fill='batch size') +
    geom_col(position='dodge', width=0.7)
  # Rplot_compare_bz_mse 900x500
  
  plot_comp_bz_cor <- ggplot(compare_b, aes(Trait, Corr, fill=batch_size)) +#, fill=colorG
    theme_bw() +
    scale_fill_grey() +
    theme(legend.position='bottom') +
    labs(fill="batch size") +
    ylab("Mean Pearson\'s correlation") +
    geom_col(position='dodge', width=0.7)
  # Rplot_compare_bz_cor 900x500
  
  
  plot_comp_wtTF_ST_mse <- ggplot(NN_ST_all, aes(Trait, MSE, fill=Effects)) +
    theme_bw() +
    #scale_fill_grey() +
    theme(legend.position='bottom') +
    labs(fill='Assigned effects') +
    geom_col(position='dodge', width=0.7) +
    #scale_fill_discrete(breaks=c('T','F'), labels=c('T', 'F'))
    scale_fill_manual(values=c('#989898', '#333333'))
  # Rplot_comp_wtTF_ST_MSE 900x500
  
  plot_comp_wtTF_ST_cor <- ggplot(NN_ST_all, aes(Trait, Corr, fill=Effects)) +
    theme_bw() +
    #scale_fill_grey() +
    theme(legend.position='bottom') +
    labs(fill='Assigned effects') +
    ylab("Mean Pearson\'s correlation") +
    geom_col(position='dodge', width=0.7) +
    scale_fill_manual(values=c('#989898', '#333333'))
  # Rplot_comp_wtTF_ST_Cor 900x500
}

