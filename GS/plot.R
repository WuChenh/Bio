#plot
source('plot_func.R')
#rice.climate <- sampPosi
#rm(list=names(globalenv())[grep('rice_', names(globalenv()))])
#save(list=names(globalenv())[grep('rice.', names(globalenv()))], file="rice.origin.RData")
#xz -9vk --threads=20 rice.origin.RData

#-------------------------- Plot_Subpopulation ------------------------#
num_ratio_subp <- count_subp()
p1 <- cbind(num_ratio_subp$origin, 'Complete')
p2 <- cbind(num_ratio_subp$no_NA, 'Removed samples with missing values')
colnames(p1) <- c('Subpopulation', 'Count', 'Ratio', 'Dataset')
colnames(p2) <- c('Subpopulation', 'Count', 'Ratio', 'Dataset')
num_ratio_subp <- rbind(p1, p2)
rm(p1, p2)
#
subp_plot <- ggplot(num_ratio_subp, aes(x=Subpopulation, y=Count, fill=Dataset, group=Dataset)) +
  ylim(c(0,110)) +
  geom_bar(position="dodge", width = .5, stat="identity") +
  labs(title='Count Subpopulations') +
  theme(legend.position = c(.22, .85),
        legend.background = element_blank()) +
  geom_text(show.legend = FALSE, 
            aes(alpha=.7, label=paste(format(round(Ratio*100, 2), nsmall=2), "% \n(", Count, ')', "\n\n", sep="")), 
            position=position_dodge2(.6), size=2.6, hjust=.4)

#-------------------------- Plot geo map ------------------------#
#load("~/rice.origin.RData")
map_posi <- rice.compl$splm[,1:8]
map_posi[,6] <- map_posi[,6] |> as.character() |> as.numeric()
map_posi[,7] <- map_posi[,7] |> as.character() |> as.numeric()
map_posi[,8] <- map_posi[,8] |> as.character()
colnames(map_posi)[8] <- "Subpopulation"
#
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(maps)
#
mp <- NULL
mapworld <- borders("world", colour="gray75", fill="gray75")
mp <- ggplot(map_posi) +
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
#mp
# output 900x450


#--------------------------- Plot NN --------------------------------#
NN_plot_data <- rbind(NN_plot_pre("elu", FALSE), NN_plot_pre("elu", TRUE),
                      NN_plot_pre("relu", FALSE), NN_plot_pre("relu", TRUE),
                      NN_plot_pre("linear", FALSE), NN_plot_pre("linear", TRUE),
                      NN_plot_pre("sigmoid", FALSE), NN_plot_pre("sigmoid", TRUE))
NN_plot <- ggplot(NN_plot_data, aes(x=Phenotype, y=R2, fill=Phenotype, color=Activation_and_Env)) +
  #theme_classic() +
  xlab("Phenotype") + 
  ylab(TeX("$R^{\\2}$")) + #------------- Or TeX("$-\\log_{10}{MAE}$")
  #geom_rect(aes(xmin=0, xmax=1.5, ymin=-Inf, ymax=Inf), fill='#C0C0C0', alpha = .01) +
  geom_violin() +
  labs(title='NN Prediction') +
  facet_wrap(~arg, nrow=4) + # or nrow=2
  theme(legend.position = "bottom") #panel.grid=element_blank()


#--------------------------- Plot_BGLR ------------------------#
rm(BGLR_singleTrait_subp_t0.7k05, BGLR_singleTrait_subp_t0.8k05,
   BGLR_singleTrait_t0.7k05, BGLR_singleTrait_t0.8k05)
BGLR_subp_plot <- Collect_BGLR(TRUE, TRUE)
BGLR_nosubp_plot <- Collect_BGLR(FALSE, TRUE)
BGLR_nosubp_plot5col <- cbind(BGLR_nosubp_plot[,1], "N/A", BGLR_nosubp_plot[,2:4])
colnames(BGLR_nosubp_plot5col) <- c("arg", "subp", "model", "phenotype", "MAE")
BGLR_all_plot <- rbind(BGLR_subp_plot, BGLR_nosubp_plot5col)
colnames(BGLR_all_plot) <- c("arg", "Subpopulation", "Model", "Phenotype", "MAE")
rm(BGLR_nosubp_plot5col)
BGLR_plot <- ggplot(BGLR_all_plot, aes(x=Phenotype, y=MAE, shape=Subpopulation, color=Model)) + #, group=subp)) +
  geom_point(alpha=0.5, size=1.5) +
  #geom_line(aes(color=model)) +
  xlab("Phenotype") + 
  ylab("-lg(MAE)") +
  labs(title='BGLR Prediction') +
  #facet_grid(arg~.)
  facet_wrap(~arg, nrow = 2)


# ------------------------------ plot rrBLUP ----------------------------#
rm(rrBLUP_subp_t0.7k05, rrBLUP_subp_t0.8k05, rrBLUP_nosubp_t0.7k05, rrBLUP_nosubp_t0.8k05)
#
rrBLUP_subp_plot <- Collect_rrBLUP(TRUE, TRUE)
rrBLUP_nosubp_plot <- Collect_rrBLUP(FALSE, TRUE)
rrBLUP_nosubp_plot4col <- cbind(rrBLUP_nosubp_plot[,1], "N/A", rrBLUP_nosubp_plot[,2:3])
colnames(rrBLUP_nosubp_plot4col) <- c("arg", "Subpopulation", "Phenotype", "MAE")
rrBLUP_all_plot <- rbind(rrBLUP_subp_plot, rrBLUP_nosubp_plot4col)
colnames(rrBLUP_all_plot) <- c("arg", "Subpopulation", "Phenotype", "MAE")
rm(rrBLUP_nosubp_plot4col)
rrBLUP_plot <- ggplot(rrBLUP_all_plot, aes(x=Phenotype, y=MAE, color=Subpopulation)) +
  geom_point(alpha=0.5, size=1.5) +
  #geom_line(aes(color=model)) +
  xlab("Phenotype") + 
  ylab("-lg(MAE)") +
  labs(title='rrBLUP Prediction') +
  #facet_grid(arg~.)
  facet_wrap(~arg, nrow = 2)


# ----------------------------------- Climate PCA -------------------------- #
climate.f <- scale(as.data.frame(rice.compl[["envi"]]))
climate.f.pr <- princomp(climate.f, cor = TRUE)
climate.f.eigen <- eigen(cor(climate.f))
screeplot(climate.f.pr, type = 'lines')
biplot(climate.f.pr)
#
library(psych)
fa.parallel(climate.f, fa = "pc", n.iter = 200, show.legend = FALSE)
climate.f.pc <- principal(climate.f, nfactors = 5, scores = TRUE)
