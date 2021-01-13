library(tidyverse)
library(reshape2)
library(plyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd <- "./SF_EyesOn.csv"
gaze <- read.csv(wd, header = TRUE)

gaze$Event <- factor(gaze$Event)

ga <- gaze[,which(names(gaze) %in% c("Sub", "Group","Drive", "Event", "Eyes_on", "Hands_on", 
                                     "Road_Ratio", "No_long_glance", "Gender", "Driving_Exp"))]
ga$EH_span <- ga$Hands_on - ga$Eyes_on
ga <- na.omit(ga)

Gaze_ttglance <- ga 
Gaze_ttglance$Group <- factor(Gaze_ttglance$Group)
Gaze_ttglance$Drive <- factor(Gaze_ttglance$Drive)
levels(Gaze_ttglance$Group) <- c("Continuous", "Intermittent", "No Feedback")

GR_col <- data.frame(Ftype = c("Continuous", "Intermittent", "No Feedback"), cols = c("#0073C2FF","#EFC000FF", "#868686FF"))
GR_color_code <- as.character(GR_col$cols[match(Gaze_ttglance$Group, GR_col$Ftype)])

##-- Scatter plot and boxplot by feedback type
p_fg <- ggplot(Gaze_ttglance, aes(x = Group, y = Eyes_on, fill = Event)) +
  geom_boxplot(aes(color = Event), alpha = 0.1, outlier.shape = NA)+
  geom_point(position = position_jitterdodge(), stat = 'identity', shape = 22, alpha = 0.6) +
  theme_bw(12)+theme(legend.position = "none") +
  stat_summary(fun.y = mean, colour = "black", geom = "point", size = 2.5,
               position = position_dodge(width = 0.75)) +
  stat_summary(aes(label=round(..y..,2)), fun.y = mean, color = "black", geom = "text", 
               position = position_nudge(x = c(-0.3,-0.1,0.1,0.3), y = c(1.8,1.8,1.2,1.5)))+
  labs(x = "Feedback Type", y = "Reaction Time [sec]") +
  theme(axis.text=element_text(color="black", size = 11), 
        axis.title.x = element_text(vjust = -1))
print(p_fg)

##-- manual drawing histogram and fitted curve
EO <- with(Gaze_ttglance, seq(min(Eyes_on), max(Eyes_on), length = 140))
fg_dist <- ddply(Gaze_ttglance, "Group", function (df) {
  data.frame(predicted = EO,
             density = dnorm(EO, mean = df$Eyes_on, sd = df$Eyes_on))
  })
fg_dist[is.na(fg_dist)] <- 0
p_fg_hist <- ggplot(Gaze_ttglance, aes(Eyes_on, fill = Group))+
  geom_histogram(aes(y = ..density..), binwidth = 1)+
  scale_fill_manual(values = as.character(unique(GR_color_code))) +
  coord_cartesian(xlim=c(-30,5), ylim = c(0, 0.65))+
  scale_y_continuous(name = "Percentage", labels = scales::percent)+
  xlab("Visual Response Time [sec]")+
  guides(fill = "none")+
  facet_wrap(~Group)+
  theme_bw()+
  theme(axis.text = element_text(color="black", size = 14),
        axis.title = element_text(color="black", size = 16),
        axis.title.x = element_text(vjust = -1),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1))
print(p_fg_hist)

# tiff("Figure 6.tiff", units="px", width=3000, height=1600, pointsize = 24,
#      restoreConsole = TRUE, compression = "lzw", res=300)
# p_fg_hist
# dev.off()

####################################################################################
###                        Between-Within Robust MANOVA                          ###
####################################################################################
##-- Since assumption of normally distributed residuals as well as homoscedasticity was violated
##-- Robust ANOVA was used. With package ##WRS2##
##-- The Robust method uses trimmed means for the analysis.
library(WRS2)

FG_rmixaov <- bwtrim(Eyes_on~Group*Event, id = Sub, data = Gaze_ttglance)
FG_rmixaov

# post hoc on fixed effect
lincon(Eyes_on~Group, data = Gaze_ttglance)

# robust correlation
rt_cor <- Gaze_ttglance[,c("Eyes_on", "Hands_on")]
winall(rt_cor)


################################################################################
#                                                                              #
#                                                                              #
#                      Below monitoring glance behavior                        #
#                                                                              #
#                                                                              #
################################################################################

wd2 <- "./SF_gaze_preTOR.csv"
preTORgaze <- read.csv(wd2, header = T)
preTORgaze$Event <- factor(preTORgaze$Event)
preTORgaze$Group <- factor(preTORgaze$Group)
preTORgaze$Drive <- factor(preTORgaze$Drive)

##-- Screen attention ratio, : sum of glance durations/total time
library(plotrix)

avgATTR <- aggregate(preTORgaze[, "Road_Ratio"], list(preTORgaze[,"Group"]), mean, na.rm=TRUE)
colnames(avgATTR) <- c("Group", "avgATTR")
avgATTR[,"sdATTR"] <- aggregate(preTORgaze[, "Road_Ratio"], list(preTORgaze[,"Group"]), sd, na.rm=TRUE)[,2]

p_roadratio <- ggplot(data = preTORgaze, aes(x = Group, y = Road_Ratio, fill = Group ))+
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.2)+
  geom_point(position = position_jitterdodge(), stat = 'identity', 
             shape = 22, alpha = 0.8, show.legend = F, size=2.5, stroke =1.5)+
  scale_fill_manual(values = c("#0073C2FF","#EFC000FF", "#868686FF"))+
  guides(fill = F)+
  scale_x_discrete(labels = c("Continuous", "Intermittent", "No Feedback"))+
  labs(x = "Feedback Type", y = "Road attention Ratio [%]")+
  theme_bw()+
  theme(axis.text = element_text(color="black", size = 14),
        axis.title = element_text(color="black", size = 16),
        axis.title.x = element_text(vjust = -1),
        panel.border = element_rect(color = "black", size = 1))
print(p_roadratio)

# tiff("Figure 4.tiff", units="px", width=3000, height=1600, pointsize = 24,
#      restoreConsole = TRUE, compression = "lzw", res=300)
# p_roadratio
# dev.off()


##-- Glance greater than 2 seconds on road

levels(preTORgaze$Group) <- c("Continuous", "Intermittent", "No Feedback")

GR_col <- data.frame(Ftype = c("Continuous", "Intermittent", "No Feedback"), cols = c("#0073C2FF","#EFC000FF", "#868686FF"))
GR_color_code <- as.character(GR_col$cols[match(preTORgaze$Group, GR_col$Ftype)]) #JCO color palettes
p_longglance <- ggplot(data = preTORgaze, aes(No_long_glance, fill = Group))+
  geom_histogram(binwidth = 0.5)+
  scale_fill_manual(values = unique(GR_color_code), guide = F)+
  facet_wrap(~Group)+
  labs(x = "\n No. of Long Glances", y = "Frequency [1/min]\n")+
  theme_bw()+
  theme(axis.text = element_text(color="black", size = 14),
        axis.title = element_text(color="black", size = 16),
        axis.title.x = element_text(vjust = -1),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1))
print(p_longglance)

# tiff("Figure 5.tiff", units="px", width=3000, height=1600, pointsize = 24,
#      restoreConsole = TRUE, compression = "lzw", res=300)
# p_longglance
# dev.off()

##-- Monitoring frequency (per 1 min): num of road glances/total time *60
preTORgaze$mon_freq <- (preTORgaze$Number_of_Glances/preTORgaze$Duration)*60
minnonzeroMF <- min(na.omit(preTORgaze[preTORgaze$mon_freq>0,"mon_freq"]))
#- log transform
preTORgaze$mon_freq <- log10(preTORgaze$mon_freq+0.5*minnonzeroMF)
p_roadmon_freq <- ggplot(data = preTORgaze, aes(x = Group, y =mon_freq, group = 1)) +
  stat_summary(fun.y = mean, fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x), geom = "pointrange") +
  stat_summary(fun.y = mean, color = "darkred", geom = "line", lty = 2)+
  scale_x_discrete(labels = c("Continuous", "Intermittent", "No Feedback"))+
  labs(x = "Feedback Type", y = "log(Monitoring frequency [min])")
print(p_roadmon_freq)


