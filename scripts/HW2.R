#set working directory
setwd('..')
#load libraries
library(data.table)
library(ggplot2)
library(reshape2)

# Read in data file
Data <- fread("data/FlintGarcia09.txt", header=TRUE)
# Convert missing data to NA
Data[Data=="."] <- NA

# set up data classes
Data$INBRED <- as.factor(Data$INBRED)
Data$
Data$Env <- as.factor(Data$Env)
#selected tassel branch count, plant yield, and cob weight as traits
  #inbreds
Data$TSLBCHCNT_Inbred  <- as.numeric(as.character((Data$TSLBCHCNT_Inbred)))
Data$CobWt_Inbred <- as.numeric(as.character((Data$CobWt_Inbred)))
Data$PltYield_Inbred  <- as.numeric(as.character((Data$PltYield_Inbred )))
  #hybrids
Data$TSLBCHCNT_Hyb  <- as.numeric(as.character((Data$TSLBCHCNT_Hyb)))
Data$CobWt_Hyb <- as.numeric(as.character((Data$CobWt_Hyb)))
Data$PltYield_Hyb  <- as.numeric(as.character((Data$PltYield_Hyb )))

#fit linear models
  #inbreds
fit_tbc_i <- lm(TSLBCHCNT_Inbred ~ INBRED + Env, data=Data)
fit_cw_i <- lm(CobWt_Inbred ~ INBRED + Env, data=Data)
fit_py_i <- lm(PltYield_Inbred ~ INBRED + Env, data=Data)
  #hybrids
fit_tbc_h <- lm(TSLBCHCNT_Hyb ~ INBRED + Env, data=Data)
fit_cw_h <- lm(CobWt_Hyb ~ INBRED + Env, data=Data)
fit_py_h <- lm(PltYield_Hyb ~ INBRED + Env, data=Data)

#anova tables
  #inbreds
anova(fit_tbc_i)
anova(fit_cw_i)
anova(fit_py_i)
  #hybrids
anova(fit_tbc_h)
anova(fit_cw_h)
anova(fit_py_h)

#genetic variances and heritabilities
  #inbreds, VA
Vg_tbc_i =(anova(fit_tbc_i)[1,3] - anova(fit_tbc_i)[3,3])/4
Ve_tbc_i = anova(fit_tbc_i)[3,3]
h2_tbc_i = Vg_tbc_i/(Vg_tbc_i + Ve_tbc_i)
h2_tbc_i
Vg_cw_i =(anova(fit_cw_i)[1,3] - anova(fit_cw_i)[3,3])/4
Ve_cw_i = anova(fit_cw_i)[3,3]
h2_cw_i = Vg_cw_i/(Vg_cw_i + Ve_cw_i)
h2_cw_i
Vg_py_i =(anova(fit_py_i)[1,3] - anova(fit_py_i)[3,3])/4
Ve_py_i = anova(fit_py_i)[3,3]
h2_py_i = Vg_py_i/(Vg_py_i + Ve_py_i)
h2_py_i
  #hybrids, VG
Vg_tbc_h =(anova(fit_tbc_h)[1,3] - anova(fit_tbc_h)[3,3])/4
Ve_tbc_h = anova(fit_tbc_h)[3,3]
H2_tbc_h = Vg_tbc_h/(Vg_tbc_h + Ve_tbc_h)
H2_tbc_h
Vg_cw_h =(anova(fit_cw_h)[1,3] - anova(fit_cw_h)[3,3])/4
Ve_cw_h = anova(fit_cw_h)[3,3]
H2_cw_h = Vg_cw_h/(Vg_cw_h + Ve_cw_h)
H2_cw_h
Vg_py_h =(anova(fit_py_h)[1,3] - anova(fit_py_h)[3,3])/4
Ve_py_h = anova(fit_py_h)[3,3]
H2_py_h = Vg_py_h/(Vg_py_h + Ve_py_h)
H2_py_h

#comparisons to heterosis 
#heterosis table
BPH <- c(4, 66, 185)
Trait <- c("Tassel Branch Count", "Cob Weight (g)", "Plant Yield (g/plant)")
FGHet <- as.data.frame(cbind(Trait, BPH))
FGHet <- as.data.frame(rbind(FGHet, FGHet))
FGHet$BPH <- as.numeric(as.character(FGHet$BPH))
FGHet$Group <- c(rep("Inbreds", 3), rep("Hybrids", 3)) 
#add heritabilities
FGHet$Heritability <- as.vector(rbind(h2_tbc_i, h2_cw_i, h2_py_i, H2_tbc_h, H2_cw_h, H2_py_h))
#add genetic variances
FGHet$Vg <- as.vector(rbind(Vg_tbc_i, Vg_cw_i, Vg_py_i, Vg_tbc_h, Vg_cw_h, Vg_py_h)) 
#add environmental variance
FGHet$Ve <- as.vector(rbind(Ve_tbc_i, Ve_cw_i, Ve_py_i, Ve_tbc_h, Ve_cw_h, Ve_py_h)) 
#correlations
Split <- split(FGHet, FGHet$Group)
cor.test(Split$Inbreds$BPH, Split$Inbreds$Heritability)
cor.test(Split$Inbreds$BPH, Split$Inbreds$Vg)
cor.test(Split$Inbreds$BPH, Split$Inbreds$Ve)
cor.test(Split$Hybrids$BPH, Split$Hybrids$Heritability)
cor.test(Split$Hybrids$BPH, Split$Hybrids$Vg)
cor.test(Split$Hybrids$BPH, Split$Hybrids$Ve)
#visualize
png("HetvHerit.png", width=800, height=600)
ggplot(FGHet, aes(y=Heritability, x = BPH, color = Group, fill = Group)) + 
  geom_point(size = 3) + 
  geom_line(size = 1, linetype = "dashed", color = "gray39") +
  scale_color_manual(values=c("navy", "firebrick")) +
  labs(title="Heterosis vs Heritability",
       x="% Better Parent Heterosis", y='Heritability') +
  theme(text = element_text(size = 14),
        axis.text=element_text(size = 14))
dev.off()
png("HetvVG.png", width=800, height=600)
ggplot(FGHet, aes(y=Vg, x = BPH, color = Group, fill = Group)) + 
  geom_point(size = 3) + 
  geom_line(size = 1, linetype = "dashed", color = "gray39") +
  scale_color_manual(values=c("navy", "firebrick")) +
  labs(title="Heterosis vs Genetic Variance",
       x="% Better Parent Heterosis", y='Genetic Variance') +
  theme(text = element_text(size = 14),
        axis.text=element_text(size = 14))
dev.off()
png("HetvVE.png", width=800, height=600)
ggplot(FGHet, aes(y=Ve, x = BPH, color = Group, fill = Group)) + 
  geom_point(size = 3) + 
  geom_line(size = 1, linetype = "dashed", color = "gray39") +
  scale_color_manual(values=c("navy", "firebrick")) +
  labs(title="Heterosis vs Environmental Variance",
       x="% Better Parent Heterosis", y='Environmental Variance') +
  theme(text = element_text(size = 14),
        axis.text=element_text(size = 14))
dev.off

FGHet$BPH <- NULL
Melted <- melt(FGHet, id = c("Trait", "Group"))

png("InbredvHybridMeasures.png", width=800, height=600)
ggplot(Melted, aes(y=value, x = Group, color = Group, fill = Group)) + 
  geom_col(position = "dodge") + 
  facet_wrap(vars(variable), scales = "free") + 
  labs(title="Inbreds v Hybrids",
       x="", y='') +
  theme(text = element_text(size = 14),
        axis.text=element_text(size = 14), 
        legend.position = "none") 
dev.off()
