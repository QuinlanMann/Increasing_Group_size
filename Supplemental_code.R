library(tidyverse)
library(glmmTMB)
library(gridExtra)
library(sjPlot)

#PCA
#bringing in the "best first date" data and morphometrics
PCA_DAT<-read.csv("C:/Users/quinl/Desktop/ANI/PCA_DATA.csv")

#PCA results, and inserting PC1 back to the main dataset
PCA_DAT$PC1<-prcomp(PCA_DAT[11:13], scale = TRUE)[["x"]][,1]
theme_set(theme_few())

#PCA values for all PCs
pca_dat <- prcomp(PCA_DAT[11:13], scale = TRUE)

#looking at the R^2 of linear models
summary(lm(pca_dat[["x"]][,1]~(PCA_DAT$DATE)))
summary(lm(PCA_DAT$TARSUS~PCA_DAT$DATE))
summary(lm(PCA_DAT$CULMEN~PCA_DAT$DATE))
summary(lm(PCA_DAT$MASS~PCA_DAT$DATE))

#quantiles for age classification
PCA_DAT%>%
  group_by(DATE)%>%
  summarise_at(vars(PC1),
               list(min=min, 
                    Q1=~quantile(., probs = 0.25),
                    median=median, 
                    Q3=~quantile(., probs = 0.75),
                    max=max))

#Setup for Jackknife Leave one out Cross validation
library(sampling)

N=length(PCA_DAT$TARSUS)

jack.tars<-numeric(N)
jack.int1<-numeric(N)
jack.rsq1<-numeric(N)
predictions.tars<-NULL
jack.pc1<-numeric(N)
jack.int2<-numeric(N)
jack.rsq2<-numeric(N)
predictions.pc1<-NULL
jack.culm<-numeric(N)
jack.int3<-numeric(N)
jack.rsq3<-numeric(N)
predictions.culm<-NULL

for (i in 1:N) {
  
  validation<-PCA_DAT[i,]
  training<-PCA_DAT[-i,]
  
  model<-lm(DATE~TARSUS, data=training)
  jack.tars[i]<-coef(model)[2] 
  jack.int1[i]<-coef(model)[1]
  jack.rsq1[i]<-summary(model)$adj.r.squared
  predictions.tars[i]<-predict(model, newdata=validation)
  
  model<-lm(DATE~PC1, data=training)
  jack.pc1[i]<-coef(model)[2] 
  jack.int2[i]<-coef(model)[1]
  jack.rsq2[i]<-summary(model)$adj.r.squared
  predictions.pc1[i]<-predict(model, newdata=validation)
  
  model<-lm(DATE~CULMEN, data=training)
  jack.culm[i]<-coef(model)[2] 
  jack.int3[i]<-coef(model)[1]
  jack.rsq3[i]<-summary(model)$adj.r.squared
  predictions.culm[i]<-predict(model, newdata=validation)
  
}

#generating bias corrected measure of slope and intercept based off Jackknifed values.
bias_pc1<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[2]) - (N - 1)*mean(jack.pc1)
bias_int2<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[1]) - (N - 1)*mean(jack.int2)

#summary of model comparing Date with PC1
tidy(lm(DATE~PC1, data=PCA_DAT))
summary(lm(DATE~PC1, data=PCA_DAT))

#plot of regression lines from JKLOOCV showing the variation in data after omitting
tiff(file="C:/Users/quinl/Desktop/ANI/Regression.tiff", width=4, height=4, units="in", res=600)
ggplot(PCA_DAT,aes(PC1, DATE))+
  geom_jitter(width=0, height=0.1)+
  geom_abline(slope = jack.pc1, intercept = jack.int2, lty=2)+
  geom_abline(slope = bias_pc1, intercept = bias_int2, color="red")+
  labs(x="PC1 values", y="Known age values", title="")+
  theme(text = element_text(size = 12, color="black"),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color="black"),
        axis.ticks = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.title=element_text(color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "white", fill=NA, size=1))
dev.off()

#plotting the error associated with JKLOOCV
#Predicted date - actual date
pc1.error<-(predictions.pc1-PCA_DAT$DATE)
dd <- with(density(pc1.error), data.frame(x, y))

#Jackknife-leave one out Plot
tiff(file="C:/Users/quinl/Desktop/ANI/DENSITY.tiff", width=4, height=3, units="in", res=600)
qplot(data=dd, x, y, geom="line")+
  geom_ribbon(data=subset(dd, x>-1.5 & x<1.5), aes(ymax=y), ymin=0,
              fill="black", colour=NA, alpha=0.25)+
  scale_y_continuous(expand=expansion(mult=c(0, 0.05)))+
  scale_x_continuous(expand=c(0, 0), breaks=c(-3,-2,-1,0,1,2,3))+
  labs(x="Difference from known date", y="Density", title="")+
  theme(text = element_text(size = 12, color="black"),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color="black"),
        axis.ticks = element_line(color="black"),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.title=element_text(color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "white", fill=NA, size=1))
dev.off()
