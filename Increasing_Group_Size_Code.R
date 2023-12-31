library(tidyverse)
library(glmmTMB)
library(gridExtra)
library(sjPlot)
library(broom.mixed)
library(bbmle)
library(performance)
library(see)
library(ggeffects)
library(kableExtra)
library(factoextra)
library(ggthemes)
library(reshape2)
library(cowplot)

#data import and cleanup, ensuring that all columns are appropriate types
ANI<-(read.csv("C:/Users/quinl/Desktop/ANI/NEST_SUMMARY_ANIS.csv")	%>% 
        mutate(NULL, 
               ADULTS=as.numeric(ADULTS), 
               EGGS_BURIED=as.numeric(EGGS_BURIED),
               EGGS_BURIED=as.numeric(EGGS_BURIED), 
               EGGS_UNBURIED=as.numeric(EGGS_UNBURIED),
               HATCHED=as.numeric(HATCHED), 
               FLEDGED=as.numeric(FLEDGED),
               TOT_EGGS=as.numeric(TOT_EGGS),
               LOCATION=as.factor(LOCATION),
               YEAR=as.factor(YEAR), 
               SITE=as.factor(SITE)	
        ))

ANI$EpEg<-(ANI$EGGS_UNBURIED/ANI$TOT_EGGS)	
ANI$HpEg<-(ANI$HATCHED/ANI$EGGS_UNBURIED)	
ANI$FpHa<-(ANI$FLEDGED/ANI$HATCHED)

#removes the missing values
ANI<- ANI[!(is.na(ANI$ADULTS)), ] 
ANI<- ANI[! (ANI$ADULTS==1), ] #removes the record of 1 adult from the dataset
ANI<-ANI %>% #removes the unknown groups
  group_by(SITE) %>%
  filter(n() > 12)

ANI2<-ANI%>%
  group_by(YEAR, LOCATION, SITE, ADULTS)%>%
  summarize(TOT_EGGS=sum(TOT_EGGS, na.rm=T), 
            EGGS_BURIED=sum(EGGS_BURIED, na.rm=T), 
            EGGS_UNBURIED=sum(EGGS_UNBURIED, na.rm=T), 
            HATCHED=sum(HATCHED, na.rm=T), 
            FLEDGED=sum(FLEDGED, na.rm=T), 
            NESTS=length(NEST_ATTMPT))

#if the number of 
ANI2<-ANI2 %>% mutate(FLEDGED = ifelse(FLEDGED == 0 && HATCHED == 0, NA, FLEDGED),
                      HATCHED = ifelse(HATCHED == 0 && EGGS_UNBURIED == 0, NA, HATCHED), 
                      EGGS_UNBURIED = ifelse(EGGS_UNBURIED == 0 && TOT_EGGS == 0, NA, EGGS_UNBURIED), 
                      TOT_EGGS = ifelse(TOT_EGGS == 0, NA, TOT_EGGS))

ANI$FEM<-floor(ANI$ADULTS/2)
ANI2$FEM<-floor(ANI2$ADULTS/2)

#settign the theme for ggplots
theme_set(ggthemes::theme_few())

#getting data from original for new datasets that omit zeros, NAs, and Infinte values
ANI_EpEg <- data.frame(ANI2$ADULTS, ANI2$EGGS_UNBURIED, ANI2$TOT_EGGS, ANI2$YEAR, ANI2$SITE, ANI2$LOCATION, ANI2$NESTS)
ANI_EpEg$ANI2.EpEg<-ANI_EpEg$ANI2.EGGS_UNBURIED/ANI_EpEg$ANI2.TOT_EGGS
ANI_EpEg <- ANI_EpEg[!is.na(ANI_EpEg$ANI2.EpEg), ] #removes NAs
ANI_EpEg <- ANI_EpEg[!(ANI_EpEg$ANI2.EpEg>1),] #removes the infinity values generated by dividing by zero

ANI_HpEg <- data.frame(ANI2$ADULTS, ANI2$EGGS_UNBURIED, ANI2$HATCHED, ANI2$YEAR, ANI2$SITE, ANI2$LOCATION, ANI2$NESTS)
ANI_HpEg$ANI2.HpEg<-ANI_HpEg$ANI2.HATCHED/ANI_HpEg$ANI2.EGGS_UNBURIED
ANI_HpEg <- ANI_HpEg[!is.na(ANI_HpEg$ANI2.HpEg), ] #removes NAs
ANI_HpEg <- ANI_HpEg[!(ANI_HpEg$ANI2.HpEg>1),] #removes the infinity values generated by dividing by zero

ANI_FpHa <- data.frame(ANI2$ADULTS, ANI2$HATCHED, ANI2$FLEDGED, ANI2$YEAR, ANI2$SITE, ANI2$LOCATION, ANI2$NESTS)
ANI_FpHa$ANI2.FpHa<-ANI_FpHa$ANI2.FLEDGED/ANI_FpHa$ANI2.HATCHED
ANI_FpHa <- na.omit(ANI_FpHa) #removes NAs
ANI_FpHa <- ANI_FpHa[!(ANI_FpHa$ANI2.FpHa>1),]

#PCA
#bringing in the "best first date" data and morphometrics
PCA_DAT<-read.csv("C:/Users/quinl/Desktop/ANI/PCA_DATA.csv")

#PCA results, and inserting PC1 back to the main dataset
PCA_DAT$PC1<-prcomp(PCA_DAT[11:13], scale = TRUE)[["x"]][,1]
theme_set(theme_few())

#PCA values for all PCs
pca_dat <- prcomp(PCA_DAT[11:13], scale = TRUE)

#looking at the R^2 of lienar models
summary(lm(pca_dat[["x"]][,1]~as.factor(PCA_DAT$DATE)))
summary(lm(PCA_DAT$DATE~PCA_DAT$TARSUS))
summary(lm(PCA_DAT$CULMEN~PCA_DAT$DATE))
summary(lm(PCA_DAT$MASS~PCA_DAT$DATE))


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

#plotting the error associated with Leave one out cross-validation (LOOCV)
pc1.error<-(predictions.pc1-PCA_DAT$DATE)

#Generating Skew and Span Dataset from PCA
#bias corrected measures
bias_pc1<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[2]) - (N - 1)*mean(jack.pc1)
bias_int2<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[1]) - (N - 1)*mean(jack.int2)

#bringing in the unknown aged chicks
UNK_DATE<-read.csv("C:/Users/quinl/Desktop/ANI/METRICS_UNK_AGE.csv")
names(UNK_DATE) <- toupper(names(UNK_DATE))
UNK_DATE$TARSUS <- as.double(UNK_DATE$TARSUS)
UNK_DATE$MASS <- as.double(UNK_DATE$MASS)
UNK_DATE<-na.omit(UNK_DATE)
UNK_DATE$AGE<-NA

#center for scaling for PC1
UNK_DATE$TARSUS_s<-(UNK_DATE$TARSUS-mean(PCA_DAT$TARSUS))/sd(PCA_DAT$TARSUS)
UNK_DATE$CULMEN_s<-(UNK_DATE$CULMEN-mean(PCA_DAT$CULMEN))/sd(PCA_DAT$CULMEN)
UNK_DATE$MASS_s<-(UNK_DATE$MASS-mean(PCA_DAT$MASS))/sd(PCA_DAT$MASS)

#multiplying by the calculated rotations from the PCA of known data to get the PC1 value
UNK_DATE$PC1<-
  (UNK_DATE$TARSUS_s*pca_dat[["rotation"]][1])+
  (UNK_DATE$CULMEN_s*pca_dat[["rotation"]][2])+
  (UNK_DATE$MASS_s*pca_dat[["rotation"]][3])


#function to generate ages
#x is the data you want to extrapolate dates
#z is the variable you want regressed against date to generate the linear model
INTER_AGE_j<-function(coeff, data, intercept){
  w = coeff*data + intercept
  return(w)
}

#use the bias corrected estimates from the JACKKNIFE dataset
#UNK_DATE$AGE_TCONT_j <- round(INTER_AGE_j(bias_tars, UNK_DATE$TARSUS, bias_int1))                   
UNK_DATE$AGE_PCONT_j <- round(INTER_AGE_j(bias_pc1, UNK_DATE$PC1, bias_int2))
#UNK_DATE$AGE_CCONT_j <- round(INTER_AGE_j(bias_culm, UNK_DATE$CULMEN, bias_int3))
UNK_DATE<-UNK_DATE[-c(7:11)]

#trying to get data from extrapolated stuff
#reordering the dataframe so that within 
UNK_DATE<-UNK_DATE[with(UNK_DATE, order(TERRITORY,DATE, CHICK)), ]
#removing the YEAR variable to assign "Year territory" name and get hatching date
UNK_DATE$DATE <- as.Date(UNK_DATE$DATE, format = "%Y-%m-%d")
#subtracting days age from hatching date
UNK_DATE$HATCH <- UNK_DATE$DATE-(UNK_DATE$AGE_PCONT_j-1)

#giving "year territory" name
UNK_DATE$YEAR <- format(UNK_DATE$HATCH, format = "%Y")
UNK_DATE$GRP <- paste(UNK_DATE$YEAR, UNK_DATE$TERRITORY)

library(data.table)

succ<-(read.csv("C:/Users/quinl/Desktop/ANI/SUCCESSFUL_NESTS.csv")	%>% 
         mutate(NULL, 
                ADULTS=as.numeric(ADULTS), 
                EGGS_BURIED=as.numeric(EGGS_BURIED),
                EGGS_BURIED=as.numeric(EGGS_BURIED), 
                EGGS_UNBURIED=as.numeric(EGGS_UNBURIED),
                HATCHED=as.numeric(HATCHED), 
                FLEDGED=as.numeric(FLEDGED),
                TOT_EGGS=as.numeric(TOT_EGGS),
                LOCATION=as.factor(LOCATION),
                YEAR=as.factor(YEAR), 
                SITE=as.factor(SITE)	
         ))

#new dataset just of entries with accurate chickdates
suc<-subset(succ, SPAN>0)

#female group size instead of #adults
suc$FEM<-floor(suc$ADULTS/2)

suc$GRP<-paste(suc$YEAR, suc$LOCATION)

#from DOI : 10.1098/rspb.2018.1452
#ti = time since beginning of span
#tm = time at the middle of the span
#p = proportion at that time

skew2<-function(ti, tm, p){
  skew<-((ti-tm)/(tm))*p
  print(skew)
}

suc$SPAN<-as.numeric(suc$SPAN)
suc$skew1<-skew2(1, (suc$SPAN/2), (suc$NEW.CHCKS_DAY1/suc$HATCHED))  
suc$skew2<-skew2(2, (suc$SPAN/2), (suc$NEW.CHCKS_DAY2/suc$HATCHED))  
suc$skew3<-skew2(3, (suc$SPAN/2), (suc$NEW.CHCKS_DAY3/suc$HATCHED))  
suc$skew4<-skew2(4, (suc$SPAN/2), (suc$NEW.CHCKS_DAY4/suc$HATCHED))  
suc$skew5<-skew2(5, (suc$SPAN/2), (suc$NEW.CHCKS_DAY5/suc$HATCHED))  
suc$skew6<-skew2(6, (suc$SPAN/2), (suc$NEW.CHCKS_DAY6/suc$HATCHED))  
suc$skew7<-skew2(7, (suc$SPAN/2), (suc$NEW.CHCKS_DAY7/suc$HATCHED))  

#sums all the values to calculate skew metric
suc$SKEW<-apply(suc[23:29], 1, sum, na.rm=T)

#return to work with Unknown chick ages
#removes duplicate measures of each chick in the group and keeps the first instance
SKEW_1<-setDT(UNK_DATE)[ , .SD[(!duplicated(CHICK))], by = GRP]

SKEW_1<-SKEW_1%>%
  group_by(GRP)%>%
  summarise(DATE_DIFF=as.numeric(HATCH-min(HATCH))+1,
            SPAN=max(DATE_DIFF))
SKEW_1<-SKEW_1%>%
  group_by(GRP, DATE_DIFF, SPAN)%>%
  summarise(n=n())

#reshaping data to be able to calculate the skew values
SKEW_1<-reshape2::dcast(SKEW_1, GRP+SPAN~DATE_DIFF)

SKEW_SUCC<-succ[1:11]

SKEW_SUCC$GRP <- paste(SKEW_SUCC$YEAR, SKEW_SUCC$LOCATION)

#merging data on group success with unknown hatching data
SKEW_1<-merge(SKEW_SUCC, SKEW_1, by="GRP")

SKEW_1$skew1<-skew2(1, (SKEW_1$SPAN/2), (SKEW_1$`1`/SKEW_1$HATCHED))
SKEW_1$skew2<-skew2(2, (SKEW_1$SPAN/2), (SKEW_1$`2`/SKEW_1$HATCHED))
SKEW_1$skew3<-skew2(3, (SKEW_1$SPAN/2), (SKEW_1$`3`/SKEW_1$HATCHED))
SKEW_1$skew4<-skew2(4, (SKEW_1$SPAN/2), (SKEW_1$`4`/SKEW_1$HATCHED))
SKEW_1$skew5<-skew2(5, (SKEW_1$SPAN/2), (SKEW_1$`5`/SKEW_1$HATCHED))
SKEW_1$skew6<-skew2(6, (SKEW_1$SPAN/2), (SKEW_1$`6`/SKEW_1$HATCHED))
SKEW_1$skew7<-skew2(7, (SKEW_1$SPAN/2), (SKEW_1$`7`/SKEW_1$HATCHED))
SKEW_1$skew8<-skew2(8, (SKEW_1$SPAN/2), (SKEW_1$`8`/SKEW_1$HATCHED))
SKEW_1$skew10<-skew2(10, (SKEW_1$SPAN/2), (SKEW_1$`10`/SKEW_1$HATCHED))

SKEW_1$SKEW<-apply(SKEW_1[23:31], 1, sum, na.rm=T)

#creating empty columns so that I can merge the known and unknown datasets together without issue
suc$NEW.CHCKS_DAY8<-NA
suc$NEW.CHCKS_DAY10<-NA

#merging known and unknown aged chick datasets together
SKEW_DATA<-rbind(SKEW_1[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", 
                           "1", "2", "3", "4",  "5", "6", "7", "8", "10", "SPAN", "SKEW")], 
                 setnames((suc[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", "NEW.CHCKS_DAY1", 
                                  "NEW.CHCKS_DAY2", "NEW.CHCKS_DAY3", "NEW.CHCKS_DAY4", "NEW.CHCKS_DAY5", 
                                  "NEW.CHCKS_DAY6", "NEW.CHCKS_DAY7", "NEW.CHCKS_DAY8", "NEW.CHCKS_DAY10", "SPAN", "SKEW")]), 
                          names((SKEW_1[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", 
                                          "1", "2", "3", "4",  "5", "6", "7", "8", "10", "SPAN", "SKEW")])))
)

SKEW_DATA<-SKEW_DATA%>%distinct(GRP, .keep_all=T)
SKEW_DATA$FpHa<-SKEW_DATA$FLEDGED/SKEW_DATA$HATCHED
SKEW_DATA$FEM<-floor(SKEW_DATA$ADULTS/2)

#Models
Tot.p.1<- glmmTMB(TOT_EGGS  ~FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),
                     data=ANI2, 	
                     family = poisson(link="log"))	
Inc.p.zi.1<- glmmTMB(EGGS_UNBURIED ~ FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),
                     data=ANI2, 	
                     ziformula = ~1,	
                     family = poisson(link="log"))	
Hatc.p.1<- glmmTMB(HATCHED ~ FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),
                     data=ANI2, 	
                     family = poisson(link="log"))	
Fled.p.1<- glmmTMB(FLEDGED ~ FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),
                     data=ANI2, 	
                     family = poisson(link="log"))

Tot.nb2.1<-glmmTMB(TOT_EGGS~FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),
                     family="nbinom2", 	
                     data = ANI2)	
Inc.nb2.zi.1<-glmmTMB(EGGS_UNBURIED~FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),	
                     family="nbinom2", 	
                     ziformula = ~1, 	
                     data = ANI2)	
Hatc.nb2.1<-glmmTMB(HATCHED~FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),	
                     family="nbinom2", 	
                     data = ANI2)	
Fled.nb2.1<-glmmTMB(FLEDGED~FEM+NESTS+SITE+(1|SITE:LOCATION)+(1|YEAR),	
                     family="nbinom2", 	
                     data = ANI2)	

Tot.nb2.2<-glmmTMB(TOT_EGGS~FEM+NESTS+SITE+offset(log(FEM))+(1|SITE:LOCATION)+(1|YEAR),	
                      family="nbinom2", 	
                      data = ANI2)	
Inc.nb2.zi.2<-glmmTMB(EGGS_UNBURIED~FEM+NESTS+SITE+offset(log(FEM))+(1|SITE:LOCATION)+(1|YEAR),	
                      family="nbinom2", 	
                      ziformula = ~1, 	
                      data = ANI2)	
Hatc.p.2<- glmmTMB(HATCHED ~ FEM+NESTS+SITE+offset(log(FEM))+(1|SITE:LOCATION)+(1|YEAR),	
                     data=ANI2, 	
                     family = poisson(link="log"))	
Fled.p.2<- glmmTMB(FLEDGED ~ FEM+NESTS+SITE+offset(log(FEM))+(1|SITE:LOCATION)+(1|YEAR),	
                     data=ANI2, 	
                     family = poisson(link="log"))

## Binomial models of successes in reproduction measures
ANI_EpEg$FEM<-floor(ANI_EpEg$ANI2.ADULTS/2)
EpEg.b.1 <- glmmTMB(ANI2.EpEg ~ FEM+ANI2.NESTS+ANI2.SITE+(1|ANI2.SITE:ANI2.LOCATION)+(1|ANI2.YEAR), 
            data=ANI_EpEg, 
            weights = ANI2.TOT_EGGS,
            family = binomial(link = "logit"))	
	
ANI_HpEg$FEM<-floor(ANI_HpEg$ANI2.ADULTS/2)
HpEg.b.1 <- glmmTMB(ANI2.HpEg~ FEM+ANI2.NESTS+ANI2.SITE+(1|ANI2.SITE:ANI2.LOCATION)+(1|ANI2.YEAR), 	
            data=ANI_HpEg, 	
            weights = ANI2.EGGS_UNBURIED,
            family = binomial(link = "logit"))	
	
ANI_FpHa$FEM<-floor(ANI_FpHa$ANI2.ADULTS/2)
FpHa.b.1 <- glmmTMB(ANI2.FpHa ~ FEM+ANI2.NESTS+ANI2.SITE+(1|ANI2.SITE:ANI2.LOCATION)+(1|ANI2.YEAR), 
            data=ANI_FpHa, 
            weights=ANI2.HATCHED,
            family = binomial(link = "logit"))

#SKEW and SPAN model
skew_mod5<-glmmTMB(cbind(FLEDGED, HATCHED-FLEDGED) ~ SPAN*SKEW+FEM+HATCHED+(1|LOCATION)+(1|YEAR), 
                   data=SKEW_DATA, 
                   family = binomial(link = "logit"))
#Tables and visualization
kbl(
  cbind(
    (rbind(
      (tidy(Tot.nb2.1)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.1)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.1)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Fled.p.1)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))))),
    (rbind(
      (tidy(Tot.nb2.2)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.2)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.2)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept"))),
      (tidy(Fled.p.2)%>%
      mutate(term=recode(term, 
                         `FEM`="Female group size",
                         `(Intercept)`="Intercept")))))
  )[-c(1,2,9:12)],
  col.names = c("grp", "Term", "Estimate", "Std. Err.", "z value", "p value","Estimate", "Std. Err.", "z value", "p value"), 
  caption="summary of probability models shown in log(odds) scale",
  booktabs = T)%>%
  pack_rows(index = c("Eggs laid"= 6,
                      "incubation" = 6, 
                      "hatching" = 6, 
                      "fledging" = 6))%>%
  kable_styling(latex_options = "HOLD_position")


grid.arrange(
  plot(ggpredict(Tot.nb2.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, TOT_EGGS), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 30, 60, 90))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs laid", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      axis.title=element_text(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  plot(ggeffect(Tot.nb2.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, TOT_EGGS/FEM), 
                color="black",
                fill="black",
                stroke=0, 
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 10, 20, 30))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs laid per capita", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(Inc.nb2.zi.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, EGGS_UNBURIED), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 10, 20, 30))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs incubated", title="c")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      axis.title=element_text(color="black"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggeffect(Inc.nb2.zi.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, EGGS_UNBURIED/FEM), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs incubated per capita", title="d")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.title=element_text(color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(Hatc.p.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, HATCHED), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 3, 6, 9, 12))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs hatched", title="e")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.title=element_text(color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggeffect(Hatc.p.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, HATCHED/FEM), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10))+
  scale_x_continuous(breaks=c(2,4,6,8,10))+
  labs(x="Female group size", y="Eggs hatched per capita", title="f")+
    theme(
      axis.title=element_text(color="black"),
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
plot(ggpredict(Fled.p.1, 
               terms ="FEM"))+
  geom_jitter(data=ANI2, aes(FEM, FLEDGED), 
              color="black",
              fill="black",
              stroke=0,
              width=0.075, 
              height=0.25, 
              pch=21, 
              size=3)+  
  coord_cartesian(xlim = c(1,10))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  scale_x_continuous(breaks=c(2,4,6,8,10))+
  labs(x="Female group size", y="Chicks fledged", title="g")+
  theme(
    text = element_text(family="Arial", size = 12, color="black"),
    axis.text.x = element_text(size=12, color = "black"),
    axis.text.y = element_text(size=12, color="black"),
    axis.ticks = element_line(color="black"),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title=element_text(color="black"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "white", fill=NA, size=1)),
plot(ggeffect(Fled.p.2, 
              terms ="FEM", 
              offset=0))+
  geom_jitter(data=ANI2, aes(FEM, FLEDGED/FEM), 
              color="black",
              fill="black",
              stroke=0,
              width=0.075, 
              height=0.25, 
              pch=21, 
              size=3)+
  coord_cartesian(xlim = c(1,10))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  scale_x_continuous(breaks=c(2,4,6,8,10))+
  labs(x="Female group size", y="Chicks fledged per capita", title="h")+
  theme(
    text = element_text(family="Arial", size = 12, color="black"),
    axis.text.x = element_text(size=12, color = "black"),
    axis.text.y = element_text(size=12, color="black"),
    axis.title=element_text(color="black"),
    axis.ticks = element_line(color="black"),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "white", fill=NA, size=1)),
ncol=2)

kbl(rbind(
  (tidy(EpEg.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site"))), 
  (tidy(HpEg.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site"))), 
  (tidy(FpHa.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site")))), 
  col.names = c("grp", "Term", "Estimate", "Std. Err.", "z value", "p value"), 
  caption="summary of probability models shown in log(odds) scale",
  booktabs = T)%>%
  pack_rows(index = c("Pr(incubation)" = 6, 
                      "Pr(hatching)" = 6, 
                      "Pr(fledging)" = 6))%>%
  kable_styling(latex_options = "HOLD_position")


grid.arrange(
  plot(ggpredict(EpEg.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_EpEg, aes(FEM, ANI2.EpEg), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075,
                pch=21, 
                size=3)+  
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Percent incubated", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.title=element_text(color="black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(HpEg.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_HpEg, aes(FEM, ANI2.HpEg), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075,
                pch=21, 
                size=3)+
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Percent hatching", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(FpHa.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_FpHa, aes(FEM, ANI2.FpHa), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                pch=21, 
                size=3)+
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Percent fledging", title="c")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      axis.title=element_text(color="black"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  ncol=1)

#skew model visulization and table
grid.arrange(
  plot(ggpredict(skew_mod5, 
               terms ="SPAN"))+
  geom_jitter(data=SKEW_DATA, aes(SPAN, FpHa), 
              color="black",
              fill="black",
              stroke=0,
              width=0.075,
              pch=21, 
              size=3)+
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10))+
  labs(x="Hatching span (days)", y="Percent fledging", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  plot(ggpredict(skew_mod5, 
                 terms ="SKEW"))+
    geom_jitter(data=SKEW_DATA, aes(SKEW, FpHa), 
                color="black",
                fill="black",
                stroke=0,
                width=0.05,
                pch=21, 
                size=3)+
    labs(x="Skew index", y="Percent fledging", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  ncol=1)

tidy(skew_mod5)[-c(1,2)]
#End of code