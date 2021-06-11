#notes----
#June 2017
#Inseason Models (Terminal)
rm(list=ls(all=TRUE))
#load----
library(MuMIn)
library(dplyr)
library(readxl)
library(tidyverse)
library(stargazer)
library(tidyverse)
library(extrafont)
library(car)
#data----
Data <- read_excel('data/Linear_Regressions_Data.xlsx', sheet="Outside")
#data clean----
Data %>% 
  mutate(Year = factor(Year),
         log.StatWeek = as.factor(log(StatWeek)),
         log.TotalRunTah = as.numeric(log(TotalRunTah)),
         log.CPUE10641Tah = as.numeric(log(CPUE10641Tah+0.00001)),
         log.Catch10641Tah = as.numeric(log(Catch10641Tah+0.00001)),
         log.CPUE108Tah = as.numeric(log(CPUE108Tah+0.00001)),
         log.Catch108Tah = as.numeric(log(Catch108Tah+0.00001)),
         log.TotalRunTuya = as.numeric(log(TotalRunTuya)),
         log.CPUE10641Tuya = as.numeric(log(CPUE10641Tuya+0.00001)),
         log.Catch10641Tuya = as.numeric(log(Catch10641Tuya+0.000001)),
         log.CPUE108Tuya = as.numeric(log(CPUE108Tuya+0.00001)),
         log.Catch108Tuya = as.numeric(log(Catch108Tuya+0.00001)),
         log.TotalRun = as.numeric(log(TotalRun)),
         log.CPUE10641 = as.numeric(log(CPUE10641+0.00001)),
         log.Catch10641 = as.numeric(log(Catch10641+0.00001)),
         log.CPUE108 = as.numeric(log(CPUE108+0.00001)),
         log.Catch108 = as.numeric(log(Catch108+0.00001)),
         Weight = as.numeric(Weight))->Data.Clean
drop.cols.Tah <- c('log.TotalRunTuya', 'TotalRunTuya','log.CPUE10641Tuya', 'CPUE10641Tuya',
               'log.Catch10641Tuya', 'Catch10641Tuya','log.CPUE108Tuya', 'CPUE108Tuya',
               'log.Catch108Tuya', 'Catch108Tuya','log.TotalRun', 'TotalRun','log.CPUE10641', 
               'CPUE10641','log.Catch10641', 'Catch10641','log.CPUE108', 'CPUE108',
               'log.Catch108', 'Catch108') 
Data.Clean %>% select(-one_of(drop.cols.Tah))->TahData

drop.cols.Tuya <- c('log.TotalRunTah', 'TotalRunTah','log.CPUE10641Tah', 'CPUE10641Tah',
                   'log.Catch10641Tah', 'Catch10641Tah','log.CPUE108Tah', 'CPUE108Tah',
                   'log.Catch108Tah', 'Catch108Tah','log.TotalRun', 'TotalRun','log.CPUE10641', 
                   'CPUE10641','log.Catch10641', 'Catch10641','log.CPUE108', 'CPUE108',
                   'log.Catch108', 'Catch108') 
Data.Clean %>% select(-one_of(drop.cols.Tuya))->TuyaData

drop.cols  <- c('log.TotalRunTah', 'TotalRunTah','log.CPUE10641Tah', 'CPUE10641Tah',
                    'log.Catch10641Tah', 'Catch10641Tah','log.CPUE108Tah', 'CPUE108Tah',
                    'log.Catch108Tah', 'Catch108Tah','log.TotalRunTuya', 'TotalRunTuya','log.CPUE10641Tuya', 
                    'CPUE10641Tuya','log.Catch10641Tuya', 'Catch10641Tuya','log.CPUE108Tuya', 'CPUE108Tuya',
                    'log.Catch108Tuya', 'Catch108Tuya') 
Data.Clean %>% select(-one_of(drop.cols))->StiData      


#analysis-----
#Determine if the data is normally distributed (p should be >0.05)
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}

eda.norm(TahData$log.TotalRunTah)
eda.norm(TuyaData$log.TotalRunTuya)
eda.norm(StiData$log.TotalRun)

#results----
#Run Models (Tahltan-D106-41)
TahData.10641 <- TahData[!(TahData$Year %in% c("2007")), ]
Tah.Catch10641.1 <- lm(formula = log.TotalRunTah ~ poly(log.Catch10641Tah,2)+log.StatWeek,data=TahData.10641, weights=Weight) 
Tah.Catch10641.2 <- lm(formula = log.TotalRunTah ~ poly(log.Catch10641Tah,2)*log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.Catch10641.3 <- lm(formula = log.TotalRunTah ~ log.Catch10641Tah+log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.Catch10641.4 <- lm(formula = log.TotalRunTah ~ log.Catch10641Tah*log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.CPUE10641.1 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE10641Tah,2)+log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.CPUE10641.2 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE10641Tah,2)*log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.CPUE10641.3 <- lm(formula = log.TotalRunTah ~ log.CPUE10641Tah+log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.CPUE10641.4 <- lm(formula = log.TotalRunTah ~ log.CPUE10641Tah*log.StatWeek,data=TahData.10641,weights=Weight)

stargazer(Tah.Catch10641.1, Tah.Catch10641.2, Tah.Catch10641.3, Tah.Catch10641.4, type="text", out="results/Terminal/Tah.D10641.Models.Catch.htm")#save results to folder
stargazer(Tah.CPUE10641.1, Tah.CPUE10641.2, Tah.CPUE10641.3, Tah.CPUE10641.4, type="text", out="results/Terminal/Tah.D10641.Models.CPUE.htm")#save results to folder

#Run Models (Tahltan-D108)
TahData.108 <- TahData[!(TahData$Year %in% c("1985", "2001", "2002","2003")), ]
Tah.Catch108.1 <- lm(formula = log.TotalRunTah ~ poly(log.Catch108Tah,2)+log.StatWeek,data=TahData.108 , weights=Weight) 
Tah.Catch108.2 <- lm(formula = log.TotalRunTah ~ poly(log.Catch108Tah,2)*log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.Catch108.3 <- lm(formula = log.TotalRunTah ~ log.Catch108Tah+log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.Catch108.4 <- lm(formula = log.TotalRunTah ~ log.Catch108Tah*log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.CPUE108.1 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE108Tah,2)+log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.CPUE108.2 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE108Tah,2)*log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.CPUE108.3 <- lm(formula = log.TotalRunTah ~ log.CPUE108Tah+log.StatWeek,data=TahData.108 ,weights=Weight) 
Tah.CPUE108.4 <- lm(formula = log.TotalRunTah ~ log.CPUE108Tah*log.StatWeek,data=TahData.108 ,weights=Weight)

stargazer(Tah.Catch108.1, Tah.Catch108.2, Tah.Catch108.3, Tah.Catch108.4, type="text", out="results/Terminal/Tah.D108.Models.Catch.htm")#save results to folder
stargazer(Tah.CPUE108.1, Tah.CPUE108.2, Tah.CPUE108.3, Tah.CPUE108.4, type="text", out="results/Terminal/Tah.D108.Models.CPUE.htm")#save results to folder

AICcResults<-data.frame (AICc(Tah.Catch108.1,Tah.Catch108.2,Tah.Catch108.3,Tah.Catch108.4,
                              Tah.CPUE108.1,Tah.CPUE108.2,Tah.CPUE108.3,Tah.CPUE108.4,Tah.Catch10641.1,Tah.Catch10641.2,Tah.Catch10641.3,Tah.Catch10641.4,
                              Tah.CPUE10641.1,Tah.CPUE10641.2,Tah.CPUE10641.3,Tah.CPUE10641.4))
write.csv(AICcResults, file="results/Terminal/AICc_Tah.csv") #save AICc results

#Run Models (Stikine-D106-41)
StiData.10641 <- StiData[!(StiData$Year %in% c("2007")), ]
Catch10641.1 <- lm(formula = log.TotalRun ~ poly(log.Catch10641,2)+log.StatWeek,data=StiData.10641, weights=Weight) 
Catch10641.2 <- lm(formula = log.TotalRun ~ poly(log.Catch10641,2)*log.StatWeek,data=StiData.10641,weights=Weight) 
Catch10641.3 <- lm(formula = log.TotalRun ~ log.Catch10641+log.StatWeek,data=StiData.10641,weights=Weight) 
Catch10641.4 <- lm(formula = log.TotalRun ~ log.Catch10641*log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.1 <- lm(formula = log.TotalRun ~ poly(log.CPUE10641,2)+log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.2 <- lm(formula = log.TotalRun ~ poly(log.CPUE10641,2)*log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.3 <- lm(formula = log.TotalRun ~ log.CPUE10641+log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.4 <- lm(formula = log.TotalRun ~ log.CPUE10641*log.StatWeek,data=StiData.10641,weights=Weight)

stargazer(Catch10641.1, Catch10641.2, Catch10641.3, Catch10641.4, type="text", out="results/Terminal/Sti.D10641.Models.Catch.htm")#save results to folder
stargazer(CPUE10641.1, CPUE10641.2, CPUE10641.3, CPUE10641.4, type="text", out="results/Terminal/Sti.D10641.Models.CPUE.htm")#save results to folder

#Run Models (Stikine-D108)
StiData.108 <- StiData[!(StiData$Year %in% c("1985", "2001", "2002","2003")), ]
Catch108.1 <- lm(formula = log.TotalRun ~ poly(log.Catch108,2)+log.StatWeek,data=StiData.108 , weights=Weight) 
Catch108.2 <- lm(formula = log.TotalRun ~ poly(log.Catch108,2)*log.StatWeek,data=StiData.108 ,weights=Weight) 
Catch108.3 <- lm(formula = log.TotalRun ~ log.Catch108+log.StatWeek,data=StiData.108 ,weights=Weight) 
Catch108.4 <- lm(formula = log.TotalRun ~ log.Catch108*log.StatWeek,data=StiData.108 ,weights=Weight) 
CPUE108.1 <- lm(formula = log.TotalRun ~ poly(log.CPUE108,2)+log.StatWeek,data=StiData.108 ,weights=Weight) 
CPUE108.2 <- lm(formula = log.TotalRun ~ poly(log.CPUE108,2)*log.StatWeek,data=StiData.108 ,weights=Weight) 
CPUE108.3 <- lm(formula = log.TotalRun ~ log.CPUE108+log.StatWeek,data=StiData.108 ,weights=Weight) 
CPUE108.4 <- lm(formula = log.TotalRun ~ log.CPUE108*log.StatWeek,data=StiData.108 ,weights=Weight)

stargazer(Catch108.1, Catch108.2, Catch108.3, Catch108.4, type="text", out="results/Terminal/Sti.D108.Models.Catch.htm")#save results to folder
stargazer(CPUE108.1, CPUE108.2, CPUE108.3, CPUE108.4, type="text", out="results/Terminal/Sti.D108.Models.CPUE.htm")#save results to folder

AICcResults<-data.frame (AICc(Catch108.1,Catch108.2,Catch108.3,Catch108.4,
                              CPUE108.1,CPUE108.2,CPUE108.3,CPUE108.4,Catch10641.1,Catch10641.2,
                              Catch10641.3,Catch10641.4,CPUE10641.1,CPUE10641.2,CPUE10641.3,CPUE10641.4))
write.csv(AICcResults, file="results/Terminal/AICc_Sti.csv") #save AICc results

#Run Models (Tuya-D106-41)
TuyaData.10641 <- TuyaData[!(TuyaData$Year %in% c("2007", "2005", "1985","1986", "1987", "1988", "1989", "1990", "1991",
                                               "1992", "1993", "1994")), ]
Tuya.Catch10641.1 <- lm(formula = log.TotalRunTuya ~ poly(log.Catch10641Tuya,2)+log.StatWeek,data=TuyaData.10641, weights=Weight) 
Tuya.Catch10641.2 <- lm(formula = log.TotalRunTuya ~ poly(log.Catch10641Tuya,2)*log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.Catch10641.3 <- lm(formula = log.TotalRunTuya ~ log.Catch10641Tuya+log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.Catch10641.4 <- lm(formula = log.TotalRunTuya ~ log.Catch10641Tuya*log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.CPUE10641.1 <- lm(formula = log.TotalRunTuya ~ poly(log.CPUE10641Tuya,2)+log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.CPUE10641.2 <- lm(formula = log.TotalRunTuya ~ poly(log.CPUE10641Tuya,2)*log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.CPUE10641.3 <- lm(formula = log.TotalRunTuya ~ log.CPUE10641Tuya+log.StatWeek,data=TuyaData.10641,weights=Weight) 
Tuya.CPUE10641.4 <- lm(formula = log.TotalRunTuya ~ log.CPUE10641Tuya*log.StatWeek,data=TuyaData.10641,weights=Weight)
    #**best model structure same as Tah
stargazer(Tuya.Catch10641.1, type="text", out="results/Terminal/Tuya.Models.D10641.Catch.htm")#save results to folder
stargazer(Tuya.CPUE10641.1, type="text", out="results/Terminal/Tuya.Models.D10641.CPUE.htm")#save results to folder

#Run Models (Tuya-D108)
TuyaData.108 <- TuyaData[!(TuyaData$Year %in% c("2001", "2002", "2003","2005","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                  "1992", "1993", "1994")), ]
Tuya.Catch108.1 <- lm(formula = log.TotalRunTuya ~ poly(log.Catch108Tuya,2)+log.StatWeek,data=TuyaData.108 , weights=Weight) 
Tuya.Catch108.2 <- lm(formula = log.TotalRunTuya ~ poly(log.Catch108Tuya,2)*log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.Catch108.3 <- lm(formula = log.TotalRunTuya ~ log.Catch108Tuya+log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.Catch108.4 <- lm(formula = log.TotalRunTuya ~ log.Catch108Tuya*log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.CPUE108.1 <- lm(formula = log.TotalRunTuya ~ poly(log.CPUE108Tuya,2)+log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.CPUE108.2 <- lm(formula = log.TotalRunTuya ~ poly(log.CPUE108Tuya,2)*log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.CPUE108.3 <- lm(formula = log.TotalRunTuya ~ log.CPUE108Tuya+log.StatWeek,data=TuyaData.108 ,weights=Weight) 
Tuya.CPUE108.4 <- lm(formula = log.TotalRunTuya ~ log.CPUE108Tuya*log.StatWeek,data=TuyaData.108 ,weights=Weight)
    #**best model structure same as Tah
stargazer(Tuya.Catch108.1, type="text", out="results/Terminal/Tuya.Models.D108.Catch.htm")#save results to folder
stargazer(Tuya.CPUE108.1, type="text", out="results/Terminal/Tuya.Models.D108.CPUE.htm")#save results to folder
#determine best models based on output of results (AICc and regression results)

#figures----

#figures of best models based on AICc score 
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

#106-41 Catch Tah
TahData.10641 <- TahData[!(TahData$Year %in% c("2007")), ]
TuyaData.10641 <- TuyaData[!(TuyaData$Year %in% c("2007", "2005", "1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                  "1992", "1993", "1994")), ]
Tah.Catch10641.1 <- lm(formula = log.TotalRunTah ~ poly(log.Catch10641Tah,2)+log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.Catch10641.1a<- lm(formula = log.TotalRunTah ~ log.Catch10641Tah+I(log.Catch10641Tah^2)+log.StatWeek,data=TahData.10641,weights=Weight) 
Tuya.Catch10641.1a<- lm(formula = log.TotalRunTuya ~ log.Catch10641Tuya+I(log.Catch10641Tuya^2)+log.StatWeek,data=TuyaData.10641,weights=Weight) 

Tah.Catch10641.1a_coeff <- summary(Tah.Catch10641.1a)$coefficients[,1 ]
Tuya.Catch10641.1a_coeff <- summary(Tuya.Catch10641.1a)$coefficients[,1 ]
write.csv(Tah.Catch10641.1a_coeff, file="results/Terminal/Tah.Catch10641.1a_coeff.csv") 
write.csv(Tuya.Catch10641.1a_coeff, file="results/Terminal/Tuya.Catch10641.1a_coeff.csv") 

a<-exp(fitted(Tah.Catch10641.1))
TahData.10641$Catch10641Tah<-exp(TahData.10641$log.Catch10641Tah+0.000001)
TahData.10641$TotalRunTah<-exp(TahData.10641$log.TotalRunTah+0.000001)

Fitted<- data.frame(a, TahData.10641$StatWeek, TahData.10641$Catch10641Tah, TahData.10641$TotalRunTah)
y<-Fitted[order(Fitted$TahData.10641.StatWeek, Fitted$TahData.10641.Catch10641Tah),]
png("figures/Terminal/Fitted_D10641Tah_Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==26, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==27, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==28, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==29, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==30, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.Catch10641Tah, data=y, subset=TahData.10641.StatWeek==31, col=1)
dev.off()

#106-41 CPUE Tah
TahData.10641 <- TahData[!(TahData$Year %in% c("2007")), ]
TuyaData.10641 <- TuyaData[!(TuyaData$Year %in% c("2007", "2005", "1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                  "1992", "1993", "1994")), ]
Tah.CPUE10641.1 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE10641Tah,2)+log.StatWeek,data=TahData.10641,weights=Weight) 
Tah.CPUE10641.1a<- lm(formula = log.TotalRunTah ~ log.CPUE10641Tah+I(log.CPUE10641Tah^2)+log.StatWeek,data=TahData.10641,weights=Weight) 
Tuya.CPUE10641.1a<- lm(formula = log.TotalRunTuya ~ log.CPUE10641Tuya+I(log.CPUE10641Tuya^2)+log.StatWeek,data=TuyaData.10641,weights=Weight) 

Tah.CPUE10641.1a_coeff <- summary(Tah.CPUE10641.1a)$coefficients[,1 ]
Tuya.CPUE10641.1a_coeff <- summary(Tuya.CPUE10641.1a)$coefficients[,1 ]
write.csv(Tah.CPUE10641.1a_coeff, file="results/Terminal/Tah.CPUE10641.1a_coeff.csv") 
write.csv(Tuya.CPUE10641.1a_coeff, file="results/Terminal/Tuya.CPUE10641.1a_coeff.csv") 

a<-exp(fitted(Tah.CPUE10641.1))
TahData.10641$CPUE10641Tah<-exp(TahData.10641$log.CPUE10641Tah+0.000001)
TahData.10641$TotalRunTah<-exp(TahData.10641$log.TotalRunTah+0.000001)

Fitted<- data.frame(a, TahData.10641$StatWeek, TahData.10641$CPUE10641Tah, TahData.10641$TotalRunTah)
y<-Fitted[order(Fitted$TahData.10641.StatWeek, Fitted$TahData.10641.CPUE10641Tah),]
png("figures/Terminal/Fitted_D10641Tah_CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==26, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==27, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==28, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==29, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==30, col=1)
plot(TahData.10641.TotalRunTah~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE Tah (D106-41)", ylab="Terminal Run (Tah)")
lines(a~TahData.10641.CPUE10641Tah, data=y, subset=TahData.10641.StatWeek==31, col=1)
dev.off()

#108 Catch Tah
TahData.108 <- TahData[!(TahData$Year %in% c("1985", "2001", "2002","2003")), ]
TuyaData.108 <- TuyaData[!(TuyaData$Year %in% c("2001", "2002", "2003","2005","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                "1992", "1993", "1994")), ]
Tah.Catch108.1 <- lm(formula = log.TotalRunTah ~ poly(log.Catch108Tah,2)+log.StatWeek,data=TahData.108,weights=Weight) 
Tah.Catch108.1a<- lm(formula = log.TotalRunTah ~ log.Catch108Tah+I(log.Catch108Tah^2)+log.StatWeek,data=TahData.108,weights=Weight) 
Tuya.Catch108.1a<- lm(formula = log.TotalRunTuya ~ log.Catch108Tuya+I(log.Catch108Tuya^2)+log.StatWeek,data=TuyaData.108,weights=Weight) 

Tah.Catch108.1a_coeff <- summary(Tah.Catch108.1a)$coefficients[,1 ]
Tuya.Catch108.1a_coeff <- summary(Tuya.Catch108.1a)$coefficients[,1 ]
write.csv(Tah.Catch108.1a_coeff, file="results/Terminal/Tah.Catch108.1a_coeff.csv") 
write.csv(Tuya.Catch108.1a_coeff, file="results/Terminal/Tuya.Catch108.1a_coeff.csv") 

a<-exp(fitted(Tah.Catch108.1))
TahData.108$Catch108Tah<-exp(TahData.108$log.Catch108Tah+0.000001)
TahData.108$TotalRunTah<-exp(TahData.108$log.TotalRunTah+0.000001)

Fitted<- data.frame(a, TahData.108$StatWeek, TahData.108$Catch108Tah, TahData.108$TotalRunTah)
y<-Fitted[order(Fitted$TahData.108.StatWeek, Fitted$TahData.108.Catch108Tah),]
png("figures/Terminal/Fitted_D108Tah_Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==26, col=1)
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==27, col=1)
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==28, col=1)
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==29, col=1)
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==30, col=1)
plot(TahData.108.TotalRunTah~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.Catch108Tah, data=y, subset=TahData.108.StatWeek==31, col=1)
dev.off()

#108 CPUE Tah
TuyaData.108 <- TuyaData[!(TuyaData$Year %in% c("2001", "2002", "2003","2005","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                "1992", "1993", "1994")), ]
TahData.108 <- TahData[!(TahData$Year %in% c("1985", "2001", "2002","2003")), ]
Tah.CPUE108.1 <- lm(formula = log.TotalRunTah ~ poly(log.CPUE108Tah,2)+log.StatWeek,data=TahData.108,weights=Weight) 
Tah.CPUE108.1a<- lm(formula = log.TotalRunTah ~ log.CPUE108Tah+I(log.CPUE108Tah^2)+log.StatWeek,data=TahData.108,weights=Weight) 
Tuya.CPUE108.1a<- lm(formula = log.TotalRunTuya ~ log.CPUE108Tuya+I(log.CPUE108Tuya^2)+log.StatWeek,data=TuyaData.108,weights=Weight) 

Tah.CPUE108.1a_coeff <- summary(Tah.CPUE108.1a)$coefficients[,1 ]
Tuya.CPUE108.1a_coeff <- summary(Tuya.CPUE108.1a)$coefficients[,1 ]
write.csv(Tah.CPUE108.1a_coeff, file="results/Terminal/Tah.CPUE108.1a_coeff.csv") 
write.csv(Tuya.CPUE108.1a_coeff, file="results/Terminal/Tuya.CPUE108.1a_coeff.csv") 

a<-exp(fitted(Tah.CPUE108.1))
TahData.108$CPUE108Tah<-exp(TahData.108$log.CPUE108Tah+0.000001)
TahData.108$TotalRunTah<-exp(TahData.108$log.TotalRunTah+0.000001)

Fitted<- data.frame(a, TahData.108$StatWeek, TahData.108$CPUE108Tah, TahData.108$TotalRunTah)
y<-Fitted[order(Fitted$TahData.108.StatWeek, Fitted$TahData.108.CPUE108Tah),]
png("figures/Terminal/Fitted_D108Tah_CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==26, col=1)
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==27, col=1)
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==28, col=1)
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==29, col=1)
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==30, col=1)
plot(TahData.108.TotalRunTah~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE Tah (D108)", ylab="Terminal Run (Tah)")
lines(a~TahData.108.CPUE108Tah, data=y, subset=TahData.108.StatWeek==31, col=1)
dev.off()

#106-41 Catch STIKINE MODEL
StiData.10641 <- StiData[!(StiData$Year %in% c("2007")), ]
Catch10641.1 <- lm(formula = log.TotalRun ~ poly(log.Catch10641,2)+log.StatWeek,data=StiData.10641,weights=Weight) 
Catch10641.1a<- lm(formula = log.TotalRun ~ log.Catch10641+I(log.Catch10641^2)+log.StatWeek,data=StiData.10641,weights=Weight) 
Catch10641.1a_coeff <- summary(Catch10641.1a)$coefficients[,1 ]
write.csv(Catch10641.1a_coeff, file="results/Terminal/Catch10641.1a_coeff.csv") 

a<-exp(fitted(Catch10641.1))
StiData.10641$Catch10641<-exp(StiData.10641$log.Catch10641+0.000001)
StiData.10641$TotalRun<-exp(StiData.10641$log.TotalRun+0.000001)

Fitted<- data.frame(a, StiData.10641$StatWeek, StiData.10641$Catch10641, StiData.10641$TotalRun)
y<-Fitted[order(Fitted$StiData.10641.StatWeek, Fitted$StiData.10641.Catch10641),]
png("figures/Terminal/Fitted_D10641_Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==26, col=1)
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==27, col=1)
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==28, col=1)
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==29, col=1)
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==30, col=1)
plot(StiData.10641.TotalRun~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.Catch10641, data=y, subset=StiData.10641.StatWeek==31, col=1)
dev.off()

#106-41 CPUE STIKINE MODEL
StiData.10641 <- StiData[!(StiData$Year %in% c("2007")), ]
CPUE10641.1 <- lm(formula = log.TotalRun ~ poly(log.CPUE10641,2)+log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.1a<- lm(formula = log.TotalRun ~ log.CPUE10641+I(log.CPUE10641^2)+log.StatWeek,data=StiData.10641,weights=Weight) 
CPUE10641.1a_coeff <- summary(CPUE10641.1a)$coefficients[,1 ]
write.csv(CPUE10641.1a_coeff, file="results/Terminal/CPUE10641.1a_coeff.csv") 

a<-exp(fitted(CPUE10641.1))
StiData.10641$CPUE10641<-exp(StiData.10641$log.CPUE10641+0.000001)
StiData.10641$TotalRun<-exp(StiData.10641$log.TotalRun+0.000001)

Fitted<- data.frame(a, StiData.10641$StatWeek, StiData.10641$CPUE10641, StiData.10641$TotalRun)
y<-Fitted[order(Fitted$StiData.10641.StatWeek, Fitted$StiData.10641.CPUE10641),]
png("figures/Terminal/Fitted_D10641_CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==26, col=1)
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==27, col=1)
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==28, col=1)
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==29, col=1)
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==30, col=1)
plot(StiData.10641.TotalRun~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE(D106-41)", ylab="Terminal Run")
lines(a~StiData.10641.CPUE10641, data=y, subset=StiData.10641.StatWeek==31, col=1)
dev.off()

#108 Catch STIKINE MODEL
StiData.108 <- StiData[!(StiData$Year %in% c("1985", "2001", "2002","2003")), ]
Catch108.1 <- lm(formula = log.TotalRun ~ poly(log.Catch108,2)+log.StatWeek,data=StiData.108,weights=Weight) 
Catch108.1a<- lm(formula = log.TotalRun ~ log.Catch108+I(log.Catch108^2)+log.StatWeek,data=StiData.108,weights=Weight) 

Catch108.1a_coeff <- summary(Catch108.1a)$coefficients[,1 ]
write.csv(Catch108.1a_coeff, file="results/Terminal/Catch108.1a_coeff.csv") 

a<-exp(fitted(Catch108.1))
StiData.108$Catch108<-exp(StiData.108$log.Catch108+0.000001)
StiData.108$TotalRun<-exp(StiData.108$log.TotalRun+0.000001)

Fitted<- data.frame(a, StiData.108$StatWeek, StiData.108$Catch108, StiData.108$TotalRun)
y<-Fitted[order(Fitted$StiData.108.StatWeek, Fitted$StiData.108.Catch108),]
png("figures/Terminal/Fitted_D108_Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==26, col=1)
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==27, col=1)
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==28, col=1)
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==29, col=1)
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==30, col=1)
plot(StiData.108.TotalRun~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch(D108)", ylab="Terminal Run")
lines(a~StiData.108.Catch108, data=y, subset=StiData.108.StatWeek==31, col=1)
dev.off()

#108 CPUE STIKINE MODEL
StiData.108 <- StiData[!(StiData$Year %in% c("1985", "2001", "2002","2003")), ]
CPUE108.4 <- lm(formula = log.TotalRun ~ log.CPUE108*log.StatWeek,data=StiData.108 ,weights=Weight)
CPUE108.4_coeff <- summary(CPUE108.4)$coefficients[,1 ]
write.csv(CPUE108.4_coeff, file="results/Terminal/CPUE108.4_coeff.csv") 

a<-exp(fitted(CPUE108.4))
StiData.108$CPUE108<-exp(StiData.108$log.CPUE108+0.000001)
StiData.108$TotalRun<-exp(StiData.108$log.TotalRun+0.000001)

Fitted<- data.frame(a, StiData.108$StatWeek, StiData.108$CPUE108, StiData.108$TotalRun)
y<-Fitted[order(Fitted$StiData.108.StatWeek, Fitted$StiData.108.CPUE108),]
png("figures/Terminal/Fitted_D108_CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==26, col=1)
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==27, col=1)
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==28, col=1)
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==29, col=1)
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==30, col=1)
plot(StiData.108.TotalRun~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE(D108)", ylab="Terminal Run")
lines(a~StiData.108.CPUE108, data=y, subset=StiData.108.StatWeek==31, col=1)
dev.off()

#test new model to match ouput in SFMM
CPUE108.4 <- lm(formula = log.TotalRun ~ log.CPUE108*log.StatWeek,data=StiData.108 ,weights=Weight)
nd<-data.frame(log.StatWeek=log(34), log.CPUE108=log(303.26)) 
nd$log.StatWeek <- factor(nd$log.StatWeek)
prediction<-predict(CPUE108.4, newdata=nd, interval="prediction")
exp(prediction)
