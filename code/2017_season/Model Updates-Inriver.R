#notes----
#June 2017
#Inseason Models (Inriver)
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
IR <- read_excel('data/Linear_Regressions_Data.xlsx', sheet="Inriver")
#data clean----
IR %>% 
  mutate(Year = factor(Year),
         log.StatWeek = as.factor(log(StatWeek)),
         log.InriverRun = as.numeric(log(InriverRun)),
         log.CPUE = as.numeric(log(CPUE+0.00001)),
         log.Catch= as.numeric(log(Catch+0.00001)),
         log.InriverTah = as.numeric(log(InriverTah)),
         log.CPUETah = as.numeric(log(CPUETah+0.00001)),
         log.CatchTah= as.numeric(log(CatchTah+0.00001)),
         log.InriverTuya = as.numeric(log(InriverTuya)),
         log.CPUETuya = as.numeric(log(CPUETuya+0.00001)),
         log.CatchTuya= as.numeric(log(CatchTuya+0.00001)),
         Weight = as.numeric(Weight))->Inriver

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

eda.norm(Inriver$log.InriverRun)

#results----
#Run Models (Inriver)
Catch.1 <- lm(formula = log.InriverRun ~ poly(log.Catch,2)+log.StatWeek,data=Inriver, weights=Weight) 
#Catch.2 <- lm(formula = log.InriverRun ~ poly(log.Catch,2)*log.StatWeek,data=Inriver, weights=Weight) 
Catch.3 <- lm(formula = log.InriverRun ~ log.Catch+log.StatWeek,data=Inriver, weights=Weight) 
Catch.4 <- lm(formula = log.InriverRun ~ log.Catch*log.StatWeek,data=Inriver, weights=Weight) 
CPUE.1 <- lm(formula = log.InriverRun ~ poly(log.CPUE,2)+log.StatWeek,data=Inriver, weights=Weight) 
#CPUE.2 <- lm(formula = log.InriverRun ~ poly(log.CPUE,2)*log.StatWeek,data=Inriver, weights=Weight) 
CPUE.3 <- lm(formula = log.InriverRun ~ log.CPUE+log.StatWeek,data=Inriver, weights=Weight) 
CPUE.4 <- lm(formula = log.InriverRun ~ log.CPUE*log.StatWeek,data=Inriver, weights=Weight)

AICcResults<-data.frame (AICc(Catch.1,Catch.3,Catch.4,CPUE.1,CPUE.3,CPUE.4))
write.csv(AICcResults, file="results/Inriver/AICc_Inriver.csv") #save AICc results
stargazer(Catch.1, Catch.3, Catch.4, type="text", out="results/Inriver/Inriver.Models.Catch.htm")#save results to folder
stargazer(CPUE.1, CPUE.3, CPUE.4, type="text", out="results/Inriver/Inriver.Models.CPUE.htm")#save results to folder

#Run Models (Inriver-Tah)
CatchTah.1 <- lm(formula = log.InriverTah ~ poly(log.CatchTah,2)+log.StatWeek,data=Inriver, weights=Weight) 
#CatchTah.2 <- lm(formula = log.InriverTah ~ poly(log.CatchTah,2)*log.StatWeek,data=Inriver, weights=Weight) 
CatchTah.3 <- lm(formula = log.InriverTah ~ log.CatchTah+log.StatWeek,data=Inriver, weights=Weight) 
CatchTah.4 <- lm(formula = log.InriverTah ~ log.CatchTah*log.StatWeek,data=Inriver, weights=Weight) 
CPUETah.1 <- lm(formula = log.InriverTah ~ poly(log.CPUETah,2)+log.StatWeek,data=Inriver, weights=Weight) 
#CPUETah.2 <- lm(formula = log.InriverTah ~ poly(log.CPUETah,2)*log.StatWeek,data=Inriver, weights=Weight) 
CPUETah.3 <- lm(formula = log.InriverTah ~ log.CPUETah+log.StatWeek,data=Inriver, weights=Weight) 
CPUETah.4 <- lm(formula = log.InriverTah ~ log.CPUETah*log.StatWeek,data=Inriver, weights=Weight)

AICcResults.Tah<-data.frame (AICc(CatchTah.1,CatchTah.3,CatchTah.4, CPUETah.1,CPUETah.3,CPUETah.4))
write.csv(AICcResults.Tah, file="results/Inriver/AICc_Inriver_Tah.csv") #save AICc results
stargazer(CatchTah.1, CatchTah.3, CatchTah.4, type="text", out="results/Inriver/InriverTah.Models.Catch.htm")#save results to folder
stargazer(CPUETah.1,  CPUETah.3, CPUETah.4, type="text", out="results/Inriver/InriverTah.Models.CPUE.htm")#save results to folder

#Run Models (Inriver-Tuya)
Inriver.Tuya <- Inriver[!(Inriver$Year %in% c("1979", "1980","1981", "1982", "1983","1984","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                                "1992", "1993", "1994")), ]
CatchTuya.1 <- lm(formula = log.InriverTuya ~ poly(log.CatchTuya,2)+log.StatWeek,data=Inriver.Tuya, weights=Weight) 
#CatchTuya.2 <- lm(formula = log.InriverTuya ~ poly(log.CatchTuya,2)*log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CatchTuya.3 <- lm(formula = log.InriverTuya ~ log.CatchTuya+log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CatchTuya.4 <- lm(formula = log.InriverTuya ~ log.CatchTuya*log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CPUETuya.1 <- lm(formula = log.InriverTuya ~ poly(log.CPUETuya,2)+log.StatWeek,data=Inriver.Tuya, weights=Weight) 
#CPUETuya.2 <- lm(formula = log.InriverTuya ~ poly(log.CPUETuya,2)*log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CPUETuya.3 <- lm(formula = log.InriverTuya ~ log.CPUETuya+log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CPUETuya.4 <- lm(formula = log.InriverTuya ~ log.CPUETuya*log.StatWeek,data=Inriver.Tuya, weights=Weight)
stargazer(CatchTuya.1, CatchTuya.3, CatchTuya.4, type="text", out="results/Inriver/InriverTuya.Models.Catch.htm")#save results to folder
stargazer(CPUETuya.1, CPUETuya.3, CPUETuya.4, type="text", out="results/Inriver/InriverTuya.Models.CPUE.htm")#save results to folder

#figures----
#figure of best model based on AICc sore
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

#Inriver Catch Tah
Inriver.Tuya <- Inriver[!(Inriver$Year %in% c("1979", "1980","1981", "1982", "1983","1984","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                              "1992", "1993", "1994")), ]
CatchTah.4 <- lm(formula = log.InriverTah ~ log.CatchTah*log.StatWeek,data=Inriver, weights=Weight) 
CatchTuya.4 <- lm(formula = log.InriverTuya ~ log.CatchTuya*log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CatchTah.4_coeff <- summary(CatchTah.4)$coefficients[,1 ]
write.csv(CatchTah.4_coeff, file="results/Inriver/CatchTah.4_coeff.csv") 
CatchTuya.4_coeff <- summary(CatchTuya.4)$coefficients[,1 ]
write.csv(CatchTuya.4_coeff, file="results/Inriver/CatchTuya.4_coeff.csv") 

a<-exp(fitted(CatchTah.4))
Inriver$CatchTah<-exp(Inriver$log.CatchTah+0.000001)
Inriver$InriverTah<-exp(Inriver$log.InriverTah+0.000001)

Fitted<- data.frame(a, Inriver$StatWeek, Inriver$CatchTah, Inriver$InriverTah)
y<-Fitted[order(Fitted$Inriver.StatWeek, Fitted$Inriver.CatchTah),]
png("figures/Inriver/Fitted_Inriver_Tah_Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==26, col=1)
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==27, col=1)
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==28, col=1)
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==29, col=1)
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==30, col=1)
plot(Inriver.InriverTah~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CatchTah, data=y, subset=Inriver.StatWeek==31, col=1)
dev.off()

#Inriver CPUE Tah
Inriver.Tuya <- Inriver[!(Inriver$Year %in% c("1979", "1980","1981", "1982", "1983","1984","1985","1986", "1987", "1988", "1989", "1990", "1991",
                                              "1992", "1993", "1994")), ]
CPUETah.4 <- lm(formula = log.InriverTah ~ log.CPUETah*log.StatWeek,data=Inriver, weights=Weight) 
CPUETuya.4 <- lm(formula = log.InriverTuya ~ log.CPUETuya*log.StatWeek,data=Inriver.Tuya, weights=Weight) 
CPUETah.4_coeff <- summary(CPUETah.4)$coefficients[,1 ]
write.csv(CPUETah.4_coeff, file="results/Inriver/CPUETah.4_coeff.csv") 
CPUETuya.4_coeff <- summary(CPUETuya.4)$coefficients[,1 ]
write.csv(CPUETuya.4_coeff, file="results/Inriver/CPUETuya.4_coeff.csv") 

a<-exp(fitted(CPUETah.4))
Inriver$CPUETah<-exp(Inriver$log.CPUETah+0.000001)
Inriver$InriverTah<-exp(Inriver$log.InriverTah+0.000001)

Fitted<- data.frame(a, Inriver$StatWeek, Inriver$CPUETah, Inriver$InriverTah)
y<-Fitted[order(Fitted$Inriver.StatWeek, Fitted$Inriver.CPUETah),]
png("figures/Inriver/Fitted_Inriver_Tah_CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==26, col=1)
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==27, col=1)
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==28, col=1)
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==29, col=1)
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==30, col=1)
plot(Inriver.InriverTah~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE Tah (Inriver)", ylab="Inriver Run (Tah)")
lines(a~Inriver.CPUETah, data=y, subset=Inriver.StatWeek==31, col=1)
dev.off()

#Inriver Catch 
Catch.4 <- lm(formula = log.InriverRun ~ log.Catch*log.StatWeek,data=Inriver, weights=Weight) 
Catch.4_coeff <- summary(Catch.4)$coefficients[,1 ]
write.csv(Catch.4_coeff, file="results/Inriver/Catch.4_coeff.csv") 

a<-exp(fitted(Catch.4))
Inriver$Catch<-exp(Inriver$log.Catch+0.000001)
Inriver$InriverRun<-exp(Inriver$log.InriverRun+0.000001)

Fitted<- data.frame(a, Inriver$StatWeek, Inriver$Catch, Inriver$InriverRun)
y<-Fitted[order(Fitted$Inriver.StatWeek, Fitted$Inriver.Catch),]
png("figures/Inriver/Fitted_Inriver__Catch.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==26, col=1)
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==27, col=1)
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==28, col=1)
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==29, col=1)
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==30, col=1)
plot(Inriver.InriverRun~Inriver.Catch, data=y, subset=Inriver.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. Catch (Inriver)", ylab="Inriver Run")
lines(a~Inriver.Catch, data=y, subset=Inriver.StatWeek==31, col=1)
dev.off()

#Inriver CPUE
CPUE.1 <- lm(formula = log.InriverRun ~ poly(log.CPUE,2)+log.StatWeek,data=Inriver, weights=Weight) 
CPUE.1a<- lm(formula = log.InriverRun ~ log.CPUE+I(log.CPUE^2)+log.StatWeek,data=Inriver,weights=Weight) 
CPUE.1a_coeff <- summary(CPUE.1a)$coefficients[,1 ]
write.csv(CPUE.1a_coeff, file="results/Inriver/CPUE.1a_coeff.csv") 

a<-exp(fitted(CPUE.1a))
Inriver$CPUE<-exp(Inriver$log.CPUE+0.000001)
Inriver$InriverRun<-exp(Inriver$log.InriverRun+0.000001)

Fitted<- data.frame(a, Inriver$StatWeek, Inriver$CPUE, Inriver$InriverRun)
y<-Fitted[order(Fitted$Inriver.StatWeek, Fitted$Inriver.CPUE),]
png("figures/Inriver/Fitted_Inriver__CPUE.png", res=600, height=4.5, width=8, units="in")
par(mfrow=c(2,3)) 
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==26, pch=16, col=1, main="StatWeek 26",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==26, col=1)
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==27, pch=16, col=1, main="StatWeek 27",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==27, col=1)
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==28, pch=16, col=1, main="StatWeek 28",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==28, col=1)
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==29, pch=16, col=1, main="StatWeek 29",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==29, col=1)
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==30, pch=16, col=1, main="StatWeek 30",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==30, col=1)
plot(Inriver.InriverRun~Inriver.CPUE, data=y, subset=Inriver.StatWeek==31, pch=16, col=1, main="StatWeek 31",
     xlab="Cum. CPUE (Inriver)", ylab="Inriver Run")
lines(a~Inriver.CPUE, data=y, subset=Inriver.StatWeek==31, col=1)
dev.off()
#test new model to match ouput in SFMM
CPUE.1 <- lm(formula = log.InriverRun ~ poly(log.CPUE,2)+log.StatWeek,data=Inriver, weights=Weight) 
nd<-data.frame(log.StatWeek=log(26), log.CPUE=log(6.59)) 
nd$log.StatWeek <- factor(nd$log.StatWeek)
prediction<-predict(CPUE.1, newdata=nd, interval="prediction")
exp(prediction)