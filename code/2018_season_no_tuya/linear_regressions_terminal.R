#notes----
#author: Sara E Miller 
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: April 2018

# load ----
source("code/helper.R")
source("code/functions.R")

#data----
read_excel('data/2018_season_no_tuya/linear_regressions_data.xlsx', sheet="outside") -> data

#data clean----
n <- 1 #small number to offset 0
names(data) <- c('year','statweek','weight', 'tah', 'mainstem',
                 'run','cpue10641tah', 'catch10641tah','cpue108tah','catch108tah',
                 'cpue10641','catch10641','cpue108','catch108',
                 'cpue10641mainstem','catch10641mainstem','cpue108mainstem',
                 'catch108mainstem')
data %>% 
  mutate_at(vars(-year, -statweek, -weight), funs(log = log(. + 1))) %>% 
  mutate(statweek = factor(statweek)) %>% 
  replace(is.na(.), 0) -> log_data

log_data %>% 
  filter(year!=2007) -> log_10641

log_data %>% 
  filter(!(year %in% c(1985, 2001, 2002, 2003))) -> log_108

#analysis-----
#Determine if the data is normally distributed (p should be >0.05)
eda.norm(log_data$ltah_log)
eda.norm(log_data$mainstem_log)
eda.norm(log_data$run_log)

#results----

# Tahltan-D108 Catch
log_108 %>% 
  do(tah_catch = lm(tah_log ~ catch108tah_log + statweek, data = .),
     tah_cpue = lm(tah_log ~ cpue108tah_log + statweek, data = .)) -> tah_108 

tah_108 %>% 
  tidy(tah_catch) %>% 
  write_csv("output/terminal/lm_tah_catch_108.csv")

tah_108 %>% 
  tidy(tah_cpue) %>% 
  write_csv("output/terminal/lm_tah_cpue_108.csv")


tah_108 %>% 
  augment(tah_catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catch108tah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, tah)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch Tahltan (D108)") + ylab("Terminal Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_108_catch
ggsave(file="figures/terminal/tah_108_catch.png", plot=tah_108_catch, width=10, height=8)

tah_108 %>% 
  augment(tah_cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpue108tah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, tah)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE Tahltan (D108)") + ylab("Terminal Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_108_cpue
ggsave(file="figures/terminal/tah_108_cpue.png", plot=tah_108_cpue, width=10, height=8)

# Tahltan-D106-41 Catch
log_10641 %>% 
  do(tah_catch = lm(tah_log ~ catch10641tah_log + statweek, data = .),
     tah_cpue = lm(tah_log ~ cpue10641tah_log + statweek, data = .)) -> tah_10641 

tah_10641 %>% 
  tidy(tah_catch) %>% 
  write_csv("output/terminal/lm_tah_catch_10641.csv")

tah_10641 %>% 
  tidy(tah_cpue) %>% 
  write_csv("output/terminal/lm_tah_cpue_10641.csv")


tah_10641 %>% 
  augment(tah_catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catch10641tah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, tah)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch Tahltan (D106-41)") + ylab("Terminal Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_10641_catch
ggsave(file="figures/terminal/tah_10641_catch.png", plot=tah_10641_catch, width=10, height=8)


tah_10641 %>% 
  augment(tah_cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpue10641tah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, tah)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE Tahltan (D106-41)") + ylab("Terminal Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_10641_cpue
ggsave(file="figures/terminal/tah_10641_cpue.png", plot=tah_10641_cpue, width=10, height=8)

# Stikine-D108 Catch
log_108 %>% 
  do(catch = lm(run_log ~ catch108_log + statweek, data = .),
     cpue = lm(run_log ~ cpue108_log + statweek, data = .)) -> run_108 

run_108 %>% 
  tidy(catch) %>% 
  write_csv("output/terminal/lm_catch_108.csv")

run_108 %>% 
  tidy(cpue) %>% 
  write_csv("output/terminal/lm_cpue_108.csv")


run_108 %>% 
  augment(catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catch108_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, run)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch Stikine (D108)") + ylab("Terminal Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> Stikine_108_catch
ggsave(file="figures/terminal/Stikine_108_catch.png", plot=Stikine_108_catch, width=10, height=8)

run_108 %>% 
  augment(cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpue108_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, run)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE Stikine (D108)") + ylab("Terminal Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> Stikine_108_cpue
ggsave(file="figures/terminal/Stikine_108_cpue.png", plot=Stikine_108_cpue, width=10, height=8)


# Stikine-D10641 Catch
log_10641 %>% 
  do(catch = lm(run_log ~ catch10641_log + statweek, data = .),
     cpue = lm(run_log ~ cpue10641_log + statweek, data = .)) -> run_10641 

run_10641 %>% 
  tidy(catch) %>% 
  write_csv("output/terminal/lm_catch_10641.csv")

run_10641 %>% 
  tidy(cpue) %>% 
  write_csv("output/terminal/lm_cpue_10641.csv")


run_10641 %>% 
  augment(catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catch10641_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, run)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch Stikine (D106-41)") + ylab("Terminal Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> Stikine_10641_catch
ggsave(file="figures/terminal/Stikine_10641_catch.png", plot=Stikine_10641_catch, width=10, height=8)

run_10641 %>% 
  augment(cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpue10641_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, run)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE Stikine (D106-41)") + ylab("Terminal Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> Stikine_10641_cpue
ggsave(file="figures/terminal/Stikine_10641_cpue.png", plot=Stikine_10641_cpue, width=10, height=8)


#test new model to match ouput in SFMM
#CPUE108.4 <- lm(formula = log.TotalRun ~ log.CPUE108*log.StatWeek,data=StiData.108 ,)
#nd<-data.frame(log.StatWeek=log(34), log.CPUE108=log(303.26)) 
#nd$log.StatWeek <- factor(nd$log.StatWeek)
#prediction<-predict(CPUE108.4, newdata=nd, interval="prediction")
#exp(prediction)
