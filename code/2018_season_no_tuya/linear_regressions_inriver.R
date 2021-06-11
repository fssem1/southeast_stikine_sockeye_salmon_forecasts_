#notes----
#author: Sara E Miller 
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: April 2018

# load ----
source("code/helper.R")
source("code/functions.R")

#data----
read_excel('data/2018_season_no_tuya/linear_regressions_data.xlsx', sheet="inriver") -> data

#data clean----
n <- 1 #small number to offset 0
names(data) <- c('year','statweek','weight','tah', 'mainstem','run','cpuetah', 'catchtah',
                 'cpue','catch','cpuemainstem','catchmainstem')

#data clean----
data %>% 
  mutate_at(vars(-year, -statweek, -weight), funs(log = log(. + 1))) %>% 
  mutate(statweek = factor(statweek)) %>% 
  replace(is.na(.), 0) -> log_data

log_data %>% 
  filter(year!=1984) -> log_inriver


#analysis-----
#Determine if the data is normally distributed (p should be >0.05)
eda.norm(log_data$run_log)

#results----
#Run Models (Inriver-Tahltan)
log_inriver %>% 
  do(tah_catch = lm(tah_log ~ catchtah_log + statweek, data = .),
     tah_cpue = lm(tah_log ~ cpuetah_log + statweek, data = .)) -> tah_inriver

tah_inriver %>% 
  tidy(tah_catch) %>% 
  write_csv("output/inriver/lm_tah_catch.csv")

tah_inriver %>% 
  tidy(tah_cpue) %>% 
  write_csv("output/inriver/lm_tah_cpue.csv")


tah_inriver %>% 
  augment(tah_catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catchtah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, tah)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch Tahltan (inriver)") + ylab("Inriver Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_catch
ggsave(file="figures/inriver/tah_catch.png", plot=tah_catch, width=10, height=8)

tah_inriver %>% 
  augment(tah_cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpuetah_log), tah = exp(tah_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, tah)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE Tahltan (inriver)") + ylab("Inriver Run (Tahltan)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> tah_cpue
ggsave(file="figures/inriver/tah_cpue.png", plot=tah_cpue, width=10, height=8)

#Run Models (Inriver-Stikine)
log_inriver %>% 
  do(catch = lm(run_log ~ catch_log + statweek, data = .),
     cpue = lm(run_log ~ cpue_log + statweek, data = .)) -> stik_inriver

stik_inriver %>% 
  tidy(catch) %>% 
  write_csv("output/inriver/lm_catch.csv")

stik_inriver %>% 
  tidy(cpue) %>% 
  write_csv("output/inriver/lm_cpue.csv")


stik_inriver %>% 
  augment(catch) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(catch = exp(catch_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(catch, run)) + geom_point() + geom_line(aes(catch, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative Catch (inriver)") + ylab("Inriver Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> catch
ggsave(file="figures/inriver/catch.png", plot=catch, width=10, height=8)

stik_inriver %>% 
  augment(cpue) %>% 
  filter(statweek %in% c('26', '27', '28', '29', '30', '31')) %>% 
  mutate(cpue = exp(cpue_log), run = exp(run_log), fit = exp(.fitted)) %>% 
  ggplot(aes(cpue, run)) + geom_point() + geom_line(aes(cpue, fit)) + facet_wrap(~statweek,  labeller = label_both, dir = 'v', nrow = 3) +
  xlab("Cumulative CPUE (inriver)") + ylab("Inriver Run (Stikine)") +
  scale_y_continuous(label=comma) + scale_x_continuous(label=comma) -> cpue
ggsave(file="figures/inriver/cpue.png", plot=cpue, width=10, height=8)
