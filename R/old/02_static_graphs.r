
#library(curl)
library(reshape2)
library(ggplot2)
require(gghighlight)
library(tidyverse)
library(magrittr)
library(deSolve)
library(data.table)
library(hellno)
options(scipen = 999, stringsAsFactors = FALSE)
library(future)
library(promises)


############ switch to graphic specific dataframe #############
filter_list<- c(
  "Italy"
  , "Switzerland"
 # , "Germany"
#  , "France"
#  , "United Kingdom"
#  , "US"
  , "Austria"
#  , "Japan"
#  ,"Spain"
   ,"Iceland" 
#  ,"United States"   
  ,"US-California"
  ,"US-California-Stanislaus"
,"US-California-Merced"
,"US-California-Tuolumne"
,"US-California-San Joaquin"
  ,"US-Tennessee"
,"US-Tennessee-Williamson"
,"US-Tennessee-Davidson"
 # "China", 
 # "Taiwan", 
 # ,"US-New York"
  ,"US-Florida"
 ,"US-New Jersey"

)

filter_days_to_go_back<- -100

min_cases <- 10

target_org <- "Italy"

pop_percentage<- .50

#remove data under min case count threshold
graph_dat<-filter(dat,test_pos>=min_cases, org %in% filter_list  )


maxdate<-max(graph_dat$report_date)
graph_dat$daystoday <- as.numeric(graph_dat$report_date - maxdate)

#status for caption at bottom

#find max number of cases
max_cases <- max(graph_dat[graph_dat$org == target_org, "test_pos"])


max_mod <- lm(
  log10(test_pos)~daystoday,
  data = graph_dat[graph_dat$org==target_org,]
)


graph_dat %<>% group_by(org) %>%
      mutate (
        date_diff= 
          round(
            as.numeric(
          -(coef(max_mod)[1] - (  mean(log10(test_pos)- coef(max_mod)[2] * daystoday )   )  )/coef(max_mod)[2]
          ),
          0)
      )
  
graph_dat$date_lag <- graph_dat$report_date + graph_dat$date_diff
graph_dat$diff_today_lag <- as.numeric(graph_dat$date_lag - maxdate)


graph_dat<- graph_dat[ graph_dat$diff_today_lag>filter_days_to_go_back,]

graph_dat$org <- factor(graph_dat$org, levels = unique(c(target_org, graph_dat$org)))
graph_dat<- graph_dat[order(graph_dat$org, graph_dat$report_date),]

graph_dat %<>% group_by(org) %>% mutate ( chg_from_prev= test_pos-lag(test_pos))

theme_set(theme_bw())



p <- ggplot(graph_dat, aes(x = diff_today_lag, y = test_pos)) + 
  geom_abline(intercept = coef(max_mod)[1], slope = coef(max_mod)[2], linetype = 2) +
  geom_point(aes(colour = org), size = 3) +
  geom_line(aes(colour = org), size = 0.75) +
  xlab(paste("Lag in days behind ", target_org, " (", max_cases, " cases on ", format(maxdate, format = "%m%-%d-%Y"), ")", sep = "")) +
  ylab("Confirmed SARS-CoV-2 cases") +
  scale_y_continuous(
    limits = c(10, NA),
    trans = "log10"
  )+
  #scale_y_log10(limits = c(1, NA)) +
  scale_x_continuous(breaks = seq(-1000, 0, 5)) +
  gghighlight(aes(group = org)
              , use_direct_label = TRUE
              , label_key = date_diff
              , label_params = list(point.padding = 1, nudge_y = -0.9, nudge_x = 1.2, size = 5.5)
              , unhighlighted_params = list(colour = grey(0.9), alpha = 0.75)) +
  
  
  #geom_hline(aes(yintercept = graph_dat$population[!duplicated(graph_dat$org)]*pop_percentage))+

  facet_wrap(~org) +
  theme(
    axis.title.y=element_text(colour = "black", size = 17, hjust = 0.5, margin=margin(0,12,0,0)),
    axis.title.x=element_text(colour = "black", size = 17, margin=margin(10,0,0,0)),
    axis.text.x=element_text(colour = "black", size=15),
    axis.text.y=element_text(colour = "black", size=15),
    legend.position="none",
    legend.text=element_text(size=12.5),
    legend.key=element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title=element_text(size=15),
    panel.grid.minor = element_blank(),
    strip.text.x=element_text(size=15)
  ) +
  annotation_logticks(base = 10, sides = "l") +
  labs(caption = "Data source: https://github.com/CSSEGISandData/COVID-19")

  

plot(p)


################### plot as percentage of population


theme_set(theme_bw())
pct <- ggplot(graph_dat, aes(x = diff_today_lag, y = test_pos / population)) + 
  #geom_abline(intercept = coef(max_mod)[1], slope = coef(max_mod)[2], linetype = 2) +
  geom_point(aes(colour = org), size = 3) +
  geom_line(aes(colour = org), size = 0.75) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(-1000, 0, 5)) +
  gghighlight(aes(group = org)
              , use_direct_label = FALSE
             # , label_key = date_diff
             # , label_params = list(point.padding = 1, nudge_y = -0.9, nudge_x = 1.2, size = 5.5)
              , unhighlighted_params = list(colour = grey(0.9), alpha = 0.75)) +
  
  #geom_hline(aes(yintercept = graph_dat$population[!duplicated(graph_dat$org)]*pop_percentage))+
  
  facet_wrap(~org) +
  theme(
    axis.title.y=element_text(colour = "black", size = 17, hjust = 0.5, margin=margin(0,12,0,0)),
    axis.title.x=element_text(colour = "black", size = 17, margin=margin(10,0,0,0)),
    axis.text.x=element_text(colour = "black", size=15),
    axis.text.y=element_text(colour = "black", size=15),
    legend.position="none",
    legend.text=element_text(size=12.5),
    legend.key=element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title=element_text(size=15),
    panel.grid.minor = element_blank(),
    strip.text.x=element_text(size=15)
  ) 
  # + annotation_logticks(base = 10, sides = "l") 

plot(pct)


################### plot daily change


theme_set(theme_bw())
chg <- ggplot(graph_dat, aes(x = diff_today_lag, y = chg_from_prev)) + 
  #geom_abline(intercept = coef(max_mod)[1], slope = coef(max_mod)[2], linetype = 2) +
  geom_point(aes(colour = org), size = 3) +
  geom_line(aes(colour = org), size = 0.75) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(-1000, 0, 5)) +
  gghighlight(aes(group = org)
              , use_direct_label = FALSE
             # , label_key = date_diff
              #, label_params = list(point.padding = 1, nudge_y = -0.9, nudge_x = 1.2, size = 5.5)
              , unhighlighted_params = list(colour = grey(0.9), alpha = 0.75)) +
  
  #geom_hline(aes(yintercept = graph_dat$population[!duplicated(graph_dat$org)]*pop_percentage))+
  
  facet_wrap(~org) +
  theme(
    axis.title.y=element_text(colour = "black", size = 17, hjust = 0.5, margin=margin(0,12,0,0)),
    axis.title.x=element_text(colour = "black", size = 17, margin=margin(10,0,0,0)),
    axis.text.x=element_text(colour = "black", size=15),
    axis.text.y=element_text(colour = "black", size=15),
    legend.position="none",
    legend.text=element_text(size=12.5),
    legend.key=element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title=element_text(size=15),
    panel.grid.minor = element_blank(),
    strip.text.x=element_text(size=15)
  ) 
# + annotation_logticks(base = 10, sides = "l") 
 
plot(chg)


##################################################

