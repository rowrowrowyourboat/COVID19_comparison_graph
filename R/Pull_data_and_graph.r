
#library(curl)
library(reshape2)
library(ggplot2)
require(gghighlight)
library(tidyverse)
library(magrittr)
library(deSolve)
library(data.table)
library(hellno)
options(scipen = 999)





# get_URL_as_csv<- function(u){
#   curl(
#     u
#    # ,.opts = list(ssl.verifypeer = FALSE)
#    
#   ) %>% read.csv(text = ., stringsAsFactors = FALSE)
#   
#   
# }

#source: CSSE at Johns Hopkins University

y <- read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  )


y%<>% select(-Lat, -Long)
names(y)[-(1:2)]%<>% substring( first = 2)


dat<- pivot_longer(y, -names(y)[1:2], names_to = "report_date", values_to = "test_pos")


dat$report_date <- as.Date(dat$report_date, format = "%m.%d.%y")

#source US census 
pop_states<- fread( file=paste0(getwd(), "/data/nst-est2019-alldata.csv"))
pop_states %<>% select(NAME, POPESTIMATE2019)
colnames(pop_states)<- c("NAME", "population")

#world pop
#pop_country<- get_URL_as_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
pop_country<- fread( file=paste0(getwd(), "/data/SYB62_1_201907_Population, Surface Area and Density.csv"), skip =1)
#pop_country<- get_URL_as_csv("https://data.un.org/_Docs/SYB/CSV/SYB62_1_201907_Population,%20Surface%20Area%20and%20Density.csv")


#colnames(pop_country)<-pop_country[1,]

pop_country$location<-pop_country[,2]
pop_country %<>% filter(Year== '2019')
pop_country %<>% filter(Series=="Population mid-year estimates (millions)")
pop_country$population <- as.numeric(pop_country$Value)*1000000
pop_country%<>% select(location, population)



pop_country[pop_country$location == "China, Taiwan Province of China", "location"]<- "Taiwan*"
pop_country[pop_country$location == "United States of America", "location"]<- "US"
pop_country[pop_country$location == "Viet Nam", "location"]<- "Vietnam"
pop_country[pop_country$location == "Russian Federation", "location"]<- "Russia"
pop_country[pop_country$location == "	Iran (Islamic Republic of)", "location"]<- "Iran"
pop_country[pop_country$location == "Bolivia (Plurinational State of)", "location"]<- "Bolivia"
pop_country[pop_country$location == "Republic of Korea", "location"]<- "Korea, South"






#create state level data
dat$org<- ifelse(dat$Province.State=="", "DELETE", paste(dat$Country.Region, dat$Province.State, sep="-"))
dat<- left_join(dat, pop_states, by=c("Province.State"= "NAME"))


#create country level data
dat_country<- dat %>%
                group_by(Country.Region, report_date) %>%
                summarize(
                  test_pos = sum(test_pos)
                )
dat_country<- left_join(dat_country, pop_country, by = c("Country.Region" = "location"))

dat_country$org<-dat_country$Country.Region
dat_country$Country.Region<-NULL

dat$Country.Region<-NULL
dat$Province.State<-NULL


dat<-bind_rows(dat, dat_country)
dat_country<-NULL

dat<- dat[dat$org!="DELETE",]





############ switch to graphic specific dataframe #############
filter_list<- c(
  "Italy"
  , "Switzerland"
  , "Germany"
  , "France"
  , "United Kingdom"
  , "US"
  , "Austria"
  , "Japan"
  ,"Spain"
  ,"US-California"
  ,"US-Tennessee"
  ,"United States", "China", "Taiwan", "US-New York"
)

filter_days_to_go_back<- -30

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

graph_dat$org <- factor(graph_dat$org, levels = c(target_org, filter_list[not(filter_list %in% target_org)]))
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
pct <- ggplot(graph_dat, aes(x = diff_today_lag, y = test_pos/population)) + 
  #geom_abline(intercept = coef(max_mod)[1], slope = coef(max_mod)[2], linetype = 2) +
  geom_point(aes(colour = org), size = 3) +
  geom_line(aes(colour = org), size = 0.75) +
  scale_y_log10() +
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
  ) 
# + annotation_logticks(base = 10, sides = "l") 
 
plot(chg)


##################################################

