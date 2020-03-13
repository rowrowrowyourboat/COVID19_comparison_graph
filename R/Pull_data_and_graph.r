library(RCurl)
library(reshape2)
library(ggplot2)
require(gghighlight)

x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
y <- read.csv(text = x)

names(y)[-(1:4)] <- substring(names(y)[-(1:4)], 2)

dat <- melt(y, id.vars = names(y)[1:4], variable.name = "date")
dat$Lat <- NULL
dat$Long <- NULL
# dat$Province.State <- NULL

dat$date <- as.Date(dat$date, format = "%m.%d.%y")

table((dat$Country.Region))

countries <- c(
  "Italy"
  , "Switzerland"
  , "Germany"
  , "France"
  , "United Kingdom"
  , "US"
  , "Austria"
  # , "Japan"
  # , "Mainland China"
)

dat_sel <- subset(dat, Country.Region %in% countries)

dat_sel$Province.State <- factor(dat_sel$Province.State)
dat_sel$Country.Region <- factor(dat_sel$Country.Region)

countries_repeats <- names(which(table(dat_sel$Country.Region, dat_sel$date)[, 1] > 1))

dat_sel$Province.State <- NULL

for(i in countries_repeats) {
  
  dat_sel <- subset(dat_sel, !Country.Region %in% i)
  
  dat_sel <- rbind(
    dat_sel
    , data.frame(
      Country.Region = i
      , date = unique(dat$date)
      , value = tapply(dat$value[dat$Country.Region %in% i], dat$date[dat$Country.Region %in% i], sum, na.rm = TRUE)
    )
  )
  
}

dat_sel$Country.Region <- factor(dat_sel$Country.Region)
dat_sel$value[dat_sel$value == 0] <- NA
# dat_sel$difftotoday <- dat_sel$date - max(dat_sel$date)
dat_sel$daystoday <- as.numeric(dat_sel$date - max(dat_sel$date))

max_case_tab <- tapply(dat_sel$value, dat_sel$Country.Region, max, na.rm = TRUE)
max_ind <- which.max(max_case_tab)
count_max <- names(max_ind)
max_cases <- max_case_tab[max_ind]

max_mod <- lm(log10(value)~daystoday, data = subset(dat_sel, Country.Region %in% count_max & date > as.Date("22.02.20", format = "%d.%m.%y")))

summary(max_mod)

100*(exp(coef(max_mod)[2]) - 1) # % Change per day

# visreg::visreg(max_mod)

dat_sel$date_diff <- NA

for(i in countries[!countries %in% count_max]) {
  
  # i <- "France"
  
  # tmp_mod <- lm(log10(value)~daystoday, data = subset(dat_sel, Country.Region %in% i & date > (max(date) - 11)))
  
  dat_tmp <- subset(dat_sel, Country.Region %in% i & date > (max(date) - 10))
  int <- mean(log10(dat_tmp$value) - coef(max_mod)[2]*dat_tmp$daystoday)
  
  # visreg::visreg(tmp_mod)
  
  dat_sel$date_diff[dat_sel$Country.Region %in% i] <- round(as.numeric(-(coef(max_mod)[1] - int)/(coef(max_mod)[2])), 0)
  
}

dat_sel$date_diff[dat_sel$Country.Region %in% count_max] <- 0

dat_sel$date_lag <- dat_sel$date + dat_sel$date_diff
dat_sel$diff_today_lag <- as.numeric(dat_sel$date_lag - max(dat_sel$date))

names(dat_sel)[1] <- "Country"

dat_sel$Country <- factor(dat_sel$Country, levels = c("Italy", sort(countries[!countries%in% "Italy"])))

theme_set(theme_bw())
p <- ggplot(subset(dat_sel, diff_today_lag >= -20), aes(x = diff_today_lag, y = value)) + 
  geom_abline(intercept = coef(max_mod)[1], slope = coef(max_mod)[2], linetype = 2) +
  geom_point(aes(colour = Country), size = 3) +
  geom_line(aes(colour = Country), size = 0.75) +
  # geom_line(aes(colour = variable), size = 1) +
  xlab(paste("Lag in days behind ", count_max, " (", max_cases, " cases on ", format(max(dat_sel$date), format = "%d.%m%.%Y"), ")", sep = "")) +
  ylab("Confirmed SARS-CoV-2 cases") +
  # scale_y_continuous(breaks = exp(seq(log(10), log(100000), length = 5)), trans = "log") +
  scale_y_log10(limits = c(1, NA)) +
  scale_x_continuous(breaks = seq(-1000, 0, 5)) +
  gghighlight(aes(group = Country)
              , use_direct_label = TRUE
              , label_key = date_diff
              , label_params = list(point.padding = 1, nudge_y = -0.9, nudge_x = 1.2, size = 5.5)
              , unhighlighted_params = list(colour = grey(0.9), alpha = 0.75)) +
  # geom_abline(intercept = 4.1231, slope = 0.1123) +
  facet_wrap(~Country) +
  theme(
    axis.title.y=element_text(colour = "black", size = 17, hjust = 0.5, margin=margin(0,12,0,0)),
    axis.title.x=element_text(colour = "black", size = 17, margin=margin(10,0,0,0)),
    # axis.title.y=element_text(size=15,hjust=0.5, vjust=1),
    axis.text.x=element_text(colour = "black", size=15),
    axis.text.y=element_text(colour = "black", size=15),
    # plot.margin=unit(c(2,2,2,2,2),"line"),
    legend.position="none",
    legend.text=element_text(size=12.5),
    # panel.grid.minor = element_blank(),
    # panel.grid.major = element_line(colour=grey(0.8), size=0.5),
    legend.key=element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title=element_text(size=15),
    # legend.key.width=unit(.01,"npc"),
    # legend.key.height=unit(.025,"npc"),
    # strip.background=element_rect(fill="white")
    panel.grid.minor = element_blank(),
    strip.text.x=element_text(size=15)
  ) +
  annotation_logticks(base = 10, sides = "l") +
  labs(caption = "Data source: https://github.com/CSSEGISandData/COVID-19")
p
