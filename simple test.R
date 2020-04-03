library(magrittr)
library(tidyverse)
library(broom)

basic_load <- read.csv(
  "file:///C:/Users/rrogers/Documents/personalcode/COVID19_comparison_graph/data/nst-est2019-alldata.csv",
  na.strings = c(".", "NA", "", "?"),
  strip.white = TRUE, encoding = "UTF-8",
  stringsAsFactors = FALSE
)


summaryinfo <- basic_load[basic_load$SUMLEV != 40, ]
filtered_load <- basic_load[basic_load$SUMLEV == 40, ]
filtered_load <- filtered_load[
  ,
  c("REGION", "NAME", grep("(2018|2019)", colnames(filtered_load),
    value = TRUE
  ))
]
filtered_load %<>% filter(REGION!="X")

filtered_load <- filtered_load[1:18]

library(rattle)


rattle()


kdata<- sapply(na.omit(crs$dataset[, crs$numeric]), rescaler, "range")
row.names(kdata)<-crs$dataset$NAME

kmodel <-kmeans(kdata,4)

fviz_cluster(kmodel, kdata, repel = TRUE)

look<-fix_data_frame(data.frame(kdata), newcol= "NAME")[c( "NAME", grep("MIG", colnames(kdata), value = TRUE))]

                                             