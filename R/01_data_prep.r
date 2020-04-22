
if (exists("dat")) {
  print("already loaded")
} else {

  # library(curl)


  debugflag <- 0


  # if (!requireNamespace("BiocManager", quietly = TRUE))
  # install.packages("BiocManager")
  # BiocManager::install("BiocFileCache")
  #### DATA PREP #####

  # source: CSSE at Johns Hopkins University

  process_CSSEGIS <- function(file_name, column_groups, fields_to_remove, valuename) {
    if (debugflag) {
      file_name <- "time_series_covid19_confirmed_global.csv"
      column_groups <- c("Country.Region", "Province.State")
      fields_to_remove <- c("Lat", "Long")
    }


    tmp <- read.csv(
      paste0(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
        file_name
      ),
      stringsAsFactors = FALSE
    )

    tmp <- tmp %>% select(-all_of(fields_to_remove))
    tmp <- pivot_longer(tmp, -all_of(column_groups), names_to = "report_date", values_to = "result")

    tmp$report_date <- tmp$report_date %>%
      str_replace("X", "") %>%
      as.Date(format = "%m.%d.%y")

    tmp %<>% filter(!is.na(report_date))

    tmp_sum <- create_groups(tmp, column_groups)
    names(tmp_sum)[names(tmp_sum) == "result"] <- valuename
    return(tmp_sum)
  }

  create_groups <- function(idata, column_groups) {
    # careful! iterative!!
    debug_iteration_num_flag <- 0 # start at zero
    #  stopifnot(length(column_groups)>0)
    # idata<-tmp
    if (debugflag) {
      if (debug_iteration_num_flag > 0) {
        idata <- comb_data
        mod_data <- NULL
        data_store <- NULL
      } else {
        idata <- tmp
        column_groups <- c("Country.Region", "Province.State")
      }
    }


    lastcolname <- column_groups[length(column_groups)]
    print(lastcolname)

    data_store <- idata

    # have to refer to it as a data frame to make sure legacy grouping info is removed
    mod_data <- data.frame(idata[!is.na(pull(idata, lastcolname)) & !is.null(pull(idata, lastcolname)) & nchar(pull(idata, lastcolname)) > 1, ])


    mod_data$org <- mod_data %>%
      select(all_of(column_groups)) %>%
      apply(1, paste, collapse = "-")

    mod_data <- mod_data %>%
      group_by(org, report_date) %>%
      summarize(result = sum(result))

    # get ready to iterate
    data_store <- data_store %>% select(-all_of(lastcolname))

    if (length(column_groups) > 1) {
      comb_data <- bind_rows(mod_data, data_store)
      column_groups <- column_groups[1:(length(column_groups) - 1)]

      comb_data <- bind_rows(mod_data, data_store)

      # iterate
      create_groups(comb_data, column_groups)
    } else {
      comb_data <- bind_rows(mod_data, data_store)
      comb_data <- comb_data %>% filter(!is.na(org))
      return(comb_data)
    }
  }


  world_data_testpos %<-% {
    tmp_world <- process_CSSEGIS(
      "time_series_covid19_confirmed_global.csv",
      c("Country.Region", "Province.State"),
      c("Lat", "Long"),
      "test_pos"
    )
    return(tmp_world)
    # putting return into a variable assignment only works because it's also a future
  }

  world_data_death %<-% {
    tmp_world <- process_CSSEGIS(
      "time_series_covid19_deaths_global.csv",
      c("Country.Region", "Province.State"),
      c("Lat", "Long"),
      "deaths"
    )
    return(tmp_world)
    # putting return into a variable assignment only works because it's also a future
  }


  us_data_testpos %<-% {
    tmp_us <- process_CSSEGIS(
      "time_series_covid19_confirmed_US.csv",
      c("Country_Region", "Province_State", "Admin2"),
      c("Lat", "Long_", "FIPS", "Combined_Key", "UID", "iso2", "iso3", "code3"),
      "test_pos"
    )
    tmp_us %<>% filter(org != "US") # use the international data
    return(tmp_us)
  }


  us_data_death %<-% {
    tmp_us <- process_CSSEGIS(
      "time_series_covid19_deaths_US.csv",
      c("Country_Region", "Province_State", "Admin2"),
      c("Lat", "Long_", "FIPS", "Combined_Key", "UID", "iso2", "iso3", "code3"),
      "deaths"
    )
    tmp_us %<>% filter(org != "US") # use the international data
    return(tmp_us)
  }


  # source US census
  pop_states %<-% {
    print(getwd())
    tmp_pop_us <- fread(file = paste0(getwd(), "/../data/nst-est2019-alldata.csv"))
    tmp_pop_us %<>% select(NAME, POPESTIMATE2019)
    colnames(tmp_pop_us) <- c("NAME", "population")
    tmp_pop_us$NAME <- paste("US", tmp_pop_us$NAME, sep = "-")
    return(tmp_pop_us)
  }

  # world pop
  pop_country %<-% {
    tmp_pop_world <- fread(file = paste0(getwd(), "/../data/SYB62_1_201907_Population, Surface Area and Density.csv"), skip = 1)

    tmp_pop_world$location <- tmp_pop_world[, 2]
    tmp_pop_world %<>% filter(Year == "2019", Series == "Population mid-year estimates (millions)")
    tmp_pop_world$population <- as.numeric(tmp_pop_world$Value) * 1000000
    tmp_pop_world %<>% select(location, population)

    tmp_pop_world[tmp_pop_world$location == "China, Taiwan Province of China", "location"] <- "Taiwan*"
    tmp_pop_world[tmp_pop_world$location == "United States of America", "location"] <- "US"
    tmp_pop_world[tmp_pop_world$location == "Viet Nam", "location"] <- "Vietnam"
    tmp_pop_world[tmp_pop_world$location == "Russian Federation", "location"] <- "Russia"
    tmp_pop_world[tmp_pop_world$location == "Iran (Islamic Republic of)", "location"] <- "Iran"
    tmp_pop_world[tmp_pop_world$location == "Bolivia (Plurinational State of)", "location"] <- "Bolivia"
    tmp_pop_world[tmp_pop_world$location == "Republic of Korea", "location"] <- "Korea, South"
    return(tmp_pop_world)
  }


  # combine
  us_data <- inner_join(us_data_testpos, us_data_death, by = c("org", "report_date"))
  world_data <- inner_join(world_data_testpos, world_data_death, by = c("org", "report_date"))
  dat <- bind_rows(us_data, world_data)

  dat <- left_join(dat, pop_states, by = c("org" = "NAME"))
  dat <- left_join(dat, pop_country, by = c("org" = "location"))

  rm(
    us_data_testpos, 
    us_data_death,
    world_data_testpos, 
    world_data_death, 
    pop_states, 
    pop_country
  )

  dat$population <- ifelse(is.null(dat$population.x), dat$population.y, dat$population.x)
  dat %<>% select(-population.x, -population.y)
  dat <- data.frame(dat)
  #dat$org %<>% as.factor()

  all_orgs <- sort(unique(dat$org))
  all_orgs <- unique(c(all_orgs[grepl("^US", all_orgs)], all_orgs))


  default_org_list <- {
    c(
      "Italy"
      #  "Switzerland"
      # , "Germany"
      #  , "France"
      #  , "United Kingdom"
      #  , "US"
      # , "Austria"
      #  , "Japan"
      #  ,"Spain"
      # , "Iceland"
      #  ,"US"
      , "US-California"
      #    "US-California-Stanislaus",
      #    "US-California-Merced",
      #    "US-California-Tuolumne",
      #    "US-California-San Joaquin",
      #    "US-Tennessee",
      , "US-Tennessee-Williamson",
      "US-Tennessee-Davidson"
      # "China",
      # "Taiwan",
      # ,"US-New York"
      #    , "US-Florida",
      #    "US-New Jersey"
    )
  }
}
