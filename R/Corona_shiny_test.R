#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
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
library(future.callr)
library(doFuture)
library(furrr)
options(shiny.error = browser) 
#options(shiny.error = recover)

options(future.global.maxSize = 1500000)
# options(future.packages = "magrittr")

# devtools::install_github('seandavi/sars2pack')
# library(sars2pack)

plan(callr) # startup a new future for each instance

registerDoFuture()

#### DATA PREP #####

source("./01_data_prep.r")

##################



daysbehind_plot <- function(gdat, #reactive
                            tmp_filter_days_to_go_back, #non-reactive
                            tmp_max_mod, 
                            tmp_maxdate, 
                            tmp_max_cases,
                            tmp_target_org
                            ) {
  # apply model against remaining entities to show how many days behind target org they are
  #note - only data, no org info getting passed
  gdat <- gdat %>%
    mutate(
      date_diff =
        round(
          as.numeric(
            -(coef(tmp_max_mod)[1] - (mean(log10(test_pos) - coef(tmp_max_mod)[2] * daystoday))) / coef(tmp_max_mod)[2]
          ),
          0
        )
    )

  gdat$date_lag <- gdat$report_date + gdat$date_diff
  gdat$diff_today_lag <- as.numeric(gdat$date_lag - tmp_maxdate)

  gdat <- filter(gdat, diff_today_lag > tmp_filter_days_to_go_back)

  return(
    ggplot(gdat, aes(x = diff_today_lag, y = test_pos)) +
      geom_abline(intercept = coef(tmp_max_mod)[1], slope = coef(tmp_max_mod)[2], linetype = 2) +
      geom_point(
        aes(colour = org_factor), 
        size = 3) +
      geom_line(
        aes(colour = org_factor), 
        size = 0.75) +
      xlab(paste("Lag in days behind ", tmp_target_org, " (", tmp_max_cases, " cases on ", format(tmp_maxdate, format = "%m%-%d-%Y"), ")", sep = "")) +
      ylab("Confirmed SARS-CoV-2 cases") +
      scale_y_continuous(
        limits = c(10, NA),
        trans = "log10"
      ) +
      scale_x_continuous(breaks = seq(-1000, 0, 5))  +
       gghighlight(
        aes(group = org_factor),
         use_direct_label = TRUE,
         label_key = date_diff,
         label_params = list(point.padding = 1, nudge_y = -0.9, nudge_x = 1.2, size = 5.5),
         unhighlighted_params = list(colour = grey(0.9), alpha = 0.75)
       ) +
       # geom_hline(aes(yintercept = graph_dat$population[!duplicated(graph_dat$org)]*pop_percentage))+
       theme(
         axis.title.y = element_text(colour = "black", size = 17, hjust = 0.5, margin = margin(0, 12, 0, 0)),
         axis.title.x = element_text(colour = "black", size = 17, margin = margin(10, 0, 0, 0)),
         axis.text.x = element_text(colour = "black", size = 15),
         axis.text.y = element_text(colour = "black", size = 15),
         legend.position = "none",
         legend.text = element_text(size = 12.5),
         legend.key = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.title = element_text(size = 15),
         panel.grid.minor = element_blank(),
         strip.text.x = element_text(size = 15)
       ) +
       annotation_logticks(base = 10, sides = "l") 
  #    + labs(caption = "Data source: https://github.com/CSSEGISandData/COVID-19")
   )
}


# Define UI for application that draws a histogram
ui <- fluidPage({
  # Fluid page description https://shiny.rstudio.com/articles/layout-guide.html
  # based on page width of '12' columns

  # Application title
  titlePanel("Corona virus data comparison")

  # Sidebar with a slider input for number of bins
  sidebarLayout({
    sidebarPanel(
      pickerInput(
        inputId = "filter_list",
        label = "Select/deselect all + format selected",
        choices = all_orgs,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE,
        selected = default_org_list
      ),
      sliderInput(
        inputId = "pop_pct",
        label = "Herd immunity at what % of population?",
        min = 1,
        max = 100,
        value = 50
      ),
      sliderInput(
        inputId = "min_cases",
        label = "Minimum number of cases to count as day 1",
        min = 1,
        max = 250,
        value = 10
      )
    )},

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow({
        column(
          12,
          # column width (out of 12 columns, per fluid row)
          # offset = 0, #offset moves item x columns to the right
          "Org level",
          # organization level grouping - create one big group for org level
          fluidRow(
            column(
              width = 8,
              # column width (out of 12 columns total)
              # offset = 0, #offset moves item x columns to the right
              # show graphs for each level
              "graph 1"

            ),
            column(
              width = 4, # column width (out of 12 columns total)
              # offset = 0, #offset moves item x columns to the right
              # show graphs for each level
              "graph 2"
            )
          )
        )
      })
      ,
      fluidRow({
        div(
          
          id= "plot-contianer",
          uiOutput(
            outputId = "graphs_ui"
          )
        )


        })

    )
  )
})

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  #TODO convert this to a filter
   filter_days_to_go_back <- -100

   #TODO convert this to a single value dropdown option - all countries
  target_org <- "Italy"
  pop_percentage <- reactive({
    input$pop_pct
  })

  # subset data to graph
  graph_dat <- reactive({
    filter(dat, org %in% input$filter_list, test_pos >= input$min_cases) %>%
      mutate(
        daystoday = as.numeric(report_date - max(report_date, na.rm = TRUE))      
        ) %>%
      group_by(org) %>%
      arrange(report_date, .by_group = TRUE) %>%
      mutate(
        chg_from_prev = replace_na(test_pos - lag(test_pos),0),
        org_factor= as.factor(org)
      )
  })


  maxdate <- reactive(max(graph_dat()$report_date, na.rm = TRUE))

  # find max number of cases in target org
  max_cases <- reactive({
    max( graph_dat()[ graph_dat()$org == target_org, "test_pos"],  na.rm = TRUE)
  })

  # Create model of disease progression in target org
  max_mod <- reactive({
    graph_dat() %>%
      filter(org == target_org) %>%
      lm(log10(test_pos) ~ daystoday, .)
  })

  ### Source for variable number of items list
  ### https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/

  ggdaysbehind <- reactive(
     
    #TODO convert this into a for-each loop so that it runs in parallel and passes all the data to each graph, but each graph highlights it's own org
    #TODO add 
   { graph_dat() %>%
      mutate(org2= org
             ) %>% #create a duplicate of org so that one of them will go into the data frame

      group_by(org2) %>%
      nest() %>%
      mutate(
      graphs = map(data,
                      daysbehind_plot
                      
               ,filter_days_to_go_back,
               max_mod(), 
               maxdate(), 
               max_cases(),
               target_org
               )
        ) %>%
      arrange(!(org2==target_org), org2) %>%  #order the results, target first
      pull(graphs) 
     }
)

  ## 2- output plots
  observe({
    req(ggdaysbehind())

    iwalk(ggdaysbehind(), ~ {
      output_name <- paste0("plot_", .y)
      output[[output_name]] <- renderPlot(.x+ scale_x_reverse())
      

      
    })

  })

  ## create alt graph- output plots
  observe({
    #req(ggdaysbehind())

    iwalk(ggdaysbehind(), ~ {
      output_name <- paste0("plot_alt", .y)
      output[[output_name]] <- renderPlot(.x)
    })

  })

  
 ## 3- render UI (JS? HTML?) code to send to UI so that it can handle the variable list
   output$graphs_ui <- renderUI({
     req(ggdaysbehind())
  
     plots_list <- imap(ggdaysbehind(), ~ {
       list(
         column(4,
          plotOutput( outputId = paste0("plot_", .y) )
         ),
        column(4,
          plotOutput( outputId = paste0("plot_alt", .y) )
        ),
        br()
       ) 
     })
      
     
     tagList(plots_list)
   })

  
      

  
  ####### original shiny example
  #   output$distPlot <- renderPlot({
  #     # generate bins based on input$bins from ui.R
  #     x <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$pop_pct + 1)
  #
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = "darkgray", border = "white")
  #   })
  #
}

# Run the application
shinyApp(ui = ui, server = server)


#stuff to add on later

#
# multiInput(
#     inputId = "Id010",
#     label = "Countries :",
#     choices = NULL,
#     choiceNames = lapply(seq_along(countries),
#                          function(i) tagList(tags$img(src = flags[i],
#                                                       width = 20,
#                                                       height = 15), countries[i])),
#     choiceValues = countries
# )
