library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)
library(plotly)


# usgs site number of ben franklin bridge site
site <- "01467200"

site_info <- whatNWISdata(siteNumbers = site, service = "dv", statCd = "00003")

param_info <- site_info$parm_cd %>% unique() %>% readNWISpCode()


site_meta <- site_info %>% 
  select(site_no, station_nm, parm_cd) %>% 
  left_join(param_info %>% 
              select(parameter_cd, srsname, parameter_units), 
            by = c("parm_cd" = "parameter_cd")) %>% 
  filter(parm_cd %in% c("00010", "00095", "00300"))

param_choices <- site_meta$parm_cd
names(param_choices) <- site_meta$srsname

wq_plotly <- function(data){
  data %>%
    plot_ly(
      x = ~Date,
      y = ~result,
      type = "scatter",
      mode = "lines+markers",
      marker = list(
        size = 4,
        color = "blue"
      ),
      line = list(
        color = "blue"
      ),
      hoverinfo = "text",
      text = ~paste(
        "Site:", station_nm,
        "<br>Parameter:", srsname,
        "<br>Date Time:", format(Date),
        "<br>Result:", result,
        "<br>Units:", parameter_units
      )
    ) %>%
    layout(
      title = paste(
        unique(data$station_nm), "<br>", 
        unique(data$srsname), 
        paste0("(", unique(data$parameter_units), ")")
      ),
      titlefont = list(
        size = 10
      ),
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = ""
      ),
      margin = list(
        t = 40
      )
    )
}


ui <- shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML('.shiny-input-container{margin-top: 20px;}'))
    ),
    div(
      fluidRow(
        column(
          4, 
          selectInput(
            inputId = "parameter",
            label = "Select Parameter(s):",
            choices = param_choices,
            multiple = TRUE
          )
        ),
        column(
          4,
          dateRangeInput(
            inputId = "date",
            label = "Select Date Range:",
            start = Sys.Date() - days(31),
            end = Sys.Date()
          )
        ),
        column(
          4, 
          actionButton(
            inputId = "submit",
            label = "Apply Changes!",
            style = "margin:40px;"
          )
        )
      ),
      fluidRow(
        div(
          id = "plot-container",
          uiOutput(
            outputId = "graphs_ui"
          )
        )
      )
    )
  )
)


server <- shinyServer(
  function(input, output, session){
    session$onSessionEnded(stopApp)
    
    # query data from USGS API
    wq_data <- eventReactive(input$submit, {
      req(input$parameter, input$date)
      
      raw_data <- readNWISdv(
        siteNumbers = site, 
        parameterCd = input$parameter,
        startDate = input$date[[1]],
        endDate = input$date[[2]]
      )
      
      output <- raw_data %>% 
        select(-contains("_cd")) %>% 
        gather(key = "parameter", value = "result", contains("X_")) %>% 
        mutate(parameter = str_replace_all(parameter, "X_|_00003", "")) %>% 
        left_join(site_meta, by = c("parameter" = "parm_cd", "site_no")) 
      
      return(output)
    })
    
    # create a list of graphs - with one for each parameter selected
    graphs <- eventReactive(input$submit, {
      req(wq_data())
      
      wq_data() %>% 
        group_by(parameter) %>% 
        nest() %>% 
        mutate(
          graphs = map(data, wq_plotly) 
        ) %>% 
        arrange(parameter) %>% 
        pull(graphs)
    })
    
    # use purrr::iwalk to create a dynamic number of outputs
    observeEvent(input$submit, {
      req(graphs())
      
      iwalk(graphs(), ~{
        output_name <- paste0("plot_", .y)
        output[[output_name]] <- renderPlotly(.x)
      })
    })
    
    # use renderUI to create a dynamic number of output ui elements
    output$graphs_ui <- renderUI({
      req(graphs())
      
      plots_list <- imap(graphs(), ~{
        tagList(
          plotlyOutput(
            outputId = paste0("plot_", .y)
          ),
          br()
        )
        
      })
      
      tagList(plots_list)
    })
    
  }
)


shinyApp(ui = ui, server = server)