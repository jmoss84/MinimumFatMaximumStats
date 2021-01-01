rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(config)
library(DBI)
library(httr)
library(rvest)
library(jsonlite)
library(AzureAuth)
library(AzureKeyVault)
library(DT)
library(shiny)
library(shinythemes)

conf <- config::get()

push_data <- function(table, dataframe) {
    
    con_str <- conf$con_str
    
    con_mfms <- dbConnect(
        odbc::odbc(),
        .connection_string = con_str,
        timeout = 5
    )
    
    dbAppendTable(
        conn = con_mfms,
        name = SQL(table),
        value = dataframe
    )
    
    dbDisconnect(con_mfms)
    
}

pull_data <- function(statement) {
    
    con_str <- conf$con_str
    
    con_mfms <- dbConnect(
        odbc::odbc(),
        .connection_string = con_str,
        timeout = 5
    )
    
    dbGetQuery(
        conn = con_mfms,
        statement = statement
    )
    
    dbDisconnect(con_mfms)
    
}

ui <- navbarPage(
    "Minimum Fat, Maximum Stats",
    
    tabPanel(
        "Check-In",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "neldo.jpg"),
                    hr()
                ),
                
                mainPanel(
                    column(
                        width = 8,
                        h2("Value Entry", align = "center"),
                        hr(),
                        h3("User ID", align = "center"),
                        fluidRow(
                            column(
                                width = 12,
                                textInput(
                                    "ent_id",
                                    label = "ID:",
                                    value = ""
                                )
                            )
                        ),
                        hr(),
                        h3("Check-In Details", align = "center"),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "ent_date",
                                    label = "Entry date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    "ent_weight",
                                    label = "Weight:",
                                    min = 150,
                                    max = 350,
                                    step = 0.1,
                                    value = ""
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    "ent_bmi",
                                    label = "BMI:",
                                    min = 75,
                                    max = 180,
                                    step = 0.1,
                                    value = ""
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 2,
                                numericInput(
                                    "ent_neck",
                                    label = "Neck:",
                                    min = 0,
                                    max = 150,
                                    step = 0.05,
                                    value = ""
                                )
                            ),
                            column(
                                width = 2,
                                numericInput(
                                    "ent_biceps",
                                    label = "Biceps:",
                                    min = 0,
                                    max = 150,
                                    step = 0.05,
                                    value = ""
                                )
                            ),
                            column(
                                width = 2,
                                numericInput(
                                    "ent_chest",
                                    label = "Chest:",
                                    min = 0,
                                    max = 150,
                                    step = 0.05,
                                    value = ""
                                )
                            ),
                            column(
                                width = 2,
                                numericInput(
                                    "ent_waist",
                                    label = "Waist:",
                                    min = 0,
                                    max = 150,
                                    step = 0.05,
                                    value = ""
                                )
                            ),
                            column(
                                width = 2,
                                numericInput(
                                    "ent_thigh",
                                    label = "Thigh:",
                                    min = 0,
                                    max = 150,
                                    step = 0.05,
                                    value = ""
                                )
                            ),
                            column(
                                width = 2,
                                br(),
                                actionButton(
                                    "ent_go_submit",
                                    label = "Submit",
                                    icon = icon("check")
                                )
                            )
                        ),
                        hr()
                    ),
                    column(
                        width = 4,
                        column(
                            width = 12,
                            h2("Submission Table", align = "center"),
                            hr(),
                            dataTableOutput("")
                        )
                    )
                )
            )
        )
    )

)

server <- function(input, output, session) {

    rv <- reactiveValues(
        
        entries = pull_data("SELECT * FROM dbo.MFMSEntries;"),
        users = pull_data("SELECT * FROM dbo.MFMSUsers;"),
        activity = pull_data("SELECT * FROM dbo.MFMSActivity;")
        
    )
    
    refresh <- function() {
        
        entries <- pull_data("SELECT * FROM dbo.MFMSEntries;")
        users <- pull_data("SELECT * FROM dbo.MFMSUsers;")
        activity <- pull_data("SELECT * FROM dbo.MFMSActivity;")
        
    }
    
    observeEvent(input$ent_go_submit, {
        
        data <- data.frame(
            UserID = c(input$ent_id)
            ,Metric = c("Weight", "BMI", "Neck", "Biceps", "Chest", "Waist", "Thigh")
            ,Value = c(input$ent_weight, input$ent_bmi, input$ent_neck, input$ent_biceps, input$ent_chest, input$ent_waist, input$ent_thigh)
            ,ReadDate = c(input$ent_date)
        ) %>% 
            filter(
                !is.na(Value)
                ,Value != ""
                ,UserID %in% users$UserID
            )
        
        push_data(conf$tables$entries, data)
        
    })

}

shinyApp(ui = ui, server = server)