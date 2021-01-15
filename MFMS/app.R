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
    
    dat <- dbGetQuery(
        conn = con_mfms,
        statement = statement
    )
    
    dbDisconnect(con_mfms)
    
    return(dat)
    
}

theme_mfms <- function() {
    
    theme_minimal() +
        theme(
            panel.grid = element_blank()
            ,axis.title = element_text(face = "bold", size = 14)
            ,axis.text = element_text(size = 13)
            ,strip.text = element_text(face = "bold", size = 14)
            ,legend.position = "none"
        )
    
}

ui <- navbarPage(
    "Minimum Fat, Maximum Stats",
    theme = shinytheme("simplex"),
    collapsible = T,
    
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
                                    label = "Weight (lbs):",
                                    min = 0,
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
                                    min = 0,
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
                                    label = "Neck (cm):",
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
                                    label = "Biceps (cm):",
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
                                    label = "Chest (cm):",
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
                                    label = "Waist (cm):",
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
                                    label = "Thigh (cm):",
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
                            dataTableOutput("ent_tbl_activity")
                        )
                    )
                )
            )
        )
    ),
    
    tabPanel(
        "Squad Progress",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "neldo.jpg"),
                    hr()
                ),
                
                mainPanel(
                    width = 10,
                    column(
                        width = 3,
                        h2("Progress", align = "center"),
                        hr(),
                        plotOutput("prog_bar", height = 800)
                    ),
                    column(
                        width = 3,
                        h2("Progress to Goal", align = "center"),
                        hr(),
                        plotOutput("prog_totals", height = 800)
                    )
                    ,column(
                        width = 6,
                        h2("Individual Progress", align = "center"),
                        hr(),
                        plotOutput("prog_lines", height = 400),
                        hr(),
                        plotOutput("prog_goalperc", height = 400)
                    )
                )
            )
            
        )
    )

)

server <- function(input, output, session) {

    rv <- reactiveValues(
        
        entries = pull_data("SELECT * FROM dbo.MFMSEntries;"),
        users = pull_data("SELECT * FROM dbo.MFMSUsers;") %>% mutate(FullName = gsub(" .*$", "", FullName)),
        activity = pull_data("SELECT * FROM dbo.vw_MFMSActivity;") %>% mutate(FullName = gsub(" .*$", "", FullName)),
        targets = pull_data("SELECT * FROM dbo.MFMSTargets;")
        
    )
    
    refresh <- function() {
        
        rv$entries <- pull_data("SELECT * FROM dbo.MFMSEntries;")
        rv$users <- pull_data("SELECT * FROM dbo.MFMSUsers;") %>% mutate(FullName = gsub(" .*$", "", FullName))
        rv$activity <- pull_data("SELECT * FROM dbo.vw_MFMSActivity;") %>% mutate(FullName = gsub(" .*$", "", FullName))
        rv$targets <- pull_data("SELECT * FROM dbo.MFMSTargets;")
        
        updateTextInput(session, "ent_id", value = "")
        updateDateInput(session, "ent_date", value = today())
        updateNumericInput(session, "ent_weight", value = "")
        updateNumericInput(session, "ent_bmi", value = "")
        updateNumericInput(session, "ent_neck", value = "")
        updateNumericInput(session, "ent_chest", value = "")
        updateNumericInput(session, "ent_waist", value = "")
        updateNumericInput(session, "ent_biceps", value = "")
        updateNumericInput(session, "ent_thigh", value = "")
        
    }
    
    refresh()
    
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
                ,UserID %in% rv$users$UserID
            )
        
        if (length(data$UserID) > 0) {
            
            showModal(
                modalDialog(
                    title = "Valid Data Entry",
                    "Ok, you've filled in the right data - now for fuck's sake just click ok, wait a minute and don't click again. It should take around 5 seconds and your data will be in."
                )
            )
            
            push_data(conf$tables$entries, data)
            
            act <- data %>% 
                select(
                    UserID
                ) %>% 
                distinct()
            
            push_data(conf$tables$activity, act)
            
        } else {
            
            showModal(
                modalDialog(
                    title = "Invalid Data Entry",
                    "Please check that your ID is correct and that at least one field is filled in before inserting data."
                )
            )
            
        }
        
        refresh()
        
    })
    
    output$ent_tbl_activity <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 15),
            rownames = F,
            
            rv$activity %>% 
                mutate(
                    SubmissionHour = ifelse(nchar(hour(ImportTimestamp)) == 1, paste0("0", hour(ImportTimestamp)), hour(ImportTimestamp))
                    ,SubmissionMinute = ifelse(nchar(minute(ImportTimestamp)) == 1, paste0("0", minute(ImportTimestamp)), minute(ImportTimestamp))
                    ,SubmissionSecond = ifelse(nchar(round(second(ImportTimestamp),0)) == 1, paste0("0", round(hour(ImportTimestamp),0)), round(second(ImportTimestamp), 0))
                    ,SubmissionDate = date(ImportTimestamp)
                    ,SubmissionTime = paste(sep = ":", SubmissionHour, SubmissionMinute, SubmissionSecond)
                    ,Submitted = paste(sep = " ", SubmissionDate, SubmissionTime)
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                ) %>% 
                select(
                    Name = FullName
                    ,Submitted
                )
        )
        
    })
    
    output$prog_bar <- renderPlot({
        
        start <- rv$entries %>% 
            filter(
                Metric == "Weight"
            ) %>% 
            group_by(
                UserID
            ) %>% 
            arrange(
                desc(ImportTimestamp)
            ) %>% 
            slice(
                n()
            ) %>% 
            mutate(
                StartProgress = "Beginning Weight"
            )
        
        progress <- rv$entries %>% 
            filter(
                Metric == "Weight"
            ) %>% 
            group_by(
                UserID
            ) %>% 
            arrange(
                desc(ImportTimestamp)
            ) %>% 
            slice(
                1
            ) %>% 
            mutate(
                StartProgress = "Current Progress"
            )
        
        dat <- rbind(start, progress)
        
        dat %>% 
            group_by(
                StartProgress
            ) %>% 
            summarise(
                TotalWeight = sum(Value, na.rm = T)
            ) %>% 
            ggplot() +
            geom_col(aes(x = 1, y = TotalWeight, fill = StartProgress), color = "white") +
            geom_hline(yintercept = 1593, size = 1, linetype = "dashed", color = "firebrick") +
            theme_mfms() +
            theme(
                axis.title.x = element_blank()
                ,axis.text.x = element_blank()
            ) +
            labs(
                y = "Group Weight"
            ) +
            scale_y_continuous(
                breaks = c(seq(1400, 2000, 100))
                ,labels = function(x) {paste0(x, " lbs")}
            ) +
            scale_fill_manual(
                values = c(
                    "Beginning Weight" = "navy"
                    ,"Current Progress" = "goldenrod"
                )
            ) +
            coord_cartesian(
                ylim = c(1550, 1921)
            ) +
            facet_grid(.~StartProgress)
        
    })
    
    output$prog_goalperc <- renderPlot({
        
        losses <- rv$entries %>% 
            filter(
                Metric == "Weight"
            ) %>% 
            group_by(
                UserID
            ) %>% 
            arrange(
                ImportTimestamp
            ) %>% 
            slice(
                1
            ) %>% 
            left_join(
                by = "UserID",
                rv$users %>% 
                    select(
                        UserID
                        ,FullName
                        ,Height
                    )
            ) %>% 
            left_join(
                by = "UserID",
                rv$entries %>% 
                    filter(
                        Metric == "Weight"
                    ) %>% 
                    group_by(
                        UserID
                    ) %>% 
                    arrange(
                        ImportTimestamp
                    ) %>% 
                    slice(
                        n()
                    ) %>% 
                    select(
                        UserID
                        ,CurrentValue = Value
                        ,LatestRead = ReadDate
                    )
            ) %>% 
            left_join(
                by = "UserID",
                rv$targets %>% 
                    filter(
                        TargetMetric == "Weight"
                    ) %>% 
                    select(
                        UserID
                        ,TargetStart
                        ,TargetEnd
                        ,TargetValue
                    )
            ) %>% 
            mutate(
                TargetDiff = Value - TargetValue
                ,DiffPerc = (TargetDiff / Value) * 100
                ,CurrentLoss = Value - CurrentValue
                ,LossPerc = (CurrentLoss / TargetDiff) * 100
            )
        
        losses %>% 
            ggplot() +
            geom_bar(aes(x = FullName, weight = LossPerc, fill = LossPerc), color = "white", width = 0.6) +
            geom_hline(yintercept = 0, size = 0.8, color = "grey75", linetype = "dotted") +
            geom_hline(yintercept = 40, size = 0.8, color = "navy", linetype = "dotted") +
            geom_hline(yintercept = 75, size = 0.8, color = "dodgerblue", linetype = "dotted") +
            geom_hline(yintercept = 100, size = 0.8, color = "goldenrod", linetype = "dotted") +
            theme_mfms() +
            theme(
                axis.text.y = element_text(margin = margin(b = 5))
            ) +
            labs (
                x = ""
                ,y = "Percentage of Goal"
            ) +
            scale_y_continuous(
                breaks = c(seq(0, 100, 20))
                ,labels = function(x) {paste0(x, "%")}
            ) +
            scale_fill_gradient(
                low = "azure2", high = "goldenrod1"
            ) +
            coord_cartesian(
                ylim = c(0, 100)
            )
        
    })
    
    output$prog_lines <- renderPlot({
        
        entries <- rv$entries %>% 
            left_join(
                by = "UserID",
                rv$users %>% 
                    select(
                        FullName
                        ,UserID
                        ,Height
                    )
            )
        
        entries %>% 
            filter(
                Metric == "Weight"
                ,wday(ReadDate) == 6
            ) %>% 
            ggplot() +
            geom_line(aes(x = ReadDate, y = Value, group = FullName), color = "grey50", size = 0.5) +
            geom_point(aes(x = ReadDate, y = Value), color = "black", size = 3) +
            geom_point(aes(x = ReadDate, y = Value, color = FullName), size = 2.5) +
            theme_mfms() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
            ) +
            scale_color_brewer(
                palette = "YlGnBu"
            ) +
            facet_wrap(.~FullName, scales = "free_y")
        
    })
    
    output$prog_totals <- renderPlot({
        
        losses <- rv$entries %>% 
            filter(
                Metric == "Weight"
            ) %>% 
            group_by(
                UserID
            ) %>% 
            arrange(
                ImportTimestamp
            ) %>% 
            slice(
                1
            ) %>% 
            left_join(
                by = "UserID",
                rv$users %>% 
                    select(
                        UserID
                        ,FullName
                        ,Height
                    )
            ) %>% 
            left_join(
                by = "UserID",
                rv$entries %>% 
                    filter(
                        Metric == "Weight"
                    ) %>% 
                    group_by(
                        UserID
                    ) %>% 
                    arrange(
                        ImportTimestamp
                    ) %>% 
                    slice(
                        n()
                    ) %>% 
                    select(
                        UserID
                        ,CurrentValue = Value
                        ,LatestRead = ReadDate
                    )
            ) %>% 
            left_join(
                by = "UserID",
                rv$targets %>% 
                    filter(
                        TargetMetric == "Weight"
                    ) %>% 
                    select(
                        UserID
                        ,TargetStart
                        ,TargetEnd
                        ,TargetValue
                    )
            ) %>% 
            mutate(
                TargetDiff = Value - TargetValue
                ,DiffPerc = (TargetDiff / Value) * 100
                ,CurrentLoss = Value - CurrentValue
                ,LossPerc = (CurrentLoss / TargetDiff) * 100
            )
        
        losses %>% 
            ggplot(aes(y = reorder(FullName, TargetDiff))) +
            geom_segment(aes(x = CurrentLoss, xend = TargetDiff, yend = reorder(FullName, TargetDiff)), size = 0.8, linetype = "dashed") +
            geom_bar(aes(weight = CurrentLoss, fill = FullName), color = "black") +
            geom_point(aes(x = TargetDiff), color = "black", size = 6) +
            geom_point(aes(x = TargetDiff, color = FullName), size = 5) +
            theme_mfms() +
            theme(
                panel.grid.major.x = element_line(size = 0.5, color = "grey80", linetype = "dotted")
                ,axis.text.y = element_text(margin = margin(r = -15))
                ,axis.text.x = element_text(margin = margin(t = -5, b = 5))
            ) +
            labs(
                y = ""
            ) +
            scale_x_continuous(
                breaks = c(seq(0, 100, 10)),
                labels = function(x) {paste0(x, " lbs")}
            ) +
            scale_color_brewer(
                palette = "YlGnBu"
            ) +
            scale_fill_brewer(
                palette = "YlGnBu"
            )
        
    })

}

shinyApp(ui = ui, server = server)