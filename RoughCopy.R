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

conf <- config::get(file = "MFMS/config.yml")

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
                        ,legend.position = "none"
                )
        
}

entries <- pull_data("SELECT * FROM dbo.MFMSEntries;")
users <- pull_data("SELECT * FROM dbo.MFMSUsers;")
activity <- pull_data("SELECT * FROM dbo.vw_MFMSActivity;")
targets <- pull_data("SELECT * FROM dbo.MFMSTargets;")

entries <- entries %>% 
        left_join(
                by = "UserID",
                users %>% 
                        select(
                                FullName
                                ,UserID
                                ,Height
                        )
        )

entries %>% head()


entries %>% 
        ggplot() +
        geom_line(aes(x = ReadDate, y = Value, color = FullName, group = FullName), size = 0.5) +
        geom_point(aes(x = ReadDate, y = Value, color = FullName), size = 1.5) +
        theme_mfms() +
        theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
                ,strip.text = element_text(size = 14, face = "bold")
        ) +
        facet_grid(Metric ~ FullName)

entries %>% 
        filter(
                Metric == "Weight"
        ) %>% 
        group_by(
                FullName
        ) %>% 
        arrange(
                ImportTimestamp
        ) %>% 
        slice(
                1
        ) %>% 
        left_join(
                by = "UserID",
                entries %>% 
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
                targets %>% 
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
        geom_bar(aes(y = FullName, weight = LossPerc, fill = LossPerc), color = "white", width = 0.6) +
        geom_vline(xintercept = 0, size = 0.8, color = "grey75", linetype = "dotted") +
        geom_vline(xintercept = 40, size = 0.8, color = "navy", linetype = "dotted") +
        geom_vline(xintercept = 75, size = 0.8, color = "dodgerblue", linetype = "dotted") +
        geom_vline(xintercept = 100, size = 0.8, color = "goldenrod", linetype = "dotted") +
        theme_mfms() +
        theme(
                
        ) +
        labs (
                x = "Percentage of Goal"
                ,y = ""
        ) +
        scale_x_continuous(
                breaks = c(seq(0, 100, 20))
                ,labels = function(x) {paste0(x, "%")}
        ) +
        scale_fill_gradient(
                low = "azure2", high = "goldenrod1"
        ) +
        coord_cartesian(
                xlim = c(0, 100)
        )