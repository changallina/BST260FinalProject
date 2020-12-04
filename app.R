library(shiny)
library(gsheet)
library(tidyverse)
library(lubridate)
library(ggplot2)

#Import Data Table 2.1 from WHR 2020
dat2_1 <- "https://docs.google.com/spreadsheets/d/1bAzkkXU3W7LALAzP2cnaahbf-s52kr8GvjJ3pIaK9Cs/edit?usp=sharing"
whr2_1 <- gsheet2tbl(dat2_1, sheetid = NULL)

# Define UI 
ui <- fluidPage(
    headerPanel("Dynamics of Happiness"),
    tabsetPanel(
        tabPanel("Cantril Ladder",  
                 sidebarLayout(
                     sidebarPanel(
                         # Select up to 6 countries
                         selectizeInput(inputId = "selectizeCountry", 
                                        label = "Choose up to 6 countries",
                                        choices = unique(whr2_1) %>% select(`Country name`),
                                        options = list(maxItems = 6, placeholder = 'Select a country name')
                         ) #selectizeInput
                     ), #sidebarPanel
                     
                     mainPanel(
                         plotOutput("plot")
                     ), #mainPanel
                 ) #sidebarLayout
            ), #tabPanel 
        
        tabPanel("World Dynamics of Other Variables",
             sidebarLayout(
                 sidebarPanel(
                     radioButtons(inputId = "variables", 
                                  label = "Select a variable to display",
                                  choices = c(`Social support` = "'Social support'", 
                                              `Life expectancy` = "`Healthy life expectancy at birth`", 
                                              Generosity = "Generosity", 
                                              `Perceptions of corruption` = "`Perceptions of corruption`")
                     ) #radioButtons
                 ), #sidebarPanel
                 
                mainPanel(
                    plotOutput("plot2")
                ), #mainPanel  
            ) #sidebarLayout
        ) #tabPanel
    ) #tabsetPanel
) #fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderPlot({
            whr2_1 %>% filter(`Country name` %in% c(input$selectizeCountry)) %>% 
            ggplot(aes(x = year, y = `Life Ladder`)) +
                geom_line(aes(color = `Country name`)) +
                ylab("Cantril Ladder") +
                ggtitle("Happiness") +
                scale_color_discrete(name = "Country") +
                theme(axis.text.x = element_text(angle = 70, hjust =1)) +
                scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
                xlab("Year")
        
    }) #renderPlot
    
    
    #TO DO - change Generosity to input$variables
    output$plot2 <- renderPlot({
        whr2_1 %>% filter(`Country name` %in% c(input$selectizeCountry)) %>% 
        ggplot(aes_string(x = "year", y = input$variables)) +
        scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
        geom_line(aes(color = `Country name`)) +
        xlab("Year") +
        ylab(sprintf("%s", input$variables)) +
        ggtitle(sprintf("%s Across Different Countries", input$variables)) +
        # ggtitle(paste(str_remove(toString(input$variables), "(`|`|'|')")),
        #         "Across Different Countries") +
        scale_color_discrete(name = "Country")
    }) #renderPlot
}

# Run the application 
shinyApp(ui = ui, server = server)
