library(shiny)
library(gsheet)
library(tidyverse)
library(lubridate)
library(ggplot2)

#todo: 
#fit gray background for tab 2

#Import Data Table 2.1 from WHR 2020
dat2_1 <- "https://docs.google.com/spreadsheets/d/1bAzkkXU3W7LALAzP2cnaahbf-s52kr8GvjJ3pIaK9Cs/edit?usp=sharing"
whr2_1 <- gsheet2tbl(dat2_1, sheetid = NULL)

#Data wrangling
whr2_1 <- whr2_1 %>% rename(GDP = `Log GDP per capita`,
                            "LifeExpectancy" = `Healthy life expectancy at birth`,
                            "SocialSupport" = `Social support`,
                            "PerceptionsOfCorruption" = `Perceptions of corruption`)

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
                                        options = list(maxItems = 6, placeholder = 'Select a country name'),
                                        multiple = TRUE,
                                        selected = "United States"
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
                   sidebarPanel(
                     width = 12,
                     # Select up to 6 countries
                     selectizeInput(inputId = "selectizeCountry2", 
                                    label = "Choose up to 3 countries",
                                    choices = unique(whr2_1) %>% select(`Country name`),
                                    options = list(maxItems = 3),
                                    multiple = TRUE,
                                    selected = "United States"
                     ), #selectizeInput
                   ), #sidebarPanel
                   
                     radioButtons(inputId = "variables", 
                                  label = "Select a variable to display",
                                  #choices = c(GDP = "GDP", 
                                  #            `Life Expectancy` = "LifeExpectancy", 
                                  #            Generosity = "Generosity", 
                                  #            `Perceptions of corruption` = "`Perceptions of corruption`")
                                  choices = colnames(whr2_1)[c(6, 5, 9)] 
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
    
    output$plot2 <- renderPlot({
        whr2_1 %>% filter(`Country name` %in% c(input$selectizeCountry2)) %>% 
        ggplot(aes_string(x = input$variables, y = "`Life Ladder`")) +
        geom_point(aes(color = `Country name`)) +
        ylab("Happiness Ladder Score") +
        xlab(sprintf("%s", input$variables)) +
        ggtitle(sprintf("%s Across Different Countries", input$variables)) +
        # ggtitle(paste(str_remove(toString(input$variables), "(`|`|'|')")),
        #         "Across Different Countries") +
        scale_color_discrete(name = "Country")
    }) #renderPlot
}

# Run the application 
shinyApp(ui = ui, server = server)
