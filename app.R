library(shiny)
library(gsheet)
library(tidyverse)
library(lubridate)
library(ggplot2)

#todo: 
#fit gray background for tab 2

#Import Data Table 2.1 from WHR 2020
dat2_1 <- "https://docs.google.com/spreadsheets/d/1oMzplKOmjs0hKKmXKqOCM90CvPsS7aCIj1adox0nvNU/edit#gid=1261774380"
whr2_1 <- gsheet2tbl(dat2_1, sheetid = NULL)

#Data wrangling
whr2_1 <- whr2_1 %>% rename(GDP = `Log GDP per capita`,
                            LifeExpectancy = `Healthy life expectancy at birth`,
                            SocialSupport = `Social support`,
                            PerceptionsOfCorruption = `Perceptions of corruption`,
                            FreedomToMakeLifeChoices = `Freedom to make life choices`
                            )
country.list <- unique(whr2_1$`Country name`)
#deploy shiny
library(rsconnect)
#rsconnect::deployApp('/Users/allinachang/Desktop/BST260/BST260FinalProject/')

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
                                        choices = country.list,
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
                                    choices = country.list,
                                    options = list(maxItems = 3),
                                    multiple = TRUE,
                                    selected = "United States"
                     ), #selectizeInput
                   ), #sidebarPanel
                   
                   sliderInput("slider_year", 
                               label ="Years", 
                               min = min(whr2_1$year), 
                               max = max(whr2_1$year), 
                               value = c(2015, 2019),
                               sep = "",
                               ticks = FALSE), # year range
                   
                     radioButtons(inputId = "variables", 
                                  label = "Select a variable to display",
                                  #choices = c(GDP = `Log GDP per capita`,
                                  #LifeExpectancy = `Healthy life expectancy at birth`,
                                  #SocialSupport = `Social support`,
                                  #PerceptionsOfCorruption = `Perceptions of corruption`,
                                  #FreedomToMakeLifeChoices = `Freedom to make life choices`)
                                  
                                  choiceNames = c("Log GDP per capita",
                                                    "Social support",
                                                  "Healthy life expectancy at birth",
                                                      "Freedom to make life choices",
                                                      "Generosity",
                                                      "Perceptions of corruption"),
                                  choiceValues = colnames(whr2_1)[4:9]
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
    years <- reactive(input$slider_year)
    output$plot2 <- renderPlot({
        whr2_1 %>% filter(`Country name` %in% c(input$selectizeCountry2), year %in% seq(years()[1], years()[2])) %>% 
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
