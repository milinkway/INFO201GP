
library(shiny)
library(htmltools)
# Define UI for application that draws a histogram
ui <- fluidPage(
    img(src = "/Users/qianweilin/Desktop/Info201/FinalProject/rconnect/pic.jpeg"),
    titlePanel("Project Overview"),
    mainPanel(
       htmlOutput("description1"),
       textOutput("title2"),
       tags$head(tags$style("#title2{font-size: 23px;}"
       )
       ),
       htmlOutput("description2"),
       tags$head(tags$style("#description2{font-size: 13px;}"
       )
       ),
       textOutput("title3"),
       tags$head(tags$style("#title3{font-size: 23px;}"
       )
       ),
       htmlOutput("description3"),
       tags$head(tags$style("#description3{font-size: 13px;}"
       )
       ),
       textOutput("title4"),
       tags$head(tags$style("#title4{font-size: 23px;}"
       )
       ),
       htmlOutput("description4"),
       tags$head(tags$style("#description4{font-size: 13px;}"
       )
       ),
       textOutput("title5"),
       tags$head(tags$style("#title5{font-size: 23px;}"
       )
       ),
       htmlOutput("description5"),
       tags$head(tags$style("#description5{font-size: 13px;}"
       )
       )
    )
    
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$description1 <- renderText("The report provides a observation of suicide rate based on countries by different factors. 
    With the results, we hope to display <b>which group has the highest suicide rate</b> 
    the most in order to encourage better mental health education and support that 
    can lead to decrease in suicide rate and overall mental health growth.
")
    output$title2 <- renderText("Audience")
    output$description2 <- renderText("We believe that anyone in the global population can benefit 
    from this report, since learning about severe mental health problems, improving self-awareness,
    and trying to resolve are important to reduce suicide rate. The general population can gain the
    first one by reading our data and analysis, yet the latter two require our target audience, 
    who are <b>schools and governments able to provide education and support to weak mental health adolescences</b>,
    to provide support.")
    output$title3 <- renderText("Dataset")
    output$description3 <- renderText({
      paste0("We will be working with the ",
             "<a href = 'https://apps.who.int/gho/data/node.main.MENTALHEALTH?lang=en' target = '_blank'>World Suicide Report</a> ",
             "dataset collected and reported by the World Health Organization(WHO).
             The dataset includes data ranges from <i>2000 - 2019</i> depending on the specific dataset,
             with suicide rates estimate in different age groups from <i>10 - 49 years</i>, mental health service
             availability and governace, and human resources by country. Most of the data are from <i>2019</i>,
             as <i>2019</i> was the latest update version. Although the dataset contains multiple suicidal aspects, 
             we narrowed down the dataset and only utilized the following two datasets:
             <b>Mental Health Service Availbility, 
             and Suicide rate estimates age-standardized</b>.")
    })    
    output$title4 <- renderText("Questions")
    output$description4 <- renderText({
      HTML("<ul>
          <li>Which country has the highest suicide rate over time?</li>
          <li>How does the suicide rate in different sex?</li>
          <li>Which country has the best mental health service facilities?</li>
        </ul>")
    })
    output$title5 <- renderText("Creators")
    output$description5 <- renderText({
      paste("Linda Wang", "Katy Ye", "Maggie Qian", sep="<br/>")
      })
}


# Run the application 
shinyApp(ui = ui, server = server)

