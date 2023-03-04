
## hiiiiiiii

library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(plotly)
library(dplyr)

a <- read_csv("dataset.csv")
# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Past Years Threatened Animals"),
  
  tabsetPanel(
    tabPanel(
      "Home",
      mainPanel(
        img(alt = "Animals", 
            src = "https://media.cntraveller.com/photos/611befdcdb797d0116fd4d86/16:9/w_3200,h_1800,c_limit/end.jpg",
            src="src", height="65%", width="65%", align="center"),
        h1("Project Overview"),
        p("The report provides a broad summary of threatened animals to be endangered within the 2000 - 2022.
             With the data, we hope to display the different animals and how likely they are to be endangered within the years. 
               We also hope to display the different types of threatened organisms which includes plants and fungi."),
        h2("Audience"),
        p("We assume people in general are all the potential targeted audiences for these set of analysis and data since we analyze this to inform the general public about the threatened animals around the world as a way to persuade them to make changes with their lifestyle
             to make a better world for both humans and animals. Some nonprofit animal shelters and environment protection organizations might also be interested in our set of data to concentrate on the urgent issues of the popularity of different species dying each day each year in different regions."),
        h2("Data Set"), 
        p("We will be working with the number of threatened species by major groups dataset made by the International Union for Conservation of Nature", a("(IUCN).", href='https://www.iucnredlist.org/resources/summary-statistics#Summary%20Tables'),
        "The dataset includes data from 2000 to 2022 and records the total number of assessed and threatened species, in total of 22 species and 29 variables are included. Some variables are included but not limited to: Mammals, Birds, Reptiles, Green algae, Mushrooms, Corals, etc. 
In this report, we are creating a more organized visual table and graph that shows the trend/changes in total threatened species from 2000 to 2022."),
        h2("Focus"),
        tags$ul(
          tags$li("The trend of threatened species over time periods, whether there is an increase or decrease
"),
          tags$li("Organization of specific species falling under each major group (4 major groups)
"),
        ),
        h2("Creators"),
        p("By:  Lisa Lu, Xintong Chen, Kyla Olitoquit, Anika Razdan"),
    )
    ),
      
  
    
    tabPanel(
      "Assessed & Threatened Species Over Time",
      sidebarLayout(
        sidebarPanel(
          p("You can analyze the threatened animals of different kinds. 
          Select the kind you are interested in. 
            You will see an anually plot and the corresponding trend lines."),
          selectInput("kind",
                      "Choose a kind of animal",
                      choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes",
                                  "Subtotal_Vertebrates","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet_worms",
                                  "Horseshoe_crabs","Other_invertebrates","Subtotal_Invertebrates","Mosses","Ferns_and_allies",
                                  "Gymnosperms","Flowering_plants","Green_algae","Red_algae","Subtotal_Plants","Lichens",
                                  "Mushrooms","Brown_algae","Subtotal_Fungi_and_protists","Total")),
          selectInput("palette", "Select a color palette:",
                      choices = c("Dark" = "Dark2",
                                  "Bright" = "Set1", "Pastel" = "Pastel1")),
        ),
        mainPanel(
          plotOutput("plot"),
          textOutput("text1")
        )
      )
    ),
    
    tabPanel(
      "Species in Major Groups",
      sidebarLayout(
        sidebarPanel(
          p("This panel displays the data about threatened animals 
          over different groups of organisms: 
            vertebrates, invertebrates, plants, and fungi&protists"),
          radioButtons("choice","Choose an option:",
                       c("Vertebrates" = "v",
                         "Invertebrates" = "i",
                         "Plants" = "p",
                         "Fungi & Protists" = "f")
          )
        ),
        mainPanel(
          textOutput("text2"),
          tableOutput("table2")
          
        )
      )
    )
)
)



server <- function(input, output) {
  ## output$text <- renderText({})
  
  output$plot <- renderPlot({
    k <- input$kind
    palette <- input$palette
    
    ggplot(data = a,(aes(Year, a[[k]], color = Category)))+
      geom_point()+
      geom_line(size=1.5)+
      scale_color_brewer(palette = palette)+
      labs(x = "Year", y = "Numbers")+
      theme(
        axis.line = element_line(size = 0.6),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
  })
  
  output$text1 <- renderText({
    paste("The average number of threatened ", input$kind, "from 2000 to 2022 is: ", 
          mean(a[[input$kind]][a$Category == "Total threatened"]))
  })
  
  output$table2 <- renderTable({
    c <- input$choice
    if(c == "v"){
      a %>% 
        select(Year, Category, Mammals, Birds, Reptiles, Amphibians, Fishes, Subtotal_Vertebrates)
    }
    else if(c == "i"){
      a %>% 
        select(Year, Category, Insects, Molluscs, Crustaceans, Corals, Arachnids, Velvet_worms,
               Horseshoe_crabs,Other_invertebrates,Subtotal_Invertebrates)
    }
    else if(c == "p"){
      a %>% 
        select(Year, Category, Mosses, Ferns_and_allies, Gymnosperms,Flowering_plants,
               Green_algae, Red_algae, Subtotal_Plants)
    }
    else if(c == "f"){
      a %>% 
        select(Year, Category, Lichens,Mushrooms,Brown_algae,Subtotal_Fungi_and_protists)
    }
  })
  
  output$text2 <- renderText({
    c <- input$choice
    if(c == "v"){
      paste("Increase of the total threatened (2000-2022): 10739-3507 = ", 10739-3507)
    }
    else if(c == "i"){
      paste("Increase of the total threatened (2000-2022): 6161-1928 = ", 6161-1928)
    }
    else if(c == "p"){
      paste("Increase of the total threatened (2000-2022): 24914-5611 = ", 24914-5611)
    }
    else if(c == "f"){
      paste("Increase of the total threatened (2000-2022): 294-0 = ", 294)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)