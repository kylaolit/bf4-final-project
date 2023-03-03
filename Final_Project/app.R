
## hiiiiiiii

library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(plotly)
library(dplyr)

a <- read_csv("../../ps06-shinyapps-CandiceTTt/ps06_shinyapp_Candicetttt/Numbers of threatened species by major groups of organisms (2000-2022).csv")
# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Past Years Threatened Animals"),
  
  tabsetPanel(
    tabPanel(
      "About",
      mainPanel(
        em("This app uses previous years threatened animals data from "),
        strong("IUCN"),
        p("We have ", n_distinct(a$Year), "years of data for the threatened animals."),
        p("Numbers of threatened species by major groups of organisms"),
        p("Here is a small (random) sample of data:"),
        tableOutput("table")
      )
    ),
    
    tabPanel(
      "Plot",
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
      "Table",
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
  output$table <- renderTable({
    a %>%
      filter(Year != "") %>% 
      sample_n(5)
  })
  
  output$plot <- renderPlot({
    k <- input$kind
    palette <- input$palette
    
    ggplot(data = a,(aes(Year, a[[k]], color = Category)))+
      geom_point()+
      geom_line()+
      scale_color_brewer(palette = palette)+
      labs(x = "Year", y = "Numbers")
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