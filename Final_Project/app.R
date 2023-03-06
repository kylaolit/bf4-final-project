
## hiiiiiiii

library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(htmltools)


a <- read_csv("dataset.csv") 


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
      "Comparison between 2000 and 2022",
      sidebarLayout(
        sidebarPanel(
      selectInput("animal","select a species type",choices = names(a)[3:28]),
      selectInput("category","select a category type",choices= unique(a$Category)))
      ,
      mainPanel(
      plotOutput('Hist')
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
    ),
    tabPanel(
      "Conclusion",
      sidebarLayout(
        sidebarPanel(
          selectInput("kind2",
                      "Choose a kind of animal",
                      choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes",
                                  "Subtotal_Vertebrates","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet_worms",
                                  "Horseshoe_crabs","Other_invertebrates","Subtotal_Invertebrates","Mosses","Ferns_and_allies",
                                  "Gymnosperms","Flowering_plants","Green_algae","Red_algae","Subtotal_Plants","Lichens",
                                  "Mushrooms","Brown_algae","Subtotal_Fungi_and_protists","Total"))
          ),
        mainPanel(
          img(alt = "Conclusion", 
              src = "https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.iucnredlist.org%2Fresources%2Fsummary-statistics&psig=AOvVaw3ce4i1A6m7BBBBzzskFF3m&ust=1678231564548000&source=images&cd=vfe&ved=0CA8QjRxqFwoTCMjt_vO5yP0CFQAAAAAdAAAAABAE",
              src="src", height="50%", width="50%", align="left"),
          tableOutput("table3"),
          tags$ul(
            tags$li("The dataset is reasonable and unbiased in a way that records the number of assessed and threatened species. We don’t see a potential population group that might be harmed but we are here to appeal to the public to protect the environment so that we all can provide better living conditions for all living beings.
"),
            tags$li("One way to advance this project could be by collecting data on the specific factors influencing animal species to be threatened over the years. By examining the animal species, and what causes are making them threatened over the years, this will allow scientists to have a better grasp. It will also aid them to help the variety of these species, and get their population numbers back up, and less animal species will be threatened overall. 
")
          )
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
  
  data1 <- reactive({
    subset(a,Category==input$category) %>%
      pull(input$animal)
  })
  
  output$Hist <- renderPlot({
    hist(data1(),breaks=10, main=paste("Histogram of",input$animal),
         xlab="Number of species")
  }) 
  
  output$table3 <- renderTable({
    k2 <- input$kind2 
    a %>% 
      filter(Year == 2022 | Year == 2000) %>% 
      select(Year, Category, k2) %>% 
      arrange(Year)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
