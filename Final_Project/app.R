library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(htmltools)


a <- read_csv("dataset.csv") 


ui <- fluidPage(
 
## Title of the project
  titlePanel("Past Years Threatened Species"),

## division of the pages
## image inserted, headings, and sub-points
  tabsetPanel(
    tabPanel(
      "Home",
      mainPanel(
        img(alt = "Species", 
            src = "https://media.cntraveller.com/photos/611befdcdb797d0116fd4d86/16:9/w_3200,h_1800,c_limit/end.jpg",
            src="src", height="75%", width="75%", align="center"),
        h1("Project Overview"),
        p("The report provides a broad summary of threatened species to be endangered throughout the years 2000 - 2022.
             With the data, we hope to display different species, organisms such as plants and fungi, and how likely they are to be endangered within the years.") ,
        h2("Audience"),
        p("We assume that people in general are the potential audiences for this dataset and analysis. Since the purpose of this analysis is to inform the general public about the global threat to species and to persuade them to make changes with their lifestyle
             to make a better world for both humans and the organisms. Another set of audience that would be interested in this dataset would be some nonprofit animal shelters and environment protection organizations with the concentrate on the urgent issues of trend within different species dying each day and year in different regions."),
        h2("Data Set"), 
        p("We work with the dataset of the number of threatened species by major groups of organisms made by the International Union for Conservation of Nature", a("(IUCN).", href='https://www.iucnredlist.org/resources/summary-statistics#Summary%20Tables'),
        "The dataset includes data from 2000 to 2022 and records the total number of assessed and threatened species, a total of 22 species. Some of the species are included but not limited to: Mammals, Birds, Reptiles, Green algae, Mushrooms, Corals, etc. 
In this Web App, we are developing a more organized visual table and graphs to show the trend and the changes in total threatened species from 2000 to 2022."),
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
      
  
## another page    
    tabPanel(
      "Assessed & Threatened Species Over Time",
      sidebarLayout(
        sidebarPanel(
## sidebar options for you to choose: contains all species
          p("You can analyze the threatened species of different kinds. 
          Select the kind you are interested in. 
            You will see an anually plot and the corresponding trend lines."),
          selectInput("kind",
                      "Choose a kind of species",
                      choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes",
                                  "Subtotal_Vertebrates","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet_worms",
                                  "Horseshoe_crabs","Other_invertebrates","Subtotal_Invertebrates","Mosses","Ferns_and_allies",
                                  "Gymnosperms","Flowering_plants","Green_algae","Red_algae","Subtotal_Plants","Lichens",
                                  "Mushrooms","Brown_algae","Subtotal_Fungi_and_protists","Total")),
          selectInput("palette", "Select a color palette:",
                      choices = c("Pastel" = "Pastel1", "Dark" = "Dark2",
                                  "Bright" = "Set1")),
        ),
        mainPanel(
## Output shown: plot and summaries
          plotOutput("plot"),
          h3("As time goes on, the total number of threatened species increases."),
          h3("In addition, the increase in the total number of threatened is also associates with the increase in total number of assessed. (You can see it more clearly when choosing 'Amphibians'.)"),
          h4(textOutput("text1"))
          )
      )
    ),
## Another page    
    tabPanel(
      "Numbers of Species Range",
      sidebarLayout(
        sidebarPanel(
          p("You can manipulate the histogram plot by using the dropdown menu to choose the type of specie from the options below. There is also the option to examine the species from assessed versus threatened. When utilizing, histogram plot and bars will appear, showing different ranges and frequencies of species variety examined in the dataset." 
),
      selectInput("animal","Select species type",choices = names(a)[3:28]),
      selectInput("category","Select category type",choices= unique(a$Category)))
      ,
      mainPanel(
## histogram shown and summaries comes after
      plotOutput('Hist'),
      h3("When looking at the histogram, there is a correlation in the histogram bars between the species assessed and species threatened. When looking at species assessed, the bars are lower, but when looking at species threatened, the bars rise higher. This is due to the fact over time, the amount of species that are threatened continue to increase and rise over the years. 
"),
      h4("The average frequencies of ranges for the all the species had an average of 9.2
"),
    )
    )
    ),
## Another page  
    tabPanel(
      "Species in Major Groups",
      sidebarLayout(
        sidebarPanel(
          p("This panel displays the data about threatened species 
          over different groups of organisms: 
            vertebrates, invertebrates, plants, and fungi&protists"),
          radioButtons("choice","Choose an option:",
                       c("Vertebrates" = "v",
                         "Invertebrates" = "i",
                         "Plants" = "p",
                         "Fungi & Protists" = "f")
          )
        ),
## Table and summary shown
        mainPanel(
          textOutput("text2"),
          tableOutput("table2")
          
        )
      )
    ),
## Last page to summarize
    tabPanel(
      "Conclusion",
      sidebarLayout(
        sidebarPanel(
## Sidebar included all the species as options for audience to choose
          selectInput("kind2",
                      "Choose a kind of species",
                      choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes",
                                  "Subtotal_Vertebrates","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet_worms",
                                  "Horseshoe_crabs","Other_invertebrates","Subtotal_Invertebrates","Mosses","Ferns_and_allies",
                                  "Gymnosperms","Flowering_plants","Green_algae","Red_algae","Subtotal_Plants","Lichens",
                                  "Mushrooms","Brown_algae","Subtotal_Fungi_and_protists","Total"))
          ),
## gif inserted, table and bullet points as summaries are shown
        mainPanel(
          img(src = "http://images5.fanpop.com/image/photos/26300000/Panda-Gif-pandas-26334363-500-280.gif",
              height="250px", width="500px", align="center"),
          tableOutput("table3"),
          tags$ul(
            tags$li("One of the notable patterns we discovered in our project was that as the years go by, the two data (", em("total assessed"), "and ", em("total threatened"), "had a big difference with how each total increases as the years go by. For example, the ", strong("line plot"), "data for ", strong("Mammals"), ". The total number of generally assessed species dramatically increased within the years 2000 to 2005, then kept increasing after 2005. As for the total threatened species line plot, there was an increase, but not as dramatic as the total number of assessed species. But when looking at all the different species, both of the total assessed and total threatened are increasing.
"),
            tags$li("Another example was the ", strong("histogram"), ". Similar to the line plot, it shows the frequency of how many species were assessed and threatened, and there???s a clear trend that most of the threat is increasing depending if it were threatened or assessed. 
"),
            tags$li("The dataset is reasonable and unbiased in a way that it records the number of assessed and threatened species. We don???t see a potential population group that might be harmed but we are here to appeal to the public to protect the environment so that we can all provide a better living conditions for all living beings.
"),
            tags$li("One way to advance this project could be by collecting data on the specific factors influencing species to be threatened over the years. By examining the species, and the causes behind what is making them threatened over the years, this will allow scientists to have a better grasp. It will also aid them to help the variety of these species, and get their population numbers back up, and less species will be threatened overall. 
")
          )
        )
        )
      )
      
    )
)




server <- function(input, output) {
  ## output$text <- renderText({})
## server for plot  
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
## server for interactive text summarizing the average total number of threatened for each species  
  output$text1 <- renderText({
    paste("The average number of threatened ", input$kind, "from 2000 to 2022 is: ", 
          mean(a[[input$kind]][a$Category == "Total threatened"]))
  })
## Server for the table  
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
## Server for caculating the difference between 2022 and 2000  
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
## Server for histogram showing the frequency of number of ranges  
  output$Hist <- renderPlot({
    hist(data1(),breaks=10, col = "powderblue", main=paste("Histogram of",input$animal),
         xlab="Number of species")
  }) 
## Server for table in the last page, only shows stats from 2000 and 2022 by each species  
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
