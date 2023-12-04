#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(tidylog)
library(stringr)
library(janitor)
library(GGally)
library(cowplot)
library(forcats)
library(ggridges)
library(shinyjs)
library(scales)
library(viridis)
library(plotly)
library(bslib)
library(htmltools)
library(shinyjs)
#setwd("ShinyNationalTeam")

data <- read_xlsx("2023-national-team.xlsx")  |> clean_names()
data <- data |> 
  select(-c("data","type", "fst_name", "discipline", 
            "trainer_name", "current_year_result_plan",
            "former_year_result_plan", "former_year_result_fact")) |>
  slice(-1)
data$sex <- factor(data$sex, levels = c("чоловіки", "жінки"))

data$sex <- fct_recode(data$sex, !!!c("man" = "чоловіки", "woman" = "жінки"))
data <- data |> mutate(sport = as.factor(tolower(sport)), 
                       sex = as.factor(sex),
                       birth_date = as.Date(lubridate::dmy(birth_date)),
                       rank = as.factor(rank),
                       region = as.factor(region)) 

data <- data |> mutate(month = as.factor(month(birth_date))) |> filter( !is.na(month))
data$month <- fct_recode(data$month, 
                         !!!c("jan" = "1", "feb" = "2",
                              "mar" = "3", "apr" = "4", "may" = "5",
                              "jun" = "6", "jul" = "7", "aug" = "8",
                              "sep" = "9", "oct" = "10", "nov" = "11",
                              "dec" = "12"))

sports_to_select <- data |> select(sport) |> unique()


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
    # Application title
    titlePanel("Month of birth distribution in Ukraine's national non-olympic teams"),

  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
     
        sidebarPanel(
          tags$style(
            "#sidebarItemExpanded {
                overflow: auto;
                
            }"
          ),
      fluidRow( 
          column(width = 3,checkboxInput("sexBox", "Sex", value = TRUE)),
          column(width = 9, radioButtons("sexButton", label = NULL,
                              choices = c("man", "woman", "both"), 
                              selected = "both", inline = T))),
          radioButtons("sexAbsoluteButton", "comparison form", 
                              choices = c("absolute", "relative"), 
                              selected = "absolute", inline = T),     
      hr(style = "border-top: 4px dotted #ff0000;"),
          # Create a slider input for age range
      fluidRow(  
       column(width = 3, checkboxInput("ageBox", "Age", value = FALSE)),
      column(width = 9,radioButtons("ageAbsoluteButton", "comparison form", 
                                     choices = c("absolute", "relative", "heat map"), 
                                    selected = "absolute", inline = T) )),
      
          sliderInput("ageSlider", "Age", min = 1, max = 100, value = c(11, 26)),
      hr(style = "border-top: 4px dotted #ff0000;"),
          # Create a select input for sport options
      fluidRow( 
       column(width = 3, checkboxInput("sportBox", "Sport", value = FALSE)),
         column(width = 9, selectInput("sportList", NULL, 
                      choices = sports_to_select,
                      selected = c("dance sport", "chess", "checkers"),
                      multiple = TRUE))), 
      hr(style = "border-top: 4px dotted #ff0000;"),
      a("Source of data: 
Information on the composition of the national team of different age groups, Ministry of Youth and Sports", 
href = "https://data.gov.ua/dataset/national-teams/resource/f6cd6495-f23e-4a5a-aaee-5c54db636e1b", tags$a(size = 6))


      
        ),  mainPanel(
          
          tags$style("#mainPanel {overflow: auto; max-height: 100vh;}"),
          plotOutput("Plot") )
     

    ), 
)

# Define server logic required to draw a histogram
server <- function(input, output, session = session) {

  chosen_sex <- reactive(input$sexBox)
  chosen_age <- reactive(input$ageBox)
  chosen_sport <- reactive(input$sportBox)
  
  sexUser <- reactive(input$sexButton)
  ageMin <- reactive( input$ageSlider[1])
  ageMax <- reactive( input$ageSlider[2])
  sportUser <- reactive(input$sportList)
  sexAbsolute = reactive(input$sexAbsoluteButton == "relative" )
  ageAbsolute = reactive(input$ageAbsoluteButton)
  
  observeEvent(input$sportList , {
    if(is.null(sportUser())) {
      updateCheckboxInput(session, "sportBox", value = FALSE)
    }
  }, ignoreNULL = FALSE)
   
  observeEvent(input$sexBox, {
    if (!input$sexBox) {
     hide("sexButton", time = 0.5, animType = "slide") 
      hide("sexAbsoluteButton", time = 0.5, animType = "slide") 
    } else {
      show("sexButton", animType = "slide",
           time = 0.5) 
      show("sexAbsoluteButton", animType = "slide",
           time = 0.5) 
    }
  })
  observeEvent(input$ageBox, {
    if (!input$ageBox) {
      hide("ageAbsoluteButton", time = 0.5, animType = "slide") 
      hide("ageSlider", time = 0.5, animType = "slide") 
    } else {
      show("ageAbsoluteButton", animType = "slide",
           time = 0.5) 
      show("ageSlider", animType = "slide",
           time = 0.5) 
    }
  })
  observeEvent(input$sportBox, {
    if (!input$sportBox) {
      hide("sportList", time = 0.5, animType = "slide") 
    } else {
      show("sportList", animType = "slide",
           time = 0.5) 
      if(is.null(sportUser()))
      updateSelectInput(session, "sportList", selected = c("dance sport", "chess", "checkers"))
      
    }
  }
  
  )

    output$Plot <- renderPlot(       
{
#      sportUser |> reactiveValuesToList()|> nrow() |> print()
      #input$sportList |> class() |> print()
    #  (c("Ушу") %in% sportUser()) |> print()
   plot <-   if(chosen_age()){
        data_age <- data |> 
          filter(between(year(birth_date), 2023 - ageMax(), 2023 - ageMin()))

            data_age_sport <- if (chosen_sport()) {
              data_age |> filter((sport) %in% sportUser())
            } else {
              data_age
            }
            data_age_sport_sex <- if(chosen_sex()){
              if(sexUser() != "both" ){
              data_age_sport |> filter(sex == sexUser())
              } else{ 
               data_age_sport
              } 
             } else{
                data_age_sport
            } 

              data_age_sport_sex |>
                ggplot(aes(x = as.numeric(year(birth_date)))) + 
                {if(ageAbsolute() == "heat map"){
                  scale_fill_viridis(option = "rocket", direction = -1)}} + 
                {
                  if(ageAbsolute() == "heat map"){
                    geom_tile(data = data_age_sport_sex |> 
                                group_by(month, year(birth_date), sex)|>
                                mutate(people_number = n()),
                            aes(y = month, fill = people_number )) 
                }else
                  {
                    if(ageAbsolute() == "absolute"){ 
                        geom_bar( aes(fill = month))}
                    else{
                      geom_bar(position = "fill", aes(fill = month))
                    }                       
                    }
              } + {if(ageAbsolute() == "relative") 
                scale_y_continuous(name = "share", labels = x <- (function (x) paste(100 * x, "%"))) }+
                {if(ageAbsolute() != "heat map") scale_fill_manual(values = 
                                        c("#000099", "#1b98e0",
                                          "#00f700", "#008000",  "#004d00",
                                          "#ff7777",   "#ff0000", "#aa0000",
                                           "#ffff55",  "#ffff00",   "#aaaa00",
                                          "#9999ff"))  }+
              labs(fill = case_when(ageAbsolute() == "heat map" ~ "number",
                                    ageAbsolute() == "absolute" ~ "month",
                                    ageAbsolute() == "relative" ~ "month"))+ 
              scale_x_continuous(name = "age", 
                                 labels = x <- function (x) round(2023 - x),
                                 trans = scales::reverse_trans(), 
                                 limits = c(2023 - ageMin(), 2023 - ageMax()), n.breaks = ifelse(sexUser() == "both" & chosen_sex(), 8,16)) +
                {if(chosen_sport()){
                  if(chosen_sex()){
                    facet_grid( sport ~ sex , scales = ifelse(sexAbsolute(), "free_y", "fixed") )
                  } else{
                    facet_wrap(  ~ sport  , scales = ifelse(sexAbsolute(), "free_y", "fixed"), ncol =  2  )
                    
                  }
                } else {
                  if(chosen_sex()){
                    facet_wrap(  ~ sex ,scales = ifelse(sexAbsolute(), "free_y", "fixed"), ncol =  2  )
                  }
                }} 
                 
             
            } 
       else{  
         
         data_age <- if(chosen_sex()){
           if(sexUser() != "both"){
             data |> filter(sex == sexUser())
           } else { 
             data
           } 
         } else{
           data
         } 
         
         
           data_age_sport <- if(chosen_sport()){
              data_age |> filter((sport) %in% sportUser())
           }else {
             data_age 
           }

           data_age_sport |>
             ggplot(aes(x = month,
                        fill = month))+
             geom_bar(stat = "count") +  scale_x_reverse()+
             scale_x_discrete(name = "month of birth")+ 
             scale_fill_manual(values = 
                                 c("#000099", "#1b98e0",
                                   "#00f700", "#008000",  "#004d00",
                                   "#ff7777",   "#ff0000", "#aa0000",
                                   "#ffff55",  "#ffff00",   "#aaaa00",
                                   "#9999ff"))+
                
             
             {if(chosen_sex()){
               if(chosen_sport()){
                facet_grid(sport~sex, scales = ifelse(sexAbsolute(), "free_y", "fixed") ) 
               } else{
                 facet_wrap(~sex,scales = ifelse(sexAbsolute(), "free_y", "fixed"), ncol =  2 )
               }
             } else{
               if(chosen_sport()){
                 facet_wrap(~sport, scales = ifelse(sexAbsolute(), "free_y", "fixed"), ncol =  2 ) 
               }
             }}
         
         
         
     # plot(data.frame(x = (c(1:10)),y =  c(1:10)))
       } 
   height <- 500 + if(chosen_sport()){ 250 * (
       if_else(chosen_sex() & sexUser() == "both",
               length(sportUser())-1,
               numbers::div(length(sportUser()),2)))
   } else {0}
   
   
   
      plot + theme_bw() +   
        theme(legend.position = "top", legend.direction = "horizontal")+
        (if(!chosen_age()) theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                                legend.title =element_blank() ))
      
      
    }, res = 100,  height = function () {x <-500 + if(chosen_sport()){ 250 * (
      if_else(chosen_sex() & sexUser() == "both",
              length(sportUser())-1,
              numbers::div(length(sportUser()),2)))
    } else {0}})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
