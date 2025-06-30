#Things I want to work to add in the future
#add some sort of point system (maybe)
#Have it so when you click on cricket button you could scroll down to the one you wanna here and click submit and hear it
#when you click cricket then on a scientific name the common name will change too
#the images are blurry I wanna go through and maybe put in new pics

library(shiny)
library(dplyr)

# Load metadata from CSV (I decided to make 2 seperate csv's for crickets and katydids)
cricket <- read.csv("Crickets.csv", stringsAsFactors = FALSE)
katydid <- read.csv("Katydid.csv", stringsAsFactors = FALSE)

# UI Section (I just added color)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e7f1dc;
        font-family: 'Segoe UI', sans-serif;
     }
       #cricket, #katydid {
      background-color: #6c9a8b;
      color: white;
    }
       #cricket:hover, #katydid:hover {
      background-color: #55877c;
    }
      .btn {
        background-color: #a3c585;
        color: white;
      }
      
      .btn:hover{
        background-color: #99b878;
        color: white;
      }
      #hint {
        background-color: #f39c12;
        color: white;
      }
      #hint:hover {
        background-color: #e67e22;
        color: white;
      }
      #submit {
        background-color: #acd8a7;
        color: white;
      }
      #submit:hover {
        background-color: #8bca84;
        color: white;
      }
  ")),    
  ),
  
  titlePanel('Ortho ID'), 
  sidebarLayout(
    sidebarPanel(

#Adding 2 more buttons
      actionButton("cricket", "Cricket"),
      actionButton("katydid", "Katydid"),
      
      actionButton("new_test", "Orthopterate!"),
      selectInput('species_answer', "Choose the correct scientific name:", choices = NULL), 
      selectInput('common_answer', "Choose the correct common name:", choices = NULL), #ADDED NEW BUTTON
      actionButton('submit', 'Submit'), 
      
#Added a hint button 
      actionButton('hint','Hint'),
      textOutput('feedback')
    ), 
    mainPanel(
      uiOutput('audio_player'),
      uiOutput('wave_displayer'),
      uiOutput('image_displayer')
    )
  )
)

# Server Section
server <- function(input, output, session) {
  quiz_data <- reactiveValues(file = NULL, species = NULL, common = NULL, group=NULL)

  # Cricket button
  observeEvent(input$cricket, {
    quiz_data$group <- "cricket"
    updateSelectInput(session, 'species_answer', choices = unique(cricket$species))
    output$audio_player <- renderUI(NULL)  # Clear audio
    output$wave_displayer <- renderUI(NULL)
    output$image_displayer <- renderUI(NULL)
  })

  # Katydid button
  observeEvent(input$katydid, {
    quiz_data$group <- "katydid"
    updateSelectInput(session, 'species_answer', choices = unique(katydid$species))
    output$audio_player <- renderUI(NULL)  # Clear audio
    output$wave_displayer <- renderUI(NULL)
    output$image_displayer <- renderUI(NULL)
  })

  # Select a random file and generate options
  observeEvent(input$new_test, {
    req(quiz_data$group) #makes sure cricket or katydid is selected before continuing
    metadata<-if(quiz_data$group=="cricket")cricket else katydid #if else statment so determine which one is being used
      
    selected <- metadata[sample(nrow(metadata), 1), ]
    quiz_data$file <- selected$filename
    quiz_data$species <- selected$species
    quiz_data$common <- selected$common
    quiz_data$wave <- selected$Wavelength
    quiz_data$hint <- selected$Images
    
    # Generate random incorrect species
    incorrect_options <- sample(metadata$species[metadata$species != quiz_data$species], 4)
    all_options <- sample(c(quiz_data$species, incorrect_options)) # Shuffle options

    incorrect_common <- sample(metadata$common[metadata$common != quiz_data$common], 4)
    all_common_options <- sample(c(quiz_data$common, incorrect_common)) # Shuffle options
    
    # Update the selectInput choices
    updateSelectInput(session, 'species_answer', choices = all_options)
    updateSelectInput(session, 'common_answer', choices = all_common_options)
    output$feedback <- renderText("") #just resets feedback so it isn't on for next question
    output$image_displayer <- renderUI("")
    
  # Display audio player
    output$audio_player <- renderUI({
      req(quiz_data$file)

    #we can change this back later
      tags$audio(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                              quiz_data$file), type = 'audio/mp3', controls = NA) 
    })
  # Wavelength
    output$wave_displayer <- renderUI({
      req(quiz_data$wave)

    #we can change this back later
      tags$img(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                              quiz_data$wave), type = 'wave/png', height="100px", width="300px") 
  })
})
  # Check answer
  observeEvent(input$submit, {
    req(input$species_answer, quiz_data$species,input$common_answer, quiz_data$common)
    
    if (tolower(input$species_answer) == tolower(quiz_data$species) && 
        tolower(input$common_answer) == tolower(quiz_data$common)) {
      output$feedback <- renderText(paste("Correct!", quiz_data$species, ';', quiz_data$common))
    } 
    else if(tolower(input$species_answer) == tolower(quiz_data$species) || 
            tolower(input$common_answer) == tolower(quiz_data$common) ){
      output$feedback <- renderText(paste("Almost! Correct answer: ", quiz_data$species, ';', quiz_data$common))
    }
    else {
      output$feedback <- renderText(paste("Wrong! Correct answer: ", quiz_data$species, ';', quiz_data$common))
    
    }
  })
  #HINT BUTTON :)
  observeEvent(input$hint, {
    req(quiz_data$hint)
    output$image_displayer <- renderUI({
    tags$img(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                          quiz_data$hint), type = 'img/jpg', height="200px", width="300px") 
  })
  })
}

# Run the App
shinyApp(ui = ui, server = server)
