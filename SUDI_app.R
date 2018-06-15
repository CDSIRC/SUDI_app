library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
library(scales)

risks_bound <- readRDS(file = "risks_bound.rds")
custom_colours <- c("#CAF2FF", "#606060", "#FAA701")

ui <- fluidPage(
  
  selectInput("selected_risk", "Deaths involving which risk:", 
              choices = levels(risks_bound$selected),
              selectize = FALSE),
  
  # Create a spot for the barplot
  plotOutput("preparedPlot"), 
  
  p(strong("CHILD DEATH AND SERIOUS INJURY REVIEW COMMITTEE OF SOUTH AUSTRALIA"), align = "center"),
  
  # Links
  downloadLink("downloadData", "Get the data"), 
  helpText(a("Get the code", href = "https://github.com/CDSIRC/", target = "_blank"))
  
)

server <- function(input, output){
  
  #create the df to plot
  risk_plot <- reactive(risks_bound %>%
                          filter(selected == input$selected_risk)
  )
  
  #create the plot object
  output$preparedPlot <- renderPlot(
    ggplot(data = risk_plot(), aes(x = risk, y = proportion, fill = highlight)) + 
      geom_bar(stat = "identity") + 
      guides(fill = FALSE) + 
      scale_y_continuous(labels = percent, limits = c(0, 1)) + 
      scale_fill_manual(values = c(custom_colours[3], custom_colours[2])) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            title = element_text(size = 16)) + 
      labs(caption = str_wrap("'Not placed on back', 'Not in an approved bed' and 'Bed sharing' refer only to the fatal sleep period. 'Parental smoking' refers to any time during pregnancy or the child's life. 'Not breast fed' refers to any time during the child's life'", 
                              width = 130), 
           x = "Risk", y = "Percent of deaths involving the selected risk", 
           title = "Sudden and unexpected deaths of infants who died after being placed to sleep\nin South Australia between 2005 and 2016")
  )
  
  #create the CSV
  output$downloadData <- downloadHandler(
    filename = "SUDI_data.csv",
    content = function(file) {
      write.csv(select(risks_bound, -highlight), file, row.names = FALSE)
    }, 
    contentType = "text/csv"
  )
  
}

shinyApp(ui, server)