library(shiny)
library(RColorBrewer)
library(data.table)
library(tidyverse)
library(rsconnect)




data <- read_tsv("degs_vst_transformed.tsv")
metadata <- read_tsv("metadata.tsv")

# Rename the column names of 'data' to match 'SampleID' in 'metadata'
names(data)[-1] <- gsub("`", "", names(data)[-1])

# Convert the wide 'data' tibble to long format
data_long <- data %>% 
  pivot_longer(cols = -ID, names_to = "SampleID", values_to = "value")

# Join 'metadata' with 'data_long'
data_joined <- data_long %>% 
  left_join(metadata, by = "SampleID")

data_joined$Condition <- factor(data_joined$Condition)



ui = fluidPage(
  titlePanel("Plot gene expression"),
  textInput("text_input", label = "Gene", value="", placeholder = "Type gene name here"),
  actionButton("actButton", "Plot gene"),
  plotOutput("plot_output", width = 800, height = 800)
)



server = function(input, output) {
  plot_data <- eventReactive(input$actButton, {
    gene_id <- input$text_input
    sel <- data_joined %>%
      filter(ID == gene_id)
    
    # Check if the gene name exists in the data
    if (nrow(sel) == 0) {
      stop("Error: The gene name does not exist in the data.")
    }
    
    return(sel)
  })
  
  output$plot_output <- renderPlot({
    tryCatch({
      sel <- plot_data()
      p <- ggplot(sel,
                aes(x=Condition,y=value,col=Condition,group=Condition)) +
        geom_point(size=4) + geom_smooth() +
        scale_y_continuous(name="VST expression") + 
        ggtitle(label=paste("Expression for: ",input$text_input)) +
        theme(axis.text=element_text(size=18), 
            axis.title=element_text(size=18,face="bold"),
            legend.text=element_text(size=18),
            legend.title=element_text(size=18,face="bold"),
      axis.text.x=element_text(angle=45, hjust=1))
    
    suppressMessages(suppressWarnings(plot(p)))
  }, error= function(e) {
    # Display the error message
    plot.new()
    title(main = e$message, col.main = "red", font.main = 4)
  })
  })
}



shinyApp(ui=ui, server=server)

