#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(readxl)
reactiveConsole(TRUE)

####
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("RNA-seq dataset rug plot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
         fileInput("dataset", "Your dataset", placeholder = "Your dataset"),
         textInput("gene", "Gene of interest", "GAPDH"),
         tableOutput("enrichment"),
         # any color input options
         textInput("col", "Highlight color", "red"),
         
       ),

        # Show the generated rug plot
        mainPanel(
          plotOutput("rugPlot"),
          p("Right click on plot to download."),
          # downloadButton("foo"),

        )
    )
)
####

# Define server logic required to draw a rug plot
server <- function(input, output) {
  
  ### placeholder static input file
  # dat <- read_excel("C:/Users/NRB37/Dropbox (Partners HealthCare)/Nina/projects/testcode/rug_plots/data_D6minusinput.xlsx")

  # make a reactive expression that yields the full dataset
  # it's an expression so will need to make into a value for
  # some functions later on esp dat_gene 
  dat <- reactive({
    
    # let shiny know it has to wait for the dataset to be uploaded
    req(input$dataset)
   # read the file - right now only excel files accepted
    # make a switch statement to accept csv, tsv etc
    read_excel(input$dataset$datapath)
  
  })
  
  # if you want to see the first four genes in the table 
  # output$head <- renderTable({
  #   head(dat(), 4)
  # })
  
  # subset the data with just the gene of interest in it
  # dat_gene <- dat %>% dplyr::filter(dat$Gene == input$gene)
  dat_gene <- reactive({
    req(input$dataset)
    
    # take output of dat () and turn into a value
    # probably would be easier to make it a reactive value but 
    # will worry about that later
    my_dat <- dat()
    gene_of_interest <- my_dat[my_dat$Gene %in% input$gene, ]
    gene_of_interest
  })
  
  # initiate a ggplot object
  plot_object <- renderPlot({
    ggplot() +
      
      # add layer to show density distribution of all values
      stat_density(dat(),
                   mapping=aes(x=Value, y=1,
                               fill=after_stat(density)),
                   geom="tile",
                   position="identity") +
      scale_fill_gradient(low="white", high="black") +
      
      # reactively
      # add layer to show individual occurrences of "gene of interest" guides
      stat_count(dat_gene(),
                 mapping=aes(x=Value),
                 geom="tile",
                 position="identity",
                 width=0.01,
                 color=input$col) +
      labs(x="sgRNA Enrichment Score", y=NULL) +
      guides(y="none") +
      theme_classic(base_size=16) + 
      theme(legend.position = "none", 
            # axis.line.x.bottom=element_line(linewidth=0.5),
            axis.ticks = element_blank())
    
  })
  
  # put it in output
  output$rugPlot <- plot_object

   
  # display the enrichment scores for gene of interest 
  output$enrichment <- renderTable({
     dat_gene()
   })

#   # download the plot
#   output$foo <- downloadHandler(
#     filename = "test_plot_download.png",
#     content = function() {
#       req(input$dataset)
#       png()
#         print(plot_object())
#         dev.off()
#   })
}


# Run the application 
shinyApp(ui = ui, server = server)
