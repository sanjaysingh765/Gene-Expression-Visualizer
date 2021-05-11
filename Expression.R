library(shiny)
library(shinydashboard)
library(reshape2)
library(ggplot2)

#expression data
tissue <- read.delim("./data/cro_tissue.txt", stringsAsFactors = FALSE)
PA <- read.delim("./data/PA_lab", stringsAsFactors = FALSE)

# Dashboard and body formatting
header <- dashboardHeader(title = "Gene expression visualizer", titleWidth = 450)
sidebar <- dashboardSidebar(
  selectInput("dataset", "Dataset",
              c("Tissue" = "tissue",
                "pa" = "pa")),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton", 
                    label = "Search dataset", icon = shiny::icon("search")),
  br(),
  tags$footer(
    tags$p("Example : CRO_T100001")),
  img(src="cro3.jpg",height = '700px', width = '200px'))


body <- dashboardBody(
    includeCSS("www/custom.css"),  
    box(title = "Paste the gene accession number in left panel to visualize gene expression",
    status = "primary",
    solidHeader = T,
    collapsible = T,
    width = 12,
    #height = 2,
    fluidRow(column(width = 10, textOutput( "instructions" )),
             column(width = 2, align = "center"
                    ))),
br(),
br(),
  tableOutput("filtered_table"),
  plotOutput("plot", click = "plot_click", width = "50%", height = "300px"),
  downloadButton('downloadPlot', 'Download Plot')
)

ui <- dashboardPage(title = 'Search', header, sidebar, body)





server <- function(input, output, session) {

    # This returns the correct dataset
  example_data  <- reactive({
    if (input$dataset == "tissue"){
      edata  <- tissue
    }
    else if (input$dataset == "pa"){
      edata <- PA
    }
    return(edata)
  })
  
   output$filtered_table <- renderTable({
    req(input$searchButton == TRUE)
    example_data()[example_data()$Gene  %in% input$searchText,]
    
       }) 
  

 plotInput <- function(){
    req(input$searchButton == TRUE)
    data1 <- example_data()[example_data()$Gene  %in% input$searchText,]
    mdata1 <- melt(data1, id.vars = "Gene")
     gg <- ggplot(data=mdata1, aes(x=variable, y=value)) +
      geom_bar(stat="identity",width=0.5, color="black", fill = "#E69F00")+
      ggtitle(data1$Gene) +
      xlab("Tissue") + ylab("Expression (FPKM)") + 
      #theme_ms()+
      theme(plot.title = element_text(color="black", size=15, face="bold.italic",hjust = 0.5,margin=margin(b = 3, unit = "pt")))+
      theme(plot.margin=unit(c(1,1,0.0,0.0),"mm"))+
      theme(axis.text=element_text(size=15, color = "black",family="Arial", face="bold"))+
      theme(axis.title=element_text(size=15,face="bold", color = "black"))+
      theme(axis.text.x = element_text(angle = 360, vjust = 1, hjust=0.5)) +
      theme(
        legend.position = "none",  #none
        legend.spacing.x = unit(0.02, 'cm'), #space between legennds unit
        legend.text = element_text(margin = margin(r = 10, unit = "pt")))+
      scale_y_continuous(expand=c(0,0))+
      theme(axis.line = element_line(size = 1, color = "black"),axis.ticks = element_line(colour = "black", size = 0.2))+
      theme(axis.ticks.length = unit(0.04, "cm"))+
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#AD7366","Lightgreen"))
     gg
     }
 
  
  output$plot <- renderPlot({
    plotInput()
  })

 
  output$downloadPlot <- downloadHandler(
    filename = "expression.png",
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    })     
  
  }

shinyApp(ui = ui, server = server)
