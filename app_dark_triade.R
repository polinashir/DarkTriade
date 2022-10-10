#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)
library(fmsb)
library(vioplot)
library(rpart)
library(rpart.plot)
library(dplyr)

data <- read.csv(file = 'C:\\Users\\polin\\Documents\\IMT\\Cours actuels\\Visualisation\\DOE datasets\\dark_triad\\dark_triade.csv',sep = ";", header=TRUE, encoding = "UTF-8")
data<-na.omit(data)
data$departement <- paste(data$Dans.quel.département.êtes.vous.., data$Quel.département.souhaitez.vous.rejoindre..)
data$departement <- trimws(data$departement, which = c("both"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dark Triade"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("select", label = h3("Le département"), 
                      choices = list("ISERM" = 'ISERM', "2ER" = '2ER', "PRISM" = 'PRISM', "2IA"='2IA', "GC"='GC', 'TOUT'=1), 
                      selected = 1),
          
          hr(),
          fluidRow(column(3, verbatimTextOutput("value"))),
          
          radioButtons("radio", label = h3("Année d'études"),
                       choices = list("Tout"=1, "1A" = '1A', "2A" = '2A', "3A" = '3A'), 
                       selected = 1),
         
        ),

        # Show a plot of the generated distribution
        mainPanel(
          titlePanel('    Radarchart'),
           plotOutput("distPlot"),
          titlePanel('    Boxplot'),
           plotOutput('distBoxplot'),
          titlePanel('    Decision Tree'),
           plotOutput('Classification')
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      max_min <- data.frame(
        Machiavélisme  = c(10, 0), Narcissisme = c(10, 0), Psychopathie = c(10, 0)
      )
      if(input$select!=1 & input$radio==1){
        scores_dep <- data.frame(
          Machiavélisme =mean(data$mac[data$departement==input$select]),
          Narcissisme=mean(data$narc[data$departement==input$select]), 
          Psychopathie=mean(data$psy[data$departement==input$select])
        )
      }
      else if(input$select==1 & input$radio==1){
        scores_dep <- data.frame(
          Machiavélisme =mean(data$mac),
          Narcissisme=mean(data$narc), 
          Psychopathie=mean(data$psy)
        )
      }
      else if(input$select==1 & input$radio!=1){
        scores_dep <- data.frame(
          Machiavélisme =mean(data$mac[data$En.quelle.année.êtes.vous..==input$radio]),
          Narcissisme=mean(data$narc[data$En.quelle.année.êtes.vous..==input$radio]), 
          Psychopathie=mean(data$psy[data$En.quelle.année.êtes.vous..==input$radio])
        )
      }
      else if(input$select!=1 & input$radio!=1){
        scores_dep <- data.frame(
          Machiavélisme =mean(data$mac[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select]),
          Narcissisme=mean(data$narc[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select]), 
          Psychopathie=mean(data$psy[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select])
        )
      }
      rownames(max_min) <- c("Max", "Min")
      dep <- rbind(max_min, scores_dep)
      score_dark<-dep[c("Max", "Min", "1"), ]
      radarchart(score_dark,pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5))
    })
    output$distBoxplot <- renderPlot({
      if(input$select!=1 & input$radio==1){
        vioplot(data$mac[data$departement==input$select], data$narc[data$departement==input$select], data$psy[data$departement==input$select], names=c("Machiavélisme", "Narcissisme", "Psychopathie"), col="gold")
      }
      else if(input$select==1 & input$radio==1){
        vioplot(data$mac, data$narc, data$psy, names=c("Machiavélisme", "Narcissisme", "Psychopathie"), col="gold")
      }
      else if(input$select==1 & input$radio!=1){
        vioplot(data$mac[data$En.quelle.année.êtes.vous..==input$radio], data$narc[data$En.quelle.année.êtes.vous..==input$radio], data$psy[data$En.quelle.année.êtes.vous..==input$radio], names=c("Machiavélisme", "Narcissisme", "Psychopathie"), col="gold")
      }
      else if(input$select!=1 & input$radio!=1){
        vioplot(data$mac[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select], data$narc[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select], data$psy[data$En.quelle.année.êtes.vous..==input$radio & data$departement==input$select], names=c("Machiavélisme", "Narcissisme", "Psychopathie"), col="gold")
      }
    })
    output$Classification <- renderPlot({
      data_tree <- data %>% select(departement, mac, narc, psy)
      split1<- sample(c(rep(0, 0.7 * nrow(data_tree)), rep(1, 0.3 * nrow(data_tree))))
      train <- data_tree[split1 == 0, ]  
      test <- data_tree[split1== 1, ] 
      tree <- rpart(departement ~., data = train, method = "class", control = ("maxdepth = 10"))
      rpart.plot(tree)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
