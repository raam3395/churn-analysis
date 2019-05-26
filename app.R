#setwd("C:/Users/TM37421/Documents/PDS")
#setwd("C:/Users/User/Documents/HCP Anywhere/Master of Data Science/Sem 1/Principle of Data Science/PDS")

data <- read.csv("newchurndataset.csv")

library(rsconnect)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggcorrplot)
library(Hmisc)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(IRdisplay) 
library(tm)
library(wordcloud)
library(RColorBrewer)
library(shinythemes)
library(googleVis)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Churn Analysis"),
  
  tabsetPanel(
    tabPanel("Main",
      sidebarLayout(
        sidebarPanel(
          selectInput("analysis", "Analysis by:",
                      c("Overall Churn" = "oc",
                        "State Churn" = "sc",
                        "Voice Plan" = "vp",
                        "International Plan" = "ip")),
          helpText("Data from https://www.kaggle.com/becksddf/churn-in-telecoms-dataset")
        ),
        mainPanel(
          plotlyOutput("churn"),
          tags$h3("Map of Churn"),
          htmlOutput("gvis")
        )
      )
    ),
    
    tabPanel("In Depth",
      sidebarLayout(
        sidebarPanel(
          selectInput("depth", "In Depth Analysis:",
                      c("Average Charges" = "ac",
                        "Average Minutes" = "am",
                        "Average Calls" = "al",
                        "Customer Services Calls" = "cs")),
          helpText("Data from https://www.kaggle.com/becksddf/churn-in-telecoms-dataset")
        ),
        mainPanel(
          plotlyOutput("depthplot")
          
        )
      )
             
    )
  )
  
)

server <- function(input, output){
  #total churn
  p <- data %>% group_by(churn) %>% summarise(count = n())
  
  #number of churn per state
  state_churn <- data %>% filter(churn == 'TRUE') %>% group_by(state) %>% summarise(yes_churn = n())
  state_no_churn <- data %>% filter(churn == 'FALSE') %>% group_by(state) %>% summarise(no_churn = n())
  combine_state = merge(state_churn, state_no_churn)
  
  #add voice mail plan into data frame
  voice_yes <- data %>% filter(churn == 'TRUE') %>% group_by(voice.mail.plan) %>% summarise(yescount = n())
  voice_no <- data %>% filter(churn == 'FALSE') %>% group_by(voice.mail.plan) %>% summarise(nocount = n())
  combine_voice <- merge(voice_yes, voice_no)
  
  #add international plan into data frame
  international_yes <- data %>% filter(churn == 'TRUE') %>% group_by(international.plan) %>% summarise(yescount = n())
  international_no <- data %>% filter(churn == 'FALSE') %>% group_by(international.plan) %>% summarise(nocount = n())
  combine_international <- merge(international_yes, international_no)
  
  
  output$gvis <- renderGvis({
    gvisGeoChart(state_churn, "state", "yes_churn", 
                 options = list(region="US", displayMode="regions", resolution="provinces", width=800, height=600,colorAxis="{colors:['#FFFFFF', '#8B0000']}")
    )})
  
  
  output$churn <- renderPlotly({
    if (input$analysis == "oc") {
      plot_ly(p, labels = ~churn, values = ~count, type = 'pie' ,marker = list(colors = c('#006400','#800000'),line = list(color = "white", width =  1.3)),textfont = list(color = '#ffffff', size = 16)) %>% 
        layout(title = "Overall Churn Ratio",  showlegend = T)
    } else 
      if (input$analysis == "sc") {
        plot_ly(combine_state, x = ~state, y = ~no_churn, type = 'bar', name = 'No') %>% 
          add_trace(y = ~yes_churn, name = 'Yes') %>% 
          layout(yaxis = list(title = 'Customers'), xaxis = list(title = 'State'), barmode = 'stack',title = "Distribution of customers according to State")
      }
     else 
      if (input$analysis == "vp") {
#        plot_ly(combine_voice, x = ~voice.mail.plan, y = ~nocount, type = 'bar', name = 'Non Churn') %>% 
#          add_trace(y = ~yescount, name = 'Yes Churn') %>% 
#          layout(yaxis = list(title = 'Customers'), xaxis = list(title = 'Voice Mail Plan'), barmode = 'group',title = "Distribution of customers according to Voicemail plans")
        
        plot_ly(voice_yes, labels = ~voice.mail.plan, values = ~yescount,type = "pie",name = 'CHURNED CUSTOMERS ', 
                domain = list(x = c(0, 0.5), y = c(0, 1)),marker = list(colors = c('#006666','#800000'),line = list(color = "white",
                                                                                                                    width =  1.3)),textfont = list(color = '#ffffff', size = 16)) %>%
          add_trace(data = voice_no, labels = ~voice.mail.plan , values = ~nocount , 
                    type = "pie",name = 'Non CHURNED CUSTOMERS', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
          
          layout(title = "Distribution of Customers with Voicemail plans",  showlegend = T,
                 annotations = list(
                   list(x = 0.15 , y = -.1, text = "CHURNED CUSTOMERS", showarrow = F, xref='paper', yref='paper'),
                   list(x = 0.9 , y = -.1, text = "NON CHURNED CUSTOMERS", showarrow = F, xref='paper', yref='paper')))
      }
     else
       if (input$analysis == "ip"){
#         plot_ly(combine_international, x = ~international.plan, y = ~nocount, type = 'bar', name = 'Non Churn') %>% 
#           add_trace(y = ~yescount, name = 'Yes Churn') %>% 
#           layout(yaxis = list(title = 'Customers'), xaxis = list(title = 'International Plan'), barmode = 'group',title = "Distribution of customers according to international plans")
      
         plot_ly(international_yes, labels = ~international.plan, values = ~ yescount,type = "pie",name = 'CHURNED CUSTOMERS ', 
                 domain = list(x = c(0, 0.5), y = c(0, 1)),marker = list(colors = c('#006666','#800000'),line = list(color = "white",
                                                                                                                     width =  1.3)),textfont = list(color = '#ffffff', size = 16)) %>%
           add_trace(data = international_no, labels = ~international.plan, values = ~nocount , 
                     type = "pie",name = 'Non CHURNED CUSTOMERS', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
           
           layout(title = "Distribution of Customers with International Plans",  showlegend = T,
                  annotations = list(
                    list(x = 0.15 , y = -.1, text = "CHURNED CUSTOMERS", showarrow = F, xref='paper', yref='paper'),
                    list(x = 0.9 , y = -.1, text = "NON CHURNED CUSTOMERS", showarrow = F, xref='paper', yref='paper'))) 
         
      }
      
  })
  
  
  #charges
  charges <- data %>% group_by(churn)%>%
    summarise(Day_Charge_Average=mean(total.day.charge),
              Eve_Charge_Average=mean(total.eve.charge),
              Night_Charge_Average=mean(total.night.charge),
              Intl_Charge_Average=mean(total.intl.charge))
  charges <- gather(charges, key = Charge, value = Average,Day_Charge_Average,
                    Eve_Charge_Average,Night_Charge_Average,Intl_Charge_Average,-churn)
  
  #Minutes
  minutes <- data %>% group_by(churn) %>% 
    summarise(Day_Minute_Average=mean(total.day.minutes),
              Eve_Minute_Average=mean(total.eve.minutes),
              Night_Minute_Average=mean(total.night.minutes),
              Intl_Minute_Average=mean(total.intl.minutes))
  minutes <- gather(minutes, key = Minute, value = Average,Day_Minute_Average,
                    Eve_Minute_Average,Night_Minute_Average,Intl_Minute_Average,-churn)
  
  #calls
  calls <- data %>% group_by(churn)%>%
    summarise(Day_Calls_Average=mean(total.day.calls),
              Eve_Calls_Average=mean(total.eve.calls),
              Night_Calls_Average=mean(total.night.calls),
              Intl_Calls_Average=mean(total.intl.calls))
  calls <- gather(calls, key = Calls, value = Average,Day_Calls_Average,
                  Eve_Calls_Average,Night_Calls_Average,Intl_Calls_Average,-churn)
  
  #Customer services calls
  cs_call <- data %>% group_by(churn) %>% summarise(Customer_Service_call=sum(customer.service.calls))
  cs_yes <- data %>% filter(churn == 'TRUE') %>% group_by(customer.service.calls) %>% summarise(yes_churn = n())
  cs_no <- data %>% filter(churn == 'FALSE') %>% group_by(customer.service.calls) %>% summarise(no_churn = n())
  combine_cs <- merge(cs_yes, cs_no)
  
  output$depthplot <- renderPlotly({
    if (input$depth == "ac") {
      ggplot(charges,aes(fill=churn, y=Average, x=Charge))  + 
        xlab("Different Charges in the day ") + 
        ylab("Average Charges")+
        geom_bar(position="dodge", stat="identity")+labs(title= "Average total charges of customers")+
        theme_minimal() +scale_fill_manual(values=c( "#006400","#800000")) +
        theme(axis.line = element_line(color="grey", size = 1))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    } else 
      if (input$depth == "am") {
        ggplot(minutes,aes(fill=churn, y=Average, x=Minute))  + 
          xlab("Different Minutes in the day ") + 
          ylab("Average Minutes")+
          geom_bar(position="dodge", stat="identity")+labs(title="Average total minutes of customers ")+
          theme_minimal() +scale_fill_manual(values=c( "#006400","#800000"))+
          theme(axis.line = element_line(color="grey", size = 1))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    else 
      if (input$depth == "al") {
        ggplot(calls,aes(fill=churn, y=Average, x= Calls))  + 
          xlab("Different Calls in the day ") + 
          ylab("Average Calls")+
          geom_bar(position="dodge", stat="identity")+labs(title="Average total calls of customers ")+
          theme_minimal() +scale_fill_manual(values=c( "#006400","#800000"))+
          theme(axis.line = element_line(color="grey", size = 1))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    else
      if (input$depth == "cs") {
#        plot_ly(cs_call, labels = ~churn, values = ~Customer_Service_call, type = 'pie' ,marker = list(colors = c('#006400','#800000'),line = list(color = "white", width =  1.3)),textfont = list(color = '#ffffff', size = 16)) %>% 
#          layout(title = "Customer Services Calls Ratio",  showlegend = T)
        
        plot_ly(combine_cs, x = ~customer.service.calls, y = ~no_churn, type = 'bar', name = 'No') %>% 
          add_trace(y = ~yes_churn, name = 'Yes') %>% 
          layout(yaxis = list(title = 'Customer'), xaxis = list(title = 'Number of customer service calls'), barmode = 'group',title = "Distribution of customers churn according to Number of Customer Service Calls")
        
      }
  })
}

shinyApp(ui = ui, server = server)
