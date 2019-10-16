library(plotly)
library(shiny)
library(tidyverse)

ui <- function(input, output, session) {
    fluidPage(
            fluidRow(
                column(radioButtons("group_select", "Select Group", choices = c(1,2,3, "ALL"), selected="ALL"), width=1),
                column(plotlyOutput("parcoords"), width=11)
                ),
            fluidRow(
                column(plotOutput("bar"), width=4),
                column(plotOutput("point"), width=4)
                )
    )
}

server <- function(input, output, session) {
    
    normalize_1 <- function(x) {
        z  <- (mean(x, na.rm=T)-min(x))/(max(x)-min(x))
        return(z)
    }
    
    normalize_2 <- function(x) {
        z <- (x-min(x))/(max(x)-min(x))
        return(z)
    }
    
    col_order <- c("Group", "rc2250", "rc2_count1", "rc1121", "rc1206", "rc1214", "rc1_count1", "rc4270", "rc4300", "rc4271", "rc4_count1", "rc15730", "rc15_count1", "rc4300", "rc5272", "rc5_count1", "rc5_count2")
    
     data <- reactive({
        data <- readxl::read_excel("Data_Cluster3_Total_Charge_Selected.xlsx")

        data <- data %>%
            group_by(Group) %>%
            mutate_at(vars(starts_with("rc")), normalize_1) %>%
            ungroup() %>%
            mutate_at(vars(starts_with("rc")), normalize_2) %>%
            select(col_order, everything(), -claim_no)
        
        data
    })

    
    dimensions <-  reactive({
        data <- data()
        dimensions <- list(list(range = c(1, 3), tickvals = c(1,2,3), label="Group", values=data$Group))
        for (i in 2:23) {
            list_i <- list(
                           range = c(min(data[[i]]), max(data[[i]])),
                           label = names(data)[i], values = data[[i]])
            dimensions[i] <- list(list_i)
        }
        dimensions
    })
    
    colorscale <- reactive({
        colors <- c('blue', 'green', 'red')
        if (input$group_select == "ALL") {
            colorscale <- list(c(0,'red'),c(0.5,'green'),c(1,'blue'))
        } else {
            colorscale <- list(c(0, 'white'), c(0.5, 'white'), c(1, 'white'))
            colorscale[[as.numeric(input$group_select)]][[2]] <- colors[as.numeric(input$group_select)]
        }
        colorscale
    })
    
    output$parcoords <- renderPlotly({
        
        p <- plot_ly(data = data(),
                     type = 'parcoords',
                     dimensions = dimensions(),
                     line = list(color = ~Group,
                                 colorscale = colorscale())
        )
        p
    })
    
    output$bar <- renderPlot({
        data() %>%
            pivot_longer(-Group, names_to="var", values_to="value") %>%
            ggplot()+
                geom_bar(mapping=aes(value, fill=Group))+
                facet_wrap(~var)+
                theme_minimal()
    })
    
    output$point <- renderPlot({
        data() %>%
            pivot_longer(-Group, names_to="var", values_to="value") %>%
            ggplot()+
            geom_point(aes(var, value, color=as.factor(Group)), size=3.5)+
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 90))
    })
    
   
}


# Run the application 
shinyApp(ui = ui, server = server)