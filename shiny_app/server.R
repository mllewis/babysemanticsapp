
library(shiny)
library(tidyverse)
library(here)
library(ggrepel)
library(viridis)
library(langcog)
library(plotly)


WORD_DIST_PATH <- "data/name_trait_distances_2000.csv"
word_dists <- read_csv(WORD_DIST_PATH)

COORDS_PATH <- "data/coords_2000.csv"
coords <- read_csv(COORDS_PATH)

word_options <- unique(word_dists$name)

alerted <- FALSE

shinyServer(function(input, output, session) {
  
  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  input_names <- reactive({
   # if (is.null(input$words)) "Emma" else input$words
    n1 <- str_trim(str_to_title(input$target_name1))
    n2 <- str_trim(str_to_title(input$target_name2))
    n3 <- str_trim(str_to_title(input$target_name3))
    n4 <- str_trim(str_to_title(input$target_name4))
    n5 <- str_trim(str_to_title(input$target_name5))
    
    provided_names <- c(n1, n2, n3, n4, n5)
    target_names <- intersect(word_options, provided_names)
    target_names
  })
  
  all_name_data <- reactive({
    if (input_names() %in% word_options) {
      word_dists %>%
        filter(name %in% input_names()) 
    } else {
      data.frame()
    }
  })

 ## semantic name plot
 plot_data <- reactive({
   coords %>%
     mutate(type2 = case_when(word %in% input_names() ~ "target",
                             TRUE ~ "other"))
 })

 name_plot <- function() {
     plot_dists <- plot_data()
    p <- ggplot(plot_dists %>% filter(type2 != "target"), aes(x = V1, y = V2, color = type, text = word)) +
      geom_point(size = 3, alpha = .3) +
      #geom_text_repel(data = plot_dists %>% filter(type2 == "target"), aes(label = word), size = 7, color = "black") +
      #geom_point(data = plot_dists %>% filter(type2 == "target"),  size = 6, color = "black") +
      geom_text(data = plot_dists %>% filter(type2 == "target"), aes(label = word, y = V2 + .4),  size = 6, color = "black") +
      geom_point(data = plot_dists %>% filter(type2 == "target"),  size = 3, color = "black") +
      theme_void() +
      theme(legend.position = "bottom")
     fig <- ggplotly(p, tooltip = "text")  %>%
       layout(legend = list(orientation = 'h')) %>%
       layout(hoverlabel = list(font=list(size=22))
       )
     fig
  }
  
  output$name_plot <- renderPlotly(name_plot())
  
  output$download_plot <- downloadHandler(
    filename = function() "name_semantics_plot.pdf",
    content = function(file) {
      cairo_pdf(file, width = 10, height = 7)
      print(name_plot())
      dev.off()
  })
  
  ## trait table
  table_data <- reactive({
    tab_data <- all_name_data()
    tab_data %>%
      group_by(name) %>%
      arrange(-cos_dist) %>%
      slice(1:25) %>%
      mutate(rank = 1:n()) %>%
      select(name, rank, trait)  %>%
      pivot_wider(names_from = name, values_from = trait) %>%
      select(rank, input_names())
  })
  
  output$tab <- renderTable(table_data())

  output$download_table <- downloadHandler(
    filename = function() "name_trait_table.csv",
    content = function(file) {
      td <- table_data()
      write_csv(td, file)
    })
  
  ## valence plot
  valence_data <- reactive({
    val_data <- all_name_data()
    val_data %>%
      group_by(name) %>%
      arrange(-cos_dist) %>%
      slice(1:trait_value()) %>% 
      multi_boot_standard(col = "valence", na.rm = T)
  })
  
  trait_value <- reactive({
      input$n_traits
  })
  
  name_valence_plot <- function() {
    valence_means <- valence_data()
      ggplot(valence_means, aes(x = name, y = mean, fill = name)) +
        scale_y_continuous(breaks = c(0,3,6,9), 
                           label = c("0\n(negative)",  "3", "6", "9\n(positive)"),
                           limits = c(0,9) ) +
      ylab("Valence of top N traits") +
      geom_bar(stat = "identity") +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper )) +
      ggtitle("Mean valence of top N traits associated with name") +
      theme_classic(base_size = 20) + 
      theme(legend.position = "none")
  }
  
  output$name_valence_plot <- renderPlot(name_valence_plot(),
                                         width = "auto",
                                         height = "auto")
  
  ## Frequency data
  
})