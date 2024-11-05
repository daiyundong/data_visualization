setwd("/Users/dongdaiyun/Desktop/visualization/") 
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(udpipe)
library(wordcloud2)
library(tm)
library(stopwords)
library(syuzhet)

# UDPipe model
model_file <- "english-ewt-ud-2.5-191206.udpipe"
if (!file.exists(model_file)) {
  udpipe_download_model(language = "english-ewt", model_dir = getwd())
}
ud_model <- udpipe_load_model(file.path(getwd(), model_file))


# UI
ui <- fluidPage(
  titlePanel("Conversation Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("speaker", "Select Speaker:", choices = c("Full", "Me", "ChatGPT"), selected = "Full"),
      checkboxGroupInput("conversation", "Select Conversation(s):", choices = c(1,2,3,4), selected = c(1,2,3,4))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("POS Bar Chart", plotlyOutput("pos_bar_chart")),
        tabPanel("Dependency Heatmap", plotlyOutput("dependency_heatmap")),
        tabPanel("Word Cloud", wordcloud2Output("word_cloud")),
        tabPanel("Word Count Pie Chart", plotlyOutput("pie_chart")),
        tabPanel("Sentiment Analysis", plotlyOutput("sentiment_line_chart"))
      )
    )
  )
)


# Server
server <- function(input, output) {
  # read and filter data
  data <- reactive({
    # read data
    df <- read.delim("conversation.txt", header = TRUE, sep="\t", stringsAsFactors = FALSE)
    df <- df %>% filter(Conversation %in% input$conversation)
    if (input$speaker != "Full") {
      df <- df %>% filter(Speaker == input$speaker)
    }
    return(df)
  })
  
  # analyse data
  annotations <- reactive({
    df <- data()
    # combine the content
    text <- paste(df$Content, collapse = " ")
    # annotation
    anno <- udpipe_annotate(ud_model, x = text)
    anno_df <- as.data.frame(anno)
    return(anno_df)
  })
  
  
  
  # POS Bar Chart
  output$pos_bar_chart <- renderPlotly({
    anno_df <- annotations()
    pos_counts <- anno_df %>% count(upos)
    p <- ggplot(pos_counts, aes(x=reorder(upos, -n), y=n, fill = upos)) +
      geom_bar(stat="identity") +
      labs(x="Part of Speech", y="Count", title="POS Counts") +
      theme_minimal()+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
    ggplotly(p)
  })
  
  
  # Heatmap
  output$dependency_heatmap <- renderPlotly({
    data <- data()
    if (nrow(data) == 0) return(NULL)
    corpus <- VCorpus(VectorSource(data$Content))
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
    m <- as.matrix(dtm)
    word_freq <- colSums(m)
    word_freq <- sort(word_freq, decreasing = TRUE)
    # get top 20 words
    top_words <- names(word_freq)[1:20]
    m_top <- m[, top_words]
    row_labels <- paste(data$Speaker, data$Conversation, sep = "-")
    p <- plot_ly(
      x = top_words,
      y = row_labels,
      z = m_top,
      type = "heatmap",
      colors = colorRamp(c("white", "red"))
    ) %>%
      layout(title = "Term Frequency Heatmap",
             xaxis = list(title = "Words"),
             yaxis = list(title = "Speaker-Conversation"))
    p
  })
  
  
  # Word Cloud
  output$word_cloud <- renderWordcloud2({
    anno_df <- annotations()
    anno_df <- anno_df %>% 
      filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
      anti_join(data.frame(word = stopwords("en")), by = c("lemma" = "word"))
    word_counts <- anno_df %>% count(lemma)
    wordcloud2(word_counts, size = 1)
  })
  
  
  # Word Count Pie Chart
  output$pie_chart <- renderPlotly({
    df <- data()
    df$Group <- paste(df$Speaker, "Conversation", df$Conversation, sep="-")
    df$WordCount <- sapply(strsplit(df$Content, "\\s+"), length)
    word_counts <- df %>% 
      group_by(Group, Speaker) %>% 
      summarise(TotalWords = sum(WordCount), .groups = 'drop')
    word_counts$Color <- ifelse(word_counts$Speaker == "ChatGPT", "skyblue", "tomato")
    p <- plot_ly(word_counts, labels = ~Group, values = ~TotalWords, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 marker = list(colors = c('skyblue', 'tomato'),
                               line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Word Counts per Speaker and Conversation',
             showlegend = TRUE)
    p
  })
  
  
  # Sentiment Analysis Line Chart
  output$sentiment_line_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    df$Sentiment <- get_sentiment(df$Content, method = "syuzhet")
    df$CumulativeSentiment <- cumsum(df$Sentiment)
    p <- ggplot(df, aes(x = seq_along(CumulativeSentiment), y = CumulativeSentiment,
                          text = paste("Speaker:", Speaker, "<br>Conversation:", Conversation))) +
      geom_line(color = "blue") +
      geom_point() +
      labs(x = "Message Index", y = "Cumulative Sentiment Score",
           title = "Sentiment Analysis Over Time") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}


shinyApp(ui = ui, server = server)

