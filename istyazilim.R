library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
library(readxl)
library(formattable)

# Data preparation

path = "/Users/gnlli/OneDrive/Masaüstü/sigorta/Sigorta.xlsx"

data <- read_excel(path=path, sheet = "Sheet1",col_types = c("text",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text")
)


firm_names <- data %>% distinct(NAME)

# Shiny
ui <- navbarPage("Sigorta Þirketlerinin Performans Deðerlendirmesi",
                 #  theme=shinythemes::shinytheme('darkly'),
                 tabPanel("Firma Durumu",
                          selectInput('firma', 'Lütfen Firma Seçiniz', choices = firm_names),
                          
                          plotOutput("hist_plot"),
                          
                          tableOutput('table_firma'),
                          
                      
                          
                          
                          plotOutput("pie_chart")
                 ),
                 
                 tabPanel("Top 10",
                          titlePanel("Poliçe Üretim Miktarýna Göre Top10 Sýrada Yer Alan Firmalar"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('tarihTop10', 'Tarih', c("31.12.2019","31.12.2020"))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel('Hayat Grubu',  DT::DTOutput("table_top10_hayat"),  plotOutput("pie_top10_hayat")),
                                tabPanel('Hayatdýþý Grubu', DT::DTOutput("table_top10_hayatDisi"), plotOutput("pie_top10_hayatdisi")),
                                tabPanel('Toplam', DT::DTOutput("table_top10_Toplam"), plotOutput("pie_top10_toplam"))
                              )
                              
                            )
                          )
                          
                          
                 ),
                 windowTitle = "ISTYAZ FINAL",
                 id = "navBarPage",
                 
                 inverse = TRUE
                 
                 
                 
                 
)

server <- function(input, output, session) {
  
  
  
  output$table_firma <- renderTable( {
    
    table_data <- data %>% filter(NAME == input$firma) %>% select(NAME, Police_Uretim_TL, Pazar_Payi,Sigorta_Grubu)
    table_data$Police_Uretim_TL <- formatC(as.numeric(table_data$Police_Uretim_TL),format = "f", digits=2, big.mark = ",", )
    table_data$Pazar_Payi <- table_data$Pazar_Payi * 100
    colnames(table_data) <- c("Firma Ýsmi", "Toplam Poliçe Üretimi (TL)", "Firmanýn Pazar Payý (%)", "Poliçe Grubu")
    table_data
    
    
  }, hover = TRUE,
  rownames = FALSE,
  digits = 2L,
  bordered = TRUE,striped = TRUE)
  
  
  output$hist_plot <- renderPlot({
    
    firma_verisi <- data %>% filter(NAME == input$firma)
    
    if("Hayat" %in% firma_verisi$Sigorta_Grubu) {
      
      ggplot(firma_verisi, aes(y=Police_Uretim_TL, x= DATE, fill=Sigorta_Grubu)) +
        geom_bar(stat = "identity",position="dodge") + 
        ylim(0,max(data$Police_Uretim_TL)) +
        labs(title = "Tarihler Bazýnda Poliçe Tutarý") +
        ylab(label = "Toplam Poliçe Üretimi (TL)") +
        xlab(label = "Tarih")
      
      
    }
    
    else {
      
      ggplot(firma_verisi, aes(y=Police_Uretim_TL, x= DATE, fill=Sigorta_Grubu)) +
        geom_bar(stat = "identity") + ylim(0,max(data$Police_Uretim_TL))+
        labs(title = "Tarihler Bazýnda Poliçe Tutarý") +
        ylab(label = "Toplam Poliçe Üretimi (TL)") +
        xlab(label = "Tarih")
      
      
    }
    
    
    
    
  })
  
  
  output$pie_chart <- renderPlot({
    
    
    
    pie_df <- data %>% 
      mutate(firma_gruplama = case_when(NAME=="Türkiye Sigorta A.Þ." ~ 1, TRUE ~ 2)) %>% 
      mutate(firma_rename = case_when(firma_gruplama==2 ~ "Sektör Geneli", TRUE~ "Türkiye Sigorta A.Þ.")) %>%
      group_by(DATE,firma_rename) %>% summarize(`Toplam Üretim` = sum(Police_Uretim_TL)) %>%
      mutate(Oran = `Toplam Üretim`/sum(`Toplam Üretim`))
    
    
    ggplot(pie_df, aes(y=Oran, x="", fill=firma_rename, color=firma_rename)) +
      geom_bar(stat = "identity", width=1, color="white") + 
      coord_polar("y", start=0) + facet_wrap(~DATE) + ggtitle(label = "Firmanýn Poliçe Üretim Tutarý Bazýnda Sektör Ýçindeki Payý")
    
  })
  
  
  output$table_top10_hayat <- DT::renderDataTable({
    
    top_10_hayat <- data %>% 
      filter(DATE == input$tarihTop10) %>% 
      filter( Sigorta_Grubu == "Hayat") %>%
      select(DATE,NAME, Police_Uretim_TL, Pazar_Payi) %>%
      top_n(Police_Uretim_TL,n=10)
    
    
    
    top_10_hayat$Police_Uretim_TL <- formatC(as.numeric(top_10_hayat$Police_Uretim_TL),format = "f", digits=2, big.mark = ",", )
    top_10_hayat$Pazar_Payi <- top_10_hayat$Pazar_Payi * 100
    colnames(top_10_hayat) <- c("Dönem", "Firma Adý", "Toplam Poliçe Üretimi (TL)", "Pazar Payý (%)")
    top_10_hayat
    
    
  })
  
  output$table_top10_hayatDisi <- DT::renderDataTable({
    
    top_10_hayatdisi <- data %>% 
      filter(DATE == input$tarihTop10) %>% 
      filter( Sigorta_Grubu == "Hayatdisi") %>%
      select(DATE,NAME, Police_Uretim_TL, Pazar_Payi) %>%
      top_n(Police_Uretim_TL,n=10)
    
    
    
    top_10_hayatdisi$Police_Uretim_TL <- formatC(as.numeric(top_10_hayatdisi$Police_Uretim_TL),format = "f", digits=2, big.mark = ",", )
    top_10_hayatdisi$Pazar_Payi <- top_10_hayatdisi$Pazar_Payi * 100
    colnames(top_10_hayatdisi) <- c("Dönem", "Firma Adý", "Toplam Poliçe Üretimi (TL)", "Pazar Payý (%)")
    top_10_hayatdisi
    
    
  })
  
  output$table_top10_Toplam <- DT::renderDataTable({
    
    top_10_Toplam <- data %>% select(DATE,NAME,Police_Uretim_TL) %>%
      filter(DATE == input$tarihTop10) %>% 
      group_by(DATE,NAME) %>%
      summarize(Total_Police = sum(Police_Uretim_TL)) %>%
      mutate(pazar_payi = Total_Police/sum(Total_Police)) %>%
      top_n(Total_Police,n=10)
    
    
    
    top_10_Toplam$Total_Police <- formatC(as.numeric(top_10_Toplam$Total_Police),format = "f", digits=2, big.mark = ",", )
    top_10_Toplam$pazar_payi <- top_10_Toplam$pazar_payi * 100
    colnames(top_10_Toplam) <- c("Dönem", "Firma Adý", "Toplam Poliçe Ãœretimi (TL)", "Pazar Payý (%)")
    top_10_Toplam
    
    
  })
  
  
  output$pie_top10_hayat <- renderPlot({
    
    pie_top_10_hayat <- data %>% 
      filter(DATE == input$tarihTop10) %>% 
      filter( Sigorta_Grubu == "Hayat") %>%
      select(DATE,NAME, Police_Uretim_TL, Pazar_Payi) %>%
      top_n(Police_Uretim_TL,n=10)
    
    colnames(pie_top_10_hayat) <- c("Tarih", "Firma Ýsmi", "Toplam Poliçe Üretimi (TL)", "Pazar Payý")
    
    
    ggplot(pie_top_10_hayat, aes(y=`Pazar Payý`, x="", fill=`Firma Ýsmi`, color=`Firma Ýsmi`)) +
      geom_bar(stat = "identity", width=1) + 
      coord_polar("y", start=0)+
      ggtitle(label = "Top 10 Hayat Grubu Bazýnda Pazar Payý Daðýlýmlarý")
    
  })
  
  output$pie_top10_hayatdisi <- renderPlot({
    
    pie_top_10_hayatdisi <- data %>% 
      filter(DATE == input$tarihTop10) %>% 
      filter( Sigorta_Grubu == "Hayatdisi") %>%
      select(DATE,NAME, Police_Uretim_TL, Pazar_Payi) %>%
      top_n(Police_Uretim_TL,n=10)
    
    colnames(pie_top_10_hayatdisi) <- c("Tarih", "Firma Ýsmi", "Toplam Poliçe Üretimi (TL)", "Pazar Payý")
    
    
    ggplot(pie_top_10_hayatdisi, aes(y=`Pazar Payý`, x="", fill=`Firma Ýsmi`, color=`Firma Ýsmi`)) +
      geom_bar(stat = "identity", width=1) + 
      coord_polar("y", start=0) +
      ggtitle(label = "Top 10 Hayadýþý Grubu Bazýnda Pazar Payý Daðýlýmlarý")
    
  })
  
  output$pie_top10_toplam <- renderPlot({
    
    top_10_Toplam <- data %>% select(DATE,NAME,Police_Uretim_TL) %>%
      filter(DATE == input$tarihTop10) %>% 
      group_by(DATE,NAME) %>%
      summarize(Total_Police = sum(Police_Uretim_TL)) %>%
      mutate(pazar_payi = Total_Police/sum(Total_Police)) %>%
      top_n(Total_Police,n=10)
    
    colnames(top_10_Toplam) <- c("Tarih", "Firma Ýsmi", "Toplam Poliçe Üretimi (TL)", "Pazar Payý")
    
    
    ggplot(top_10_Toplam, aes(y=`Pazar Payý`, x="", fill=`Firma Ýsmi`, color=`Firma Ýsmi`)) +
      geom_bar(stat = "identity", width=1) + 
      coord_polar("y", start=0) +
      ggtitle(label = "Top 10 Toplam Bazýnda Pazar Payý Daðýlýmlarý")
    
  })
  
  
  
}



shinyApp(ui, server)







