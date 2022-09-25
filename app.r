## app.R ##

#### Pacotes ####
library(openxlsx)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)
library(ggiraph)
library(shinyBS)
library(DT)
library(httr)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "StatBot",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Michelangelo",
                                                   message = "mranjos91@gmail.com",
                                                   icon = icon("envelope", lib = "glyphicon"),
                                                   time = Sys.time()
                                                 ))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Over O.5 HT", tabName = "over05ht", icon = icon("dashboard"))
                        # ,menuItem("Back Favorito HT", tabName = "bfavht", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tags$style(HTML("
                                .box.box-solid.box-warning>.box-header {
                                  color:#fff;
                                  background:#00A65A
                                                    }
                                
                                .box.box-solid.box-warning{
                                border-bottom-color:#00A65A;
                                border-left-color:#00A65A;
                                border-right-color:#00A65A;
                                border-top-color:#00A65A;
                                }"
                      )
                      ),
                      tabItems(
                        # First tab content
                        tabItem(tabName = "over05ht",
                                fluidRow(
                                  box(title = "Valor da Banca",
                                      width = 6,
                                      status = "warning", 
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      # Banca
                                      sliderInput("slider_banca", label = "",
                                                  min = 100, max = 10000, value = 100, step = 100)
                                  ),
                                  box(title = "Stake (% sobre a banca)", 
                                      status = "warning", 
                                      width = 6,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      # Stake
                                      sliderInput("slider_stake", label = "",
                                                  min = 2, max = 100, value = 2, step = 2, post  = " %")
                                  )
                                ),
                                fluidRow(
                                  # valuebox winrate
                                  valueBoxOutput("winrate",width = 3),
                                  bsTooltip("winrate", "Taxa de acerto (100 * Total de acertos / Total de entradas)",
                                            "top", options = list(container = "body",
                                                                  animation = "true")),
                                  # valuebox yield
                                  valueBoxOutput("yield",width = 3),
                                  bsTooltip("yield", "Taxa de retorno (100 * Soma de lucro e prejuízo / Total apostado)",
                                            "top", options = list(container = "body",
                                                                  animation = "true")),
                                  # valuebox roi
                                  valueBoxOutput("roi",width = 3),
                                  bsTooltip("roi", "Retorno sobre o investimento (100 * Soma de lucro e prejuízo / Valor da banca)",
                                            "top", options = list(container = "body",
                                                                  animation = "true")),
                                  # valuebox row
                                  valueBoxOutput("ros",width = 3),
                                  bsTooltip("ros", "Retorno sobre a stake (Soma de lucro e prejuízo / Valor da Stake)",
                                            "top", options = list(container = "body",
                                                                  animation = "true"))
                                ),
                                fluidRow(
                                  box(
                                    # title = "Relação Green e Red",
                                    # status = "warning",
                                    # solidHeader = TRUE,
                                    # collapsible = TRUE,
                                    dataTableOutput('RDT_GREEN_RED')
                                  ),
                                  box(
                                    # title = "Estatísticas sobre Odds",
                                    # status = "warning",
                                    # solidHeader = TRUE,
                                    # collapsible = TRUE,
                                    dataTableOutput('RDT_ODDS_SUMMARY')
                                  ),
                                  column(12,
                                         tabBox(width = 12,
                                                title = "Acompanhamento Mensal",
                                                # The id lets us use input$tabset1 on the server to find the current tab
                                                id = "tabset1",
                                                tabPanel("WinRate", ggiraphOutput("PLT_WR_MES")),
                                                tabPanel("Yield", ggiraphOutput("PLT_YD_MES")),
                                                tabPanel("ROI", ggiraphOutput("PLT_ROI_MES")),
                                                tabPanel("ROS", ggiraphOutput("PLT_ROS_MES")),
                                                tabPanel("Desempenho",
                                                         selectInput("mes_desempenho","Mês:",choices = ""),
                                                         ggiraphOutput("PLT_DES_MES")
                                                )
                                         )
                                         # box(width = 12,
                                         #     ggiraphOutput("PLT_WR_MES")
                                         # )
                                  )
                                ),
                        ),
                        #Second tab content
                        tabItem(tabName = "bfavht",
                                fluidRow(
                                  # A static valueBox
                                  valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                                  # Dynamic valueBoxes
                                  valueBoxOutput("progressBox2"),

                                  valueBoxOutput("approvalBox2")
                                )
                        )
                      )
                    )
)

server <- function(input, output, session) { 
  
  ##############################################.#
  ################# OVER 0.5 HT ################## 
  ##############################################.#
  
  req <- GET("https://api.github.com/repos/mranjos/Bots_Trader/git/trees/main?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  listxlsx <- grep("Database/Over05HT/", filelist, value = TRUE, fixed = TRUE)
  ldf = list()
  
  #### Leitura dos dados ####
  # setwd("https://raw.githubusercontent.com/mranjos/Bots_Trader/master/Database/Over05HT/")
  # ldf <- list() # creates a list
  # listxlsx <- dir(path = "https://raw.githubusercontent.com/mranjos/Bots_Trader/master/Database/Over05HT/", pattern = "*.xlsx")
  
  for (k in 1:length(listxlsx)){
    ldf[[k]] <-  read.xlsx(paste0("https://raw.githubusercontent.com/mranjos/Bots_Trader/master/",listxlsx[k]),na.strings=c(""," ","NA"))
    
    if (k == length(listxlsx)) {
      TB_OVER05 = do.call(rbind,ldf)
      rm(ldf,listxlsx,k)
    }
  }
  
  TB_OVER05 = TB_OVER05 %>% select("Nome.do.Bot"  
                                   ,"ID.da.ordem"
                                   ,"Data"
                                   ,"Odd"
                                   ,"Stake"
                                   ,"P/L"
                                   ,"ID.do.Evento") %>% 
    mutate("Data" = as.Date(Data),
           "GR" = ifelse(TB_OVER05$`P/L` < 0, "Red","Green"),
           "Resposta" = ifelse(TB_OVER05$`P/L` < 0, "0","1"),
           "PLR" = ifelse(TB_OVER05$`P/L` > 0, TB_OVER05$`P/L`-(TB_OVER05$`P/L`*0.065),TB_OVER05$`P/L`))
  
  # RELAÇÃO GREEN E RED ----
  TB_GREEN_RED = TB_OVER05 %>% 
    group_by(GR) %>% 
    summarise(Total = n()) %>% 
    mutate(Prop = round(100*(Total/sum(Total)),2)) %>% 
    adorn_totals(.,"row") %>% 
    rename("Resultado" = "GR")
  
  #### RENDER GREEN E RED ----
  output$RDT_GREEN_RED <- renderDataTable(
    datatable(TB_GREEN_RED,
              rownames = F,
              options = list(
                className = 'dt-center',
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00A65A', 'color': 'white'});",
                  "}")
              )
    )
  )
  
  #### RENDER VALUE WINRATE ----
  output$winrate <- renderValueBox({
    valueBox(paste0(round(100*TB_GREEN_RED$Total[TB_GREEN_RED$Resultado == "Green"]/TB_GREEN_RED$Total[TB_GREEN_RED$Resultado == "Total"],2),"%"), 
             "WinRate", 
             icon = icon("screenshot", lib = "glyphicon")
    )
  })
  
  #### RENDER VALUE YIELD ----
  output$yield <- renderValueBox({
    valueBox(paste0(round(100*sum(TB_OVER05$PLR)/sum(TB_OVER05$Stake),2),"%"), 
             "Yield", 
             icon = icon("briefcase", lib = "glyphicon"),
             color = "purple"
    )
  })
  
  #### RENDER VALUE ROI ----
  output$roi <- renderValueBox({
    valueBox(paste0(round(100*sum(TB_OVER05$PLR)/input$slider_banca,2),"%"), 
             "ROI", 
             icon = icon("usd", lib = "glyphicon"),
             color = "green"
    )
  })
  
  #### RENDER VALUE ROS ----
  output$ros <- renderValueBox({
    valueBox(paste0(round(sum(TB_OVER05$PLR)/(input$slider_banca*input$slider_stake*0.01),2)),
             "ROS", 
             icon = icon("credit-card", lib = "glyphicon"),
             color = "yellow"
    )
  })
  
  #### RENDER SUMMARY ODDS ----
  output$RDT_ODDS_SUMMARY <- renderDT(
    datatable(data.frame(Minimo = round(min(TB_OVER05$Odd),2),
                         Maximo = round(max(TB_OVER05$Odd),2),
                         Media = round(mean(TB_OVER05$Odd),2),
                         Mediana = round(median(TB_OVER05$Odd),2),
                         Dp = round(sd(TB_OVER05$Odd),2)),
              rownames = F,
              options = list(
                className = 'dt-center',
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00A65A', 'color': 'white'});",
                  "}")
              )
    )
  )
  
  #### RENDER PLOT WINRATE POR MES
  output$PLT_WR_MES <- renderggiraph({
    TB_WR_MES = TB_OVER05 %>% 
      group_by(Safra = year(Data)*100 + month(Data), GR) %>% 
      summarise(Total = n()) %>% 
      spread(GR, Total) %>%
      mutate(Winrate = Green/(Green+Red),
             Data = as.Date(paste0(str_sub(as.character(Safra),1,4),"-", 
                                   str_sub(as.character(Safra),5,6),"-01")),
             Total = Green + Red)
    
    TB_WR_MES$tooltip_bar = paste0("Total: ", TB_WR_MES$Total)
    TB_WR_MES$tooltip_point = paste0("Winrate: ", round(100*TB_WR_MES$Winrate,2), "%")
    
    g = ggplot(TB_WR_MES) +
      geom_bar_interactive(mapping = aes(x = Data, 
                                         y = Total*max(Winrate,na.rm = T)/max(Total,na.rm = T),
                                         fill="Entradas",
                                         tooltip = tooltip_bar),
                           stat = "identity") +
      geom_point_interactive(mapping = aes(x = Data, 
                                           y = Winrate,
                                           tooltip = tooltip_point), 
                             size = 3.5, 
                             color = "darkred") +
      geom_line(mapping = aes(x= Data, y=Winrate, group = 1, color = "Winrate"),linetype="dashed", size=1.3) +
      scale_x_date(name = "Safra", breaks = unique(TB_WR_MES$Data[order(TB_WR_MES$Data)]), 
                   labels = function(date){return(paste(lubridate::month(date, label = TRUE),lubridate::year(date),sep = "/"))}) +
      scale_colour_manual(" ", values=c("Winrate" = "darkred", "Entradas" = "lightgreen")) +
      scale_fill_manual("",values="lightgreen") +
      scale_y_continuous(name = "Winrate",
                         sec.axis = sec_axis(~ . * (max(TB_WR_MES$Total,na.rm = T)/max(TB_WR_MES$Winrate, na.rm = T)),
                                             name = "Entradas"),
                         limits = c(0,(max(TB_WR_MES$Winrate,na.rm = T)+0.05)),labels = scales::percent) +
      ggtitle("Winrate por Mês") +
      theme_minimal(15) + 
      theme(
        legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title=element_blank(),
        axis.title.x = element_blank()
      ) +
      guides(fill = "none")
    
    girafe(ggobj = g 
           ,fonts = list(sans = "Arial", serif = "Times New Roman", mono = "Courier", symbol = "Symbol")
           ,options = list(opts_tooltip(css = "padding:5px;background:#374567;color:white;border-radius:2px 2px 2px 2px; font-family: Arial;font-weight: bold;")
                           ,opts_sizing(rescale = TRUE, width = 1)
           ))
    
  })
  
  #### RENDER PLOT YIELD POR MES
  output$PLT_YD_MES <- renderggiraph({
    TB_YD_MES = TB_OVER05 %>% 
      group_by(Safra = year(Data)*100 + month(Data)) %>% 
      summarise(PLR = sum(PLR),
                Stake = sum(Stake)) %>% 
      mutate(Yield = PLR/Stake,
             Data = as.Date(paste0(str_sub(as.character(Safra),1,4),"-", 
                                   str_sub(as.character(Safra),5,6),"-01")))
    
    
    TB_YD_MES$tooltip_bar = paste0("Investido: ", TB_YD_MES$Stake)
    TB_YD_MES$tooltip_point = paste0("Yield: ", round(100*TB_YD_MES$Yield,2), "%")
    
    g = ggplot(TB_YD_MES) +
      geom_bar_interactive(mapping = aes(x = Data, 
                                         y = Stake*max(Yield,na.rm = T)/max(Stake,na.rm = T),
                                         fill="Entradas",
                                         tooltip = tooltip_bar),
                           stat = "identity") +
      geom_point_interactive(mapping = aes(x = Data, 
                                           y = Yield,
                                           tooltip = tooltip_point), 
                             size = 3.5, 
                             color = "darkred") +
      geom_line(mapping = aes(x= Data, y=Yield, group = 1, color = "Yield"),linetype="dashed", size=1.3) +
      scale_x_date(name = "Safra", breaks = unique(TB_YD_MES$Data[order(TB_YD_MES$Data)]), 
                   labels = function(date){return(paste(lubridate::month(date, label = TRUE),lubridate::year(date),sep = "/"))}) +
      scale_colour_manual(" ", values=c("Yield" = "darkred", "Stake" = "lightgreen")) +
      scale_fill_manual("",values="lightgreen") +
      scale_y_continuous(name = "Yield",
                         sec.axis = sec_axis(~ . * (max(TB_YD_MES$Stake,na.rm = T)/max(TB_YD_MES$Yield, na.rm = T)),
                                             name = "Stake"),
                         limits = c(-0.05,(max(TB_YD_MES$Yield,na.rm = T)+0.05)),labels = scales::percent) +
      ggtitle("Yield por Mês") +
      theme_minimal(15) + 
      theme(
        legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title=element_blank(),
        axis.title.x = element_blank()
      ) +
      guides(fill = "none")
    
    girafe(ggobj = g 
           ,fonts = list(sans = "Arial", serif = "Times New Roman", mono = "Courier", symbol = "Symbol")
           ,options = list(opts_tooltip(css = "padding:5px;background:#374567;color:white;border-radius:2px 2px 2px 2px; font-family: Arial;font-weight: bold;")
                           ,opts_sizing(rescale = TRUE, width = 1)
           ))
    
  })
  
  #### RENDER PLOT ROI POR MES
  output$PLT_ROI_MES <- renderggiraph({
    
    TB_ROI_MES = TB_OVER05 %>% 
      group_by(Safra = year(Data)*100 + month(Data)) %>% 
      summarise(PLR = sum(PLR),
                Banca = input$slider_banca) %>% 
      mutate(ROI = PLR/Banca,
             Data = as.Date(paste0(str_sub(as.character(Safra),1,4),"-", 
                                   str_sub(as.character(Safra),5,6),"-01")))
    
    TB_ROI_MES$tooltip_point = paste0("ROI: ", round(100*TB_ROI_MES$ROI,2), "%")
    
    g = ggplot(TB_ROI_MES) +
      geom_point_interactive(mapping = aes(x = Data, 
                                           y = ROI,
                                           tooltip = tooltip_point), 
                             size = 3.5, 
                             color = "darkred") +
      geom_line(mapping = aes(x= Data, y=ROI),linetype="dashed", color = "darkred", size=1.3) +
      scale_x_date(name = "Safra", breaks = unique(TB_ROI_MES$Data[order(TB_ROI_MES$Data)]), 
                   labels = function(date){return(paste(lubridate::month(date, label = TRUE),lubridate::year(date),sep = "/"))}) +
      scale_y_continuous(name = "ROI",labels = scales::percent) +
      ggtitle("ROI por Mês") +
      theme_minimal(15) + 
      theme(
        legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title=element_blank(),
        axis.title.x = element_blank()
      )
    
    girafe(ggobj = g 
           ,fonts = list(sans = "Arial", serif = "Times New Roman", mono = "Courier", symbol = "Symbol")
           ,options = list(opts_tooltip(css = "padding:5px;background:#374567;color:white;border-radius:2px 2px 2px 2px; font-family: Arial;font-weight: bold;")
                           ,opts_sizing(rescale = TRUE, width = 1)
           ))
    
  })
  
  #### RENDER PLOT ROS POR MES
  output$PLT_ROS_MES <- renderggiraph({
    
    TB_ROS_MES = TB_OVER05 %>% 
      group_by(Safra = year(Data)*100 + month(Data)) %>% 
      summarise(PLR = sum(PLR),
                Stake = input$slider_banca*input$slider_stake*0.01) %>% 
      mutate(ROS = PLR/Stake,
             Data = as.Date(paste0(str_sub(as.character(Safra),1,4),"-", 
                                   str_sub(as.character(Safra),5,6),"-01")))
    
    TB_ROS_MES$tooltip_point = paste0("ROS: ", trunc(TB_ROS_MES$ROS,2), " Stakes")
    
    g = ggplot(TB_ROS_MES) +
      geom_point_interactive(mapping = aes(x = Data, 
                                           y = ROS,
                                           tooltip = tooltip_point), 
                             size = 3.5, 
                             color = "darkred") +
      geom_line(mapping = aes(x= Data, y=ROS),linetype="dashed", color = "darkred", size=1.3) +
      scale_x_date(name = "Safra", breaks = unique(TB_ROS_MES$Data[order(TB_ROS_MES$Data)]), 
                   labels = function(date){return(paste(lubridate::month(date, label = TRUE),lubridate::year(date),sep = "/"))}) +
      ggtitle("ROS por Mês") +
      theme_minimal(15) + 
      theme(
        legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title=element_blank(),
        axis.title.x = element_blank()
      )
    
    girafe(ggobj = g 
           ,fonts = list(sans = "Arial", serif = "Times New Roman", mono = "Courier", symbol = "Symbol")
           ,options = list(opts_tooltip(css = "padding:5px;background:#374567;color:white;border-radius:2px 2px 2px 2px; font-family: Arial;font-weight: bold;")
                           ,opts_sizing(rescale = TRUE, width = 1)
           ))
    
  })
  
  mesOutput <- reactive({
    
    update_mes_output = c("total",unique(as.character(month(TB_OVER05$Data, label = TRUE))))
    
  })  
  
  observe ({
    
    updateSelectizeInput(session, inputId='mes_desempenho', 
                         choices = c(mesOutput()), selected = NULL)
    
  })
  
  #### RENDER PLOT EVOLUÇÃO BANCA (Dinheiro e Percentual)
  observeEvent(input$mes_desempenho, {
    
    if (input$mes_desempenho == "total") {
      TB_DES_MES = TB_OVER05 %>% 
        group_by(Data) %>% 
        summarise(Retorno = sum(PLR)) %>% 
        arrange(Data) %>% 
        mutate(Acumulado = cumsum(Retorno),
               tooltip_point = paste0("Retorno: R$ ", input$slider_banca + round(Acumulado,2)))
    } else {
      TB_DES_MES = TB_OVER05 %>% 
        group_by(Data) %>% 
        summarise(Retorno = sum(PLR)) %>% 
        arrange(Data) %>% 
        filter(month(Data,label = T) == input$mes_desempenho) %>% 
        mutate(Acumulado = cumsum(Retorno),
               tooltip_point = paste0("Retorno: R$ ", input$slider_banca + round(Acumulado,2)))
    }
    
    output$PLT_DES_MES = renderGirafe({
      
      g = TB_DES_MES %>% 
        ggplot(aes(x = Data, 
                   y = input$slider_banca+Acumulado)) +
        geom_line(color = "darkgreen",linetype="dashed", size=1.3) +
        geom_point_interactive(aes(x = Data, y = input$slider_banca+Acumulado,tooltip = tooltip_point),
                               color = "darkgreen", 
                               size = 2) + 
        labs(x = "", y = "Banca (R$)") +
        scale_y_continuous(n.breaks = 10,
                           labels = scales::dollar_format(prefix = "R$", decimal.mark = ".")) +
        scale_x_date(date_breaks = "1 day", date_labels = "%d/%b/%y") +
        scale_color_viridis_d() +
        theme_bw() +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 45,hjust=1, size=7),
              axis.text.y = element_text(size=10),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12))
      
      girafe(
        ggobj = g, width_svg = 9
      )
      
    })
    
  })

}

shinyApp(ui, server)
# runApp(list(ui = ui, server = server),launch.browser = TRUE)
