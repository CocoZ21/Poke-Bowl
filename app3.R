#import packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(viridisLite)
library(heatmaply)
library(Rcpp)
library(dashboardthemes)
library(ggthemes)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(fmsb)

#getwd()
data <- read_csv("pokemon.csv")

data <- data %>%
  distinct(pokedex_number, .keep_all = TRUE)
data <- data[0:721,]
data$pokedex_number = str_pad(data$pokedex_number, 3, side = "left", "0")


# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Pokemon Battle!"),
  dashboardSidebar(sidebarMenu(
    menuItem("HomePage", tabName = "page1", icon = icon("dashboard")),
    menuItem(
      "Overview",
      tabName = "page2",
      icon = icon("area-chart")
    ),
    menuItem(
      "Pick Your Pokemon",
      tabName = "page3",
      icon = icon("map-o")
    ),
    menuItem("Let's Battle!", tabName = "page4", icon = icon("flash"))
  )),
  dashboardBody(
    setBackgroundImage(src = "wallpaper3.jpg",
                       shinydashboard = TRUE),
    tabItems(
      ################################################first page
      tabItem(
        tabName = "page1",
        fluidRow(
          align = "center",
          box(
            status = "warning",
            collapsible = TRUE,
            width = 12,
            div(tags$image(
              src = "1.jpg",
              width = "100%",
              height = "500"
            )),
            br(),
            div(
              tags$span(style = "color:orange;font-size:36px", "introduction")
            ),
            br(),
            div("blabla"),
            div("blabla"),
            br()
          )
        ),
        fluidRow(
          align = "center",
          box(
            status = "warning",
            width = 12,
            collapsible = TRUE,
            column(
              12,
              tags$iframe(
                src = "https://www.youtube.com/embed/xM24hiwTAe8",
                width = "80%",
                height = "500",
                frameborder = "0",
                allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
              ),
              style = "text-align:center",
              p(
                a("Video Link", href = "https://www.youtube.com/watch?v=0uyLRPmmYPk&t=1s")
              ),
              br()
            )
          ),
          br()
        ),
        fluidRow(
          box(
            title = "Teams",
            solidHeader = TRUE,
            status = "warning",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(fluidRow(
                     column(
                       4,
                       tags$img(
                         src = "pokemon.jpg",
                         height = 158,
                         width = 280,
                         align = "center"
                       )
                     ),
                     column(
                       8,
                       tags$div(
                         "All team members are MSBARM candidates at Carey Business School, Johns Hopkins University:",
                         style = "font-size:16px"
                       )
                     )
                   )),
                   br(),
                   tags$li("blabla"))
          ),
          br()
        )
      ),
      
      ################################################second page
      tabItem(tabName = "page2",
              mainPanel(tabsetPanel(
                tabPanel("General Dashboard",
                         h2("Average stats by generation"),
                         fluidRow(
                           selectInput(
                             "attribute1",
                             "Select Attribute:",
                             choices = c("attack","defense", "hp", "speed", "sp_attack", "sp_defense"),
                             selected = "attack"
                           )),
                         fluidRow(plotOutput("averageplot")),
                         br(),
                         br()
                ),
                tabPanel("Attribute Analysis",
                         br(),
                         fluidRow(
                           selectInput(
                             "attribute2",
                             "Select Attribute:",
                             choices = c("attack","defense", "hp", "speed", "height_m", "weight_kg", 
                                         "sp_attack", "sp_defense", "is_legendary"),
                             selected = "attack"
                           )
                         ),
                         fluidRow(
                           column(1,),
                           column(5,plotOutput("densityplot",height = 500, width = 500 )),
                           column(5,),
                           column(1,dataTableOutput("table1"),width=5)
                         ))
                
              ))),
      
      ################################################third page
      tabItem(
        tabName = "page3",background = 'blue',
        #width=12,
              fluidRow(
                column(12, h1("Learn About Your Pokemon", style = "text-align:center"))),
            
              fluidRow(
                  column(1),
                  column(5,
                  selectInput(
                    "Pokemon",
                    "Select Pokemon:",
                    choices = unique(data$name),
                    selected = data$name[1]
                  )
                ),
                column(6, )),
            
            
                fluidRow(
                  fluidRow(column(1, ),
                           column(5, 
                                  fluidRow(
                                    column(6,infoBox("Japanese Name", textOutput('textname'), color="purple",width = 12)),
                                    column(6,infoBox("Generation",  textOutput('textgen'),color="aqua", width=12))
                                    ),
                                  
                                  fluidRow(
                                    column(6,infoBox("Ability",   textOutput('textability'), color="red", width=12)),
                                    column(6,infoBox("Capture Rate",  textOutput('textcap'), color="green", width=12))
                                  ),
                                  
                                  fluidRow(
                                    column(6,infoBox("Weight",  textOutput('textweight'),  color="yellow", width=12)),
                                    column(6,infoBox("Height",  textOutput('textheight'), color="light-blue", width=12))
                                  ),
                                  
                                  fluidRow(plotOutput("radarplot",height = 500, width = 500 ))
                           ),
                           column(4,
                                  fluidRow(h2("Pokémon!")),
                                  fluidRow(column(4,br(),uiOutput('leg')),
                                           column(4,br(), uiOutput('img2')),
                                           column(4,br(), uiOutput('img3'))
                                           ),
                                  fluidRow(uiOutput("img"))),
                           column(1, ),
                  )
            
                )
              )
      ,
      ################################################fourth page
      tabItem(tabName = "page4",
              fluidRow(align = "center",
                       h1("pick your team")),
              fluidRow(align = "center",
                       column(1,),
                       column(
                         5,
                         selectInput(
                           "Pokemon1",
                           "Select Pokemon:",
                           choices = unique(data$name),
                           selected = data$name[1]
                         ),
                         br(),
                         uiOutput("img_f1")
                       ),
                       column(5,selectInput(
                         "Pokemon2",
                         "Select Pokemon:",
                         choices = unique(data$name),
                         selected = data$name[1]
                       ),
                       br(),
                       uiOutput("img_f2")),
                       column(1,)))
      
    )
  )
)


server <- function(input, output, session) {

################################################second page  
  # output$densityplot1 <- renderPlot({
  #   den <- ggplot(data, aes(input$attribute1))+ 
  #     geom_density(col="white",fill="pink", alpha=0.8) + 
  #     ggtitle(paste0("Density Plot of ",input$attribute1))
  #   return(den)
  # })
  
  output$densityplot <- renderPlot({
    den <- ggplot(data,aes_string(input$attribute2))+
      geom_density(col="white",fill="yellow", alpha=0.8) + 
      ggtitle(paste0("Density Plot of ",input$attribute2))
    return(den)
  })
  
  output$averageplot <- renderPlot({
    avgstats = data %>%
      select(c("attack","defense", "hp", "speed", "sp_attack", "sp_defense","generation"))%>%
      group_by(generation)%>%
      summarise(across(everything(), list(mean)))
    
    colnames(avgstats) <- c('generation',"attack","defense", "hp", "speed", "sp_attack", "sp_defense")
    
    avgstats=as.data.frame(avgstats)
    
    gen=ggplot(avgstats, mapping=aes(x=generation, y=aes_string(input$attribute1))) + 
      geom_line(linetype="dashed", color = "#0099f9", size=2)
    return(gen)
  })
  
  output$table1=renderDataTable({
    data <- data %>% 
      arrange(desc(attack)) %>%
      select(name,attack) %>%
      head(n=15)
    
    headerCallbackRemoveHeaderFooter <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )
    return(datatable(data = data, 
                     rownames = TRUE,
                     filter = "none",
                     selection = 'none',
                     width = 1000,
                     callback = JS(
                       "$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       headerCallback = JS(headerCallbackRemoveHeaderFooter)
                     )))
  })
  
################################################third page
  da <- reactive({
    input$Pokemon
  })
  
  output$leg=renderUI({
    legend=data$is_legendary[which(data$name==da())]
    if (legend== TRUE ){
      tags$img(
        src = "LegendaryLogo.png",
        height = 30,
        width = 100,
        align = "left")}
  })
  output$img <- renderUI({
    number=data$pokedex_number[which(data$name==da())]
    tags$div(img(
      src = paste0(
        'https://assets.pokemon.com/assets/cms2/img/pokedex/full/',
        number,
        '.png'
      ),
      width = "550",
      height = "550"
    ), style = "right")
  })
  output$img2 <- renderUI({
    type1 =data$type1[which(data$name==da())]
    tags$div(img(
      src = paste0(
        'https://www.serebii.net/pokedex-bw/type/',
        type1,
        '.gif'
      ),
      width = "100",
      height = "30",
      align = "center"
    ))
  })
  output$img3 <- renderUI({
    type2 =data$type2[which(data$name==da())]
    if (is.na(type2) ==FALSE  ){
      tags$div(img(
        src = paste0(
          'https://www.serebii.net/pokedex-bw/type/',
          type2,
          '.gif'
        ),
        width = "100",
        height = "30",
        align = "center"
      )) }

    })


  output$textname = renderText({
    data$japanese_name[which(data$name==da())]
  })
  output$textgen = renderText({
    data$generation[which(data$name==da())]
  })
  output$textability = renderText({
    data$abilities[which(data$name==da())]
  })
  output$textcap = renderText({
    data$capture_rate[which(data$name==da())]
  })
  output$textweight = renderText({
    data$weight_kg[which(data$name==da())]
  })
  output$textheight = renderText({
    data$height_m[which(data$name==da())]
  })
  
  
  
  
  
  
  
  # rank=data%>%
  #   select(speed,attack,defense,hp)%>%
  #   mutate(r_s=rank(speed)/721*100,r_a=rank(attack)/721*100,r_d=rank(defense)/721*100,r_h=rank(hp)/721*100)%>%
  #   filter(data$name==da())
  output$table1=renderDataTable({
    
    type1 =data$type1[which(data$name==da())]
    
    colorselect = if(type1 == 'grass'){'#47d147'
    } else if (type1 == 'fire'){'#ff0000'}else if (type1 == 'water'){'#0066ff'}else if (type1 == 'bug'){'#999966'
    }else if (type1 == 'normal'){'#d9d9d9'}else if (type1 == 'poison'){'#9900cc'}else if (type1 == 'electric'){'#ffff00'
    }else if (type1 == 'ground'){'#ff80ff'}else if (type1 == 'fairy'){'#ff80ff'}else if (type1 == 'fighting'){'#ff5c33'}else if (type1 == 'psychic'){'#ff3399'
    }else if (type1 == 'rock'){'#cc6600'}else if (type1 == 'ghost'){'#a366ff'}else if (type1 == 'Ice'){'#66ffff'
    }else if (type1 == 'dragon'){'#0000cc'}else if (type1 == 'dark'){'#00264d'}else if (type1 == 'steel'){'#bfbfbf'
    }else{'#99ffff'}
    
    colorselect <- alpha(colorselect,0.3)
    
    data=data%>%
      filter(name==da())%>%
      select(pokedex_number,japanese_name,generation,abilities,capture_rate,weight_kg,height_m)%>%
      t()
    
    row.names(data)[1:7]=(c("Pokemon Index","Japanese Name","Generation","Ability","Capture Rate","Weight","Height"))
    

    headerCallbackRemoveHeaderFooter <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )
    return(datatable(data = data, 
                     rownames = TRUE,
                     filter = "none",
                     selection = 'none',
                     width = 500,
                     callback = JS(
                       "$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       headerCallback = JS(headerCallbackRemoveHeaderFooter)
                     ))%>% formatStyle(columns = "V1", target = "cell", backgroundColor = colorselect))
  })

  output$radarplot=renderPlot({
    
    
    type1 =data$type1[which(data$name==da())]
    
    colorselect = if(type1 == 'grass'){'#47d147'
    } else if (type1 == 'fire'){'#ff0000'}else if (type1 == 'water'){'#0066ff'}else if (type1 == 'bug'){'#999966'
    }else if (type1 == 'normal'){'#d9d9d9'}else if (type1 == 'poison'){'#9900cc'}else if (type1 == 'electric'){'#ffff00'
    }else if (type1 == 'ground'){'#ff80ff'}else if (type1 == 'fairy'){'#ff80ff'}else if (type1 == 'fighting'){'#ff5c33'}else if (type1 == 'psychic'){'#ff3399'
    }else if (type1 == 'rock'){'#cc6600'}else if (type1 == 'ghost'){'#a366ff'}else if (type1 == 'Ice'){'#66ffff'
    }else if (type1 == 'dragon'){'#0000cc'}else if (type1 == 'dark'){'#00264d'}else if (type1 == 'steel'){'#bfbfbf'
    }else{'#99ffff'}
    
    colorselect <- alpha(colorselect,0.3)
    
    data2 = data%>%
      arrange(pokedex_number)%>%
      data.frame(row.names = c('name'))%>%
      select( c('hp','speed','attack','sp_attack',"defense","sp_defense"))
    max_min <- data.frame(row.names = c("Max", "Min"),
                          hp = c(max(data2$hp),min(data2$hp)),
                          speed = c(max(data2$speed),min(data2$speed)),
                          attack = c(max(data2$attack),min(data2$attack)),
                          sp_attack = c(max(data2$sp_attack),min(data2$sp_attack)),
                          defense = c(max(data2$defense),min(data2$defense)),
                          sp_defense = c(max(data2$sp_defense),min(data2$sp_defense))
    )
    data2 <- rbind(max_min,data2)
    radarnum <- data2[c("Max", "Min", da()),]
    
    colnames(radarnum)[1:6]=(c("Title","Hp","Speed","Attack","Special Attack","Defense","Special Defense"))
    
    return(radarchart(radarnum,
                             cglty = 1,       # Grid line type
                             cglcol = "grey", # Grid line color
                             plwd = 1,        # Width for each line
                             plty = 1,
                             pcol = 'grey',  ##line color
                             pfcol = colorselect,#fill color
                             vlcex=1.5
                             ))
    #return(raderchart1)
  })
################################################fourth page  
  
  da1 <- reactive({
    input$Pokemon1
  })
  da2 <- reactive({
    input$Pokemon2
  })
  output$img_f1 <- renderUI({
    number=data$pokedex_number[which(data$name==da1())]
    tags$div(img(
      src = paste0(
        'https://assets.pokemon.com/assets/cms2/img/pokedex/full/',
        number,
        '.png'
      ),
      width = "550",
      height = "550"
    ), style = "right")
  })
  output$img_f2 <- renderUI({
    number=data$pokedex_number[which(data$name==da2())]
    tags$div(img(
      src = paste0(
        'https://assets.pokemon.com/assets/cms2/img/pokedex/full/',
        number,
        '.png'
      ),
      width = "550",
      height = "550"
    ), style = "right")
  })
  
}

shinyApp(ui = ui, server = server)
