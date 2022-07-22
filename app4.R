#import packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(viridisLite)
library(heatmaply)
library(Rcpp)
library(dashboardthemes)
library(ggthemes)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(fmsb)
library(ggplot2)
library(formattable)

#getwd()
data <- read_csv("pokemon.csv")
data <- data %>%
  distinct(pokedex_number, .keep_all = TRUE)
data <- data[0:721,]
data$pokedex_number = str_pad(data$pokedex_number, 3, side = "left", "0")

data1=as.data.frame(data)

mu_at=round(mean(data1$attack),0)
mu_base=round(mean(data1$base_total),0)
mu_df=round(mean(data1$defense),0)
mu_hp=round(mean(data1$hp),0)
mu_sp=round(mean(data1$speed),0)
mu_len=round(mean(data1$is_legendary),2)

nhp=aggregate(hp~generation,data1,mean,)
nspeed=aggregate(speed~generation,data1,mean)
nattack=aggregate(attack~generation,data1,mean)
nsp_attack=aggregate(sp_attack~generation,data1,mean)
ndefense=aggregate(defense~generation,data1,mean)
nsp_defense=aggregate(sp_defense~generation,data1,mean)

df = nhp%>%
  full_join(nspeed,by = 'generation')%>%
  full_join(nattack,by = 'generation')%>%
  full_join(nsp_attack,by = 'generation')%>%
  full_join(ndefense,by = 'generation')%>%
  full_join(nsp_defense,by = 'generation')%>%
  pivot_longer(c('hp','speed','attack','sp_attack',"defense","sp_defense"),
               names_to= "Attribute", values_to= "average")


# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Pokemon Battle!"),
  dashboardSidebar(sidebarMenu(
    menuItem("HomePage", tabName = "page1", icon = icon("dashboard")),
    menuItem(
      "Sprites Gallery",
      tabName = "page2",
      icon = icon("book")
    ),
    menuItem(
      "Pick Your Pokemon",
      tabName = "page4",
      icon = icon("map-o")
    ),
    menuItem(
      "Overview",
      tabName = "page3",
      icon = icon("area-chart")
    ),
    menuItem("Let's Battle!", tabName = "page5", icon = icon("flash"))
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
            status = "danger",
            collapsible = TRUE,
            width = 12,
            div(tags$image(
              src = "1.jpg",
              width = "100%",
              height = "500"
            )),
            br(),
            h1("Welcome to the Pokémon Bowl",style="color:red;font-size:50px"),
            br(),
            h2("Explore the Pokémon universe and start your first battle",style="color:red;font-size:20px"),
            br()
          )
        ),
        fluidRow(
          align = "center",
          box(
            status = "danger",
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
            status = "danger",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(
                     tags$div(
                       "All team members are 2022 MSBARM candidates at Carey Business School, Johns Hopkins University:",
                       style = "font-size:16px;text-align:center"
                     )
                   )
            ),
            br(),
            br(),
            fluidRow(style = "text-align:center",
                     column(1),
                     column(3,userBox(title = userDescription(title ="Coco Zhang",type = 2,image = "52.jpg"),width = 12,
                                      background = "gray",gradient = TRUE,"szhan170@jh.edu")),
                     column(3,userBox(title = userDescription(title ="Suning Cheng",type = 2,image = "94.jpg"),width = 12,
                                      background = "navy",gradient = TRUE,"scheng52@jh.edu")),
                     column(3,userBox(title = userDescription(title ="Qingxing Song",type = 2,image = "492.jpg"),width = 12,
                                      background = "olive",gradient = TRUE,"qsong9@jh.edu"))
            ),
            fluidRow(style = "text-align:center",
                     column(2),
                     column(3,userBox(title = userDescription(title ="Heather Wang",type = 2,image = "7.jpg"),width = 12,
                                      background = "light-blue",gradient = TRUE,"rwang121@jh.edu")),
                     column(3,userBox(title = userDescription(title ="Mike Feng",type = 2,image = "155.jpg"),width = 12,
                                      background = "yellow",gradient = TRUE,"jfeng40@jh.edu"))

            )
            )
        ),
        br()
      )
      ,
      ################################################second page
      tabItem(tabName = "page2",
              box(width=12,
                  h1("List of Pokémon",style = "text-align:center"),
                  fluidRow(
                    DTOutput("tableall")%>% withSpinner(color="#f39c12")
                  ))
      ),
      
      
      ################################################third page
      tabItem(
        tabName = "page4",background = 'blue',
        fluidRow(
          column(12, h1("Learn about Your Pokemon", style = "text-align:center"))),
        
        fluidRow(
          column(5,style = "text-align:center",
                 #tags$select("#Pokemon ~ .selectize-control .option:nth-child(odd) {background-color: rgba(30,144,255,0.5);"),
                 selectInput(
                   "Pokemon",
                   label = tags$span(style="color: red;","Select Pokemon:"),
                   choices = unique(data$name),
                   selected = data$name[25]
                 )
          ),
          column(6, )),
        fluidRow(
          fluidRow(column(5, style = "text-align:center",
                          fluidRow(
                            column(6,box(strong("Japanese Name"), textOutput('textname'), color='white',width = 12)),
                            column(6,box(strong("Generation"),  textOutput('textgen'),color='white', width=12))
                          ),
                          
                          fluidRow(
                            column(6,box(strong("Ability"),   textOutput('textability'), color="white", width=12)),
                            column(6,box(strong("Capture Rate"),  textOutput('textcap'), color="white", width=12))
                          ),
                          
                          fluidRow(
                            column(6,box(strong("Weight"),  textOutput('textweight'),  color="white", width=12)),
                            column(6,box(strong("Height"),  textOutput('textheight'), color="white", width=12))
                          ),
                          
                          fluidRow(
                            column(1),
                            column(11,plotOutput("radarplot",height = 500, width = 500 )))
          ),
          column(1, ),
          column(4,
                 fluidRow(h2("Pokémon!")),
                 fluidRow(column(4,br(),uiOutput('leg')),
                          column(4,br(), uiOutput('img2')),
                          column(4,br(), uiOutput('img3'))
                 ),
                 fluidRow(
                   uiOutput("img"))),
          column(1, ),
          )
          
        )
      )
      ,
      
      ################################################fourth page
      tabItem(tabName = "page3",
              mainPanel(width = 12,
                        tabsetPanel(
                          tabPanel("General Dashboard",
                                   box(width=12,
                                       fluidRow(width=12,
                                                column(3,box(h2(strong("721")),br(),h4('Total Pokemon'), style = "text-align:center",height = 50, width = 12,background = "red")),
                                                column(3,box(h2(strong(length(unique(data$classfication)))),br(),h4('Total Class'), style = "text-align:center",height = 50, width = 12,background = "red",solidHeader = FALSE)),
                                                column(3,box(h2(strong(length(unique(data$type1)))),br(),h4('Total Type'), style = "text-align:center",height = 50, width = 12,background = "red",solidHeader = FALSE)),
                                                column(3,box(h2(strong(length(unique(data$abilities)))),br(),h4('Total Ability Set'), style = "text-align:center",height = 50, width = 12,background = "red",solidHeader = FALSE))),
                                       fluidRow(width=12,
                                                box(width=12,title = "Generation Stats", status = "danger",solidHeader = TRUE,
                                                column(4,plotOutput("generation",height = 250, width = 380 )%>% withSpinner(color="red")),
                                                column(8,plotOutput('Attributegen',height = 250, width = 750 )%>% withSpinner(color="red")))),
                                       fluidRow(width=12,
                                     box(width=12,title = "Type Stats",status = "danger",solidHeader = TRUE,
                                                column(5,plotOutput("type1",height = 300, width = 500 )%>% withSpinner(color="red")),
                                                column(7,plotOutput("type2",height = 300, width = 650 )%>% withSpinner(color="red")))
                                       ),
                                     fluidRow(width=12,
                                              box(width=12,title = "Pie Stats",status = "danger",solidHeader = TRUE,
                                                  column(6,plotOutput("pie_len",height = 300, width = 500 )%>% withSpinner(color="red")),
                                                  column(6,plotOutput("pie_male",height = 300, width = 500 )%>% withSpinner(color="red"))
                                     ))
                                     ),
                                   
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
                                     ),
                                     br(),br()
                                   ),
                                   fluidRow(
                                     column(1,),
                                     column(5,br(),br(),plotOutput("densityplot",height = 550, width = 500 )%>% withSpinner(color="red")),
                                     column(5,h4('Top 15 Pokemon'),dataTableOutput("table1",height = 550, width = 500)%>%withSpinner(color="red")),
                                     column(1,)
                                   ))
                          
                        ))),
      
      
      ################################################fifth page
      tabItem(tabName = "page5",
              fluidRow(align = "center",
                       h1("Pick Your Team")),
              fluidRow(align = "center",
                       column(1,),
                       column(
                         5,
                         selectInput(
                           "Pokemon1",
                           "Select Pokemon:",
                           choices = unique(data$name),
                           selected = data$name[25]
                         ),
                         br(),
                         uiOutput("img_f1")
                       ),
                       column(5,selectInput(
                         "Pokemon2",
                         "Select Pokemon:",
                         choices = unique(data$name),
                         selected = data$name[26]
                       ),
                       br(),
                       uiOutput("img_f2")),
                       column(1,))
              ,
              fluidRow(
                align = "center",
                h2(""),
                actionButton(
                  "battlebutton",
                  "Start the BATTLE!",
                  icon = icon("fa-light fa-gamepad"),
                  style = "pill",
                  width = "300px",
                  class = "btn-lg"
                ),
                h2(""),
                textOutput("BattleStatus"),
                uiOutput("Crown"),
                uiOutput("Winner"),
                uiOutput("Crown2")
              ))

    )
  )
)




server <- function(input, output, session) {
  
  ################################################second page
  img_src=NULL
  for (i in 1:nrow(data)){
    a=paste0('<img width="50px" src= "https://assets.pokemon.com/assets/cms2/img/pokedex/full/',data$pokedex_number[i],'.png"></img>')
    img_src=append(img_src,a)
  }
  
  output$tableall = renderDT({
    new_df <- data.frame(img_src,data$pokedex_number,data$name,data$generation ,data$type1,data$type2,data$attack,data$defense,data$hp, data$speed, paste0(data$weight_kg,"kg"),paste0(data$height_m,"m"),paste0(data$percentage_male,"%"),data$capture_rate,data$base_total)
    colnames(new_df) <- c("Image","Pokedex","Name","Generation","Type One", "Type Two", "Attack", "Defense","HP","Speed","Weight", "Height","Male Percentage","Capture Rate","Total Base")
    return(datatable(data=new_df,
                     rownames= FALSE,
                     width=12, 
                     escape= FALSE,
                     filter = "none",
                     selection = 'none',
                     callback = JS(
                       "$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(
                       pageLength = 7)
    ))
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
    
  })
  
  
  ################################################fourth page
  
  output$type1 = renderPlot({
    data %>%
      count(type1) %>%
      arrange(n)%>%
      ggplot(aes(x=reorder(type1,-n), y=n)) + 
      geom_bar(stat="identity", fill=alpha(c('#6890F0', '#A8A878', '#78C850', '#A8B820', '#F85888', '#F08030', '#B8A038', '#F8D030',
                         '#E0C068', '#705898', '#705848', '#C03028', '#7038F8', '#705898', '#B8B8D0', '#98D8D8',
                         '#EE99AC', '#A890F0'),0.8), colour="white") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(x="Type 1", y="Number of Pokémon",title="Number of Pokémon for Primary Type ") 
       
  })

  
  output$type2 = renderPlot({
    type12 <- data %>%
      group_by(type1, type2) %>%
      summarise(n=n()) 
    ggplot(type12, aes(x=type1,y=type2)) +
      geom_tile(aes(fill=n), show.legend=FALSE) +
      geom_text(aes(label=n)) +
      theme_bw() +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(x="Type 1", y="Type 2",
           title="Number of Pokémon for each type combination") +   
      scale_fill_gradient(low=alpha("red",0.2), high="red") 
  })

  
  
  output$Attributegen <- renderPlot({
    
    attgen <- df%>%
      ggplot(aes(x=generation ,y=average, colour = Attribute),group=1)+
      geom_line(size=1.25)+
      coord_cartesian(ylim = c(55,85))+
      labs(x="Generation", y="Average Stats",title="Average Stats for Each Generation")+
      scale_color_manual(values =c("#a6cee3","#1f78b4",'#b2df8a','#33a02c','#fb9a99','#e31a1c'))+
      theme(axis.title.y=element_text(face="bold", size=20))+
      theme_bw() 
    return(attgen)
    
  })


  output$Avgtotal <- renderPlot({
    data %>%
      group_by(generation) %>%
      summarize(n=mean(base_total)) %>%
      ggplot(aes(x=generation,y=n,group=1)) +
      geom_col(color="black",fill=alpha("red",0.4)) +
      labs(y="Average Total") +
      geom_label(aes(label=n))+
      theme_bw()+
      theme_classic()
  })
 
  
  output$generation <- renderPlot({
    data %>%
      count(generation) %>%
      ggplot(aes(x=generation, y=n)) + 
      geom_bar(stat="identity", fill=c("#a6cee3","#1f78b4",'#b2df8a','#33a02c','#fb9a99','#e31a1c'), colour="white") +
      labs(x="Generation", y="Number of Pokémon",title="Total Pokemon for Each Generation") +
      geom_label(aes(label=n))+
      theme_bw() 
    
  })
   
  output$pie_len=renderPlot({
    #create data frame
    len <- data.frame("category" = c("Lengendary","Normal"),
                       "Percentage" = c(round(mean(data$is_legendary),3),(1-round(mean(data$is_legendary),3))))
    
    #create pie chart
    ggplot(len, aes(x="", y=Percentage, fill=category)) +
      geom_bar(stat="identity", width=1) +
      
      geom_text(aes(label = paste0(Percentage*100, "%")), position = position_stack(vjust=0.5))+
      coord_polar("y", start=0) +
      theme_void() 
  })
  
  output$pie_male=renderPlot({
    #create data frame
    len <- data.frame("category" = c("Male","Female"),
                      "Percentage" = c(round(mean(data$percentage_male,na.rm=TRUE),3),(100-round(mean(data$percentage_male,na.rm=TRUE),3))))
    
    #create pie chart
    
    ggplot(len, aes(x="", y=Percentage, fill=category)) +
      geom_bar(stat="identity", width=1) +
      geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust=0.5))+
      coord_polar("y", start=0) +
      scale_color_manual(values =c("red","gray"))+
      theme_void() 
  })


  
  output$densityplot <- renderPlot({
    den <- ggplot(data,aes_string(input$attribute2))+
      geom_density(col="white",fill="red", alpha=0.3) + 
      ggtitle(paste0("Density Plot of ",input$attribute2))+
      theme_classic()
    return(den)
  })
  
  
  output$table1=renderDataTable({
    data <- data %>% 
      select(name,input$attribute2)
    
    data = data[order(data[input$attribute2], decreasing = TRUE),]
    data = data %>%
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
                     width = 500,
                     height = 450,
                     callback = JS(
                       "$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                     options = list(
                       dom = "t",
                       #ordering = TRUE,
                       paging = FALSE,
                       searching = FALSE,
                       headerCallback = JS(headerCallbackRemoveHeaderFooter)
                     )) 
           %>% formatStyle( colnames(data), backgroundColor = alpha("red",0.3)))
  })

  
 output$rank=renderPlot({
    data %>%
      select(name, base_total) %>%
      arrange(desc(base_total)) %>%
      slice(1:5) %>%
      ggplot(aes(x=reorder(name, base_total), y=base_total)) +
      geom_bar(stat="identity", aes(fill=base_total), colour="black", show.legend=FALSE) +
      scale_fill_gradient(low=alpha("red",0.5), high=alpha("red",0.6)) +
      coord_flip() +
      geom_label(aes(label=base_total))+
      labs(x="Name") +
      theme_bw() 
  })
  
  ################################################fifth page  
  
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
  
  battleresult = eventReactive(input$battlebutton,{if(runif(1)>0.5){1}else{0}})
  battlemsg = eventReactive(input$battlebutton, {
    if (battleresult()){
      paste(input$Pokemon1,
            " has won the battle! Try again with different pokemons!")}
    else{paste(input$Pokemon2,
               " has won the battle! Try again with different pokemons!")}
  })
  
  output$BattleStatus = renderText(battlemsg())
  Winnerimg = eventReactive(input$battlebutton, {
    if (battleresult()){number = data$pokedex_number[which(data$name == da1())]}
    else{number = data$pokedex_number[which(data$name == da2())]}
    tags$div(img(
      src = paste0(
        'https://assets.pokemon.com/assets/cms2/img/pokedex/full/',
        number,
        '.png'
      ),
      width = "300",
      height = "300"
    ), style = "right")
  })
  Crownimg =  eventReactive(input$battlebutton, {
    tags$div(img(
      src = 
        "crown.webp",
      width = "250",
      height = "250"
    ))
  })
  output$Winner = renderUI(Winnerimg())
  output$Crown = renderUI(Crownimg())
  output$Crown2 = renderUI(Crownimg())
  
  
}

shinyApp(ui = ui, server = server)







