#install.packages(c("shiny","DBI","RPostgres","RSQLite"))
library(shiny)
library(DBI)
library(RPostgres)
library(RSQLite)
library(shinyWidgets)
library(DT)
# Define UI
ui <- fluidPage(
  
  
  
  column(12),
  column(2,tags$img(align="left",style="margin-left:auto;margin-right:auto;padding-top:40px",
                    src="Blake.JPG",height='200px',width="170px")), 
  
  
  
  column(10,style="",
         tags$div(id="introduction_paragraph",style="padding-top:5px;",tags$p(align="center",
                                                                              style="font-weight:bold;font-size:150%;color:darkred",
                                                                              "Storytime with Blake"))
  ),
  
  sidebarLayout(
    
    sidebarPanel(style = "background-color: #E1AAAA",
                 
                 selectInput("titleID","Choose Your Story!",choices=c("Madeline","The Snowy Day","The Rainbow Fish", "Matilda", "The Story of Ferdinand",
                                                                      "Daddy and Me and the Rhyme to Be", "Goodnight Moon", "The Cat in the Hat", "If You Give a Mouse a Cookie"),multiple=FALSE),
                 actionButton("exploratory.go","Search",icon=icon("play-circle")),
                 width=8
    ),
    
    # Show the output as a table
    mainPanel(
      dataTableOutput("tableoutput"), width = 12
    )
  ),
  tags$style(type = 'text/css',".shiny-title-panel{color: #E1AAAA;}"),
  
  column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="margin-top:0px;margin-bottom:15px;border-top: 0.5px solid #E1AAAA;")),
  
  column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",
                                                         style="font-weight:normal;font-size:95%;color:black","Version 0.1; Published: 05/01/2024"))),
  
  column(12, tags$div(
    tags$footer(style="margin-center",tags$footer(align="center",color="white",
                                                  style="font-weight:normal;font-size:95%;color:black",
                                                  "Storytime with Blake"
    )
    )))
)
server <- function(input, output) {
  observeEvent(input$exploratory.go,
               {
                 output$tableoutput <-renderDataTable({
                   
                   req(input$titleID)
                   # SQLite
                   #sqlite_conn <- dbConnect(RSQLite::SQLite(), dbname ='blake.db')
                   #filename <- "C:/Users/oepie/Desktop/Database Project/Shiny App 6354/blake.db"
                   filename<-"blake.db"
                   sqlite.driver <- dbDriver("SQLite")
                   
                   sqlite_conn<- dbConnect(sqlite.driver,
                                           dbname = filename)
                   
                   dbListTables(sqlite_conn)
                   
                   # SQL command
                   sqlite_sql <- paste("SELECT 
                     a.filestore_link,
                     b.author_name,
                     b.book_name,
                     B.FILESTORE_LINK AS VIDEO_LINK
                   FROM 
                     IMAGE a 
                   INNER JOIN
                     (SELECT 
                        BOOK.ISBN,
                        BOOK.AUTHOR_NAME,
                        BOOK.BOOK_NAME,
                        AUDIO_VIDEO.FILESTORE_LINK
                      FROM BOOK
                      LEFT JOIN
                        CHAPTER_BOOK
                        ON
                        BOOK.ISBN = CHAPTER_BOOK.ISBN
                      LEFT JOIN
                        AUDIO_VIDEO
                        ON
                        CHAPTER_BOOK.ISBN = AUDIO_VIDEO.ISBN
                        AND
                        CHAPTER_BOOK.CHAPTER_NO = AUDIO_VIDEO.CHAPTER_NO
                     ) AS B
                     ON
                     a.ISBN = B.ISBN
                  WHERE B.BOOK_NAME=='",input$titleID,"';",sep="")
                   
                   conn <- sqlite_conn
                   str_sql <- sqlite_sql
                   
                   on.exit(dbDisconnect(conn), add = TRUE)
                   table_df <- dbGetQuery(conn, paste0(str_sql, " LIMIT ", 9, ";"))
                   table_df$COVERART <- c(paste0('<img src="', table_df$COVERART, '"></img>'))
                   table_df$'Video URL'<-paste0("<a href='",table_df$VIDEO_,"'>'",input$titleID,"</a>")
                   table_df$Video <- c(paste0('<video controls><source src="', table_df$VIDEO, '" type="video/mp4"></video>'))
                   # table_df$VIDEO <- c(paste0('<iframe width="560" height="315" src="', "https://www.youtube.com/watch?v=bQG7tKMSJFc", '" frameborder="0" allow="accelerometer;
                   #autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                   
                   # Rename columns
                   colnames(table_df)[colnames(table_df) == "FILESTORE_LINK"] <- "Cover Art"
                   colnames(table_df)[colnames(table_df) == "AUTHOR_NAME"] <- "Author Name"
                   colnames(table_df)[colnames(table_df) == "BOOK_NAME"] <- "Book Name"
                   
                   # Select required columns
                   table_df <- table_df[,c('Cover Art', 'Author Name', 'Book Name', 'Video', 'Video URL')]
                   
                   if(input$titleID=="The Snowy Day"){
                     table_df$`Cover Art`<-c(paste0('<img src="', "SnowyDay_coverart.jpg", '" height="175" width="200"></img>'))
                     table_df$`Video URL` <- 'https://www.youtube.com/watch_popup?v=FmZCQfeWjeQ'
                     table_df$Video <-               HTML(paste0(
                       '<html>
                            <body>
                              <iframe id="existing-iframe"
                                  width="100%" height="360"
                                  src="https://www.youtube.com/watch_popup?v=FmZCQfeWjeQ" ###This URL needs to change dynamically based on which link the user clicks in output$table
                                  frameborder="0"
                              ></iframe>
                    
                              <script type="text/javascript">
                                var tag = document.createElement(\'script\');
                                tag.src = \'https://www.youtube.com/iframe_api\';
                                var firstScriptTag = document.getElementsByTagName(\'script\')[0];
                                firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
                    
                                var player;
                                function onYouTubeIframeAPIReady() {
                                  player = new YT.Player(\'existing-iframe\');
                                }
                              </script>
                            </body>
                          </html>'
                     ))
                   }else{
                     if(input$titleID=="The Rainbow Fish"){
                       table_df$`Cover Art`<-c(paste0('<img src="', "RainbowFish_coverart.jpg", '" height="200" width="150"></img>'))
                     }else{
                       if(input$titleID=="Madeline"){
                         table_df$`Cover Art`<-c(paste0('<img src="',  "Madeline_coverart.jpg", '" height="200" width="150"></img>')) 
                       }else{
                         if(input$titleID=="The Story of Ferdinand"){
                           table_df$`Cover Art`<-c(paste0('<img src="',  "Ferdinand_coverart.jpg", '" height="200" width="200"></img>')) 
                         }else{
                           if(input$titleID=="If You Give a Mouse a Cookie"){
                             table_df$`Cover Art`<-c(paste0('<img src="',  "MouseCookie_coverart.jpg", '" height="200" width="175"></img>')) 
                           }else{
                             if(input$titleID=="Goodnight Moon"){
                               table_df$`Cover Art`<-c(paste0('<img src="',  "GoodnightMoon_coverart.jpg", '" height="200" width="225"></img>')) 
                             }else{
                               if(input$titleID=="Matilda"){
                                 table_df$`Cover Art`<-c(paste0('<img src="',  "Matilda_coverart.jpg", '" height="200" width="150"></img>')) 
                               }else{
                                 if(input$titleID=="Daddy and Me and the Rhyme to Be"){
                                   table_df$`Cover Art`<-c(paste0('<img src="',  "DaddyMe_coverart.jpg", '" height="200" width="150"></img>')) 
                                 }else{
                                   if(input$titleID=="The Cat in the Hat"){
                                     table_df$`Cover Art`<-c(paste0('<img src="',  "CatHat_coverart.jpg", '" height="200" width="150"></img>')) 
                                   }
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                   
                   return(table_df)
                 }, escape = FALSE)
                   
               })
}

# Run the application 
shinyApp(ui = ui, server = server)
