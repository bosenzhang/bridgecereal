library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(data.table)
library(dplyr)
library(shinyBS)
library(shinythemes)

library(reshape2) # New one!


administrator_path <- '/home/xianranli/';

working_folder <- paste(administrator_path,"Deven_lab",'/V2/',sep='');



d1 <- read.table(paste(working_folder,"fst_raw_results_graingenes.txt",sep=''), header=T, sep='\t')

d_rsb <- read.table(paste(working_folder,"rsb_raw_results_graingenes.txt",sep=''), header=T, sep='\t')

d_xpe <- read.table(paste(working_folder,"xpe_raw_results_graingenes.txt",sep=''), header=T, sep='\t')

d_pic <- read.table(paste(working_folder,"pic_raw_results_graingenes.txt",sep=''), header=T, sep='\t')

html_ <- 'https://wheat.pw.usda.gov/cgi-bin/GG3/report.cgi?class=marker&name='

######################################################


all_chr <- unique(d1[,2])

colname_work <- colnames(d1)  # with bc cells removed

colname_work_ <- colname_work[-c(1:3)]

d1 <- d1[ ,colname_work]


###################


ui <-  function(request){

    tagList(
        
         fluidPage(theme = shinytheme("readable")),

        # column(12,offset=1,align='center', h2("WebApp For Deven's Lab",style="text-align:center") ),

   #sidebarLayout(

   #sidebarPanel(

   #), # sidebarPanel

 navlistPanel(


     widths = c(3, 9),

    "Need a name here ",

 tabPanel("Main",
     
     column(10,offset=0,align='center', h2("WebApp For Deven's Lab",style="text-align:center") ),
     column(10,align='center', h3("This is your main panel")),
     column(10,align='center', h3("Your app's name; What the data is; How to use the data; Citations ...")),
     column(10,align='center', h3("Some words here ...")),




        ), # # tabPanel


########### Panel 1
###########

 tabPanel("Panel 1 (One program, one chromosome)",

    #  h3("This is plot panel 1"),

  mainPanel(

   fluidRow(

############
    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),
  
    column(3,offset=3,align='center',
      pickerInput(
        inputId = "PickCol", 
        label = "Pick a program (Please select one!) :", 
        choices = colname_work_ ,
        selected = c('S.MSU'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 50,
       'selected-text-format' = "count > 1"
                      ), 
        multiple = FALSE,
                )
         ),
    
    column(3,offset=3,align='center',
      pickerInput(
        inputId = "PickChr", 
        label = "Pick a chromosome (Please select one!) :", 
        choices = all_chr ,
        selected = c('1A'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 21,
       'selected-text-format' = "count > 1"
                      ), 
        multiple = FALSE,
                )
         ),
    
    column(12, offset=3,align='center',h4("Select an area to interact with this figure") ),
    column(12, offset=3,align="center", h3("")),

    column(12, offset=3,align='center',h4("Frequency color: Red (>= 0.8); Blue (<= 0.2); Grey (0.2-0.8)") ),
   # column(12,actionButton("submit", label = "Submit",class = "btn-warning")),
    column(12, offset=3,align="center", plotOutput("plot",click = NULL,dblclick = NULL, hover = NULL, brush = "plot_brush", width = "100%",height = 'auto')),

    column(12, offset=3,align="center", h3("")),

 #  column(12, offset=3,align="center",verbatimTextOutput("info") ),

    column(12, offset=3,align="center", h3("")),

    column(12,offset=3,align='center',
      uiOutput("PickRange"),
         ),

    column(12, offset=3,align="center", plotOutput("plot1",click = NULL,dblclick = NULL, hover = NULL, brush = NULL, width = "100%",height = 'auto')),

    column(12, offset=3,align="center", h3("")),

 #  column(12, offset=3,align="center",verbatimTextOutput("info1") ),

    column(12, offset=3,align="center", h3("")),

    column(12, offset = 3,align="center", DT::dataTableOutput("table1"),style='padding-top:5px; padding-bottom:5px'),


) # fluidRow

   ) # mainPanel

    ), # tabPanel "First",

########### Panel 1
###########

########### Panel 2
###########

    tabPanel("Panel 2 (Average and sd on each chromosome)",

    #  h3("This is Plot panel 1"),
      
      mainPanel(

    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),

   fluidRow(

     column(12,offset=1,align='center',
      pickerInput(
        inputId = "PickCol_", 
        label = "All programs selected: (Please select at least three programs!)", 
        choices = colname_work_ ,
        selected = colname_work_, ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 50,
       'selected-text-format' = "count > 2"
                      ), 
        multiple = TRUE,
                )
         ),
    
    column(12, offset=3,align="center", h3("")),

    column(3,offset=3,align='center',
      pickerInput(
        inputId = "PickChr_", 
        label = "Pick a chromosome (Please select one!) :", 
        choices = all_chr ,
        selected = c('1A'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 21,
       'selected-text-format' = "count > 1"
                      ), 
        multiple = FALSE,
                )
         ),

      column(3,offset=3,align='center',
      pickerInput(
        inputId = "Pick_mean_sd", 
        label = "Plot Mean or Sd (Please select one!) :", 
        choices = c('Mean','Sd') ,
        selected = c('Mean'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 21,
       'selected-text-format' = "count > 1"
                      ), 
        multiple = FALSE,
                )
         ),

     column(12, offset=3,align='center',h4("Color for the figure: Black (Mean); Orange (Sd)" ) ),
     column(12, offset=3,align="center", plotOutput("plot2",click = NULL,dblclick = NULL, hover = NULL, brush = NULL, width = "100%",height = 'auto')),
     column(12, offset=3,align="center", h3("")),
     column(12, offset=3,align="center", h3("")),

     column(12, offset=3,align='center',h4("Color for the table: Red (>= 0.4); Blue (<= 0.1); Grey (0.1-0.4)") ),
     column(12, offset=3,align="center", h3("")),
     column(12, offset = 3,align="center", DT::dataTableOutput("table3"),style='padding-top:5px; padding-bottom:5px'),


  )  # fluidRow another Panel
   ) # mainPanel another Panel


), # # another panel,

########### Panel 2
###########

########### Panel 3
###########

   tabPanel("Panel 3 (Heatmap on each chromosome)",
      
      mainPanel(

    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),
    column(12, offset=3,align="center", h3("")),

   fluidRow(

    column(6,
      pickerInput(
        inputId = "PickPro", 
        label = "Pick programs: (Please select at least two programs!)", 
        choices = colname_work_ ,
        selected = c('both.all2','both.CA'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 50,
       'selected-text-format' = "count > 2"
                      ), 
        multiple = TRUE,
                )
         ),
    
    column(6,
      pickerInput(
        inputId = "PickChr__", 
        label = "Pick a chromosome (Please select one!) :", 
        choices = all_chr ,
        selected = c('1A'), ## by default
        options = list(
        'actions-box' = TRUE, 
        size = 21,
       'selected-text-format' = "count > 1"
                      ), 
        multiple = FALSE,
                )
         ),
     
     column(12, offset=3,align='center',h4("Select an area to interact with this figure") ),
     column(12, offset=3,align="center", h3("")),
     column(12, offset=3,align='center',h4("Frequency color: Red (>= 0.4); Blue (<= 0.1); Grey (0.1-0.4)") ),

     column(12, offset=3,align="center", plotOutput("plot4",click = NULL,dblclick = NULL, hover = NULL, brush = "plot4_brush", width = "100%",height = 'auto')),

     column(12, offset=3,align="center", h3("")),

     column(12,offset=3,align='center',
      uiOutput("PickRange_"),
         ),

      column(12, offset=3,align="center", plotOutput("plot5",click = NULL,dblclick = NULL, hover = NULL, brush = NULL, width = "100%",height = 'auto')),

      column(12, offset=3,align="center", h3("")),

      column(12, offset = 3,align="center", DT::dataTableOutput("table4"),style='padding-top:5px; padding-bottom:5px'),


  )  # fluidRow another Panel
   ) # mainPanel another Panel

), # # another panel,

########### Panel 3
###########



) # navlistPanel

#) # sidebarLayout



      ) # For tagList
    } # For ui function of page_1
    

# To add server function part for page1

server <- function(input, output, session){



################################################ For Plot panel 1
observeEvent(c(input$PickCol, input$PickChr) ,{


d2 <- d1[which(d1[,2]==input$PickChr), ]  # chromosome 1A

d3 <- melt(d2, id = c("SNPid","CHR", "POS") )

d4 <- d3

colnames(d4)<-c("SNPid","CHR", "Pos","Pro","Fre")

d5 <- d4[which(d4$Pro==input$PickCol), ]

coor_start <- d5$Pos[1]
coor_end <- d5$Pos[nrow(d5)]
x_lim <- c(coor_start,coor_end)


table_1A <- d2[ ,c(1,2,3, which(colnames(d2)==input$PickCol)) ]




output$plot <- renderPlot({

#plot(d5$Pos,d5$Fre,xlim = x_lim, ylim = c(-0.1, 0.5), xlab = paste('Chr',input$PickChr,sep=''), ylab = paste('Frequency',sep='') )

plot(-100, -100, xlim = x_lim , ylim = c(-0.2, 0.5), xlab = paste('Chr',input$PickChr,sep=''), ylab = paste('Frequency',sep=''),yaxt="n");

rect(d5$Pos[1], -0.02 ,d5$Pos[nrow(d5)], -0.1,border='grey',col='green')
axis(2, at=c(0, 0.1,0.2,0.3,0.4,0.5), labels=c(0, 0.1,0.2,0.3,0.4,0.5) ,tick=TRUE   )

mtext(paste('Plot of ',input$PickCol,sep=''),side=3,line=0.25,cex=1.5)


for( index in 1:nrow(d5) ) {

  lines(c(d5$Pos[index],d5$Pos[index]),y=c(-0.1,-0.02))
  points(d5$Pos[index], d5$Fre[index],pch=16,col= ifelse(as.numeric(d5$Fre[index]) >= 0.8, "red", ifelse(as.numeric(d5$Fre[index]) <= 0.2,"blue", 'grey')) )
  

}


  }, height = function() {300})


#output$info <- renderText({
#    req(input$plot_brush$xmin, input$plot_brush$xmax)
#    paste('left = ', round(as.numeric(input$plot_brush$xmin),0),' ','right = ',round(as.numeric(input$plot_brush$xmax),0),sep='')   
#    })


output$PickRange <- renderUI({

       req(input$plot_brush$xmin, input$plot_brush$xmax)
       sliderInput("Range", "Selected genomeic range: ", min = round(as.numeric(input$plot_brush$xmin),0), max = round(as.numeric(input$plot_brush$xmax),0), 
        value = c( round(as.numeric(input$plot_brush$xmin),0)+100 ,round(as.numeric(input$plot_brush$xmax),0) -100 ), width='150%' )

    })


output$plot1 <- renderPlot({
    
    req(input$plot_brush$xmin, input$plot_brush$xmax)

   # x_min <- input$plot_brush$xmin
   # x_max <- input$plot_brush$xmax

    req(input$Range[1], input$Range[2])

     x_min <- input$Range[1]
     x_max <- input$Range[2]


    for(index in 1:length(d5$Pos)){
  
      min_test <- d5$Pos[index]-x_min
  
      if(min_test>=0){
       min_index <- index    
       break
       }

     }

    for(index in 1:length(d5$Pos)){
  
      max_test <- d5$Pos[index]-x_max
  
      if(max_test>=0){
      max_index <- index-1    
      break
       }
      
     }

    
  # if(!exists('max_index') ){ max_index<-nrow(d5) }

    if(!exists('max_index') ){
    d6 <- d5[c(min_index:nrow(d5)), ]
    } else if(exists('max_index')){
    d6 <- d5[c(min_index:max_index), ]
    }

    x_lim_ <- c(x_min,x_max)

    #print(x_lim_[2])
    #print(nrow(d6))
    #print(d6$Pos[nrow(d6)])

    #plot(d6$Pos,d6$Fre,xlim = x_lim_, ylim = c(-0.1, 0.5), xlab = paste('Chr',input$PickChr,sep=''), ylab = paste('Frequency',sep='') )

     plot(-100, -100, xlim = x_lim_ , ylim = c(-0.2, 0.5), xlab = paste('Chr',input$PickChr,sep=''), ylab = paste('Frequency',sep=''),yaxt="n");

     #rect(d6$Pos[1], -0.02 ,d6$Pos[nrow(d6)], -0.1,border='grey',col='green')

     rect(x_lim_[1], -0.02 ,x_lim_[2], -0.1,border='grey',col='green')
     axis(2, at=c(0, 0.1,0.2,0.3,0.4,0.5), labels=c(0, 0.1,0.2,0.3,0.4,0.5) ,tick=TRUE   )

     mtext(paste('Zoom in on selected segment',sep=''),side=3,line=0.25,cex=1.5)

     for( index in 1:nrow(d6) ) {

     lines(c(d6$Pos[index],d6$Pos[index]),y=c(-0.1,-0.02))
     points(d6$Pos[index], d6$Fre[index],pch=16,col= ifelse(as.numeric(d6$Fre[index]) >= 0.8, "red", ifelse(as.numeric(d6$Fre[index]) <= 0.2,"blue",  'grey')) )
     #col= ifelse(as.numeric(d6$Fre[index]) >= 0.4, "red", ifelse(as.numeric(d6$Fre[index]) <= 0.1,"blue", 'grey'))

      }

    }, height = function() {300})



output$table1 <-DT::renderDataTable({

    req(input$plot_brush$xmin, input$plot_brush$xmax)

   # x_min <- input$plot_brush$xmin
   # x_max <- input$plot_brush$xmax
   
    req(input$Range[1], input$Range[2])

     x_min <- input$Range[1]
     x_max <- input$Range[2]

    

    for(index in 1:length(d5$Pos)){
  
      min_test <- d5$Pos[index]-x_min
  
      if(min_test>=0){
       min_index <- index    
       break
       }

     }

    for(index in 1:length(d5$Pos)){
  
      max_test <- d5$Pos[index]-x_max
  
      if(max_test>=0){
      max_index <- index-1    
      break
       }
      
     }

   # if(!exists('max_index') ){ max_index<-nrow(d5) }

    if(!exists('max_index') ){
    d6 <- d5[c(min_index:nrow(d5)), ]
    } else if(exists('max_index')){
    d6 <- d5[c(min_index:max_index), ]
    }

  # d6 <- d5[c(min_index:max_index), ]

    table_1A <- table_1A[which(table_1A[ ,3] %in% d6$Pos), ]

    #print(table_1A)

     if((table_1A[1,3] < x_min) & (table_1A[1,3] > x_max) ){   ###???

        table_flag <- 0

     } else {

        table_flag <- 1
     }
     
     if(table_flag ==1){


     d_rsb_ <- d_rsb[which(d_rsb$SNPid %in% table_1A$SNPid), input$PickCol]

     d_xpe_ <- d_xpe[which(d_xpe$SNPid %in% table_1A$SNPid), input$PickCol]

     d_pic_ <- d_pic[which(d_pic$SNPid %in% table_1A$SNPid), input$PickCol]



     table_1A$rsb <- d_rsb_

     table_1A$xpe <- d_xpe_

     table_1A$pic <- d_pic_

     colnames(table_1A) <- c('SNPid','CHR','POS', paste(input$PickCol, '(FST)', sep=' '), paste(input$PickCol, '(RSB)', sep=' '), 

                                paste(input$PickCol, '(XPE)', sep=' '), paste(input$PickCol, '(PIC)', sep=' ') )

     table_1A_ <- round( table_1A[ ,4:ncol(table_1A)], 3)

     table_1A[ ,4:ncol(table_1A)] <- table_1A_

     ### marker link

    html_working <- as.data.frame( paste(html_, table_1A$SNPid, sep='') )

    colnames(html_working) <- 'GrainGenes_link'

    Marker_names <- table_1A$SNPid

    url <- html_working$GrainGenes_link

    refs <- paste0("<a href='",  url, "' target='_blank'>" , Marker_names , "</a>")


    table_1A$SNPid <- data.frame(refs)

    ### marker link


     datatable(table_1A, escape = TRUE, caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table 1: ', htmltools::em('Selected items')
        ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
           class="cell-border stripe",
             options = list(dom = "Blfrtip",columnDefs = 
                           list(list(className = 'dt-center', 
                                     targets = "_all")),
                buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=50, autoWidth = TRUE,
                            
                            searchHighlight = TRUE, filter = "top")) %>% 

                            formatStyle(columns=4, target = c("cell"),  background =  styleInterval(c(0, 1), c('lightblue', 'lightgray', 'coral') ) ) %>%

                            formatStyle(columns=7, target = c("cell"),  background =  styleInterval(c(0, 0.5), c('lightblue', 'lightgray', 'coral') ) )                            

               }

  }) # DT::renderDataTable 


})   # observeEvent c(input$PickCol, input$PickChr)


################################################ For Plot panel 1

################################################ For plot panel 2 

observeEvent(c(input$PickCol_, input$PickChr_, input$Pick_mean_sd) ,{

req(input$PickCol_, input$PickChr_, input$Pick_mean_sd)


if(length(input$PickCol_)>=3){


d2 <- d1[which(d1[,2] == input$PickChr_), ]  # chromosome 1A



table_2A <- d2[ ,c(1,2,3, which(colnames(d2) %in% input$PickCol_) ) ]


table_2A_ <- d2[  ,which(colnames(d2) %in% input$PickCol_)]

table_3A <- as.data.table( round(rowMeans(table_2A_,na.rm=TRUE),3) )

table_3A_sd <- as.data.table( round( apply(table_2A_, 1, sd, na.rm=TRUE), 3) )

colnames(table_3A_sd)<-'Sd'

colnames(table_3A)<-'Mean'

table_3A <- cbind(table_2A, table_3A, table_3A_sd)


#d3_ <- melt(table_3A, id.vars = "CS.seq.start" )

d3_ <- melt(table_3A, id = c("SNPid","CHR", "POS" ) )


#d4_ <-d3_[which(d3_[,"variable"] %in% unique(d3_[,"variable"])[-c(1:7)]), ]

d4_ <- d3_[ ,3:5]

colnames(d4_)<-c('Pos','Pro','Fre')


#d5_<-d4_[which(d4_$Pro=='Mean'), ]  # input$Pick_mean_sd
d5_<-d4_[which(d4_$Pro==input$Pick_mean_sd), ]


if( length(which(is.na(d5_$Fre))) >=1 ){
d5_ <- d5_[-which(is.na(d5_$Fre)), ]
}

coor_start_mean <- d5_$Pos[1]
coor_end_mean <- d5_$Pos[nrow(d5_)]
x_lim_mean <- c(coor_start_mean,coor_end_mean)


output$plot2 <- renderPlot({

    req(input$PickCol_, input$PickChr_, input$Pick_mean_sd)

    if(length(input$PickCol_)>=3){

        plot(-100, -100, xlim = x_lim_mean , ylim = c(-0.2, 0.5), xlab = paste('Chr',input$PickChr_,sep=''), ylab = paste('Frequency',sep=''),yaxt="n");

        rect(d5_$Pos[1], -0.02 ,d5_$Pos[nrow(d5_)], -0.1,border='grey',col='green')
        axis(2, at=c(0, 0.1,0.2,0.3,0.4,0.5), labels=c(0, 0.1,0.2,0.3,0.4,0.5) ,tick=TRUE   )


        if(input$Pick_mean_sd=='Mean'){

        mtext(paste('Mean of ',length(input$PickCol_),' programs',sep=''),side=3,line=0.25,cex=1.5)
        
        } else if (input$Pick_mean_sd=='Sd'){

        mtext(paste('Standard Deviation of ',length(input$PickCol_),' programs',sep=''),side=3,line=0.25,cex=1.5)

        }



    for( index in 1:nrow(d5_) ) {

        lines(c(d5_$Pos[index],d5_$Pos[index]),y=c(-0.1,-0.02))
 
        if(input$Pick_mean_sd=='Mean'){

        points(d5_$Pos[index], d5_$Fre[index],pch=16,col='black')
  
        } else if(input$Pick_mean_sd=='Sd'){
  
        points(d5_$Pos[index], d5_$Fre[index],pch=16,col='orange')

        }

    }

    }

}, height = function() {300})



output$table3 <-DT::renderDataTable({

req(input$PickCol_, input$PickChr_, input$Pick_mean_sd)

if(length(input$PickCol_)>=3){


d_rsb_ <- d_rsb[which(d_rsb$SNPid %in% table_3A$SNPid), input$PickCol_]

d_rsb_mean <- as.data.table( round(rowMeans(d_rsb_,na.rm=TRUE),3) )


d_rsb_sd <- as.data.table( round( apply(d_rsb_, 1, sd, na.rm=TRUE), 3) )

colnames(d_rsb_mean)<-'RSB (mean)'

colnames(d_rsb_sd)<-'RSB (sd)'


d_xpe_ <- d_xpe[which(d_xpe$SNPid %in% table_3A$SNPid), input$PickCol_]

d_xpe_mean <- as.data.table( round(rowMeans(d_xpe_,na.rm=TRUE),3) )

d_xpe_sd <- as.data.table( round( apply(d_xpe_, 1, sd, na.rm=TRUE), 3) )

colnames(d_xpe_mean)<-'XPE (mean)'

colnames(d_xpe_sd)<-'XPE (sd)'


d_pic_ <- d_pic[which(d_pic$SNPid %in% table_3A$SNPid), input$PickCol_]

d_pic_mean <- as.data.table( round(rowMeans(d_pic_,na.rm=TRUE),3) )

d_pic_sd <- as.data.table( round( apply(d_pic_, 1, sd, na.rm=TRUE), 3) )

colnames(d_pic_mean)<-'PIC (mean)'

colnames(d_pic_sd)<-'PIC (sd)'


d_fst_mean_sd <- table_3A[ ,c('SNPid', 'CHR','POS','Mean','Sd')]

colnames(d_fst_mean_sd) <- c(c('SNPid', 'CHR','POS','FST (mean)','FST (sd)'))

table_3A_combined <- cbind(d_fst_mean_sd, d_rsb_mean, d_rsb_sd, d_xpe_mean, d_xpe_sd, d_pic_mean, d_pic_sd)


     ### marker link

    html_working <- as.data.frame( paste(html_, table_3A_combined$SNPid, sep='') )

    colnames(html_working) <- 'GrainGenes_link'

    Marker_names <- table_3A_combined$SNPid

    url <- html_working$GrainGenes_link

    refs <- paste0("<a href='",  url, "' target='_blank'>" , Marker_names , "</a>")


    table_3A_combined$SNPid <- data.frame(refs)

    ### marker link




 datatable(table_3A_combined, escape = TRUE, caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table 2: ', htmltools::em('Selected items ...')
        ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
           class="cell-border stripe",
             options = list(dom = "Blfrtip",columnDefs = 
                           list(list(className = 'dt-center', 
                                     targets = "_all")),
                buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=100, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top")) %>% 

                    formatStyle(columns=4:ncol(table_3A_combined), target = c("cell"), background =  styleInterval(c(0.1, 0.4), c('lightblue', 'lightgray', 'coral') ) )

    }

    })




}

}) # observeEvent input$PickCol_, input$PickChr_
################################################ For plot panel 2 


################################################ For plot panel 3

observeEvent(c(input$PickPro,input$PickChr__) ,{

req(input$PickPro,input$PickChr__)

if(length(input$PickPro)>=2){



d2 <- d1[which(d1[,2] == input$PickChr__), ]  #

#d2 <- d2[which(d2[,5]!=0), ]   # coordinate start != 0
#d2 <- d2[which(d2[,6]!=0), ]   # coordinate end != 0

#d2 <- d2[order(d2[,5]), ]  ##

#d3 <- melt(d2, id.vars = "CS.seq.start" )

d3 <- melt(d2, id = c("SNPid","CHR", "POS") )

d4 <- d3

#d4 <-d3[which(d3[,"variable"] %in% unique(d3[,"variable"])[-c(1:7)]), ]

#colnames(d4)<-c('Pos','Pro','Fre')

colnames(d4)<-c("SNPid","CHR", "Pos","Pro","Fre")


d5<-d4[ which(d4$Pro %in% input$PickPro), ]

if( length(which(is.na(d5$Fre))) >=1 ){
d5 <- d5[-which(is.na(d5$Fre)), ]
}


Pro_num <- length( input$PickPro )

x_lim <- range(d5$Pos)



output$plot4 <- renderPlot({

req(input$PickPro,input$PickChr__)

if(length(input$PickPro)>=2){

#coor_start <- d5$Pos[1]
#coor_end <- d5$Pos[nrow(d5)]
#x_lim <- c(coor_start,coor_end)

par(mar = c(1.2, 1.0, 0, 0) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .9, cex.lab = 1, family = "Times New Roman"); ## ??

plot(-100, -100, xlim = x_lim, ylim = c(-0.5, Pro_num), xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n");

count <- 1

for(index in input$PickPro ){

d5_plot <- d5[which(d5$Pro==index), ]

rect(d5_plot$Pos, Pro_num - count - 0.25 ,d5_plot$Pos[nrow(d5_plot)], Pro_num - count - 0.1, border='grey',col='white')

#axis(2, at=c(0+ Pro_num - count, 0.1+ Pro_num - count,0.2+ Pro_num - count,0.3+ Pro_num - count,0.4+ Pro_num - count,0.5+ Pro_num - count), labels=c(0, 0.1,0.2,0.3,0.4,0.5) ,tick=TRUE   )

text(x =x_lim[1], y =  Pro_num - count  , label = paste('',index,sep=''), cex=1.2, col='black',srt=0)

for( index_ in 1:nrow(d5_plot) ) {

lines(c(d5_plot$Pos[index_],d5_plot$Pos[index_]), y=c(Pro_num- count - 0.25, Pro_num- count - 0.1),col= ifelse(as.numeric(d5_plot$Fre[index_]) >= 0.4, "red", ifelse(as.numeric(d5_plot$Fre[index_]) <= 0.1,"blue", 'grey')), lwd=2 )

}

#legend(min(x_lim), Chr_num - count + 0.5, paste('chr',index ,sep=''), bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.0 )
count <- count+1

}

}


},height = function() {900} ) # plot 4



output$PickRange_ <- renderUI({

if(length(input$PickPro)>=2){

       req(input$plot4_brush$xmin, input$plot4_brush$xmax)

       sliderInput("Range_", "Selected genomeic range: ", min = round( as.numeric(input$plot4_brush$xmin), 0), max = round( as.numeric(input$plot4_brush$xmax), 0), 
        value = c( round(as.numeric(input$plot4_brush$xmin), 0)+100 ,round( as.numeric(input$plot4_brush$xmax), 0)-100), width='150%' )
}
    })





table_4A <- d2[ ,c(1,2,3, which(colnames(d2) %in% input$PickPro)) ]


#####
output$plot5 <- renderPlot({
    
    req(input$plot4_brush$xmin, input$plot4_brush$xmax)

    if(length(input$PickPro)>=2){

   # x_min <- input$plot_brush$xmin
   # x_max <- input$plot_brush$xmax

    req(input$Range_[1], input$Range_[2])

     x_min <- input$Range_[1]
     x_max <- input$Range_[2]


    for(index in 1:length(d5$Pos)){
  
      min_test <- d5$Pos[index]-x_min
  
      if(min_test>=0){
       min_index <- index    
       break
       }

     }

    for(index in 1:length(d5$Pos)){
  
      max_test <- d5$Pos[index]-x_max
  
      if(max_test>=0){
      max_index <- index-1    
      break
       }
      
     }

    
  # if(!exists('max_index') ){ max_index<-nrow(d5) }

    if(!exists('max_index') ){
    d6 <- d5[c(min_index:nrow(d5)), ]
    } else if(exists('max_index')){
    d6 <- d5[c(min_index:max_index), ]
    }

    x_lim_ <- c(x_min,x_max)

  par(mar = c(1.2, 1.0, 0, 0) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .9, cex.lab = 1, family = "Times New Roman"); ## ??

  plot(-100, -100, xlim = x_lim_, ylim = c(-0.5, Pro_num), xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n");

  count <- 1

  for(index in input$PickPro ){

  d5_plot <- d5[which(d5$Pro==index), ]

  rect(d5_plot$Pos, Pro_num - count - 0.25 ,d5_plot$Pos[nrow(d5_plot)], Pro_num - count - 0.1, border='grey',col='white')

  #axis(2, at=c(0+ Pro_num - count, 0.1+ Pro_num - count,0.2+ Pro_num - count,0.3+ Pro_num - count,0.4+ Pro_num - count,0.5+ Pro_num - count), labels=c(0, 0.1,0.2,0.3,0.4,0.5) ,tick=TRUE   )

  text(x =x_lim_[1], y =  Pro_num - count  , label = paste('',index,sep=''), cex=1.2, col='black',srt=0)

  for( index_ in 1:nrow(d5_plot) ) {

  lines(c(d5_plot$Pos[index_],d5_plot$Pos[index_]), y=c(Pro_num- count - 0.25, Pro_num- count - 0.1),col= ifelse(as.numeric(d5_plot$Fre[index_]) >= 0.4, "red", ifelse(as.numeric(d5_plot$Fre[index_]) <= 0.1,"blue", 'grey')), lwd=4 )

  }

  #legend(min(x_lim), Chr_num - count + 0.5, paste('chr',index ,sep=''), bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.0 )
  count <- count+1

  }

}

    }, height = function() {300})

#####



output$table4 <-DT::renderDataTable({

    req(input$plot4_brush$xmin, input$plot4_brush$xmax)

   # x_min <- input$plot_brush$xmin
   # x_max <- input$plot_brush$xmax
   
    req(input$Range_[1], input$Range_[2])

    if(length(input$PickPro)>=2){

     x_min <- input$Range_[1]
     x_max <- input$Range_[2]


    for(index in 1:length(d5$Pos)){
  
      min_test <- d5$Pos[index]-x_min
  
      if(min_test>=0){
       min_index <- index    
       break
       }

     }

    for(index in 1:length(d5$Pos)){
  
      max_test <- d5$Pos[index]-x_max
  
      if(max_test>=0){
      max_index <- index-1    
      break
       }
      
     }

   # if(!exists('max_index') ){ max_index<-nrow(d5) }

    if(!exists('max_index') ){
    d6 <- d5[c(min_index:nrow(d5)), ]
    } else if(exists('max_index')){
    d6 <- d5[c(min_index:max_index), ]
    }

  # d6 <- d5[c(min_index:max_index), ]


    table_4A <- table_4A[which(table_4A[ ,3] %in% d6$Pos), ]

    #print(head(table_4A))


     if((table_4A[1,3] < x_min) & (table_4A[1,3] > x_max) ){  ###???

        table_flag <- 0

     } else {

        table_flag <-1
     }

     
     if(table_flag ==1){


     d_rsb_ <- d_rsb[which(d_rsb$SNPid %in% table_4A$SNPid), input$PickPro]

     d_xpe_ <- d_xpe[which(d_xpe$SNPid %in% table_4A$SNPid), input$PickPro]

     d_pic_ <- d_pic[which(d_pic$SNPid %in% table_4A$SNPid), input$PickPro]


     table_4A <- cbind(table_4A, d_rsb_, d_xpe_, d_pic_)

     #print(head(table_4A))

     colnames(table_4A) <- c('SNPid','CHR','POS', paste(input$PickPro, rep('(FST)', length(input$PickPro) ), sep=' '), 

                                paste(input$PickPro, rep('(RSB)', length(input$PickPro)), sep=' '), 

                                paste(input$PickPro, rep('(XPE)', length(input$PickPro)), sep=' '), 

                                paste(input$PickPro, rep('(PIC)', length(input$PickPro)), sep=' ') )



     table_4A_ <- round( table_4A[ ,4:ncol(table_4A)], 3)

     table_4A[ ,4:ncol(table_4A)] <- table_4A_



     ### marker link

    html_working <- as.data.frame( paste(html_, table_4A$SNPid, sep='') )

    colnames(html_working) <- 'GrainGenes_link'

    Marker_names <- table_4A$SNPid

    url <- html_working$GrainGenes_link

    refs <- paste0("<a href='",  url, "' target='_blank'>" , Marker_names , "</a>")


    table_4A$SNPid <- data.frame(refs)

    ### marker link


     datatable(table_4A, escape = TRUE, caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table 4: ', htmltools::em('Selected items')
        ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
           class="cell-border stripe",
             options = list(dom = "Blfrtip",columnDefs = 
                           list(list(className = 'dt-center', 
                                     targets = "_all")),
                buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=50, autoWidth = TRUE,
                             # searchHighlight = TRUE, filter = "top")) %>% formatStyle(columns=7:ncol(table_4A), target = c("cell"), background = styleColorBar(table_4A[ , 7:ncol(table_4A)], 'red'))
                               searchHighlight = TRUE, filter = "top")) %>% 

                        formatStyle(columns=4:ncol(table_4A), target = c("cell"), background =  styleInterval(c(0.1, 0.4), c('lightblue', 'lightgray', 'coral') ) ) 
               
               }


  }


  }) # DT::renderDataTable 


} # length(input$PickPro)>=2


})  # observeEvent
################################################ For plot panel 3






    } # server function


shinyApp(ui = ui, server = server)
