library(shiny)
library(Biostrings)
library(shinyjs)
library(brochure)
library(shinyWidgets)
library(DT)
library(data.table)
library(dplyr)
library(seqinr)
library(dendextend)
library(shinyBS)
library(sortable)
library(rjson)
library(shinythemes)
library(mailtoR) # email to ...
#####
library(reshape2) # New one! #8/28/23

library(doParallel) # New one
library(parallel) # New one
library(foreach) # New one

library(magick) # New one

########################################################

BRIDGEcereal_main <- function(){

  page(

    href = "/",

    ui <-  function(request){

      tagList(
        
        #fluidPage(theme = shinytheme("readable")),

        h2("BRIDGEcereal: survey and graph indels variation in pan-genomes",style="text-align:center"),

        useShinyjs(),

        mainPanel(

          fluidRow(

          ) # fluidRow

        ) # mainPanel

      ) # For tagList

    }, # For ui function of page_0

    # To add server function part for page0

    server <- function(input, output, session){



    } # server function of Page_0

  ) # page for Page_0

} # Page_0 function


#shinyParallel::runApp( myapp(), max.sessions = Inf, users.per.session = 1 )
############ To combine pages together

brochureApp(
  
    BRIDGEcereal_main()


)

