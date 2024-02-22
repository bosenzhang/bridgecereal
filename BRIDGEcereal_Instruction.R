### 03/31/23
BRIDGEcereal_Instruction <- function(Stream_folder){

    CSS <- "
        p {
        line-height: 1.6; 
        font-family: Helvetica;
        text-align: justify;
        margin: 0;
        font-size: 22px;
        }

        .step1 {
        float: left;
        }

        .step2 {
        float: left;
        }

        .Panel1 {
        float: left;
        }

        .Panel2 {
        float: left;
        }

        .switch {
        float: left;
        }
    "

    page(

        href = "/Instruction",

        ui <-  function(request){

            tagList(
        
                #fluidPage(theme = shinytheme("readable")),

                h2("BRIDGEcereal instructions",style="text-align:center"),

                nav_links,

                useShinyjs(),

                #sidebarLayout(

                #sidebarPanel(

                #), # sidebarPanel

                #tags$script('
                #             var dimension = [0, 0];
                #             $(document).on("shiny:connected", function(e) {
                #             dimension[0] = window.innerWidth;
                #             dimension[1] = window.innerHeight;
                #             Shiny.onInputChange("dimension", dimension);
                #                             });
                #                             $(window).resize(function(e) {
                #                                 dimension[0] = window.innerWidth;
                #                                 dimension[1] = window.innerHeight;
                #                                 Shiny.onInputChange("dimension", dimension);
                #                             });
                #                         '),

                #tags$head(tags$style(type="text/css", "body { overflow-y: scroll; overflow-x: scroll;}") ),

                #tags$head(tags$style( "body { word-wrap: break-word; }" ) ),
                tags$head(
                    tags$style(HTML(CSS))
                ),

                mainPanel(

                    fluidRow(

                    #   column(12, offset=3,align="center", h3("BRIDGEcereal instructions:",style = "font-size: 32px; font-style: normal; font-weight: bold;")),

                    ############
                    # verbatimTextOutput("dimension_display"),

                    column(12, offset=2,align="left",
                        tags$li(
                        tags$a(href = "#Workflow", "BRIDGEcereal workflow") ,
                    ) ),

                    column(12, offset=2,align="left",
                        tags$li(
                        tags$a(href = "#Gene_CDS", "Use gene ID and CDS"),
                    ) ),

                    column(12, offset=3,align="left",
                        tags$li(
                        tags$a(href = "#Step1", "Step 1: Input gene ID", style = "font-size: 24px;"),
                    ) ),

                    column(12, offset=3,align="left",
                        tags$li(
                        tags$a(href = "#Step2", "Step 2: Adjust search boundries (optional) and submit", style = "font-size: 24px;"),
                    ) ),

                    column(12, offset=3,align="left",
                        tags$li(
                        tags$a(href = "#Step3", "Step 3: Clustering indels variation and cutting the tree", style = "font-size: 24px;"),
                    ) ),

                    column(12, offset=3,align="left",
                        tags$li(
                        tags$a(href = "#Step4", "Step 4: Plot selected haplotypes", style = "font-size: 24px;"),
                    ) ),

                    column(12, offset=3,align="left",
                        tags$li(
                        tags$a(href = "#Step5", "Step 5: To obtain final figure, trim the unwanted regions", style = "font-size: 24px;"),
                    ) ),

                    column(12, offset=2,align="left",
                        tags$li(
                        tags$a(href = "#Demo_mp4_1", "Demo video"),
                    ) ),

                    column(12, offset=2,align="left",
                        tags$li(
                        tags$a(href = "#Instructions", "BRIDGEcereal additional instructions"),
                    ) ),

                    # column(12, offset=2,align="left",
                    # tags$li(
                    # tags$a(href = "#CLIPS", "CLIPS algorithm demonstration"),
                    # ) ),

                    tags$style("li a {font-size:38px; font-weight:bold; list-style-type: circle;}"),
   
                    tags$head( tags$style(" li {font-size:0;}") ),

                    ############
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=3,align="center",   h3(
                        id = "Workflow",
                        p(
                            tags$em("BRIDGEcereal workflow"),
                        ),style = "font-size:24px; color:blue;"), 
                    ),

                    column(12, offset=3,align="left", h4("
                        Red outline denotes steps with parameter selected based on user’s discretion. 
                        The brackets indicate the steps involved in CHOICE and CLIPS algorithms.
                        ",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
                    column(12, offset=3,align="left", h4("
                        Hover your mouse over boxed text to view details. 
                        ",style = "font-size: 24px; font-style: normal; font-weight: bold;")),

                    #column(4, offset=3,align="left", plotOutput("plotWorkflow",click = NULL,dblclick = NULL,hover = "plotWorkflow_hover", width = "120%",height = 'auto')),
                    #column(4, offset=3,align="left", plotOutput("plotWorkflow",click = NULL,dblclick = NULL,
                    #        hover = "plotWorkflow_hover", width = "30vw",height = '80vh')),

                    #column(4,offset=1, align="center", textOutput("info_Workflow")), #03/07/23
                    #     tags$head(tags$style("#info_Workflow{
                    #                                 font-size: 32px;
                    #                                 font-style: normal;
                    #                                 font-weight: lighter;
                    #                                 }"
                    #                         )
                    #     ),

                    #uiOutput("LinkButton"),

                    #column(4, offset=3,align="left", plotOutput("plotWorkflow2",click = NULL,dblclick = NULL,hover = "plotWorkflow2_hover",brush = NULL, width = "120%",height = 'auto')),


                    column(9, offset=3,align="center", plotOutput("plotWorkflow2",click = NULL,dblclick = NULL,hover = "plotWorkflow2_hover",
                        brush = NULL, width = "120%",height = 'auto'),

                    ),

                    #verbatimTextOutput("info"),

                    column(9, offset=3,align="center", 

                        textOutput("info_Workflow2"),

                        tags$head(tags$style("#info_Workflow2{

                                 font-size: 28px;
                                 font-style: normal;
                                 font-weight: lighter;
                                 
                                 float: left;
                                 text-align: center;

                                 }"
                            ),
                        ),
                    ),

                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=3,align="center",   h3(
                        id = "Gene_CDS",
                        p(
                            tags$em("Use gene ID and CDS"),
                        ),style = "font-size:28px; color:blue;"), 
                    ),

                    #column(12, offset=3,align="center", h5("Input option 1 (default): Use gene ID and CDS" ,style = "font-size:18px; color:blue;")),

                    column(12,offset=2,align="left",   h3(
                        id = "Step1",
                        p(
                            tags$em("Step 1"),
                        ),style = "font-size:22px; color:blue;"), 
                    ),

                    column(width = 9,offset=3, align="center",img(width="812", height="487",class = "step1",src = paste(Stream_folder,"Steps_1.png",sep='') ),
                        h4(" 1). Input the gene model ID in the Gene name box, then click '(1) Check Gene ID' button to fill the boxes for reference and chromosome.
                        A hyperlink, connected to the corresponding crop’s genomic database, will be available (in blue color) for the query gene ID.
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),

                        h4("*** Update: Input transcript model ID in the transcript name box, such as TraesCS4A02G058900.1 or TraesCS4A02G058900.2 for IWGSC.",

                            style = "font-size: 22px; font-style: normal; font-weight: lighter; color:blue;"),

                        
                    ),

                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=2,align="left",   h3(
                        id = "Step2",
                        p(
                            tags$em("Step 2"),
                        ),style = "font-size:22px; color:blue;"), 
                    ),

                    column(width = 9,offset=3, align="center",img(width="754", height="496",class = "step2",src = paste(Stream_folder,"Steps_2.png",sep='') ),
                        h4("
                            2). Adjust the values for the Upstream and Downstream boxes to define the search boundaries, with a maximum input limit of 100kb. 
                            By default, the Upstream/Downstream input (in kb) is set at 10% of the target gene size. 
                            Choose the Genomes to be included in the analysis by selecting or deselecting them (all pan-genomes are selected by default). 
                            Click the '(2) Submit' button to initiate the process. 
                            The alignment with the selected genomes will be displayed in the top right panel (as Panel 1).
                            ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),

                        h4("
                            For more information about the two default parameters, please refer to the 'Two default CHOICE parameters explained' part in 'BRIDGEcereal additional instructions'.
                            ",style = "font-size: 22px; font-style: normal; font-weight: bold;"),
                    ),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),
                    column(6, offset=2,align="center", h3("Panel 1:")),

                    column(width = 9,offset=3, align="center",img(width="725", height="325",class = "Panel1",src = paste(Stream_folder,"Steps_3.png",sep='') ),
                        h4("
                            *** After Panel 1 is revealed, you can modify the sizes (in kb) of the Upstream and Downstream regions for the query gene. 
                            Simply input your preferred sizes in the corresponding Upstream and Downstream boxes, and BRIDGEcereal will automatically update the results and Panel 1 with the new information. 
                            There is no need to click the '(2) Submit' button again.***
                            ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),

                    ),

                    column(12, offset=3,align="center", h3("")),
                    column(12,offset=2,align="left",   h3(
                        id = "Step3",
                        p(
                            tags$em("Step 3"),
                        ),style = "font-size:22px; color:blue;"), 
                    ),

                    column(6, offset=2 ,align="center", h3("Panel 2:")),

                    column(width = 9,offset=3, align="center",img(width="805", height="514",class = "Panel2",src = paste(Stream_folder,"Steps_4.png",sep='') ),
                        h4("
                            3). To plot the phylogenetic tree clustering genomes based on shared indels, click the '(3) Tree' button, which will display Panel 2. 
                            To determine the number of haplotypes based on your tree-cut, click on the tree plot in the top right corner. 
                            *** If your tree-cut does not meet your satisfaction, you can click the '(3) Tree' button again and perform a new tree-cut at your preferred branches.***
                            ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),
                    ),

                    column(12, offset=3,align="center", h3("")),
                    column(12,offset=2,align="left",   h3(
                        id = "Step4",
                        p(
                            tags$em("Step 4"),
                        ),style = "font-size:22px; color:blue;"), 
                    ),

                    column(12, offset=2,align="center", h4("
                        4). Click the '(4) Plot selected haplotypes' button to plot the alignment among genomes representing each haplotype (Panel 3). 
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),
                    
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),
                    
                    column(12, offset=3,align="center", h3("Panel 3:")),
                    
                    column(6,offset=3, align="center", tags$img(width="1490", height="342", src=paste(Stream_folder,"Steps_5.png",sep=''))),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(width = 9,offset=3, align="center",img(width="751", height="209",class = "switch",src = paste(Stream_folder,"Steps_6.png",sep='') ),
                        h4("
                        *** You can rearrange the preferred plot order in the left bucket by dragging and dropping the haplotypes. 
                        Additionally, if you want to remove unwanted haplotypes from the plot, simply drag them from the 'Order of plot' bucket to an 'Empty bucket'. 
                        The Panel 3 will be automatically updated based on the new input.***
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),
                    ),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=2,align="left",   h3(
                        id = "Step5",
                        p(
                            tags$em("Step 5"),
                        ),style = "font-size:22px; color:blue;"), 
                    ),

                    column(12, offset=3,align="left", h4("
                        5). To remove unwanted regions from the final haplotype presentation, single click on the third panel (Panel 3) to set the left boundary, and then double click to set the right boundary. 
                        Once both boundaries are set, the '(5) Trim' button will become clickable. Click this button to view Panel 4, which will show the trimmed presentation.
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),
                    column(6,offset=3, align="center", tags$img(width="1492", height="455", src=paste(Stream_folder,"Steps_7.png",sep=''))),
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("Panel 4:")),
                    column(6,offset=3, align="center", tags$img(width="1561", height="487", src=paste(Stream_folder,"Steps_8.png",sep=''))),
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),


                    column(12, offset=3,align="left", h4("
                        (Optional) If you want to extract the DNA sequence within the trimmed region, simply click on the 'Extract trimmed fasta' button. 
                        This will generate a fasta file containing representative haplotypes and their corresponding DNA sequences. 
                        The file will be saved in a final downloadable .zip file.
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

                    column(12, offset=3,align="left", h4("
                        *** After trimming, click the 'Save .zip file to...' button to save a .zip file to your preferred folder. 
                        The trimmed Panel 4 will be included in the .zip file in PNG format. 
                        If the saved PNG figure is not satisfactory or needs improvement, you can adjust it using the PNG parameters on the left-hand side (slider bars for width, height, and point size). 
                        This output and save process is automated, and you will find an updated Panel 4 (PNG) in a new .zip file folder. ***
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

                    column(12, offset=3,align="left", h4("
                        (Optional). Click red 'Done' button to delete your submitted job and related files.
                        ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

                    #    column(12, offset=3,align="left", h4("
                    #6). If the search boundaries (Upstream /Downstream) need to be modified, go to step 2 and run step 2-5 again.
                    #    ")),
                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=3,align="center",   h4(
                        id = "Demo_mp4_1",
                        p(
                            tags$em("Demo video"),
                            ),style = "font-size:28px; color:blue;"), 
                    ),

                    column(12, offset=3,align="center",actionButton("Play", label = "Play tutorial video",class = "btn-warning", width='400px',style='padding:12px; font-size:150%')), #5/11/23

                    column(12, offset=3,align="center",uiOutput("Video") ),#5/11/23


                    #column(12, offset=3, align="center", tags$iframe(width="1920", height="1080", src=paste(Stream_folder,"BridgeCereal_tutorial.mp4", sep=''), frameborder="0", allowfullscreen=NA, autoplay = NA),
                    #column(12, offset=3, align="center", tags$video(width="1200", height="800", src=paste(Stream_folder,"BridgeCereal_tutorial_.mp4", sep=''), type = "video/mp4", autoplay = NA, controls = NA, preload= "none") ),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    column(12,offset=3,align="center",   h3(
                        id = "Instructions",
                        p(
                            tags$em("BRIDGEcereal additional instructions"),
                        ),style = "font-size:24px; color:blue;"), 
                    ),

                    #column(12, offset=3, align="center", tags$iframe(width="1400", height="800", src=paste(Stream_folder,"BRIDGEcereal_instructions.pdf", sep=''), frameborder="0", allowfullscreen=NA)),
                    column(12, offset=3, align="center", tags$a("Click here to get the BRIDGEcereal_additional_instructions.pdf", href=paste(Stream_folder,"BRIDGEcereal_additional_instructions.pdf", sep=''),target='_blank',style = "font-size: 36px;" ) ),

                    column(12, offset=3,align="center", h3("")),
                    column(12, offset=3,align="center", h3("")),

                    #  column(12,offset=3,align="center",   h4(
                    #      id = "CLIPS",
                    #      p(
                    #        tags$em("CLIPS algorithm demonstration"),
                    #      ),style = "font-size:28px; color:blue;"), 
                    #    ),
                    ################################################################


                    ) # fluidRow

                ) # mainPanel

                #) # sidebarLayout

            ) # For tagList
        }, # For ui function of page_1
    

        # To add server function part for page1

        server <- function(input, output, session){

            #output$Demo_text <- renderText({paste("Short demo video:",sep="")})

            output$dimension_display <- renderText({

                paste(input$dimension[1], input$dimension[2], round(input$dimension[2]/input$dimension[1], 2) )
   
            })

           # Working_Dir <- '/mnt/compbiolab/bszhang/script_V2/'

            Working_Dir <- Stream_folder

            my_img <- reactive({
                
                frink <- image_read( paste(Working_Dir,'steps.png',sep='') )
                
                w <- image_info(frink)$width
                
                h <- image_info(frink)$height
                
                list(
                    raster = as.raster(frink),
                    w = w,
                    h = h
                )
            })

            img_dim_f <- function(parm) {
            
                function() {
                    p <- 0
                    i <- my_img()
                    if (isTruthy(i)) {
                        if (isTruthy(i[[parm]])) {
                            p <- i[[parm]]
                        }
                    }
                    p
                }
            }

            output$plotWorkflow2 <- renderPlot(
                expr = {
                    i <- req(my_img())
                    r <- req(i$raster)
                    plot(r)
                },
                width = img_dim_f("w"),
                height = img_dim_f("h")
            )

            #output$info_Workflow2 <- renderText({
            #        if(!is.null(input$plotWorkflow2_hover)){
            #           paste( 'x=', round(input$plotWorkflow2_hover$x, 1), 'y=', round(input$plotWorkflow2_hover$y, 1) , sep=' ')      
            #       }
            #    })
            # output$info <- renderText({
            #    xy_range_str <- function(e) {
            #      if(is.null(e)) return("NULL\n")
            #      paste0( " ymin=", round(e$ymin, 0)," ymax=", round(e$ymax, 0), 
            #              " xmin=", round(e$xmin, 0), " xmax=", round(e$xmax, 0) )
            #    }
            #    paste0("brush: ", xy_range_str(input$plotWorkflow2_brush))
            #  })


            observeEvent(input$plotWorkflow2_hover,{

                req(input$plotWorkflow2_hover)

                x_axis<-reactive({
    
                    x_info<-input$plotWorkflow2_hover$x
                })

                y_axis<-reactive({

                    y_info<-input$plotWorkflow2_hover$y
                })

                x_axis1<-x_axis()
                
                y_axis1<-y_axis()

                if(y_axis1>=800 & y_axis1<=856 & x_axis1>=123 & x_axis1<=416){

                    output$info_Workflow2 <- renderText({paste("Input the gene ID, then click button (1). The referene and chromosome boxes will be filled in automatically. 
                    If you want to submit a CDS sequence, please refer to the 'Submit a CDS' part in 'BRIDGEcereal additional instructions'.",sep='') })


                } else if(y_axis1>=700 & y_axis1<=758 & x_axis1>=121 & x_axis1<=416){

                    output$info_Workflow2 <- renderText({paste("Filled in automatically when you have a gene ID",sep='') })

                } else if(y_axis1>=605 & y_axis1<=660 & x_axis1>=122 & x_axis1<=415){

                    output$info_Workflow2 <- renderText({paste("Filled in automatically when you have a gene ID",sep='') })

                } else if(y_axis1>=504 & y_axis1<=562 & x_axis1>=123 & x_axis1<=417){

                    output$info_Workflow2 <- renderText({paste("Filled in automatically when you have a gene ID. 
                        By default, 10% of query gene size will be filled in for up- and down-stream of query gene. 
                        Modify them based on your preferred sizes.",sep='') })

                } else if(y_axis1>=408 & y_axis1<=467 & x_axis1>=121 & x_axis1<=417){

                    output$info_Workflow2 <- renderText({paste("Click (2) Submit button to start the analysis, and a pan-genome graph will be plotted in panel 1. 
                    The (3) Tree button (blue) will be clickable after the process.",sep='') })

                } else if(y_axis1>=314 & y_axis1<=369 & x_axis1>=122 & x_axis1<=416){

                    output$info_Workflow2 <- renderText({paste("Click (3) Tree button (blue) to cluster assemblies, and then panel 2 will be plotted.",sep='') })

                } else if(y_axis1>=213 & y_axis1<=268 & x_axis1>=121 & x_axis1<=416){

                    output$info_Workflow2 <- renderText({paste("Do the tree-cut on the panel 2 based on your preferred height, 
                    determining the number of haplotypes for the next steps.",sep='') })

                } else if(y_axis1>=108 & y_axis1<=188 & x_axis1>=121 & x_axis1<=416){

                    output$info_Workflow2 <- renderText({paste("You may update/reset up- and down-stream boundaries after panel 1 or panel 2 is revealed.",sep='') })

                } else if(y_axis1>=19 & y_axis1<=77 & x_axis1>=123 & x_axis1<=420){

                    output$info_Workflow2 <- renderText({paste("Click (4) Plot selected haplotypes button to plot panel 3. 
                    Reorder the representatives in the 'Order of plot', and trimming haplotypes at preferred positions.",sep='') })

                } else if(y_axis1>=600 & y_axis1<=662 & x_axis1>=440 & x_axis1<=528){

                    output$info_Workflow2 <- renderText({paste("Please refer to the section 'Upload a contig or a chromosome' in 'BRIDGEcereal additional instructions'.",sep='') })

                } else {

                    output$info_Workflow2 <- renderText({paste("",sep='') })
                }


            })



            ################# 5/11/23
            observeEvent(input$Play, {

                output$Video <- renderUI({

                    tags$iframe(width="1920", height="1080", src = paste(Stream_folder,"BridgeCereal_tutorial_subs3.mp4", sep=''),

                    frameborder="0", allowfullscreen=NA, autoplay = NA)
                })

            })
            ################# 5/11/23
            ##################
            ###################
        } # server function of Page_1

    ) # page for Page_1

} # Page_1 function
############ To combine pages together