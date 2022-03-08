# This file is part of Pain2D
# 
# Pain2D is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Pain2D is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with server.R  If not, see <http://www.gnu.org/licenses/>.
########################################################################################
###############----------------------------------------------
# @autor Natasza Szczypien
##############----------------------------------------------


library(shiny)
library(shinyFiles)
library(googleVis)
library(shiny.i18n)
library(shinyjs)

i18n <- Translator$new(translation_json_path = "translation1.json")
# 
# # change this to en


ui <- shinyUI(
  
  tagList(
    shiny.i18n::usei18n(i18n),
    
    tags$link(href="https://fonts.googleapis.com/css2?family=Exo+2:wght@400;700;800;900&display=swap", rel="stylesheet"),
    column(1,tags$img(src="logo_tool/logo_pain2d_tool.png", width=120, style="padding:left")),
    column(9, 
           navbarPage("",
                         theme = "bootstrap.css",
                         id = "navbar",
                      #-------------------------------------------------- Template  ----------------------------------------------------------------------------  
                      tabPanel("", icon = icon("align-justify"), id = "extract_view", 
                               
                               column(2,
                                      class = "extract-row panel panel-default fade-in one ",
                                      style = "font-size:large;",
                                      tags$div(class = "label-new", id = "choose_dir_div"),
                                      tags$h3("Import template pain drawing", style="color:white; font-weight: bold;text-shadow: 2px 2px #110d54;"),
                                      tags$hr(),
                                      tags$div(class ="input-ex", fileInput("file_input_template", label = "Select ZIP",  multiple = TRUE, accept = ".zip")),
                                      shiny::tags$head(shiny::tags$style(shiny::HTML(
                                        "#selected_ex { height: 200px; overflow: auto; }"
                                      ))),
                                      actionButton(
                                        inputId = "submit_template",
                                        label = "Confirm",
                                        class = "btn2 action_button"
                                      ),
                                      uiOutput('pdfviewer')
                               )
                      ),        
                         #-------------------------------------------------- EXTRACT  ----------------------------------------------------------------------------
                         tabPanel(i18n$t("Extract"), id = "extract_view",
                                  
                                  column(2,
                                         class = "extract-row panel panel-default fade-in one ",
                                         style = "font-size:large;",
                                         tags$div(class = "label-new", id = "choose_dir_div"),
                                         tags$h3(i18n$t("Pain points extraction"), style="color:white; font-family: 'Exo 2', sans-serif; font-weight: 700; text-shadow: 2px 2px #110d54;"),
                                         tags$hr(),
                                         tags$div(class ="input-ex", fileInput("file_input", label = i18n$t("Select pain drawings in PDF"),  multiple = TRUE, accept = ".pdf")),
                                         shiny::tags$head(shiny::tags$style(shiny::HTML(
                                           "#selected_ex { height: 200px; overflow: auto; }"
                                         ))),
                                         actionButton(
                                           inputId = "submit_extraction",
                                           label = i18n$t("Confirm"),
                                           class = "btn2 action_button"
                                         ),
                                         downloadButton("download_extraction", label = i18n$t("Download"))
                                  )
                         ),
                         
                         
                         
                         #-------------------------------------------------- Pain Profile----------------------------------------------------------------------------
                         tabPanel(i18n$t("Pain profile"), id="men_view",
                                  sidebarLayout(
                                    sidebarPanel(
                                      ####################################
                                      class = "mean-row panel panel-default fade-in one ",
                                      style = "font-size:large; font-family: 'Exo 2', sans-serif; font-weight: 400;",
                                      tags$div(class = "label-new", id = "choose_dir_div"),
                                      tags$h3(i18n$t("Pain profile"), style="color:white; font-family: 'Exo 2', sans-serif; font-weight: 700; text-shadow: 2px 2px #110d54;"),
                                      tags$hr(),
                                      ############## LOAD #################
                                      helpText(i18n$t("Load"), class ="white-text "),
                                      fileInput("disease_input", label = "",  multiple = FALSE, accept = ".csv", placeholder = "Select csv-File"),
                                      ############### CREATE #################
                                      helpText(i18n$t("Create"), class ="white-text "),
                                      fileInput("create_input", label = "",  multiple = TRUE, accept = ".json", placeholder = "Select JSON-files"),
                                      tags$head(tags$style("#plot_pain_profile{height:72vh !important;}")),
                                      tags$hr(),
                                      ################ BUTTONS ###############
                                      radioButtons("color_choise", label = h4(i18n$t("Choose color")),
                                                   choices = c( "Gray" = 1, "Blue-Yellow-Red" = 2, "Red-Yellow-Blue" = 3),
                                                   selected = 1),
                                      actionButton(
                                        inputId = "submit_mean",
                                        label = i18n$t("Confirm"),
                                        class = "btn2 action_button"
                                      ),
                                      tags$hr(),
                                      downloadButton(outputId = "sickness_download_btn",
                                                     label = i18n$t("Download data"),
                                                     class = "btn action_button" ),
                                      downloadButton(outputId = "sickness_download_png_btn",
                                                     label = i18n$t("Download PNG"),
                                                     class = "btn action_button" )
                                    ),
                                    ################ IMAGE ###############
                                    mainPanel(class="mean-plot-row panel panel-default fade-in one ",
                                              htmlOutput(outputId= "pp_slider"),
                                              plotOutput(outputId = "plot_pain_profile", width = 640 ,height = 720 ))
                                  )
                         ),
                         
                      #-------------------------------------------------- Pain drawing----------------------------------------------------------------------------
                      tabPanel("Pain drawing", 
                               sidebarLayout(
                                 sidebarPanel(
                                   ####################################
                                   class = "mean-row panel panel-default fade-in one ",
                                   style = "font-size:large;",
                                   tags$div(class = "label-new", id = "choose_dir_div"),
                                   tags$h3("Pain drawing", style="color:white; font-weight: bold; text-shadow: 2px 2px #110d54;"),
                                   tags$br(),
                                   ############## LOAD PP #################
                                   helpText("Load", class ="white-text "),
                                   fileInput("pd_input", label = "", multiple = TRUE, accept = ".json", placeholder = "Select JSON-files"),
                                   ################ BUTTONS ###############
                                   actionButton(
                                     inputId = "submit_display_pd",
                                     label = "Confirm",
                                     class = "btn2 action_button"
                                   ),
                                
                                   htmlOutput("select_pd"),
                                   #selectInput("select_pd", label = "", choices = list()),
                                   
                                   tags$hr()
                                 ),
                                 ################ IMAGE ###############
                                 mainPanel(class="mean-plot-row panel panel-default fade-in one ",
                                           plotOutput(outputId = "plot_pain_drawing", width = 620 ,height = 700 ))
                               )
                      ),
                         
                         #-------------------------------------------------- AUC ----------------------------------------------------------------------------
                         tabPanel(i18n$t("Binary Classifier"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      class = "mean-row panel panel-default fade-in one ",
                                      style = "font-size:large; font-family: 'Exo 2', sans-serif; font-weight: 400;",
                                      tags$div(class = "label-new", id = "choose_dir_div"),
                                      tags$h3(i18n$t("Binary Classifier"),style="color:white; font-weight: 700; font-family: 'Exo 2', sans-serif; font-weight: 700; text-shadow: 2px 2px #110d54;"),
                                      
                                      tags$hr(),
                                      textInput(inputId = "sick_name1", label = i18n$t("Type 1'st disease name"), value = ""),
                                      fileInput(inputId = "auc_input_d1",i18n$t("Upload files from 1'st disease"), multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                      tags$br(),
                                      textInput(inputId = "sick_name2", label = i18n$t("Type 2'nd disease name"), value = ""),
                                      fileInput(inputId = "auc_input_d2",i18n$t("Upload files from 2'nd disease"), multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                      tags$hr(),
                                      
                                      actionButton(
                                        inputId = "submit_jack",
                                        label = i18n$t("Confirm"),
                                        class = "btn2 action_button"
                                      ),
                                      downloadButton(outputId = "jack_download_btn",
                                                     label = i18n$t("Download CSV"),
                                                     class = "btn action_button" )
                                    ),
                                    mainPanel(class=" mean-plot-row panel panel-default fade-in one ",
                                              plotOutput("view_ROC", width = 500, height = 500),
                                              verbatimTextOutput("view_jack")
                                              
                                    )
                                  )
                         ),
                      #-------------------------------------------------- NEAREST NEIGHBOUR ----------------------------------------------------------------------------
                      tabPanel("Nearest Neighbours",
                               sidebarLayout(
                                 sidebarPanel(
                                   
                                   class = "mean-row panel panel-default fade-in one ",
                                   style = "font-size:large;",
                                   tags$div(class = "label-new", id = "choose_dir_div"),
                                   tags$h3("Nearest Neighbour", style="color:white; font-weight: bold;text-shadow: 2px 2px #110d54;"),
                                   tags$hr(),
                                   tags$h4("Upload",style="color:white"),
                                   fileInput(inputId = "NN.upload", label = "Upload calculated NN File", multiple = FALSE, accept = ".csv", placeholder = "Select CSV-files"),
                                   
                                   tags$hr(),
                                   tags$h4("Calculate",style="color:white"),
                                   
                                   textInput("NN.static.file.input1.name", "Type 1'st disease name"),
                                   fileInput(inputId = "NN.static.file.input1", label = "Upload Files from 1'st disease",multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                   textInput("NN.static.file.input2.name", "Type 2'nd disease name"),
                                   fileInput(inputId = "NN.static.file.input2", label = "Upload Files from 2'nd disease",multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                   
                                   uiOutput("files_ui"),
                                   
                                   actionButton("add_btn", "Add Disease"),
                                   actionButton("rm_btn", "Delete Disease"),
                                   tags$hr(),
                                   
                                   actionButton(
                                     inputId = "submit_nn",
                                     label = "Confirm",
                                     class = "btn2 action_button"
                                   ),
                      
                                   downloadButton(outputId = "NN_download_btn",
                                                  label = "Download CSV",
                                                  class = "btn action_button" )
                                 ),
                                 mainPanel(class=" mean-plot-row panel panel-default fade-in one ",
                                           plotOutput("heatmap_nn", click="plot_click",height="800px"),
                                           verbatimTextOutput("info_nn"),
                                           uiOutput("checkbox_for_boxplot_nn"),
                                           plotOutput("boxplot_nn", height="800px"),
                                 )
                               )
                      ),
                         #-------------------------------------------------- KDC ----------------------------------------------------------------------------
                      tabPanel("KDC",
                               sidebarLayout(
                                 sidebarPanel(
                                   
                                   class = "mean-row panel panel-default fade-in one ",
                                   style = "font-size:large;",
                                   tags$div(class = "label-new", id = "choose_dir_div"),
                                   tags$h3("K Disease Classifier", style="color:white; font-weight: bold;text-shadow: 2px 2px #110d54;"),
                                   tags$hr(),
                                   tags$h4("Choose in each File chooser pain drawings from different disseases.",style="color:white"),
                                   
                                   textInput("loo.static.file.input1.name", "Type 1'st disease name"),
                                   fileInput(inputId = "loo.static.file.input1", label = "Upload Files from 1'st disease",multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                   textInput("loo.static.file.input2.name", "Type 2'nd disease name"),
                                   fileInput(inputId = "loo.static.file.input2", label = "Upload Files from 2'nd disease",multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                   textInput("loo.static.file.input3.name", "Type 3'nd disease name"),
                                   fileInput(inputId = "loo.static.file.input3", label = "Upload Files from 3'nd disease",multiple = TRUE,accept = ".json", placeholder = "Select JSON-files"),
                                   
                                   uiOutput("loo_files_ui"),
                                   actionButton("loo_add_btn", "Add Fileinput"),
                                   actionButton("loo_rm_btn", "Delete Fileinput"),
                                   tags$hr(),
                                  
                                   actionButton(
                                     inputId = "submit_loo",
                                     label = "Submit",
                                     class = "btn action_button"
                                   ),
                                   downloadButton(outputId = "KDC_download_btn",
                                                  label = "Download CSV",
                                                  class = "btn action_button" )
                                   
                                 ),
                                 mainPanel(class=" mean-plot-row panel panel-default fade-in one ",
                                           verbatimTextOutput("view_loo"),
                                           verbatimTextOutput("view_chi_squared"),
                                           verbatimTextOutput("view_spec_sens")
                                 )
                               )
                      ),
                      #-------------------------------------------------- INFO ----------------------------------------------------------------------------
                      
                      tabPanel("Info",
                                 tagList(
                                   tags$div(
                                     class = "text-output",
                               
                                 "Pain2D-Tool is part of Pain2D-Software package. With this application you can: ",
                                 tags$ul(
                                   tags$li(tags$b("Setup - "), "upload a template pain drawing which you created in Pain2D-Designer"),
                                   tags$li(tags$b("Extract - "), "upload pain drawings in PDF and extract all pain points"),
                                   tags$li(tags$b("Pain profile - "), "Create new or load existing pain profiles"),
                                   tags$li(tags$b("AUC - "), "Analyse two diseases with a binary clasifier and get the ROC and statistics"),
                                   tags$li(tags$b("Save - "), "Analyse more than two diseases with K Disease Classifier and get the stistics")),
                                 
                                 tags$hr(),
                                 tags$br(),
                                 "You can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or
      (at your option) any later version.",
                                 tags$br(),
                                 
                                 tags$b("Pain2D-Tool is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Main.R  If not, see http://www.gnu.org/licenses/."),
                                 tags$br(),
                                 tags$a(tags$img(src ="https://img.shields.io/badge/License-GPLv3-blue.svg", alt = "GPL licence v.3"), 
                                        rel="license", href="https://www.gnu.org/licenses/gpl-3.0.en.html"),
                                 tags$hr(),
                                 "Developers: Natasza Szczypien",
                                 tags$br(),
                                 "Supervisor: Prof. Frank Klawonn",
                                 tags$br(),
                                 "The images provided by this application are licensed under the following CC license",
                                 tags$br(),
                                 tags$a(tags$img(src ="https://i.creativecommons.org/l/by-sa/4.0/88x31.png", alt = "creative commons license"), 
                                        rel="license", href="http://creativecommons.org/licenses/by-sa/4.0/"),
                                 tags$br(),
                                 tags$hr(),
                                 tags$b("Contact:")," pain2dsoftware@gmail.com",
                                 easyClose = TRUE,
                                 footer = NULL
                                 )
                                 )
                               )
                        
                         #   #-------------END-----------------
    )
    )
    ,column(2,style="position:relative; margin-top:2%;",
               tabsetPanel(id = "language",
                           tabPanel("EN", value = "en"),
                             tabPanel("DE", value = "de")
               )
    )
  )
)
