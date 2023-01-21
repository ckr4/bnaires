# load packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(pins)
library(shinyjs)
library(plotly)
library(dplyr)

# load helper scripts
source("plotWealthWeight.R")
source("plotWealthLength.R")
source("plotWealthHeight.R")
source("plotWealthComp.R")
source("plotWealthVs.R")
source("modalUIs.R")

# register github board
#board_register_github(repo="ckr4/wealthComp", token="this will not work without the token")

# Get data from board -----> Has to be registered first
wealth <- pin_get('bnaires', board='github')
fict_char <- pin_get('fict-char', board='github')
hist_fig <- pin_get('hist-fig', board='github')

# Define UI
ui <- function(request) {
  
  fluidPage(theme = shinytheme("readable"),
        
        titlePanel(tagList(
          span(div(h4("Putting extraordinary wealth in perspective")), 
               div(actionButton('about', 'About', width='100px',
                                style='color: white; 
                                border-radius=5px; 
                                font-weight: bold;'),
                   tags$style("#about {background-color: #4582ec;} 
                              #about:hover {background-color: #2a6bda;"),
                   actionButton('help', "Help", width='100px', 
                                style='color: white; 
                                border-radius=5px; 
                                font-weight: bold;'),
                   tags$style("#help {background-color: #4582ec;} 
                              #help:hover {background-color: #2a6bda;"),
                 style = "font-size:14px;position:absolute;top:12px;right:42px;"))),
          windowTitle = "Putting extraordinary wealth in perspective"
        ),
        
        useShinyjs(),
  
        tags$head(tags$style(HTML('#sidebar {margin: 0px; 
                                  padding: 0px 10px 8px 10px; border-width: 0px}'),
                             HTML('#mainpan {margin: 0px; 
                                  padding: 0px; border-width: 20px}'))),
        
        sidebarPanel(id='sidebar', width=4,

          div(id="csel_pi",
              pickerInput(
                inputId="c_sel",
                h5(style="margin-top:20px; margin-bottom:2px;",
                   "Select current billionaire(s)"),
                multiple=TRUE,
                choices=wealth$Name,
                selected=wealth$Name[1:5]
              )
          ),
          
          div(id="ctsel_pi",
              pickerInput(
                inputId="ct_sel",
                label=h5(style="margin-top: 4px; margin-bottom:2px;", 
                         "Filter by country/territory:"),
                multiple=TRUE,
                choices=unique(wealth$Country),
                selected=unique(wealth$Country),
                options=list('actions-box' = TRUE),
              )
          ),
          
          sliderInput(
            inputId="slider_age", 
            label=h5(style="margin-top: 4px; margin-bottom:2px;", "Filter by age:"),
            min=min(wealth$Age),
            max=max(wealth$Age),
            value=c(min(wealth$Age), max(wealth$Age))

          ),
            
          fluidRow(id='fr_denom',
            column(width=8, offset = 0, style='padding-left:4px;
                   padding-right:0px;',
                   h6(style="text-align: center;", "Denomination ($):")
                   ),
            column(width=4, offset = 0, style='padding-right:8x;
                   padding-left:0px;',
                   selectInput(
                     inputId="d_sel",
                     label=NULL,
                     width='80px',
                     choices=c(1,5,10,20,50,100))),
            tags$style("#fr_denom {padding-bottom=0px;")
            
          ), # close fluidRow
          
          fluidRow(id="fr_states",
            column(id="col_clear", width=6, 
                   actionButton("clear_l", "Reset", width='100%'),
                   ), tags$style("#col_clear {padding-right:4px; margin-right:0px;}"),
            column(id="col_rand", width=6,
                   actionButton("rand_l", "Random", width='100%')
                   ), tags$style("#col_rand {padding-left:4px; margin-left:0px;}"),
            
          ), tags$style("#fr_states {margin-top:0px; margin-bottom:12px;}"),
  
          bookmarkButton(id="bm_page", width='100%'), 
          tags$style('#bm_page {margin-bottom:24x;}'),
          
          textOutput("dol_info", container=tags$h5),
          tags$style('#dol_info {text-align: center; margin-top:20px;'),
          
        ), # close sidebarPanel
        
        # create plots
        mainPanel(id='mainpan', width=8,

          tabsetPanel(
            id="tabs", type="pills",
            
            tabPanel(id="tab_l", "Length", width=10,
              value=1,
              echarts4rOutput("length", height="492px", width="700px"),
            ),
            
            tabPanel(id="tab_h", "Height",
              value=2,
              echarts4rOutput("height", height="492px", width="700px"),
            ),

            tabPanel("Weight",
              value=3,
              plotOutput("weight", height="542px", width="700px"),
            ),

            tabPanel("Comparison",
              value=4,
              plotlyOutput(outputId="comp", height="542px", width="700px")
            ),

            tabPanel("Versus",
              value=5,
              echarts4rOutput("versus", height="408px", width="762px"),
              fluidRow(
                column(4, offset=0, style='padding:0px 0px 0px 10px;;',
                  radioButtons(
                    inputId="vs",
                    label=h5("Versus"),
                    choiceNames=c("Fictional Characters", "Historical Figures"),
                    choiceValues=c("f", "h")
                  )
                ),
                column(8, offset=0, style='padding:0px;',
                  pickerInput(
                    inputId="vs_inp",
                    h5("Select 1"),
                    choices=fict_char$Name,
                  )
                )
              )

            ) # close Versus tabPanel

          ) # close tabsetPanel

        ) # close mainPanel 
  ) # close fluidPage
} # close function

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # create reactive vector for calling plot
  values <- reactiveValues()
  
  bookmarkingWhitelist <- c("c_sel", "ct_sel", "slider_age", "d_sel", "vs", "vs_inp")
  
  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
  })
  
  onBookmark(function(state) {
    state$values$current_tab <- input$tabs
    if (input$tabs==5) {state$values$versus_sel <- input$vs_inp}
  })
  
  onRestore(function(state) {
    updateTabsetPanel(session, "tabs", selected=state$values$current_tab)
    if (state$values$current_tab==5) {vs_inp=state$values$versus_sel}
  })
  
  observeEvent(input$bm_page, {
    session$doBookmark()
  })
    
  # Use reactive vector to capture user input (names)
  observe({
      values$c_sel <- input$c_sel
      values$d_sel <- input$d_sel
      values$vs_inp <- input$vs_inp
      values$tab <- input$tabs
    })
  
  # Enable and disable inputs based on tab selection
  observe({
    validate(need(!is.null(input$tabs), ""))
    if (input$tabs == 1) {
      disable("d_sel")
      updateSelectInput(session, "d_sel", selected=100)
      updatePickerInput(session, "c_sel", options=list('actions-box' = FALSE))
    } else if (input$tabs == 4) {
      disable("d_sel")
      updatePickerInput(session, "c_sel", options=list('actions-box' = TRUE))
    } else if (input$tabs == 5) {
      disable("d_sel")
    } else {
      enable("d_sel")
      updatePickerInput(session, "c_sel", options=list('actions-box' = FALSE))
    }
  })
  
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "About",
      size=c("l"),
      aboutUI
    ))
  })
  
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      size=c("m"),
      helpUI
    ))
  })
  
  # Clear selections on button press
  observeEvent(input$clear_l, {
    updatePickerInput(session, "c_sel", selected="")
    updatePickerInput(session, "ct_sel", selected=unique(wealth$Country))
    updateSelectInput(session, "d_sel", selected=100)
    updateSliderInput(session, "slider_age", 
                      value=c(min(wealth$Age), max(wealth$Age)))
  })
  
  # Get random selection of names and denomination
  observeEvent(input$rand_l, {
    updatePickerInput(session, "c_sel", selected="")
    updatePickerInput(session, "ct_sel", selected=unique(wealth$Country))
    updateNumericInput(session, "min_age", value=min(wealth$Age))
    updateNumericInput(session, "max_age", value=max(wealth$Age))
    updateSelectInput(session, "d_sel", selected=sample(c(1,5,10,20,50,100), 1))
    updatePickerInput(session, "c_sel", selected=sample(wealth$Name, 5))
  })
  
  # create reactive to capture user age input
  age <- reactive({
    filter(wealth, Age >= input$slider_age[1] & Age <= input$slider_age[2])
  }) 
  
  # use user age input to update name list
  observeEvent(age(), {
    choices <- age()$Name
    age_filsel <- intersect(values$c_sel, choices)
    updatePickerInput(session, inputId = "c_sel", choices=choices,
                      selected=age_filsel)
  })
  
  # create reactive to capture user country input
  country <- reactive({
    filter(wealth, Country %in% input$ct_sel )
  }) 

  # use user country input to update name list
  observeEvent(country(), {
    choices <- country()$Name
    ct_filsel <- intersect(values$c_sel, choices)
    updatePickerInput(session, inputId = "c_sel", choices=choices,
                      selected=ct_filsel)
  })
  
  observeEvent(input$vs, {
    if (input$vs == "f") {
      updatePickerInput(session,
                        inputId = "vs_inp",
                        choices = fict_char$Name)
    } else if (input$vs == "h") {
      updatePickerInput(session,
                        inputId = "vs_inp",
                        choices = hist_fig$Name)
    }
  }, ignoreInit=TRUE)

  output$dol_info <- renderText({
    if (input$tabs==1) {'Each dollar bill is 15.5956 millimeters long 
                (6.14 inches)'}
    else if (input$tabs==2) {'Each dollar bill is 0.1092 millimeters thick
                (.0043 inches)'}
    else if (input$tabs==3) {'Each dollar bill weighs 1 gram (0.035274 ounces)'}
    else {''}
  })
  
  # create plots
  
  output$length <-

    renderEcharts4r({

      # ensure 1-15 names selected
      validate(
        need(length(values$c_sel) > 0, "Please select at least 1"),
        need(length(values$c_sel) < 16, "Please select no more than 15")
      )

      # if correct # selected, create plot
      if (length(values$c_sel)>0 & length(values$c_sel)<16) {
        plot_l(match(values$c_sel, wealth$Name), wealth)
      }
      
    })
  
  output$height <-
    
    renderEcharts4r({
      
      # ensure 1-5 names selected
      validate(
        need(length(values$c_sel) >0, "Please select at least 1"),
        need(length(values$c_sel) <6, "Please select no more than 5")
      )
      
      # if correct # selected, create plot
      if (length(values$c_sel)>0 & length(values$c_sel)<6) {
        plot_h(match(values$c_sel, wealth$Name), as.numeric(values$d_sel), wealth)
      }
    
    })


  output$weight <- 
    
    renderPlot({
      
      # ensure 1-5 names selected
      validate(
        need(length(values$c_sel) > 0, "Please select at least 1"),
        need(length(values$c_sel) < 6, "Please select no more than 5")
      )
      
      # if correct # selected, create plot
      if (length(values$c_sel)>0 & length(values$c_sel)<6) {
        plot_w(match(values$c_sel, wealth$Name), as.numeric(values$d_sel), wealth)
      }
      
    })
  
  output$comp <-
    
    renderPlotly({
      
      # create plot
      if (length(values$c_sel) > 0) {
        plot_c(match(values$c_sel, wealth$Name), wealth, hist_fig, fict_char)
      } else {
        plot_c(0, wealth, hist_fig, fict_char)
      }
    })
  
  output$versus <-
    
    renderEcharts4r({
      
      # ensure 1-5 names selected
      validate(
        need(length(values$c_sel) > 0, "Please select at least 1 current billionaire"),
        need(length(values$c_sel) < 6, "Please select no more than 5 current billionaires"),
        need(length(values$vs_inp) > 0, "Please select a basis for comparison"),
      )
      
      # Set vs_sel
      if (input$vs == "h") {vs_sel = match(values$vs_inp, hist_fig$Name)}
      if (input$vs == "f") {vs_sel = match(values$vs_inp, fict_char$Name)}
      
      # if correct # selected, create plot
      if (length(values$c_sel) > 0 & 
          length(values$c_sel) < 6 & 
          length(values$vs_inp) > 0 &
          !is.na(vs_sel)) {
        plot_vs(match(values$c_sel, wealth$Name), input$vs, vs_sel, 
                wealth, hist_fig, fict_char)
      }
      
    })
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")

