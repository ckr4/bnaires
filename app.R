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
source("insets.R")

# register github board
board_register_github(repo="ckr4/wealthComp", token="")

# Get data from board -----> Has to be registered first
wealth <- pin_get('bnaires', board='github')
fict_char <- pin_get('fict-char', board='github')
hist_fig <- pin_get('hist-fig', board='github')
rich_folk <- pin_get('rich-folk', board='github')

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
  
        tags$head(tags$style(HTML('#sidebar {margin: 0px; width: 320px;
                                  padding: 0px 10px 8px 10px; border-width: 0px}'),
                             HTML('#mainpan {margin: 0px; 
                                  padding: 0px; border-width: 20px}'))),
        
        sidebarPanel(id='sidebar',

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
            column(width=6, align='center',
                   h6("Denomination ($):")
                   ),
            column(width=6, align='center',
                   selectInput(
                     inputId="d_sel",
                     label=NULL,
                     width='80px',
                     choices=c(1,5,10,20,50,100),
                     selected='100')),
            tags$style("#fr_denom {padding-bottom=0px;")
            
          ), # close fluidRow
          

          actionButton("clear_l", "Reset", width='100%'),
 
          actionButton("rand_l", "Random", width='100%'),
          
          actionButton("incl_me", "Include me", width='100%'),
          
          bookmarkButton(id="bm_page", width='100%'), 
          tags$style('#clear_l {margin-bottom:4px;}
                      #rand_l {margin-bottom:4px;}
                      #incl_me {margin-bottom:4px;}
                      #bm_page {margin-bottom:0px;}
                      '
          ),
          
          textOutput("dol_info", container=tags$h5),
          tags$style('#dol_info {text-align: center; margin-top:12px;'),
          
        ), # close sidebarPanel
        
        # create plots
        absolutePanel(id='mainpan',

          tabsetPanel(
            id="tabs", type="pills",
            
            tabPanel(id="tab_l", "Length", width=10,
              value=1,
              echarts4rOutput("length", height="492px", width="700px"),
              absolutePanel(
                echarts4rOutput("user_l", height="492px", width="168px"),
                left="654px", width="190px", top="43px", height="500px",
                tags$style("#user_l {border-left: 2px solid white}")
              )
            ),
            
            tabPanel(id="tab_h", "Height",
              value=2,
              echarts4rOutput("height", height="492px", width="690px"),
              absolutePanel(
                
                echarts4rOutput("user_h", height="492px", width="134px"),
                left="636px", width="154px", top="43px", height="500px"
              )
            ),

            tabPanel("Weight",
              value=3,
              plotOutput("weight", height="542px", width="700px"),
              absolutePanel(
                plotOutput("user_w", height="542px", width="148px"),
                left="692px", width="180px", top="43px", height="542px",
                tags$style("#user_w {border-left: 2px solid white}")
              )
            ),

            tabPanel("Comparison",
              value=4,
              plotlyOutput(outputId="comp", height="660px", width="700px")
            ),

            tabPanel("Versus",
              value=5,
              echarts4rOutput("versus", height="408px", width="762px"),
              absolutePanel(
                echarts4rOutput("user_v", height="408px", width="150px"),
                left="734px", width="164px", top="43px", height="408px",
              ),
              fluidRow(
                column(4, offset=0, style='padding:0px 0px 0px 10px;;',
                  radioButtons(
                    inputId="vs",
                    label=h5("Versus"),
                    choiceNames=c("Fictional Characters", "Historical Figures", "Richest Celebrities"),
                    choiceValues=c("f", "h", "c")
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

          ),
          left='370px', width='900px',top='50px',height='600px' # close tabsetPanel

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
    state$values$d_sel <- input$d_sel
    if (input$tabs==5) {
      state$values$versus_sel <- input$vs_inp
      state$values$versus <- input$vs
    }
  })
  
  onRestore(function(state) {
    req(state$values)
      updateTabsetPanel(session, "tabs", selected=state$values$current_tab)
  })
  
  onRestored(function(state) {
    if (state$values$current_tab==2) {enable("d_sel")}
    else if (state$values$current_tab==3) {enable("d_sel")}
    updateSelectInput(session, "d_sel", selected=state$values$d_sel)
    if (state$values$current_tab==5) {
      if (state$values$versus == "f") {
        updatePickerInput(session,
                          inputId = "vs_inp",
                          choices = c(state$values$versus_sel, fict_char$Name))
      } else if (state$values$versus == "h") {
        updatePickerInput(session,
                          inputId = "vs_inp",
                          choices = c(state$values$versus_sel, hist_fig$Name))
      } #else if (state$values$versus == "c") {
      #   updatePickerInput(session,
      #                     inputID = "vs_inp",
      #                     choice = c(states$values$versus_sel, rich_folk$Name))
      # }
    }
      
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
      updateSelectInput(session, "d_sel", selected='100')
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
  
  observeEvent(input$incl_me, {
    showModal(inclModal())
  })
  
  observeEvent(input$incl_ok, {
    if (!is.null(input$ti_incl)) {
      ti_inp <- as.character(input$ti_incl)
    }
    if (!is.null(input$ti_incl) && nchar(ti_inp) > 0 && ti_inp != "" &&
        nchar(ti_inp) <= 20 && input$ni_incl >= 0 && input$ni_incl < 1000000000) {
      values$ti_inp <- ti_inp
      values$ni_incl <- input$ni_incl
      removeModal()
    } else {
      name_fail = nw_fail_l = nw_fail_h = FALSE
      if (!is.null(input$ti_incl) && 
          nchar(ti_inp) == 0 | ti_inp == "" | nchar(ti_inp) > 20) {
        name_fail = TRUE} 
      if (input$ni_incl < 0) {nw_fail_l = TRUE} 
      if (input$ni_incl >= 1000000000) {nw_fail_h = TRUE} 
      showModal(inclModal(name_fail, nw_fail_l, nw_fail_h))
    }
  })
  
  observeEvent(input$mod_canx, {
    removeModal()
  })
  
  # Clear selections on button press
  observeEvent(input$clear_l, {
    updatePickerInput(session, "c_sel", selected="")
    updatePickerInput(session, "ct_sel", selected=unique(wealth$Country))
    updateSelectInput(session, "d_sel", selected=100)
    updateSliderInput(session, "slider_age", 
                      value=c(min(wealth$Age), max(wealth$Age)))
    values$ti_inp = ""
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
    } else if (input$vs == "c") {
      updatePickerInput(session,
                        inputId = "vs_inp",
                        choices = rich_folk$Name)
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
  
  output$user_l <-

    renderEcharts4r({
      if (!is.null(values$ti_inp) && values$ti_inp != "" &&
          values$ni_incl >= 0 && values$ni_incl < 1000000000) {
        inset_l(values$ti_inp, values$ni_incl)
      }
    })
  
  output$length <-

    renderEcharts4r({

      # ensure 1-15 names selected
      validate(
        need(length(values$c_sel) > 0, "Please select at least 1"),
        need(length(values$c_sel) < 16, "Please select no more than 15")
      )

      # if correct # selected, create plot
      if (length(values$c_sel)>0 && length(values$c_sel)<16) {
        pl <- plot_l(match(values$c_sel, wealth$Name), wealth)
      }

    })
  
  output$user_h <-
    
    renderEcharts4r({
      if (length(values$c_sel)>0 && length(values$c_sel)<6 &&
          !is.null(values$ti_inp) && values$ti_inp != "" &&
          values$ni_incl >= 0 && values$ni_incl < 1000000000) {
        inset_h(match(values$c_sel, wealth$Name), as.numeric(values$d_sel), 
                wealth, values$ti_inp, values$ni_incl)
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
      if (length(values$c_sel)>0 && length(values$c_sel)<6) {
        plot_h(match(values$c_sel, wealth$Name), as.numeric(values$d_sel), wealth)
      }
    
    })


  output$user_w <-
    
    renderPlot({
      
      if (length(values$c_sel)>0 && length(values$c_sel)<6 && 
          !is.null(values$ti_inp) && values$ti_inp != "" &&
          values$ni_incl >= 0 && values$ni_incl < 1000000000) {
        two_l <- ifelse(max(nchar(values$c_sel)) > 18, TRUE, FALSE)
        inset_w(as.numeric(values$d_sel), values$ti_inp, values$ni_incl, two_l)
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
      if (length(values$c_sel)>0 && length(values$c_sel)<6) {
        plot_w(match(values$c_sel, wealth$Name), as.numeric(values$d_sel), wealth)
      }
      
    })
  
  output$comp <-
    
    renderPlotly({
      
      # create plot
      user_ind = FALSE
      user_inp = c()
      selected = match(values$c_sel, wealth$Name)
      if (!is.null(values$ti_inp) && values$ti_inp != "" &&
          values$ni_incl >= 0 && values$ni_incl < 1000000000) {
        user_inp <- c( 
          Name=values$ti_inp, NetWorth=values$ni_incl / 1000000000,
          Change="", Age=0, Source="", Country="", Name2=values$ti_inp)
          user_ind = TRUE
      }
      if (length(selected) > 0) {
        plot_c(selected, wealth, hist_fig, fict_char, rich_folk, user_inp, user_ind)
      } else {
        plot_c(0, wealth, hist_fig, fict_char, rich_folk, user_inp, user_ind)
      }
    })
  
  output$user_v <-
    
    renderEcharts4r({
      if (length(values$c_sel)>0 && length(values$c_sel)<6 &&
          !is.null(values$ti_inp) && values$ti_inp != "" &&
          values$ni_incl >= 0 && values$ni_incl < 1000000000) {
        
        if (input$vs == "h") {vs_sel = match(values$vs_inp, hist_fig$Name)}
        if (input$vs == "f") {vs_sel = match(values$vs_inp, fict_char$Name)}
        if (input$vs == "c") {vs_sel = match(values$vs_inp, rich_folk$Name)}
        
        # if correct # selected, create plot
        if (length(values$c_sel) > 0 && length(values$c_sel) < 6 && 
            length(values$vs_inp) > 0 && !is.na(vs_sel)) {
          inset_v(match(values$c_sel, wealth$Name), input$vs, vs_sel, 
                  wealth, hist_fig, fict_char, rich_folk, values$ti_inp, 
                  values$ni_incl)
        }
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
      if (input$vs == "c") {vs_sel = match(values$vs_inp, rich_folk$Name)}
      
      # if correct # selected, create plot
      if (length(values$c_sel) > 0 & 
          length(values$c_sel) < 6 & 
          length(values$vs_inp) > 0 &
          !is.na(vs_sel)) {
        plot_vs(match(values$c_sel, wealth$Name), input$vs, vs_sel, 
                wealth, hist_fig, fict_char, rich_folk)
      }
      
    })
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")
