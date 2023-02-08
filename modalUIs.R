
aboutUI <- navlistPanel(widths=c(3,9),
                        
                        tabPanel(
                          title="Purpose", 
                          
                          p(" A billion is a very big number. So big, in fact, that it's just about impossible 
  to wrap your head around. That doesn't really matter most of the time, but when it
  comes to wealth inequality, understanding the sheer scale of that number matters. 
  So, to provide some perspective on just how much money a billion dollars is, this
  tool provides a series of visualizations of the scale of the wealth of the world's
  richest people were it to be held in physical currency, specifically, in some 
  denomination of dollar bills."),
                          p("If you've ever wondered how heavy 150 billion dollars in hundred dollar bills
  would be, expressed in terms of Saturn V rockets, or how high a stack of that many
  bills would reach - tip: think miles, not feet - or how far they'd stretch when
  laid end-to-end, which is going to blow your mind, you're in luck."),
                          p("Also, just for fun, you can compare the world's current 25 richest people with
  some of the richest individuals in history, excluding rulers and others who derived
  their wealth from their positions in government, and the richest fictional characters 
  ever. How does Elon Musk compare to Andrew Carnegie or John D. Rockefeller. Does 
  Scrooge McDuck, a cartoon caricature of greed, have more money than Jeff Bezos, a 
  real-life caricature of greed? Is Carlisle Cullen, a vampire who's been collecting 
  compound interest for hundreds of years, as rich as a modern day Croesus?"),
                          p("The answers await.")
                          
                        ),
                        
                        tabPanel(
                          title="Sources", 
                          
                          tags$b(tags$u("Current billionaires and richest celebrities")),
                          br(),
                          a(href='https://www.forbes.com/real-time-billionaires/#1cf229123d78', 
                                 "Forbes: The World's Real-Time Billionaires",
                                 target="_blank"),
                          p("Note: You can click on each indivdiual in this list to see a short profile."),
                          br(),
                          tags$b(tags$u('Fictional Characters:')),
                          br(),
                          a(href='https://www.payset.io/post/the-top-20-richest-villains',
                                 'Payset: The Top 20 Richest Villains',
                                 target="_blank"),
                          br(),
                          a(href='https://www.forbes.com/special-report/2013/fictional-15/',
                                 'The Forbes Fictional 15',
                                 target="_blank"),
                          br(),
                          br(),
                          tags$b(tags$u('Historical Figures:')),
                          br(),
                          a(href='https://www.visualcapitalist.com/wp-content/uploads/2017/08/richest-people-in-human-history.html',
                                 'The Money Project Presents: The Richest People in History Up Until the Industrial Revolution',
                                 target="_blank"),
                          br(),
                          a(href='https://money.com/the-10-richest-people-of-all-time-2/',
                                 'Money: The 10 Richest People of All Time',
                                 target="_blank"),
                          br(),
                          a(href='https://allthatsinteresting.com/richest-people-in-history',
                                 "All That's Interesting: The Ten Richest People of All Time",
                                 target="_blank"),
                          br(),
                          br(),
                          tags$b(tags$u('Physical dimensions of dollar bills')),
                          br(),
                          a(href='https://www.alliantcreditunion.org/money-mentor/the-dollar-bill-believe-it-or-not',
                                 'The dollar bill dimensions and fun facts: believe it or not',
                                 target="_blank"),
                          br(),
                          br(),
                          tags$b(tags$u('Images used for backgrounds of Height charts:')),
                          br(),
                          a(href='https://www.pexels.com/photo/starry-sky-998641/',
                                 'Starry night: Photo by Francesco Ungaro', target="_blank"),
                          br(),
                          a(href='https://www.pexels.com/photo/planet-earth-220201/',
                                 'Planet Earth 220201', target="_blank"),
                          br(),
                          a(href='https://upload.wikimedia.org/wikipedia/commons/e/e1/The_station_pictured_from_the_SpaceX_Crew_Dragon_5_%28cropped%29.jpg',
                                 'ISS: The station pitured from the SpaceX Crew Dragon 5',
                                 target="_blank"),
                          br(),
                          a(href='https://education.nationalgeographic.org/resource/gps-satellite',
                                 'GPS satellite: by NOAA', target="_blank"),
                          br(),
                          a(href='https://spacenews.com/ses-orders-four-more-o3b-mpower-satellites-from-boeing/',
                                 'O3B mPower Satellites: by SES', target="_blank"),
                          br(),
                          a(href='https://www.highspeedsat.com/iridium-satellite.php',
                                 'Iridium Satellites', target="_blank")
                        ),
                        
                        tabPanel(
                          title="Disclaimer", 
                          
                          p("The data and art used to make these tools are in the public domain or 
            belong to the artists and organizations that produced them and/or own 
            their rights. I have repurposed the materials solely to highlight and
            contextualize the staggering immensity of the wealth some individuals
            possess. Please don't sue me."),
                          # TODO -----------------> Add info on difficult of:
                          p("Estimates of net worth are educated guess work taken from a variety of
            sources using an assortment of methods. They should only be used as a frame of refernce
            from which to assess the comparative wealth of the individuals and characters for which
            they are provided.")
                          
                        ) # Close Disclaimers tabPanel
  ) # Close navlistPanel


helpUI <- navlistPanel(widths=c(3,9),
                       
                       tabPanel(
                         title="Length", 
                         p("Select up to 15 current billionaires to display using 
            drop down list in sidebar, which can be filtered using age slider and
            country/territory drop down list. Denomination cannot be changed on
            this tab, as the plot is only calculated based on the networth of
            of the selected individuals in $100 bills.")
                         
                       ),
                       
                       tabPanel(
                         title="Height", 
                         p("Select up to 5 current billionaires to display using 
            drop down list in sidebar, which can be filtered using age slider and
            country/territory drop down list. Denomination can be set to $1, $5,
            $10, $20, $50, or $100 bills.")
                       ),
                       
                       tabPanel(
                         title="Weight", 
                         p("Select up to 5 current billionaires to display using 
            drop down list in sidebar, which can be filtered using age slider and
            country/territory drop down list. Denomination can be set to $1, $5,
            $10, $20, $50, or $100 bills.")
                       ),
                       
                       tabPanel(
                         title="Comparison", 
                         p("Unselect categories in legend to remove from chart.
            Double-click on a category in the legend to show only that category.
            Select autoscale in menu to zoom to selected categories. Select 
            current billionaires to display using drop down list in sidebar,
            which can be filtered using age slider and country/territory drop
            down list.")
                       ),
                       
                       tabPanel(
                         title="Versus",
                         p("Select up to 5 current billionaires to display using 
            drop down list in sidebar, which can be filtered using age slider and
            country/territory drop down list. Use the historical figure, 
            fictional character and richest celebrities buttons to determine which 
            list is available in the drop down list, then select the figure or 
            character to use as the basis for comparison to the current billionaires
            you have selected.")
                       ) # Close Versus tabPanel
                       
) # Close navlistPanel

inclModal <- function(name_fail = FALSE, nw_fail_l = FALSE, nw_fail_h = FALSE) { 
  modalDialog(
    fluidRow(
      column(12, align='center',
        textInput("ti_incl", "Enter your name"),
        if (name_fail) {HTML("Ensure name is between 1 and 20 characters.")},
        numericInput("ni_incl", "Enter your net worth", value=121700, min=0, max=999999999),
        if (nw_fail_l) {
          HTML("I don't doubt that you owe more than you've got, but I can't <br>
          graph that, so please pretend you have 0 or more dollars.") },
        if (nw_fail_h) {HTML("You don't have a billion dollars. Calm down.")},
        h6("* The median networth of a U.S. household was $121,700 in 2019")
      )
    ),
    footer = tagList(
      fluidRow(
        column(12, align='center',
          actionButton("mod_canx", "Cancel", width='100px', 
                       style='color: white; 
                              border-radius=5px;
                              font-weight: bold;'),
          actionButton("incl_ok", "OK", width='100px', 
                       style='color: white; 
                              border-radius=5px; 
                              font-weight: bold;')
        )
      ),
      tags$style(".modal-dialog {width: fit-content !important;}
                  #mod_canx {background-color: #4582ec;}
                  #mod_canx:hover {background-color: #2a6bda}
                  #incl_ok {background-color: #4582ec;} 
                  #incl_ok:hover {background-color: #2a6bda;")

    )
  )
}
