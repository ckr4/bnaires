# libraries
library(plyr)
library(ggplot2)
library(waffle)
library(ggthemes)
library(emojifont)

plot_w <- function(c_sel, bill_sel, wealth) {
  ### set conversion factors 
  # (www.alliantcreditunion.org/money-mentor/the-dollar-bill-believe-it-or-not)
  bill_wt <- .001 * 2.20462262185 # weight in lbs
  
  #### bar chart of weight of wealth in $1, $5, $10, $20, $50 and $100 bills
  
  ### Set theme and options
  theme_set(theme_fivethirtyeight())
  theme_update(axis.title.y=element_text(size=rel(1.3), angle=90, margin=margin(r=8)))
  # theme_set(theme_gray())
  options(scipen=999)
  
  ### Create constants
  bill_denom <- c(1,5,10,20,50,100)
  sel <- which(bill_denom == as.numeric(bill_sel))
  df_const <- data.frame(
    icon = c('rocket', 'plane', 'plane', 'space-shuttle', 'subway', 'fighter-jet'),
    icon_wt = c(3000, 400, 200, 80, 40, 16),
    icon_sz = c(6.5, 5, 5.5, 4.5, 5, 4.5),
    icon_col = c('red', '#466D1D', 'darkblue', 'blue', 'purple', '#687b88'),
    icon_tit = c('rockets', 'full planes', 'empty planes', 'space shuttles', 'subway cars', 'fighter jets'),
    icon_txt = c('The Saturn V rocket weighed over 3000 tons',
                 'A fully loaded Boeing 747 weighs about 400 tons',
                 'An empty Boeing 747 weighs about 400 tons',
                 'The space shuttle weighed about 80 tons',
                 'The R160 subway car built for the New York City Subway weighs over 40 tons ',
                 'An F-18 Super Hornet, the fighter jet from Top Gun, weighs 16 tons'))
  
  
  ### Calculate weight of wealth of selected individuals
  weight_tons = wealth$NetWorth[c_sel] * 1000000000 / bill_sel * 
    bill_wt / 2000 # Networth in B converted to billions / bill denom *
                   #  bill wt (lbs) / 2000 to convert to tons

  ### set y axis limit based on max weight
  max_y <- 1.14*max(weight_tons)
  max_y <- if(round_any(max_y, 10000, f=ceiling)>=90000) {
    round_any(max_y, 10000, f=ceiling)
  } else if(round_any(max_y, 1000, f=ceiling)>=9000) {
    round_any(max_y, 1000, f=ceiling)
  } else {round_any(max_y, 100, f=ceiling)}
  # if(max(weight_tons) > .8 * max_y){max_y = 1.2*max_y}
  
  # Make df for plotting
  df_weight <- data.frame(
    x=wealth$Name2[c_sel],
    weight=weight_tons,
    nw=wealth$NetWorth[c_sel],
    denom=factor(rep(df_const[sel, 1], length(c_sel)))
  )

  df_weight$x <- factor(df_weight$x, levels = unique(df_weight$x))
  
  # plot size should be ~ 8w x 5.2h
  # Create plot
  ggplot(df_weight, aes(label= x,
                        values = round(weight / df_const[sel,2]),
                        color = denom)) +
    geom_text( 
      stat = "waffle", 
      n_rows = 5, 
      make_proportional = FALSE, 
      size = df_const[sel,3], 
      flip = TRUE, 
      family = "fontawesome-webfont",
      color=df_const[sel,4],
      position = position_nudge(y = -.8), 
      vjust=0) +
    facet_wrap(~factor(x), nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels=function(x) x * df_const[sel,2] * 5, # x * wt * n_rows
                       expand = c(0,0),
                       limits = c(0, max_y / (df_const[sel,2] * 5)),
                       breaks = seq(0, max_y / (df_const[sel,2] * 5), 
                                    by=max_y / (df_const[sel,2] * 5)/4)) +
    geom_text(label=paste0('$', df_weight$nw, ' B\n', 
                           round(df_weight$weight / df_const[sel,2]),
                           ' ', df_const[sel,5]),
              x=3, lineheight=1,
              y=round_any(round(weight_tons / df_const[sel,2]), 5, f=ceiling) / 5 +
                .044 * (max_y / (df_const[sel,2] * 5)), # ht of icons + 2/25th of max y
              color='black',
              size=4.1) +
    scale_label_pictogram(
      name = NULL,
      values = df_weight$denom)+
    ggthemes::scale_fill_tableau(name=NULL) +
    labs(
      title = paste0(" Weight of wealth in $", bill_sel,
                     " bills, measured in ", df_const[sel,5]),
      subtitle = paste0("  ", df_const[sel,6])
    ) +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    ylab('Tons') +
    theme(legend.position = "none")

  
  }  

# plot_w(c(1,3,5,7,26), 5, wealth)
