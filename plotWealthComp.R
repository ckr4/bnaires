# libraries
library(ggtext)
library(ggplot2)
library(plotly)
library(pins)

plot_c <- function(c_sel, wealth, hist_fig, fict_char) {
  
  # get and organize data
  if (length(c_sel)>=1) {
    w_all <- wealth[c_sel,c(1,2,5:7)]
    w_all$cat <- rep('Current Billionaire', length(w_all$Name))
  } else {
    w_all <- data.frame(colnames(wealth)[c(1,2,5:7)])
  }
  h_all <- hist_fig[,c(1,3,2,5,6)]
  h_all$cat <- rep('Historical Figure', length(h_all$Name))
  f_all <- fict_char[,c(1,4,3,6,7)]
  f_all$cat <- rep('Fictional Character', length(f_all$Name))
  colnames(h_all) <- colnames(w_all)
  colnames(f_all) <- colnames(w_all)
  
  # select records from current, hist, and fict, and combine data
  #c_sel <- c(1:25)
  #h_sel <- c(1:5)
  #f_sel <- c(1:5)
  df_all <- rbind(w_all, h_all, f_all)
  df_all <- df_all[order(-df_all$NetWorth),]
  
  # determine sort based on NetWorth (0) or Name (1)
  sort_sel <- 0
  
  # if statement to sort and mutate data based on name or net worth
  p <- if (sort_sel == 0) {
    df_all %>%
      arrange(NetWorth) %>%
      mutate(Name=factor(Name, levels=Name))
  } else {
    df_all %>%
      arrange(desc(Name)) %>%
      mutate(Name=factor(Name, levels=Name))
  } 
  
  # Create sorted bar chart
  q <- p %>% ggplot(aes(x=Name, y=NetWorth, fill=cat, 
               text=paste0(Name, '\n$', NetWorth, ' billion'))) +
    geom_bar(stat='identity', show.legend=TRUE) +
    coord_flip() +
    labs(fill='', x='', y="<span style = 'font-size:14pt'>Billions of Dollars") +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_textbox_simple(
        width=NULL,
        padding=margin(0,0,0,0),
        margin=margin(0,0,0,0),
        r = grid::unit(8, 'pt')
      ),
      plot.margin=unit(c(.4,1,1.1,-.5), 'cm'),
      legend.title = element_blank(),
      legend.direction = 'vertical',
      legend.box.background=element_rect(linetype=1, linewidth=1.5, colour=8)
    )
  
  # Use plotly to get tooltips
  ggplotly(q, tooltip = c("text")) %>%
    layout(legend = list(
      orientation='v',
      x=.65, y=.1,
      bordercolor='black', 
      borderwidth=.9),
      xaxis = list(title=list(standoff=15)),
      hoverlabel=list(bgcolor='white')
    )
  
}

#plot_c(c(1,3,5,8), wealth, hist_fig, fict_char)
