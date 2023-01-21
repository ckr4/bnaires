# libraries
library(ggtext)
library(readr)
library(echarts4r)
library(echarts4r.assets)
library(pins)

plot_vs <- function(c_sel, vs, vs_sel, wealth, hist_fig, fict_char) {
  
  ### subset and sort current list (country, source, gender, age, etc.)
  #c_sel <- c(1:5)
  df_c <- wealth[c_sel,]
  df_c <- df_c[order(-df_c$NetWorth),]
  
  ### current list / subset in terms of historical figure or fictional character
  # get selection (hist or fict) then get record and prep for plotting
  #vs <- 'f'
  #vs_sel <- 2
  if (vs == 'h') {
    vs_comp <- hist_fig[vs_sel,c(1,3,2,5,6)]
    vs_img <- hist_fig[vs_sel, 7]
    vs_lab_col <- hist_fig[vs_sel, 9]
    vs_img_src <- hist_fig[vs_sel, 8]
  } else {
    vs_comp <- fict_char[vs_sel,c(1,4,2,6,7)]
    colnames(vs_comp) <- c('Name', 'NetWorth', 'Source', 'Country','Name2')
    vs_img <- fict_char[vs_sel, 8]
    vs_lab_col <- fict_char[vs_sel, 10]
    vs_img_src <- fict_char[vs_sel, 9]
  }
  
  vs_comp$NetWorth <- as.numeric(vs_comp$NetWorth)
  
  # set chart values
  if (vs_comp$NetWorth < max(df_c$NetWorth)) {
    vs_ht <- vs_comp$NetWorth / max(df_c$NetWorth) * 278
    vs_w <- (1 - (1 - vs_comp$NetWorth / max(df_c$NetWorth)) * .75) * 135
    vs_left <- (158 - vs_w) / 2
    img_rpt <- TRUE
    symb_size <- NA
  } else {
    vs_ht <- 278
    vs_w <- 135
    vs_left <- 12
    img_rpt <- FALSE
    symb_size <- c(min(102, vs_w),vs_ht)
  }
  symbol <- paste0('image://', vs_img)
  vs_title <- 'Comparative Wealth'
  df_c$comp <- paste0(vs_comp$Name, ',', df_c$NetWorth/vs_comp$NetWorth)
  
  # dev.new(width=6.367, height=3.2, unit="in")
  # create plot
  ## -------------> get images for other hist figs and fict chars
  df_c %>%
    # arrange(NetWorth) %>%
    # mutate(Name=factor(Name, levels=Name)) %>%
    e_charts(Name2) %>%
    e_pictorial(NetWorth, symbol, bind=comp, 
                #symbolRepeat=img_rpt, z=-1,
                barWidth='96%',
                symbolClip=TRUE,
                symbolSize=symb_size,
                tooltip=list(
                  formatter=
                    htmlwidgets::JS("
                        function(params){
                        var val = params.name.split(',')
                        var pct = Math.round(val[1] * 1000).toFixed(2) / 10
                        valueFormatter: (pct) => pct
                        return('$<strong>' + params.value[1] + '</strong>' + 
                        ' billion is <strong>' + pct +
                        '</strong>%<br />of a <strong>' + val[0] + '</strong>');
                        }
                        "))
    ) %>%
    e_pictorial(NetWorth, symbol, # remove bg image
                symbolRepeat=FALSE, z=-998,
                barWidth='96%',
                symbolClip=FALSE,
                symbolSize=c(min(106, vs_w),vs_ht),
                itemStyle=list(opacity=.1),
                tooltip=list(show=FALSE)
    ) %>%
    e_x_axis(splitLine = list(show=FALSE),
             axisTick=FALSE, 
             axisLine=list(show=FALSE), 
             axisLabel=list(interval=0, margin=12, color='black')
    ) %>%
    e_y_axis(splitLine=list(show=FALSE),
             interval=max(df_c$NetWorth, vs_comp$NetWorth), min=0,
             max=max(df_c$NetWorth, vs_comp$NetWorth),
             name="Billions of Dollars", 
             nameLocation="middle",
             nameTextStyle=list(fontSize=18, padding=list(0,0,-6,0))
    ) %>%
    e_image_g(
      bottom=76, left=vs_left, z=-999,
      style = list(
        width=vs_w,
        height=vs_ht,
        image = vs_img,
        opacity = 1,
        text=vs_comp$Name2, 
        textPosition='bottom', 
        textOffset=c(0,6),
        fontSize=14, 
        fontStyle='bold', 
        textFill=vs_lab_col
      )
    ) %>%
    e_grid(top=56, bottom=76, left=184, right=14) %>%
    e_labels(show=TRUE, 
             position="insideBottom", 
             color='white',
             fontSize=12, 
             fontStyle='bold', 
             backgroundColor=vs_lab_col,
             lineHeight=20, 
             offset=c(0,1), 
             borderRadius=5, 
             padding=list(1,0,0,0),
             formatter=htmlwidgets::JS("
                        function(params){
                        var val = params.name.split(',')
                        var pct = Math.round(val[1] * 1000).toFixed(2) / 10
                        return(' ' + params.value[1] + ' | ' + pct + '% ');
                        }
                        ")
             ) %>%
    e_legend(show=FALSE) %>%
    e_title(vs_title, 
            x='right', 
            padding=list(24,210,0,0), 
            textStyle=list(fontWeight='bold', 
                           fontSize=18, 
                           color=vs_lab_col,
                           lineHeight=20)
    ) %>%
    e_title(paste0('$', vs_comp$NetWorth,' Billion'), 
            x='left', y='bottom',
            padding=list(0,0,84 + vs_ht,30-nchar(vs_comp$NetWorth)), 
            textStyle=list(fontWeight='bold', 
                           fontSize=16, 
                           color=vs_lab_col,
                           lineHeight=20)
    ) %>%
    e_title(vs_img_src, 
            x='center', y='bottom',
            padding=list(0,0,9,0),
            textStyle=list(fontSize=9.2, color=vs_lab_col)
    ) %>%
    e_theme("fivethirtyeight") %>%
    e_tooltip(trigger='item')

}

#grDevices::dev.size("px")
#plot_vs(c(1,2,3,4,5), "h", 10, wealth, hist_fig, fict_char)

