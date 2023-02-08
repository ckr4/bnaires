# libraries
library(dplyr)
library(echarts4r)
library(ggplot2)
library(waffle)
library(ggthemes)

inset_l <- function(user_name, user_nw) {
  
  # set conversion factor
  bill_l <- 6.14 # length in inches
  
  hundo_vert_uri <- "image://data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBzdGFuZGFsb25lPSJ5ZXMiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1MiIgaGVpZ2h0PSIxMDAiPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0wIDBMMSAxTDAgMHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzMzMzQzMjsgc3Ryb2tlOm5vbmU7IiBkPSJNMS42MDM0IDEuMDI3NzhMMCAxMEwwIDM2TDAgODNMMS42MDM0IDk4Ljk3MjJMMjkgMTAwTDUwLjM5NjYgOTguOTcyMkw1MiA5MEw1MiA2NEw1MiAxN0w1MC4zOTY2IDEuMDI3NzhMMjMgMEwxLjYwMzQgMS4wMjc3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNNTEgMEw1MiAxTDUxIDBNMi42MDMzOSAyLjAyNzc4TDEgMTFMMSAzNkwxIDgyTDIuNzQyMjggOTcuOTcyMkwyOSA5OUw0OS4zOTY2IDk3LjgyMUw1MSA4OUw1MSA2NEw1MSAxOEw0OS4zOTY2IDIuMDI3NzhMMjMgMUwyLjYwMzM5IDIuMDI3Nzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTEzLjU0NDggNS4wMjc3OEMtMi43MDY5NCAxNi4yOTQzIDQgNDcuMjU0OSA0IDY1QzQgNzAuNjgyMSAyLjEyNDQ2IDgwLjQwNCA0LjYwMzM5IDg1LjU4NTZDOS4xNDg2MSA5NS4wODY0IDI5LjE4MTEgMTAxLjQwMiAzOC40NTUyIDk0Ljk3MjJDNTQuNzA2OSA4My43MDU3IDQ4IDUyLjc0NTEgNDggMzVDNDggMjkuMzE3OSA0OS44NzU1IDE5LjU5NiA0Ny4zOTY2IDE0LjQxNDRDNDIuODUxNCA0LjkxMzYgMjIuODE4OSAtMS40MDE1NCAxMy41NDQ4IDUuMDI3Nzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTYgNDFMNyA0MUMxNS40NzI5IDI0LjY3NzkgMzYuNTI3MSAyNC42Nzc5IDQ1IDQxQzQ3LjEzNjggMzUuOTA3OCA0OC4wNDMyIDE5LjM5MzYgNDQuOTczIDE0LjU5NDFDMzYuOTkzOSAyLjEyMDk5IDE1LjAwNjEgMi4xMjA5OSA3LjAyNzAxIDE0LjU5NDFDMy4zMzczNCAyMC4zNjE5IDYgMzQuMjQ4NyA2IDQxeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojMzMzNDMyOyBzdHJva2U6bm9uZTsiIGQ9Ik0yNC4zNzU4IDkuMDI3NzdDMjAuNjM1OSAxMS42NjY4IDI2LjU5ODMgMTUuMTY1NyAyOS4zNzczIDEyLjk3MjJDMzIuOTA5MSAxMC4xODQ2IDI3LjE5MzYgNy4wMzkzOCAyNC4zNzU4IDkuMDI3Nzd6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTI0IDEwQzI1Ljk1ODEgMTIuOTk0NCAyOC4wNDE5IDEyLjk5NDQgMzAgMTBMMjQgMTB6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTI1LjE0ODEgMTUuNzg1NUMyMC4xNDc1IDE3LjcyMjcgMjQuODU1MyAyMi4xNzA5IDI4LjY3ODIgMjAuMzk1OEMzMi44NjE3IDE4LjQ1MzQgMjguNDY5OCAxNC40OTg3IDI1LjE0ODEgMTUuNzg1NXoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjMgMThMMjMgMTlMMzAgMjBMMzAgMTdMMjMgMTh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTIyIDIzTDI2IDI3TDI1IDI0TDMxIDI0TDIyIDIzeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0yMSAzMC41MjkzQy00LjE1MDAzIDM2LjM0MTQgNS43Mzk1IDc1LjMwODMgMzEgNjkuNDcwN0M1Ni4xNSA2My42NTg2IDQ2LjI2MDUgMjQuNjkxNyAyMSAzMC41MjkzeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojMzMzNDMyOyBzdHJva2U6bm9uZTsiIGQ9Ik0yMCA0MUwxNSA0OEwxMCA0OEwxMCA1MkwxNSA1MkMxOC4xNDE3IDY3LjEwODggMjcuNzE5NiA1MS4xMTYzIDMwIDQ1QzM1Ljc3MiA0OS44NTkgMzAuODk1OCA1My4wOTc5IDMwIDU5TDQyIDUyTDQyIDQ4QzM4LjIxNTcgNDYuODE5NSAzNy4yMjA1IDQzLjI1MjIgMzMuNzg3IDQxLjc0MzFDMjUuNTIwNiAzOC4xMDk2IDIzLjE4ODcgNDguMjU2MyAyMiA1NEwxOSA1NEwyMiA0NkwyMCA0MXoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNNiA1OUw2IDg2TDE1LjM3NSA5My4zOTY2TDM2LjYxMDMgOTMuMzk3NEw0NiA4Nkw0NiA1OUw0NSA1OUMzNC45ODU2IDc4LjI5MTYgMTcuNTE0OSA3MS43NjA4IDYgNTl6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTI0LjM3NTggNzUuMDI3OEMyMC42MzU5IDc3LjY2NjggMjYuNTk4MyA4MS4xNjU3IDI5LjM3NzMgNzguOTcyMkMzMi45MDkxIDc2LjE4NDYgMjcuMTkzNiA3My4wMzk0IDI0LjM3NTggNzUuMDI3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjQgNzhMMzAgNzhDMjguMDQxOSA3NS4wMDU2IDI1Ljk1ODEgNzUuMDA1NiAyNCA3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzMzMzQzMjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjQuMzYxMSA4Mi4wMjc4QzIwLjIyMTkgODQuODQzMyAyNi4zNzQzIDg4LjQyMSAyOS4zNzgxIDg1Ljk3NjFDMzIuOTA5NyA4My4xMDE0IDI3LjE3NzggODAuMTExOCAyNC4zNjExIDgyLjAyNzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTI0IDgzQzI1Ljk1ODEgODUuOTk0NCAyOC4wNDE5IDg1Ljk5NDQgMzAgODNMMjQgODN6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTIzIDg4TDI1IDkzTDI2IDkzTDI1IDkwTDMxIDkwTDMxIDg4TDIzIDg4eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0wIDk5TDEgMTAwTDAgOTlNNTEgOTlMNTIgMTAwTDUxIDk5eiIvPgo8L3N2Zz4K"
  
  bill_sel <- 100 # set denomination of bill to use (hard coded to 100)
  
  # calculate length of each ind net worth in selected denomination of bills
  length_miles = round(user_nw / bill_sel * bill_l / 12 / 5280, 2)
  
  if (nchar(user_name) > 12) {
    user_name <- strsplit(user_name, " ")[[1]][1]
  }
  
  df_length <- data.frame(
    x=user_name,
    value=length_miles,
    nw=user_nw
  )
  
  df_length %>%
    e_charts(x) %>%
    e_pictorial(value, bind=nw,
                symbol=hundo_vert_uri,
                symbolRepeat=TRUE,
                symbolSize=c(30,60),
                symbolMargin='1',
                symbolClip=TRUE) %>%
    e_image_g(
      top=-240, left=-240, z=-999,
      style = list( # crop image to exclude moon? )
        width=1200, height=900,
        image = "https://i.imgur.com/yB9ivPf.jpg",
        opacity = 1)
    #e_color(background='black'
    ) %>%
    e_title("", 
            x='right', padding=list(24,24,0,0), 
            textStyle=list(
              color="white", fontWeight='bold', fontSize=16, lineHeight=20),
    ) %>%
    e_legend(show=FALSE) %>%
    e_x_axis(splitLine=list(show=FALSE), 
             axisLabel=list(interval=0, rotate=45, color='#051931', margin=12,
                            fontWeight='bold')) %>%
    e_y_axis(splitLine=list(show=FALSE),
             axisLabel=list(color='white'),
             max=250000, position='left',
             name='Miles', nameLocation='end',
             nameTextStyle= list(
               color='white', fontSize=16, padding=list(0,0,0,-59))
    ) %>%
    e_grid(top=90, bottom=108, left=60, right=36) %>%
    #e_theme("dark-blue") %>%
    e_tooltip(formatter=htmlwidgets::JS("
                                      function(params){
                                      return('$' + '<strong>' + params.name + '</strong>' + 
                                      ' in $100 bills laid end' + 
                                      '<br />to end would stretch ' + '<strong>' + 
                                      echarts.format.addCommas(params.value[1]) + 
                                      '</strong>' + ' miles');
                                      }
                                      ")
    ) %>%
    e_datazoom(type='slider', show='true', yAxisIndex= 0, 
               start=0, end=200*length_miles/250000)
}

# test plot
#inset_l('alexander graham bell', 100000000)

######################################################

inset_h <- function(c_sel, bill_sel, wealth, user_name, user_nw) {
  
  options(scipen=999)
  
  # icon for stack of bills
  stack2_uri <- "image://data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBzdGFuZGFsb25lPSJ5ZXMiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzODAiIGhlaWdodD0iMTM1Ij4KPHBhdGggc3R5bGU9ImZpbGw6IzAxMDEwMTsgc3Ryb2tlOm5vbmU7IiBkPSJNMCAwTDAgMTM1TDM4MCAxMzVMMzgwIDBMMCAweiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojNDU1ZjUzOyBzdHJva2U6bm9uZTsiIGQ9Ik0xMyA4TDEzIDQ1TDE0LjAyNzggNTguMzk2NkwyMyA2MEw0NiA2MEwxNDIgNjBMMTQyIDU5TDE0IDU5QzE0IDQ0LjY4OTIgMTguNTA4NCAyMS4xMjczIDEzIDh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiM2Y2NkYTE7IHN0cm9rZTpub25lOyIgZD0iTTE0IDhMMTQgNTlMMTQxIDU5TDE0MSA4TDE0IDh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiM4NTgzNzU7IHN0cm9rZTpub25lOyIgZD0iTTE0MSA4TDE0MSA1OUwxNDIgNTlDMTQyIDQ0LjY4OTEgMTQ2LjUwOCAyMS4xMjczIDE0MSA4TTE1OCA4TDE1OCA1OUwxNTkgNTlDMTU5IDQ0LjY4OTEgMTYzLjUwOCAyMS4xMjczIDE1OCA4eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojZjZmMmNlOyBzdHJva2U6bm9uZTsiIGQ9Ik0xNTkgOEwxNTkgNTlMMjI3IDU5TDIyNyA4TDE1OSA4eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojNmNjZGExOyBzdHJva2U6bm9uZTsiIGQ9Ik0yNDQgOEwyNDQgNTlMMzcyIDU5TDM3MiA4TDI0NCA4eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojNDU1ZjUzOyBzdHJva2U6bm9uZTsiIGQ9Ik0xNTggNTlMMTU4IDYwTDIyNyA2MEwyMDUgNTlMMTU4IDU5TTI0NCA1OUwyNDQgNjBMMzcyIDYwQzM2MS43MzQgNTUuNjkyMyAzNDQuMTU4IDU5IDMzMyA1OUwyNDQgNTlNOCA3NEw4IDc1TDEzNiA3NUMxMjUuNzM0IDcwLjY5MjMgMTA4LjE1OCA3NCA5NyA3NEw4IDc0eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojODU4Mzc1OyBzdHJva2U6bm9uZTsiIGQ9Ik0xNTMgNzRMMTUzIDc1TDIyMSA3NUwyMjEgMTI2QzIyNS4zMDggMTE1LjczNCAyMjIgOTguMTU4MSAyMjIgODdDMjIyIDgzLjg0OTIgMjIzLjIxNiA3Ny4yNjEgMjIwLjM5NyA3NS4wMjc4QzIxNi43MDYgNzIuMTA0OSAyMDYuNTEzIDc0IDIwMiA3NEwxNTMgNzR6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiM0NTVmNTM7IHN0cm9rZTpub25lOyIgZD0iTTIyMSA3NEwyMjIgNzVMMjIxIDc0TTIzOCA3NEwyMzggNzVMMzY2IDc1TDM2NSAxMjZMMzY2IDEyNkMzNzAuNTEyIDExMC43MzYgMzY3IDg5LjkzMTQgMzY3IDc0TDIzOCA3NHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzZjY2RhMTsgc3Ryb2tlOm5vbmU7IiBkPSJNOCA3NUw4IDEyNkwxMzYgMTI2TDEzNiA3NUw4IDc1eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojZjZmMmNlOyBzdHJva2U6bm9uZTsiIGQ9Ik0xNTMgNzVMMTUzIDExMUwxNTQuMDI4IDEyNC4zOTdMMTcxIDEyNkwyMjEgMTI2TDIyMSA3NUwxNTMgNzV6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiM4NTgzNzU7IHN0cm9rZTpub25lOyIgZD0iTTIzOCA3NUwyMzggMTI1TDIzOSAxMjVDMjM5IDExMC45NTEgMjQzLjQwOSA4Ny44ODk3IDIzOCA3NXoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzZjY2RhMTsgc3Ryb2tlOm5vbmU7IiBkPSJNMjM5IDc1TDIzOSAxMjZMMzMzIDEyNkwzNTYgMTI2TDM2NC45NzIgMTI0LjM5N0wzNjYgMTExTDM2NiA3NUwyMzkgNzVNMTUzIDEyNUwxNTQgMTI2TDE1MyAxMjV6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiM0NTVmNTM7IHN0cm9rZTpub25lOyIgZD0iTTIzOCAxMjVMMjM5IDEyNkwyMzggMTI1eiIvPgo8L3N2Zz4K"
  
  # set conversion factor
  bill_th <- .0043 # thickness in inches
  
  img_ref_d <- c(1,5,10,20,50, 100)
  img_ref_max_y <- c(16500, 4400, 4400, 750, 320, 240)
  img_refs <- c("https://imgur.com/Zp3EIPC.png",
                "https://imgur.com/Oz7EN0u.png",
                "https://imgur.com/Oz7EN0u.png",
                "https://imgur.com/D2NDrdU.png",
                "https://imgur.com/Y3sies5.png", 
                "https://imgur.com/Y3sies5.png")
  img_ref_t <- c(3,3,3,3,3,-120)
  img_ref_l <- c(-636,-636,-636,-636,-636,-819)
  img_ref_h <- c(492,492,492,492,492,650)
  img_ref_w <- c(770,770,770,770,770,1008)
  img_ref_txt <- c("","","",
                   "Iridium Satellite\nAltitude: 485 miles",
                   "","")
  img_ref_rot <- c(0,0,0,0,0,0)
  img_ref_os <- list(c(0,0), c(0,0), c(0,0), c(200,146), c(0,0), c(0,0))
  
  # calculate height based on wealth and bill denomination and convert to feet
  ht_miles <- round(wealth$NetWorth[c_sel]*1000000000 / bill_sel * bill_th / 12 / 5280, 1)
  img_sel <- match(bill_sel, img_ref_d)
  ht_max_y <- img_ref_max_y[img_sel]
  
  user_ht <- round(user_nw / bill_sel * bill_th / 12 / 5280, 6)
  
  # create dataframe for plotting
  df_ht <- data.frame(
    x=user_name,
    value=user_ht,
    nw=paste0(user_nw, ',', bill_sel, ',', round(user_ht * 5280, 1))
  )
  
  # create chart
  df_ht %>%
    e_charts(x) %>%
    e_pictorial(value, bind=nw,
                symbol=stack2_uri,
                symbolRepeat=TRUE,
                symbolSize=c(60,30),
                symbolMargin='0',
                symbolClip=TRUE) %>%
    e_legend(show=FALSE) %>%
    e_x_axis(splitLine=list(show=FALSE), 
             axisLabel=list(interval=0, rotate=45, color='white', 
                            fontWeight='bold', margin=14)) %>%
    e_y_axis(splitLine=list(show=FALSE),
             axisLabel=list(color='white', fontWeight='bold'),
             interval=ht_max_y/4, min=0, max=ht_max_y,
             name="Miles", 
             nameLocation="middle",
             margin=10,
             nameTextStyle= list(
               color='white', fontSize=14, padding=list(0,0,16,0))
    ) %>%
    e_grid(top=70, bottom=110, left=68, right=10) %>%
    e_color(background='black') %>% # get starry night bg for this
    e_image_g(
      top=img_ref_t[img_sel],
      left=img_ref_l[img_sel],
      z=-998,
      style = list(
        height=img_ref_h[img_sel],
        width=img_ref_w[img_sel],
        image = img_refs[img_sel],
        opacity = 1,
        text=img_ref_txt[img_sel],
        textPosition="top",
        textOffset=img_ref_os[[img_sel]],
        textRotation=img_ref_rot[img_sel],
        textAlign='center',
        fontSize=12,
        fontStyle='bold',
        textFill='white')
    ) %>%
    e_tooltip(formatter=htmlwidgets::JS("
                                    function(params){
                                    var vals = params.name.split(',')
                                    return('<strong>' + params.value[0] + '</strong>' + 
                                    '<br />A stack of $' + '<strong>' + vals[0] + 
                                    '</strong> in $' + vals[1] + 
                                    ' bills would<br />be <strong>' + 
                                    echarts.format.addCommas(params.value[1]) + 
                                    '</strong>' + ' miles or <strong>' + 
                                    echarts.format.addCommas(vals[2]) +
                                    '</strong> ft high');
                                    }
                                    ")
    ) %>%
    e_datazoom(type='slider', show='true', left='16px', width='24px',
               yAxisIndex= 0, start=0, 
               end=round_any(200*user_ht/ht_max_y, .0001, f=ceiling)) 
}

#inset_h(c(1,3,5,7,9), 100, wealth, 'Alexander', 500000)

######################################################
inset_w <- function(bill_sel, user_name, user_nw, twoline) {
    
    ### set conversion factors 
    # (www.alliantcreditunion.org/money-mentor/the-dollar-bill-believe-it-or-not)
    bill_wt <- .001 * 2.20462262185 # weight in lbs
    
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
    
    weight_user = user_nw / bill_sel * bill_wt / 2000

    ### set y axis limit based on max weight
    max_y <- 10 * weight_user
    
    if (nchar(user_name) > 18) {
      user_name <- paste0(strwrap(user_name, 18), collapse="\n")
    } else if (twoline) {
      user_name <- paste0(user_name, "\n")
    }
    
    # Make df for plotting
    df_weight <- data.frame(
      x=user_name,
      weight=weight_user,
      nw=user_nw,
      denom=factor(df_const[sel, 1])
    )
    
    df_weight$x <- factor(df_weight$x, levels = unique(df_weight$x))
    pers_lab <- paste0("$", format(user_nw, big.mark=",", trim=TRUE), "\n",
                       round(weight_user / df_const[sel,2],  2), " ", 
                       df_const[sel,5])
   
     # Create plot
p <- ggplot(df_weight, aes(label= x,
                           values = round(weight / df_const[sel,2]),
                           color = denom)) +
        facet_wrap(~factor(x), nrow = 1, strip.position = "bottom") +
        labs(
          title = "",
          subtitle = ""
        ) +
        ggthemes::scale_fill_tableau(name=NULL) +
        theme(panel.grid = element_blank(), axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(legend.position = "none") +
        scale_y_continuous(labels=function(x) x * df_const[sel,2], # x * wt
                     expand = c(0,0),
                     limits = c(0, max_y / (df_const[sel,2])),
                     breaks = seq(0, max_y / (df_const[sel,2]), 
                                  by=max_y / (df_const[sel,2])/4))
    
q <- if (weight_user / df_const[sel,2] > .5) {
        p + 
          geom_text( 
            stat = "waffle", 
            n_rows = 1, 
            make_proportional = FALSE, 
            size = df_const[sel,3], 
            flip = TRUE, 
            family = "fontawesome-webfont",
            color=df_const[sel,4],
            position = position_nudge(y = -.8), 
            vjust=0) +
          scale_x_discrete() +
          scale_label_pictogram(
            name = NULL,
            values = df_weight$denom) +
          geom_text(label=pers_lab,
                    x= 1,
                    y= 1.1 * (weight_user / df_const[sel,2]),
                    color='black')
  } else {
    p +
      geom_text(label=pers_lab,
                    x=.5,
                    y= weight_user / df_const[sel,2],
                    color='black')
  }
  q
}  

# inset_w(100, "Chad", 99900000, FALSE)

###########################################################

inset_v <- function(c_sel, vs, vs_sel, wealth, hist_fig, fict_char, rich_folk, 
                    user_name, user_nw) {
  
  user_name2 <- ifelse(nchar(user_name) > 18, 
                       paste0(strwrap(user_name, 18), collapse="\n"),
                       user_name)
  
  df_u <- data.frame(Name = user_name,
                     NetWorth = as.numeric(user_nw)/1000000000,
                     Name2 = user_name2)
  
  ### subset and sort current list (country, source, gender, age, etc.)
  #c_sel <- c(1:5)
  df_c <- wealth[c_sel,]
  df_c <- df_c[order(-df_c$NetWorth),]
  
  rich_folk$Source <- rep("", length(rich_folk$Name))
  rich_folk$Country <- rep("", length(rich_folk$Name))
  
  ### current list / subset in terms of historical figure or fictional character
  # get selection (hist or fict) then get record and prep for plotting
  #vs <- 'f'
  #vs_sel <- 2
  if (vs == 'h') {
    vs_comp <- hist_fig[vs_sel,c(1,3,2,5,6)]
    vs_img <- hist_fig[vs_sel, 7]
    vs_lab_col <- hist_fig[vs_sel, 9]
    vs_img_src <- hist_fig[vs_sel, 8]
  } else if (vs == 'f') {
    vs_comp <- fict_char[vs_sel,c(1,4,2,6,7)]
    colnames(vs_comp) <- c('Name', 'NetWorth', 'Source', 'Country','Name2')
    vs_img <- fict_char[vs_sel, 8]
    vs_lab_col <- fict_char[vs_sel, 10]
    vs_img_src <- fict_char[vs_sel, 9]
  } else if (vs == 'c') {
    vs_comp <- rich_folk[vs_sel,c(1,2,7,8,3)]
    colnames(vs_comp) <- c('Name', 'NetWorth', 'Source', 'Country','Name2')
    vs_img <- rich_folk[vs_sel, 4]
    vs_lab_col <- rich_folk[vs_sel, 6]
    vs_img_src <- rich_folk[vs_sel, 5]
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
  df_u$comp <- paste0(vs_comp$Name, ',', df_u$NetWorth/vs_comp$NetWorth)
  
  # dev.new(width=6.367, height=3.2, unit="in")
  # create plot
  ## -------------> get images for other hist figs and fict chars
  df_u %>%
    # arrange(NetWorth) %>%
    # mutate(Name=factor(Name, levels=Name)) %>%
    e_charts(Name2) %>%
    e_pictorial(NetWorth, symbol, bind=comp, 
                #symbolRepeat=img_rpt, z=-1,
                barWidth='96%',
                symbolClip=TRUE,
                symbolSize=symb_size,
                label=list(show=TRUE,
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
                                        ")),
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
    e_pictorial(NetWorth, symbol,
                symbolRepeat=FALSE, z=-998,
                barWidth='96%',
                symbolClip=FALSE,
                symbolSize=c(min(106, vs_w),vs_ht),
                itemStyle=list(opacity=.09),
                tooltip=list(show=FALSE)
    ) %>%
    e_x_axis(splitLine=FALSE,
             axisTick=FALSE, 
             axisLine=FALSE, 
             axisLabel=list(interval=0, margin=12, color='black')
    ) %>%
    e_y_axis(splitLine=FALSE,
             axisTick=FALSE,
             axisLabel=FALSE,
             interval=max(df_c$NetWorth, vs_comp$NetWorth), min=0,
             max=max(df_c$NetWorth, vs_comp$NetWorth)
    ) %>%
    e_grid(top=56, bottom=76, left=10, right=10) %>%
    # e_labels(show=TRUE,
    #          position="insideBottom", 
    #          color='white',
    #          fontSize=12, 
    #          fontStyle='bold', 
    #          backgroundColor=vs_lab_col,
    #          lineHeight=20, 
    #          offset=c(0,1), 
    #          borderRadius=5, 
    #          padding=list(1,0,0,0),
    #          formatter=htmlwidgets::JS("
    #                     function(params){
    #                     var val = params.name.split(',')
    #                     var pct = Math.round(val[1] * 1000).toFixed(2) / 10
    #                     return(' ' + params.value[1] + ' | ' + pct + '% ');
    #                     }
    #                     ")
    # ) %>%
    e_legend(show=FALSE) %>%
    e_theme("fivethirtyeight") %>%
    e_tooltip(trigger='item')
  
}

#grDevices::dev.size("px")
# inset_v(c(1,2,3,4,5), "h", 10, wealth, hist_fig, fict_char, 
#         'Kenny Lee Jones', 500000000)


