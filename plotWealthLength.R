# libraries
library(dplyr)
library(echarts4r)

plot_l <- function(c_sel, wealth) {
  
  # set conversion factor
  # (www.alliantcreditunion.org/money-mentor/the-dollar-bill-believe-it-or-not)
  bill_l <- 6.14 # length in inches
  
  #### infograph of length of bills end to end
  hundo_vert_uri <- "image://data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBzdGFuZGFsb25lPSJ5ZXMiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1MiIgaGVpZ2h0PSIxMDAiPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0wIDBMMSAxTDAgMHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzMzMzQzMjsgc3Ryb2tlOm5vbmU7IiBkPSJNMS42MDM0IDEuMDI3NzhMMCAxMEwwIDM2TDAgODNMMS42MDM0IDk4Ljk3MjJMMjkgMTAwTDUwLjM5NjYgOTguOTcyMkw1MiA5MEw1MiA2NEw1MiAxN0w1MC4zOTY2IDEuMDI3NzhMMjMgMEwxLjYwMzQgMS4wMjc3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNNTEgMEw1MiAxTDUxIDBNMi42MDMzOSAyLjAyNzc4TDEgMTFMMSAzNkwxIDgyTDIuNzQyMjggOTcuOTcyMkwyOSA5OUw0OS4zOTY2IDk3LjgyMUw1MSA4OUw1MSA2NEw1MSAxOEw0OS4zOTY2IDIuMDI3NzhMMjMgMUwyLjYwMzM5IDIuMDI3Nzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTEzLjU0NDggNS4wMjc3OEMtMi43MDY5NCAxNi4yOTQzIDQgNDcuMjU0OSA0IDY1QzQgNzAuNjgyMSAyLjEyNDQ2IDgwLjQwNCA0LjYwMzM5IDg1LjU4NTZDOS4xNDg2MSA5NS4wODY0IDI5LjE4MTEgMTAxLjQwMiAzOC40NTUyIDk0Ljk3MjJDNTQuNzA2OSA4My43MDU3IDQ4IDUyLjc0NTEgNDggMzVDNDggMjkuMzE3OSA0OS44NzU1IDE5LjU5NiA0Ny4zOTY2IDE0LjQxNDRDNDIuODUxNCA0LjkxMzYgMjIuODE4OSAtMS40MDE1NCAxMy41NDQ4IDUuMDI3Nzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTYgNDFMNyA0MUMxNS40NzI5IDI0LjY3NzkgMzYuNTI3MSAyNC42Nzc5IDQ1IDQxQzQ3LjEzNjggMzUuOTA3OCA0OC4wNDMyIDE5LjM5MzYgNDQuOTczIDE0LjU5NDFDMzYuOTkzOSAyLjEyMDk5IDE1LjAwNjEgMi4xMjA5OSA3LjAyNzAxIDE0LjU5NDFDMy4zMzczNCAyMC4zNjE5IDYgMzQuMjQ4NyA2IDQxeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojMzMzNDMyOyBzdHJva2U6bm9uZTsiIGQ9Ik0yNC4zNzU4IDkuMDI3NzdDMjAuNjM1OSAxMS42NjY4IDI2LjU5ODMgMTUuMTY1NyAyOS4zNzczIDEyLjk3MjJDMzIuOTA5MSAxMC4xODQ2IDI3LjE5MzYgNy4wMzkzOCAyNC4zNzU4IDkuMDI3Nzd6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTI0IDEwQzI1Ljk1ODEgMTIuOTk0NCAyOC4wNDE5IDEyLjk5NDQgMzAgMTBMMjQgMTB6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTI1LjE0ODEgMTUuNzg1NUMyMC4xNDc1IDE3LjcyMjcgMjQuODU1MyAyMi4xNzA5IDI4LjY3ODIgMjAuMzk1OEMzMi44NjE3IDE4LjQ1MzQgMjguNDY5OCAxNC40OTg3IDI1LjE0ODEgMTUuNzg1NXoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjMgMThMMjMgMTlMMzAgMjBMMzAgMTdMMjMgMTh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTIyIDIzTDI2IDI3TDI1IDI0TDMxIDI0TDIyIDIzeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0yMSAzMC41MjkzQy00LjE1MDAzIDM2LjM0MTQgNS43Mzk1IDc1LjMwODMgMzEgNjkuNDcwN0M1Ni4xNSA2My42NTg2IDQ2LjI2MDUgMjQuNjkxNyAyMSAzMC41MjkzeiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojMzMzNDMyOyBzdHJva2U6bm9uZTsiIGQ9Ik0yMCA0MUwxNSA0OEwxMCA0OEwxMCA1MkwxNSA1MkMxOC4xNDE3IDY3LjEwODggMjcuNzE5NiA1MS4xMTYzIDMwIDQ1QzM1Ljc3MiA0OS44NTkgMzAuODk1OCA1My4wOTc5IDMwIDU5TDQyIDUyTDQyIDQ4QzM4LjIxNTcgNDYuODE5NSAzNy4yMjA1IDQzLjI1MjIgMzMuNzg3IDQxLjc0MzFDMjUuNTIwNiAzOC4xMDk2IDIzLjE4ODcgNDguMjU2MyAyMiA1NEwxOSA1NEwyMiA0NkwyMCA0MXoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNNiA1OUw2IDg2TDE1LjM3NSA5My4zOTY2TDM2LjYxMDMgOTMuMzk3NEw0NiA4Nkw0NiA1OUw0NSA1OUMzNC45ODU2IDc4LjI5MTYgMTcuNTE0OSA3MS43NjA4IDYgNTl6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTI0LjM3NTggNzUuMDI3OEMyMC42MzU5IDc3LjY2NjggMjYuNTk4MyA4MS4xNjU3IDI5LjM3NzMgNzguOTcyMkMzMi45MDkxIDc2LjE4NDYgMjcuMTkzNiA3My4wMzk0IDI0LjM3NTggNzUuMDI3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6I2MxZDRhNjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjQgNzhMMzAgNzhDMjguMDQxOSA3NS4wMDU2IDI1Ljk1ODEgNzUuMDA1NiAyNCA3OHoiLz4KPHBhdGggc3R5bGU9ImZpbGw6IzMzMzQzMjsgc3Ryb2tlOm5vbmU7IiBkPSJNMjQuMzYxMSA4Mi4wMjc4QzIwLjIyMTkgODQuODQzMyAyNi4zNzQzIDg4LjQyMSAyOS4zNzgxIDg1Ljk3NjFDMzIuOTA5NyA4My4xMDE0IDI3LjE3NzggODAuMTExOCAyNC4zNjExIDgyLjAyNzh6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiNjMWQ0YTY7IHN0cm9rZTpub25lOyIgZD0iTTI0IDgzQzI1Ljk1ODEgODUuOTk0NCAyOC4wNDE5IDg1Ljk5NDQgMzAgODNMMjQgODN6Ii8+CjxwYXRoIHN0eWxlPSJmaWxsOiMzMzM0MzI7IHN0cm9rZTpub25lOyIgZD0iTTIzIDg4TDI1IDkzTDI2IDkzTDI1IDkwTDMxIDkwTDMxIDg4TDIzIDg4eiIvPgo8cGF0aCBzdHlsZT0iZmlsbDojYzFkNGE2OyBzdHJva2U6bm9uZTsiIGQ9Ik0wIDk5TDEgMTAwTDAgOTlNNTEgOTlMNTIgMTAwTDUxIDk5eiIvPgo8L3N2Zz4K"
  
  # make selections
  bill_sel <- 100 # set denomination of bill to use (hard coded to 100)
  # c_sel <- c(1:8) # choose subset of up to 10 current billionaires 
  
  # calculate length of each ind net worth in selected denomination of bills
  length_miles = round(wealth$NetWorth[c_sel]*1000000000 / bill_sel * bill_l / 12 / 5280, -1)
  
  df_length <- data.frame(
    x=wealth$Name2[c_sel],
    value=length_miles,
    nw=wealth$NetWorth[c_sel]
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
      style = list(
        width=1200, height=900,
        image = "https://i.imgur.com/yB9ivPf.jpg",
        opacity = 1)
    ) %>%
    e_title(paste0("Length of wealth in $100 bills laid end to end\n\n\n", 
                   "                                                            ",
                   "                                                            ",
                   "Average distance from Earth\n",
                   "                                                        ",
                   "                                                        ",
                   "     to the Moon is ~250,000 miles"), 
            x='left', padding=list(12,12,12,12), 
            textStyle=list(
              color="white", fontWeight='bold', fontSize=12, lineHeight=20),
    ) %>%
    e_legend(show=FALSE) %>%
    e_x_axis(splitLine=list(show=FALSE), 
             axisLabel=list(interval=0, rotate=45, color='#051931', margin=12,
                            fontWeight='bold')) %>%
    e_y_axis(splitLine=list(show=FALSE),
             axisLabel=list(color='white'),
             max=250000,
             name="Miles", 
             nameLocation="middle",
             nameTextStyle= list(
               color='white', fontSize=20, padding=list(0,0,60,0))
    ) %>%
    e_grid(top=90, bottom=108, left=120, right=55) %>%
    #e_theme("dark-blue") %>%
    e_tooltip(formatter=htmlwidgets::JS("
                                      function(params){
                                      return('<strong>' + params.value[0] + '</strong>' + 
                                      '<br />$' + '<strong>' + params.name + '</strong>' + 
                                      ' billon in $100 bills laid end' + 
                                      '<br />to end would stretch ' + '<strong>' + 
                                      echarts.format.addCommas(params.value[1]) + 
                                      '</strong>' + ' miles');
                                      }
                                      ")
    )
}

# test plot
#plot_l(c(1,2,3,8,10), wealth)
