lambda2color <-
  function(lambda){
    # center [nm]
    r0 = 700.0
    g0 = 546.1
    b0 = 435.8
    o0 = 605.0
    y0 = 580.0
    c0 = 490.0
    p0 = 400.0

    # fwhm
    wR = 90
    wG = 80
    wB = 80
    wO = 60
    wY = 50
    wC = 50
    wP = 40

    # relative int
    iR = 0.95
    iG = 0.74
    iB = 0.75
    iO = 0.4
    iY = 0.1
    iC = 0.3
    iP = 0.3

    # calc normal distribution for responses to colors
    r = iR * exp( - ( lambda - r0 ) * ( lambda - r0 ) / ( wR * wR )  )
    g = iG * exp( - ( lambda - g0 ) * ( lambda - g0 ) / ( wG * wG )  )
    b = iB * exp( - ( lambda - b0 ) * ( lambda - b0 ) / ( wB * wB )  )
    o = iO * exp( - ( lambda - o0 ) * ( lambda - o0 ) / ( wO * wO )  )
    y = iY * exp( - ( lambda - y0 ) * ( lambda - y0 ) / ( wY * wY )  )
    c = iC * exp( - ( lambda - c0 ) * ( lambda - c0 ) / ( wC * wC )  )
    p = iP * exp( - ( lambda - p0 ) * ( lambda - p0 ) / ( wP * wP )  )

    # orange #ffa500  1: 0.715 : 0.230
    # yellow #ffff00  1: 1     : 0
    # cian   #00ff00  0: 1     : 1
    # purple #804080  1: 0.5   : 1

    r = r + o + y + p
    g = g + o*0.715 + y*0.83 + c + p *0.50
    b = b + o*0.23 + c + p

    r <- min(255, round(r*255, 0))
    g <- min(255, round(g*255, 0))
    b <- min(255, round(b*255, 0))

    to_hex <-
      function(x){
        hexed <-
          as.hexmode(x) %>%
          as.character
        if(stringr::str_count(hexed) == 1){
          paste0("0", hexed) %>% return
        } else {
          hexed
        }
      }


    paste0("#", to_hex(r), to_hex(g), to_hex(b)) %>%
      return
  }


rainbow_bar <-
  function(wavelength, y, ...){
    lapply(wavelength, function(i){
      annotate(geom = "point", x = i, y = y, col = lambda2color(i), ...)
    })
  }
