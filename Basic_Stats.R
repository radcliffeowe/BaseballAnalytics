WPI_PD <- read.csv("~/Baseball-Analytics/WPI_PD.csv", header = TRUE, sep = ",")
# @rdname update_table
# @rdname update data table

update_table <- function(){
  for(pid in WPI_PD$PID){
    WPI_PD$ERA[pid] <<- era(pid)
    WPI_PD$WHIP[pid] <<- whip(pid)
    WPI_PD$FIP[pid] <<- fip(pid)
    WPI_PD$xFIP[pid] <<- xfip(pid)
    WPI_PD$KRATE[pid] <<- krate(pid)
    WPI_PD$BBRATE[pid] <<- bbrate(pid)
    WPI_PD$GBRATE[pid] <<- gbrate(pid)
    WPI_PD$FBRATE[pid] <<- fbrate(pid)
    WPI_PD$BABIP[pid] <<- babip(pid)
    WPI_PD$FB_HR[pid] <<- fb_hr(pid)
  }
  write.csv(WPI_PD, "~/Baseball-Analytics/WPI_PD.csv", row.names = TRUE)
}

# @rdname era
# @description Calculate a given pitcher's era
# @param ip, Innings Pitched
# @param er, Earned Runs
# @return earned run average

era <- function(id){
  ip <- WPI_PD$IP[id]
  er <- WPI_PD$ER[id]
  x <- (er/ip)*9
  return(x)
}

# @rdname whip
# @description Calculate a given pitcher's WHIP
# @param bb, Number of walks, not including IBB
# @param hits, Number of hits yielded
# @param ip, Innings Pitched
# @return (bb + hits)/ip

whip <- function(id){
  ip <- WPI_PD$IP[id]
  hits <- WPI_PD$H[id]
  bb <- WPI_PD$BB[id]
  x <- (bb + hits)/ip
  WPI_PD$WHIP[id] <<- x
  return(x)
}

# @rdname fip
# @description Calculate a given pitcher's FIP
# @param hr, Number of homeruns yielded
# @param k, Number of strikeouts
# @param bb, Number of walks, not including IBB
# @param hbp, Number of hit by pitches
# @param ip, Innings Pitched
# @return Fielding Independent Pitching

fip <- function(id){
  hr <- WPI_PD$HR[id]
  k <- WPI_PD$SO[id]
  bb <- WPI_PD$BB[id]
  hbp <- WPI_PD$HBP[id]
  ip <- WPI_PD$IP[id]
  num <- (13*hr + 3*(bb + hbp) - 2*k)
  x <- (num/ip) + 3.1 #FIP constant assumed to be ~3.10
  return(x)
}

# @rdname xfip
# @description Calculate a given pitcher's expected FIP
# @param fb, Number of fly balls yielded (fly outs + xbh)
# @param k, Number of strikeouts
# @param bb, Number of walks, not including IBB
# @param hbp, Number of hit by pitches
# @param ip, Innings Pitched
# Assumptions/Approximations: FIP constant ~3.10, lgHR/FB% ~7%
# @return expected Fielding Independent Pitching

xfip <- function(id){
  fb <- WPI_PD$FB[id]
  k <- WPI_PD$SO[id]
  bb <- WPI_PD$BB[id]
  hbp <- WPI_PD$HBP[id]
  ip <- WPI_PD$IP[id]
  num <- (13*(fb*0.07) + 3*(bb + hbp) - 2*k)
  x <- (num/ip) + 3.1 #FIP constant assumed to be ~3.10
  return(x)  
}

# @rdname krate
# @description Calculate a pitcher's strikeout rate
# @param k, Strikeouts
# @param bf, Batters Faced
# @return % of batters faced that struck out

krate <- function(id){
  k <- WPI_PD$SO[id]
  bf <- WPI_PD$AB[id] + WPI_PD$BB[id]
  x <- k/bf
  return(x*100)
}

# @rdname bbrate
# @description Calculate a pitcher's walk rate
# @param bb, Walks
# @param bf, Batters Faced
# @return % of batters faced that walked

bbrate <- function(id){
  bb <- WPI_PD$BB[id]
  bf <- WPI_PD$AB[id] + WPI_PD$BB[id]
  x <- bb/bf
  return(x*100)
}

# @rdname gbrate
# @description Percentage of balls in play that are ground balls
# @param bip, Balls in play
# @param gb, Ground balls
# @return percent of balls in play that are ground balls

gbrate <- function(id){
  bip <- WPI_PD$AB[id] - WPI_PD$SO[id]
  gb <- WPI_PD$GB[id]
  x <- gb/bip
  return(x*100)
}

# @rdname fbrate
# @description Percentage of balls in play that are fly balls
# @param bip, Balls in play
# @param fb, Fly balls
# @return percent of balls in play that are fly balls

fbrate <- function(id){
  bip <- WPI_PD$AB[id] - WPI_PD$SO[id]
  fb <- WPI_PD$FB[id]
  x <- fb/bip
  return(x*100)
}

# @rdname babip
# @description Batting Average on Balls in Play
# @param h, hits
# @param hr, homeruns
# @param ab, at-bats
# @param k, strikeouts
# @param sf, sac fly
# @return the batting average on balls in play 

babip <- function(id){
  h <- WPI_PD$H[id]
  hr <- WPI_PD$HR[id]
  ab <- WPI_PD$AB[id]
  k <- WPI_PD$SO[id]
  sf <- WPI_PD$SF[id]
  x <- (h-hr)/(ab -k -hr +sf)
  return(x)
}

# @rdname fb_hr
# @description fly ball to home run ratio
# @param fb, fly balls
# @param hr, home runs
# @return the ratio of fly balls to home runs

fb_hr <- function(id){
  fb <- WPI_PD$FB[id]
  hr <- WPI_PD$HR[id]
  return((hr/fb)*100)
}

# @rdname plot_k_bb
# @description scatter plot, SO on x-axis, BB on y-axis
# @return plots a chart

plot_k_bb <- function(){
  plot(WPI_PD$SO, WPI_PD$BB, type = "p", xlab = 'Strikeouts', ylab = 'Walks', axes = TRUE)
  }
