
  #us_all <- pbs()
  incidents <- us_all$data[which(us_all$data$date <= as.Date("2020-12-31")),]

  incidents$state_city <- paste(incidents$city, incidents$state, sep = ", ")
  city_counts <- sort(table(incidents$state_city), decreasing = TRUE)

  us_chem <- chems(us_all)
  chem_incidents <- us_chem$data[
                      which(us_chem$data$date < as.Date("2020-12-31")), ]
  chem_incidents$state_city <- paste(chem_incidents$city, 
                                     chem_incidents$state,
                                     sep = ", ")
  city_chems <- sort(table(chem_incidents$state_city), decreasing = TRUE)

  city_counts_chem <- data.frame(city_counts)
  city_counts_chem$chem <- NA
  for(i in 1:nrow(city_counts_chem)){
    spot <- which(names(city_chems) == city_counts_chem$Var1[i])
    xx <- city_chems[spot]
    xx[length(xx) == 0] <- 0
    city_counts_chem$chem[i] <- xx
  } 

  city_counts_chem2 <- city_counts_chem[city_counts_chem$Freq >= 10, ]
  city_counts_chem2$Var1 <- gsub("DC, ", "", city_counts_chem2$Var1)

  population_sizes <- read.csv("population_sizes.csv")

  jpeg("poster_1.jpg", width = 14, height = 6, units = "in", res = 1000)

  par(mar = c(10, 5.5, 0.5, 5.5))
  plot(1, 1, type = "n", bty = "U", xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "", xlim = c(1, 17), ylim = c(0, 400))

  par(new = TRUE)
  plot(1:17, population_sizes$population, type = "o", lwd = 2, pch = 16, 
       cex = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", 
       ylim = c(0, 8.5), col = grey(0.1))  
  points(1:17, population_sizes$population, type = "p", lwd = 2, pch = 16, 
         col = "white", cex = 1.5)
  points(1:17, population_sizes$population, type = "p", lwd = 2, pch = 1, 
         cex = 1.5, col = grey(0.1))
  axis(4, at = seq(0, 8, 2), line = -0.25, lwd = 0, las = 1, cex.axis = 1.75)
  axis(4, at = seq(0, 8, 2), labels = FALSE, tck = -0.025)
  axis(4, at = seq(0, 8, 1), labels = FALSE, tck = -0.01)
  mtext(side = 4, "Population (million)", line = 2.75, cex = 2.125)

  par(new = TRUE)
  plot(1, 1, type = "n", bty = "U", xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "", xlim = c(1, 17), ylim = c(0, 400))


  for(i in 1:17){
    rect(i - 0.25, 0, i + 0.25, city_counts_chem2$Freq[i], lwd = 2,
         col = grey(0.95, alpha = 0.25))
    rect(i - 0.25, 0, i + 0.25, city_counts_chem2$chem[i], lwd = 2,
         col = grey(0.05, alpha = 0.25))
  }

  rect(16, 350, 16.5, 400, col = grey(0.95, alpha = 0.25), lwd = 2)
  rect(16, 275, 16.5, 325, col = grey(0.05, alpha = 0.25), lwd = 2)
  text(15.75, 375, "Total", adj = 1, font = 2, cex = 1.5)
  text(15.75, 300, "Chemical", adj = 1, font = 2, cex = 1.5)

  axis(2, at = seq(0, 400, 100), cex = 0.5, tck = -0.025, labels = FALSE)
  axis(2, at = seq(0, 400, 100), cex.axis = 1.75, line = -0.25, lwd = 0,
       las = 1)
  axis(2, at = seq(0, 400, 50), labels = FALSE, tck = -0.01)

  axis(1, at = 1:17, labels = FALSE, tck = -0.01)
  text(1:17, rep(-30, 16), city_counts_chem2$Var1, xpd = TRUE, srt = 45, 
       cex = 1.25, adj = 1)

  mtext(side = 2, "Incidents", line = 3.75, cex = 2.125)


  dev.off()









  summary <- read.csv("summary_data.csv")
  mcmc <- read.csv("mcmc_table.csv")


  jpeg("poster_2.jpg", width = 14, height = 6, units = "in", res = 1000)

  day <- as.Date(summary$day)


  
  x<-1:31

  day <- as.Date(summary$day)[48:78]

  scs <- summary$crowd_size[48:78] / max(summary$crowd_size[48:78]) * 100
  y <- scs
  mod_scs <- loess(y ~ x, span = 0.5)
  py_scs <- pmax(0, predict(mod_scs))

  sto <- summary$time_out[48:78] / max(summary$time_out[48:78]) * 100
  y <- sto
  mod_sto <- loess(y ~ x, span = 0.5)
  py_sto <- pmax(0, predict(mod_sto))

  colsbp <- viridis(2, 0.6, 0.3, 0.6, option = "A")
  colsbl <- viridis(2, 0.8, 0.3, 0.6, option = "A")
  midc <- grey(1, 0)

  cans_mean <- apply(mcmc[,3:33], 2, mean)
  cans_low <- apply(mcmc[,3:33], 2, quantile, 0.025) 
  cans_up <- apply(mcmc[,3:33], 2, quantile, 0.975)

  y <- cans_mean
  mod_cans <- loess(y ~ x, span = 0.5)
  py_cans <- pmax(0, predict(mod_cans))


  par(mar = c(4, 6.5, 1, 4.75), bty = "U")

  plot(day, cans, type = "n", lwd = 1, col = 0, las = 1, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       xlim = c(min(day) - 0.5, max(day) + 0.5), ylim = c(0, 7))

  axis(4, las = 1, at = seq(0, 7, 1), cex.axis = 1.75, line = -0.2, lwd = 0)
  axis(4, las = 1, at = seq(0, 7, 1), cex.axis = 1.75, 
       labels = FALSE, tck = -0.02)
  mtext(side = 4, "HC Grenades Deployed", line = 3, cex = 2.125)


  for(i in 1:31){
    points(rep(day[i], 2), c(cans_low[i], cans_up[i]), 
           type = "l", lwd = 2, col = grey(0.2, 0.6))
  }

  points(day, cans, cex = 2.25, pch = 15, lwd = 2, col = "white")
  points(day, cans, cex = 2.25, pch = 0, lwd = 3, col = grey(0.2, 0.6))
  points(day, cans, cex = 2.25, pch = 15, lwd = 2, col = midc)

  points(day, py_cans, type = "l", lwd = 4, col = grey(0.2, 0.6)) 


  par(new = TRUE)

  plot(day, scs, type = "n", lwd = 3, col = colsbp[1], las = 1, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 100),
       xlim = c(min(day) - 0.5, max(day) + 0.5))


  lab1 <- seq(0, 3000, length.out = 4)
  axis(2, las = 1, at = seq(0, 100, length.out = 4), 
       cex.axis = 1.75, line = -0.25, 
       labels = lab1, lwd = 0, col.axis = colsbl[1])
  axis(2, las = 1, at = seq(0, 100, length.out = 4), cex.axis = 1, 
       labels = FALSE, tck = -0.025)
  axis(2, las = 1, labels = FALSE, at = seq(0, 100, length.out = 7), 
       tck = -0.02)
  axis(2, las = 1, labels = FALSE, at = seq(0, 100, length.out = 31), 
       tck = -0.01)

  lab2 <- seq(0, 160, 30)
  axis(2, las = 1, at = seq(0, 160, 30) / 160 * 100, 
       cex.axis = 0.75, line = -2, hadj = 0,
       labels = lab2, lwd = 0, col.axis = colsbl[2])
  axis(2, las = 1, at = seq(0, 160, 30) / 160 * 100, cex.axis = 1, 
       labels = FALSE, tck = 0.03)
  axis(2, las = 1, labels = FALSE, at = seq(0, 160, 10) / 160 * 100, 
       tck = 0.015)

  mtext(side = 2, c("DHS Time Out", "and", "Crowd Size"), at = c(20, 53, 81),
        line = 4.5, cex = 2.125, col = c(colsbl[2], 1, colsbl[1]))

  axis(1, at = seq(min(day), max(day), 1), labels = FALSE, tck = -0.01)
  lblx <- as.character(format(seq(min(day), max(day), 7), "%B %d"))
  lblx <- gsub("0", "", lblx)
  axis(1, at = seq(min(day), max(day), 7), labels = FALSE, tck = -0.025)
  text(y = -12, x = seq(min(day), max(day), 7), lblx, xpd = TRUE, cex = 2)



  points(day, scs, type = "p", pch = 1, lwd = 3, col = colsbp[1], cex = 2.5)
  points(day, scs, type = "p", pch = 16, lwd = 2, col = midc, cex = 2.25)
  points(day, sto, type = "p", pch = 1, lwd = 3, col = colsbp[2], cex = 2.5)
  points(day, sto, type = "p", pch = 16, lwd = 2, col = midc, cex = 2.25)

  points(day, py_scs, type = "l", lwd = 4, col = colsbl[1])
  points(day, py_sto, type = "l", lwd = 4, col = colsbl[2])

  dev.off()










