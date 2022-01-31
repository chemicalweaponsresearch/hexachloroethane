# pb2020 data

api_url <- function(...){
  url_base <- "https://api.846policebrutality.com/api/incidents"
  url_filters <- filters(...)
  paste0(url_base, "?", url_filters)
}

filters <- function(...){
  filter_list <- list(...)
  nfilters <- length(filter_list)
  if(nfilters == 0){
    return()
  }
  filters_text <- character(nfilters)
  for(i in 1:nfilters){
    filters_text[i] <- filter_text(filter_list[i])
  }
  paste0(filters_text, collapse = "&")
}


filter_text <- function(filter_list_entry){
  filter_name <- names(filter_list_entry)
  filter_entries <- entries(filter_list_entry)
  paste0("filter[", filter_name, "]=", filter_entries)
}

entries <- function(entry){
  nentries <- length(entry)
  all_entries <- ""
  for(i in 1:nentries){
    new_entry <- gsub(" ", "+", entry[[i]])
    all_entries <- paste0(all_entries, new_entry, collapse  = ",")
  }
  all_entries
}


pbs <- function(...){
  RESTful_url <- api_url(...)
  pbs_json_response <- GET(RESTful_url)
  pbs_json <- content(pbs_json_response, as = "text", encoding = "UTF-8")
  fromJSON(pbs_json)
}


chem_tags <- c("gas", "marking-round", "pepper-ball", "pepper-spray", 
               "spray", "tear-gas", "tear-gas-canister")

chems <- function(pbs_list, tags = chem_tags){
  data_tags <- pbs_list$data$tags
  ndata_tags <- length(data_tags)
  keepers <- rep(FALSE, ndata_tags)
  for(i in 1:ndata_tags){
    keepers[i] <- any(data_tags[[i]] %in% tags)
  }
  pbs_list$data <- pbs_list$data[keepers, ]
  pbs_list
}


# jags model

run_model <- function(execute = TRUE){
  if(execute == FALSE){
    return(invisible())
  }

  summary <- read.csv("summary_data.csv")

  OR <- summary$cans_observed_recovered[48:78]
  OnR <- summary$cans_observed_not_recovered[48:78]
  nOR <- summary$cans_not_observed_recovered[48:78]
  FT <- summary$time_out[48:78] / 60
  C <- summary$cans[48:78]
  crowd <- (summary$crowd_size[48:78])/max(summary$crowd_size[48:78])

  # Model in the JAGS format

  model <- "model { 
    lambda_r ~ dnorm(0, 1)
    nu_r ~ dnorm(0, 1)
    rho_r ~ dnorm(0, 1)
    beta ~ dnorm(0, 1)
    sigma ~ dunif(0, 100)

    nu <- ilogit(nu_r)
    rho <- ilogit(rho_r)
    tau <- 1 / sigma

    for(i in 1:31){ 
      eps[i] ~ dnorm(0, tau)
      lambda_i[i] <- exp(lambda_r + eps[i])
      D[i] ~ dpois(lambda_i[i] * FT[i])T(C[i], )
      O[i] ~ dbinom(nu, D[i])
      R[i] ~ dbinom(rho, D[i])
      OR[i] ~ dbinom(rho, O[i])T( , R[i])
    } 

    TD <- sum(D)
  }"

  data <- list(O = OR + OnR, R = nOR + OR, OR = OR, FT = FT, C = C)
  inits1 <- list(lambda_r = -0.2,
                 nu_r = 1, rho_r = 1, sigma = 10,
                 .RNG.name = "base::Super-Duper", .RNG.seed = 1)
  inits2 <- list(lambda_r = -0.2,
                 nu_r = 1, rho_r = -1, sigma = 20,
                 .RNG.name = "base::Super-Duper", .RNG.seed = 2)
  inits3 <- list(lambda_r = 0.2,
                 nu_r = -1, rho_r = 1, sigma = 10,
                 .RNG.name = "base::Super-Duper", .RNG.seed = 1)
  inits4 <- list(lambda_r = 0.2,
                 nu_r = -1, rho_r = -1, sigma = 30,
                 .RNG.name = "base::Super-Duper", .RNG.seed = 4)

  results <- run.jags(model = model, 
                      monitor = c("TD", "D", 
                                  "lambda_r", "lambda_r", 
                                  "nu_r", "rho_r", "sigma"), 
                      data = data, n.chains = 4, method = "rjags", 
                      inits = list(inits1, inits2, inits3, inits4),
                      adapt = 10000, burnin = 100000, sample = 10000, 
                      thin = 100)

  save(results, file = "results.RData")
  write.csv(summary(results), "results_table.csv")

  mcmcs <- rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]],
                 results$mcmc[[4]])
  write.csv(mcmcs, "mcmc_table.csv")
  D_tables <- apply(mcmcs[,1:32], 2, table)
  D_tables_names <- vector("list", length = 32)

  for(i in 1:32){
    temp <- D_tables[[i]]
    D_tables[[i]] <- as.numeric(temp)
    D_tables_names[[i]] <- names(temp)
  }
  D_tables_and_names <- list(D_tables, D_tables_names)
  write_yaml(D_tables_and_names, "D_tables_and_names.yaml")
  calculate_fatalities(mcmcs)
}

# results text 

results_text <- function(){

  mcmcs <- read.csv("mcmc_table.csv")

  fatalities <- read.csv("fatalities_table.csv")

  print("TD")
  print(quantile((mcmcs$TD), c(0.025, 0.5, 0.975)))
  print("lambda")
  print(quantile(exp(mcmcs$lambda_r), c(0.025, 0.5, 0.975)))
  print("nu")
  print(quantile(exp(mcmcs$nu_r)/(exp(mcmcs$nu_r) + 1), c(0.025, 0.5, 0.975)))
  print("rho")
  print(quantile(exp(mcmcs$rho_r)/(exp(mcmcs$rho_r) + 1), 
                 c(0.025, 0.5, 0.975)))
  print("fatalities")
  print(quantile(fatalities[, 2], c(0.025, 0.5, 0.975)))
}

calculate_fatalities <- function(mcmcs, seed = 1){
  set.seed(seed)
  TD <- mcmcs[, "TD"]
  doses <- TD * 412.3
  n <- length(TD)
  fatalities <- numeric(n)
  for(i in 1:n){
    remain <- doses[i]
    while(remain > 0){
      person <- (LD50() * weight()) / 1000
      remain_t <- remain - person
      if(remain_t > 0){
        fatalities[i] <- fatalities[i] + 1
        remain <- remain_t
      } else{
        frac <- remain / person
        fatalities[i] <- fatalities[i] + frac
        remain <- 0
      }
    }    
  }

  write.csv(fatalities, "fatalities_table.csv")

}


LD50 <- function(n = 1){
  vals <- c(350, 58, 1100, 24, 1260, 91, 330, 200) 
  logvals <- log(vals)
  exp(rnorm(n, mean(logvals), sd(logvals)))
}

weight <- function(n = 1){
  exp(rnorm(n, 4.42, 0.22))
}


# tables

table1 <- function(){
  results <- read.csv("results_table.csv")
  out <- results[c(1, 33:36),]
  out[ ,2] <- round(out[ ,2], 3)
  out[ ,3] <- round(out[ ,3], 3)
  out[ ,4] <- round(out[ ,4], 3)
  out[ ,5] <- round(out[ ,5], 3)
  out[ ,6] <- round(out[ ,6], 2)
  out[ ,8] <- round(out[ ,8], 3)
  out[ ,11] <- round(out[ ,11], 3)
  out[ ,12] <- round(out[ ,12], 2)
  write.csv(out, "table1.csv")
}

# figures 

fig1 <- function(){
  us_all <- pbs()
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

  png("fig1.png", width = 12, height = 9, units = "cm", res = 200)

  par(mar = c(4.5, 2.5, 1, 2))
  plot(1, 1, type = "n", bty = "U", xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "", xlim = c(1, 17), ylim = c(0, 400))
  for(i in 1:17){
    rect(i - 0.25, 0, i + 0.25, city_counts_chem2$Freq[i], lwd = 1)
    rect(i - 0.25, 0, i + 0.25, city_counts_chem2$chem[i], lwd = 1,
         col = grey(0.65))
  }

  rect(16, 350, 16.5, 375)
  rect(16, 300, 16.5, 325, col = grey(0.65))
  text(15.75, 365, "Total", adj = 1, font = 2, cex = 0.5)
  text(15.75, 315, "Chemical", adj = 1, font = 2, cex = 0.5)

  axis(2, at = seq(0, 400, 100), cex = 0.5, tck = -0.03, labels = FALSE)
  axis(2, at = seq(0, 400, 100), cex.axis = 0.5, line = -0.5, lwd = 0,
       las = 1)
  axis(2, at = seq(0, 400, 50), labels = FALSE, tck = -0.015)

  axis(1, at = 1:17, labels = FALSE, tck = -0.01)
  text(1:17, rep(-30, 16), city_counts_chem2$Var1, xpd = TRUE, srt = 45, 
       cex = 0.5, adj = 1)

  mtext(side = 2, "Incidents", line = 1.5, cex = 0.75)

  par(new = TRUE)
  plot(1:17, population_sizes$population, type = "o", lwd = 2, pch = 16, 
       cex = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", 
       ylim = c(0, 8.5))  
  points(1:17, population_sizes$population, type = "p", lwd = 2, pch = 16, 
         cex = 1, col = "white")
  points(1:17, population_sizes$population, type = "p", lwd = 2, pch = 1, 
         cex = 1)
  axis(4, at = seq(0, 8, 2), line = -0.5, lwd = 0, las = 1, cex.axis = 0.5)
  axis(4, at = seq(0, 8, 2), labels = FALSE, tck = -0.03)
  axis(4, at = seq(0, 8, 1), labels = FALSE, tck = -0.015)
  mtext(side = 4, "Population (million people)", line = 0.75, cex = 0.75)

  dev.off()
}

fig2 <- function(){

  summary <- read.csv("summary_data.csv")

  png("fig2.png", width = 11, height = 11, units = "cm", res = 200)
  par(fig = c(0, 1, 0.6, 1), mar = c(3, 4, 2, 4))

  day <- as.Date(summary$day)
  x <- 1:length(day)

  y <- summary$gt_black_lives_matter
  mod_blm <- loess(y ~ x, span = 0.1)
  py_blm <- predict(mod_blm)

  y <- summary$gt_protests
  mod_p <- loess(y ~ x, span = 0.1)
  py_p <- predict(mod_p)

  y <- summary$gt_federal
  mod_f <- loess(y ~ x, span = 0.1)
  py_f <- predict(mod_f)

  colsp <- viridis(3, 0.6, 0.15, 0.75, option = "A")
  colsl <- viridis(3, 0.8, 0.15, 0.75, option = "A")

  plot(day, summary$gtrend_federal, type = "n", lwd = 3, col = colsp[1],  
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 105), 
       las = 1, bty = "L")
  axis(2, las = 1, at = seq(0, 100, 25), cex.axis = 0.6, line = -0.6, lwd = 0)
  axis(2, las = 1, at = seq(0, 100, 25), cex.axis = 0.6, 
       labels = FALSE, tck = -0.03)
  axis(2, las = 1, labels = FALSE, at = seq(0, 100, 5), tck = -0.015)
  mtext(side = 2, "Google Trends", line = 1.5, cex = 0.75)

  lb <- as.character(c("May 15", "June 1", "June 15", "July 1", "July 15",
                       "Aug 1", "Aug 15", "Sep 1"))
  axis(1, at = day[c(1, 18, 32, 48, 62, 79, 93, 110)], labels = FALSE, 
       tck = -0.04, cex.axis = 0.6)
  axis(1, at = seq(min(day) - 5, max(day) + 5, 1), labels = FALSE, 
       tck = -0.01)
  text(y = -15, x = day[c(1, 18, 32, 48, 62, 79, 93, 110)], cex = 0.6, lb,
       srt = 45, xpd = TRUE, adj = 1)

  points(day, summary$gt_protests, type = "p", lwd = 1, col = colsp[3],
         cex = 0.6, pch = 16)
  points(day, summary$gt_blm, type = "p", lwd = 1, col = colsp[2],
         cex = 0.6, pch = 16)
  points(day, summary$gt_federal, type = "p", lwd = 1, col = colsp[1],
         cex = 0.6, pch = 16)

  points(day, py_p, type = "l", lwd = 2, col = colsl[3])
  points(day, py_blm, type = "l", lwd = 2, col = colsl[2])
  points(day, py_f, type = "l", lwd = 2, col = colsl[1])

  text(day[1], 115, "\"Protests\"", col = colsl[3], font = 2, 
       cex = 0.6, adj = 0, xpd = TRUE)
  text(day[41] - 12, 115, "\"Black Lives Matter\"", col = colsl[2], 
       font = 2, cex = 0.6, adj = 0, xpd = TRUE)
  text(day[81] - 3, 115, "\"Federal\"", col = colsl[1], font = 2, 
       cex = 0.6, adj = 0, xpd = TRUE)


  par(fig = c(0, 1, 0, 0.7), mar = c(3, 4.5, 2, 3), new = TRUE, bty = "U")
  
  x<-1:31

  day <- as.Date(summary$day)[48:78]
  cans <- summary$cans[48:78]
  scs <- summary$crowd_size[48:78] / max(summary$crowd_size[48:78]) * 100
  y <- scs
  mod_scs <- loess(y ~ x, span = 0.5)
  py_scs <- predict(mod_scs)

  sto <- summary$time_out[48:78] / max(summary$time_out[48:78]) * 100
  y <- sto
  mod_sto <- loess(y ~ x, span = 0.5)
  py_sto <- predict(mod_sto)

  colsbp <- viridis(2, 0.6, 0.3, 0.6, option = "A")
  colsbl <- viridis(2, 0.8, 0.3, 0.6, option = "A")

  plot(day, scs, type = "n", lwd = 3, col = colsp[1], las = 1, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 100),
       xlim = c(min(day) - 3, max(day)))

  lab1 <- seq(0, 3000, length.out = 4)
  axis(2, las = 1, at = seq(0, 100, length.out = 4), 
       cex.axis = 0.6, line = -0.5, 
       labels = lab1, lwd = 0, col.axis = colsbl[1])
  axis(2, las = 1, at = seq(0, 100, length.out = 4), cex.axis = 0.6, 
       labels = FALSE, tck = -0.03)
  axis(2, las = 1, labels = FALSE, at = seq(0, 100, length.out = 7), 
       tck = -0.015)
  axis(2, las = 1, labels = FALSE, at = seq(0, 100, length.out = 31), 
       tck = -0.008)

  lab2 <- seq(0, 160, 30)
  axis(2, las = 1, at = seq(0, 160, 30) / 160 * 100, 
       cex.axis = 0.6, line = -1.5, hadj = 0,
       labels = lab2, lwd = 0, col.axis = colsbl[2])
  axis(2, las = 1, at = seq(0, 160, 30) / 160 * 100, cex.axis = 0.6, 
       labels = FALSE, tck = 0.03)
  axis(2, las = 1, labels = FALSE, at = seq(0, 160, 10) / 160 * 100, 
       tck = 0.015)

  mtext(side = 2, c("Fed Time Out", "and", "Crowd Size"), at = c(22, 50, 75),
        line = 1.75, cex = 0.75, col = c(colsbl[2], 1, colsbl[1]))

  points(day, scs, type = "p", pch = 16, lwd = 1, col = colsbp[1], cex = 0.75)
  points(day, sto, type = "p", pch = 16, lwd = 1, col = colsbp[2], cex = 0.75)

  points(day, py_scs, type = "l", lwd = 3, col = colsbl[1])
  points(day, py_sto, type = "l", lwd = 3, col = colsbl[2])

  par(new = TRUE)

  plot(day, cans, type = "n", lwd = 1, col = 0, las = 1, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       xlim = c(min(day) - 3, max(day)), ylim = c(0, 7))
  axis(1, at = seq(min(day), max(day), 1), labels = FALSE, tck = -0.01)
  lblx <- as.character(format(seq(min(day), max(day), 7), "%B %d"))
  lblx <- gsub("0", "", lblx)
  axis(1, at = seq(min(day), max(day), 7), labels = FALSE, tck = -0.025)
  text(y = -0.8, x = seq(min(day), max(day), 7), lblx, srt = 14, xpd = TRUE,
       adj = 1, cex = 0.6)

  axis(4, las = 1, at = seq(0, 7, 1), cex.axis = 0.6, line = -0.6, lwd = 0)
  axis(4, las = 1, at = seq(0, 7, 1), cex.axis = 0.6, 
       labels = FALSE, tck = -0.03)
  axis(4, las = 1, labels = FALSE, at = seq(0, 7, 1), tck = -0.015)
  mtext(side = 4, "HC Grenades Deployed", line = 1, cex = 0.75)

  points(day, cans, type = "l", lwd = 2, col = grey(0.2, 0.7)) 

  for(i in 1:nrow(summary)){
    points(day[i], cans[i], cex = 0.75, pch = 15, lwd = 1, 
           col = grey(0.2, 0.5))
    points(day[i], cans[i], cex = 0.75, pch = 0, lwd = 1, 
           col = grey(0.2, 0.7))
  }

  par(fig = c(0, 1, 0, 1), mar = c(1, 1, 1, 1), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
       bty = "n", xlim = c(0, 1), ylim = c(0, 1))
  points(c(0.24, 0.45), c(0.625, 0.675), type = "l", lwd = 2) 
  points(c(0.24, 0.24), c(0.6, 0.625), type = "l", lwd = 2) 
  points(c(0.45, 0.45), c(0.675, 0.75), type = "l", lwd = 2) 

  points(c(0.92, 0.65), c(0.625, 0.675), type = "l", lwd = 2) 
  points(c(0.92, 0.92), c(0.6, 0.625), type = "l", lwd = 2) 
  points(c(0.65, 0.65), c(0.675, 0.75), type = "l", lwd = 2) 

  dev.off()
}

fig5 <- function(){

  D_tables_and_names <- read_yaml("D_tables_and_names.yaml")
  D_tables <- D_tables_and_names[[1]]
  for(i in 1:32){
    names(D_tables[[i]]) <- D_tables_and_names[[2]][[i]]
  }
  DT <- D_tables[[1]]
  mcmcs <- read.csv("mcmc_table.csv")

  png("fig5.png", width = 9, height = 6, units = "cm", res = 200)
  par(mfrow = c(1, 2), mar = c(2, 2, 1, 0))

  plot(1, 1, type = "n", xlim = c(25, 45), ylim = c(0, 0.5), xaxt = "n", 
       yaxt = "n", xlab = "", ylab = "", bty = "L")
  for(i in 1:length(DT)){
    x <- as.numeric(names(DT)[i])
    y <- DT[i] / sum(DT)
    rect(x - 0.15, 0, x + 0.15, y, col = grey(0.5))
  }
  axis(1, at = seq(25, 45, 6), cex = 0.5, tck = -0.025, labels = FALSE)
  axis(1, at = seq(25, 45, 5), cex = 0.5, tck = -0.025, labels = FALSE)
  axis(1, at = seq(25, 45, 5), cex.axis = 0.5, line = -1.15, lwd = 0)
  axis(1, at = seq(25, 45, 1), labels = FALSE, tck = -0.015)
  mtext(side = 1, "HC Grenades Deployed", line = 0.75, cex = 0.75)

  axis(2, at = seq(0, 0.5, 0.1), cex = 0.5, tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.5, 0.1), cex = 0.5, tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.5, 0.1), cex.axis = 0.5, line = -0.5, lwd = 0, 
       las = 1)
  axis(2, at = seq(0, 0.5, 0.05), labels = FALSE, tck = -0.03)

  mtext(side = 2, "Probability Density", line = 1.25, cex = 0.75)

  lambda <- exp(mcmcs$lambda_r)
  spots <- seq(0, max(lambda) + 0.01 * 2, 0.01)
  spots1 <- spots[-length(spots)]
  nspots <- length(spots)
  nspots1 <- length(spots1)
  dens <- numeric(nspots1)
  for(i in 1:nspots1){
    incl <- which(lambda >= spots[i] & lambda < spots[i+1])
    dens[i] <- length(lambda[incl]) / length(lambda)
  }


  plot(1, 1, type = "n", xlim = c(0, 1.3), ylim = c(0, 0.07), xaxt = "n", 
       yaxt = "n", xlab = "", ylab = "", bty = "L")
  points(spots1, dens, type = "l", lwd = 1)

  axis(1, at = seq(0, 1.2, 0.2), cex = 0.5, tck = -0.025, labels = FALSE)
  axis(1, at = seq(0, 1.2, 0.2), cex = 0.5, tck = -0.025, labels = FALSE)
  axis(1, at = seq(0, 1.2, 0.2), cex.axis = 0.5, line = -1.15, lwd = 0)
  axis(1, at = seq(0, 1.3, 0.1), labels = FALSE, tck = -0.015)
  mtext(side = 1, expression(lambda), line = 0.75, at = 0.2, cex = 1)
  mtext(side = 1, "(grenades / hr)", line = 0.65, at = 0.75, cex = 0.75)

  axis(2, at = seq(0, 0.07, 0.01), cex = 0.5, tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.07, 0.01), cex = 0.5, tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.07, 0.01), cex.axis = 0.5, line = -0.5, lwd = 0, 
       las = 1)
  axis(2, at = seq(0, 0.07, 0.005), labels = FALSE, tck = -0.03)

  dev.off()
}


