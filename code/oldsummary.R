library(viridis)
library(runjags)
summary <- read.csv("summary_data.csv")

head(summary)
day <- as.Date(summary$day)

x <- 1:length(day)
y <- summary$crowd_size
plot(y~x)
mod_cs <- loess(y ~ x, span = 0.5)
py_cs <- predict(mod_cs)
points(x, py_cs, type = "l")

y <- summary$time_out
plot(y~x)
mod_to <- loess(y ~ x, span = 0.5)
py_to <- predict(mod_to)
points(x, py_to, type = "l")

y <- summary$gtrend_blm
plot(y~x)
mod_blm <- loess(y ~ x, span = 0.5)
py_blm <- predict(mod_blm)
points(x, py_blm, type = "l")

y <- summary$gtrend_protests
plot(y~x)
mod_p <- loess(y ~ x, span = 0.5)
py_p <- predict(mod_p)
points(x, py_p, type = "l")

y <- summary$gtrend_federal
plot(y~x)
mod_f <- loess(y ~ x, span = 0.5)
py_f <- predict(mod_f)
points(x, py_f, type = "l")

scs <- summary$crowd_size / max(summary$crowd_size) * 100
y <- scs
plot(y~x)
mod_scs <- loess(y ~ x, span = 0.5)
py_scs <- predict(mod_scs)
points(x, py_scs, type = "l")


sto <- summary$time_out / max(summary$time_out) * 100
y <- sto
plot(y~x)
mod_sto <- loess(y ~ x, span = 0.5)
py_sto <- predict(mod_sto)
points(x, py_sto, type = "l")

cols <- viridis(5, 0.6, 0.1, 0.8, option = "A")


windows(12, 6)

par(bty = "U", mar = c(3, 5, 4, 5))
plot(day, summary$cans, type = "h", lwd = 1, col = 0, las = 1, 
     xlab = "", ylab = "Hexachloroethane Cans Deployed", xaxt = "n",
     xlim = range(day), ylim = c(0, 7), cex.lab = 1.5, cex.axis = 1.25)
axis(1, at = seq(min(day) - 5, max(day) + 5, 1), labels = FALSE, tck = -0.01)
lblx <- as.character(format(seq(min(day), max(day), 7), "%B %d"))
lblx <- gsub("0", "", lblx)
axis(1, at = seq(min(day), max(day), 7), labels = lblx, tck = -0.025, 
     cex.axis = 1.25)


for(i in 1:nrow(summary)){
  rect(day[i] - 0.4, 0, day[i] + 0.4, summary$cans[i], border = grey(0.2), 
       col = grey(0.6), lwd = 2)
}


par(new = TRUE)
plot(day, summary$gtrend_federal, type = "n", lwd = 3, col = cols[1], las = 1, 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 100))
axis(4, las = 1, cex.axis = 1.25)
axis(4, las = 1, labels = FALSE, at = seq(0, 100, 10), tck = -0.015)
mtext(side = 4, "Normalized Fed Time, Crowd Size, Google Trends", 
      line = 3.25, cex = 1.5)

points(day, summary$gtrend_protests, type = "p", lwd = 2, col = cols[3],
       cex = 1.5)
points(day, summary$gtrend_blm, type = "p", lwd = 2, col = cols[2],
       cex = 1.5)
points(day, summary$gtrend_federal, type = "p", lwd = 2, col = cols[1],
       cex = 1.5)
points(day, scs, type = "p", lwd = 2, col = cols[4], cex = 1.5)
points(day, sto, type = "p", lwd = 2, col = cols[5], cex = 1.5)


points(day, py_p, type = "l", lwd = 5, col = cols[3])
points(day, py_blm, type = "l", lwd = 5, col = cols[2])
points(day, py_f, type = "l", lwd = 5, col = cols[1])
points(day, py_scs, type = "l", lwd = 5, col = cols[4])
points(day, py_sto, type = "l", lwd = 5, col = cols[5])


text(day[length(day)] - 30, 115, "Fed Time", col = cols[5], font = 2, 
     cex = 1.5, adj = 0, xpd = TRUE)
text(day[length(day)] - 25, 115, "Crowd Size", col = cols[4], font = 2, 
     cex = 1.5, adj = 0, xpd = TRUE)
text(day[length(day)] - 17, 115, "\"Protests\"", col = cols[3], font = 2, 
     cex = 1.5, adj = 0, xpd = TRUE)
text(day[length(day)] - 12, 115, "\"Black Lives Matter\"", col = cols[2], 
     font = 2, cex = 1.5, adj = 0, xpd = TRUE)
text(day[length(day)] - 3, 115, "\"Federal\"", col = cols[1], font = 2, 
     cex = 1.5, adj = 0, xpd = TRUE)

par(new = TRUE)
plot(day, summary$cans, type = "h", lwd = 1, col = 0, las = 1, 
     xlab = "", ylab = "Hexachloroethane Cans Deployed", xaxt = "n",
     xlim = range(day), ylim = c(0, 7), cex.lab = 1.5, cex.axis = 1.25)

for(i in 1:nrow(summary)){
  rect(day[i] - 0.4, 0, day[i] + 0.4, summary$cans[i], border = grey(0.2), 
       col = grey(0.6, 0.1), lwd = 2)
}
