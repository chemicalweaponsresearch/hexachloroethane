library(viridis)

gtrends <- read.csv("gtrends.csv")
cans <- read.csv("daily_hc_cans.csv")
head(gtrends)
day <- as.Date(gtrends$Day)
cday <- as.Date(cans$day)

y <- gtrends[,4]
x <- 1:length(y)
mod3 <- loess(y ~ x, span = 0.12, degree = 1)
py3 <- predict(mod3)

y <- gtrends[,3]
x <- 1:length(y)
mod2 <- loess(y ~ x, span = 0.12, degree = 1)
py2 <- predict(mod2)

y <- gtrends[,2]
x <- 1:length(y)
mod1 <- loess(y ~ x, span = 0.12, degree = 1)
py1 <- predict(mod1)

cols <- viridis(3, 0.6, 0.1, 0.6, option = "A")


windows(12, 6)

par(bty = "U", mar = c(3, 5, 4, 5))
plot(cday, cans[ , 2], type = "h", lwd = 1, col = 0, las = 1, 
     xlab = "", ylab = "Hexachloroethane Cans Deployed",
     xlim = range(day), ylim = c(0, 6), cex.lab = 1.5, cex.axis = 1.25)
rect(as.Date("2020-07-01"), 0, as.Date("2020-09-10"), 5.8,
     col = rgb(0.47, 0.53, 0.42, 0.5), border = NA)
axis(1, at = seq(min(day) - 5, max(day) + 5, 1), labels = FALSE, tck = -0.01)
for(i in 1:nrow(cans)){
  rect(cday[i] - 0.4, 0, cday[i] + 0.4, cans[i, 2], border = grey(0.2), 
       col = grey(0.6), lwd = 2)
}

abline(v = as.Date("2020-05-25"), lwd = 3)
text(as.Date("2020-05-25"), 6.9, "Murder of", cex = 1.25, 
     font = 2, xpd = TRUE)
text(as.Date("2020-05-25"), 6.5, "George Floyd", cex = 1.25, 
     font = 2, xpd = TRUE)

text(as.Date("2020-07-01"), 6.5, "Federal PACTF", cex = 1.25, font = 2,
     col = rgb(0.47, 0.53, 0.42, 0.6), adj = 0, xpd = TRUE)
text(as.Date("2020-07-01"), 6.1, "Deployment", cex = 1.25, font = 2,
     col = rgb(0.47, 0.53, 0.42, 0.66), adj = 0, xpd = TRUE)



par(new = TRUE)
plot(day, gtrends[ , 2], type = "n", lwd = 3, col = cols[1], las = 1, 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(4, las = 1, cex.axis = 1.25)
mtext(side = 4, "Google Search Rate (Normalized)", line = 3, cex = 1.5)
points(day, gtrends[ , 4], type = "p", lwd = 2, col = cols[3], pch = 1)
points(day, gtrends[ , 3], type = "p", lwd = 2, col = cols[2], pch = 1)
points(day, gtrends[ , 2], type = "p", lwd = 2, col = cols[1], pch = 1)

points(day, py3, type = "l", lwd = 5, col = cols[3])
points(day, py2, type = "l", lwd = 5, col = cols[2])
points(day, py1, type = "l", lwd = 5, col = cols[1])

text(day[length(day)] + 3, 118, "`Protests`", col = cols[1], font = 2, 
     cex = 1.5, adj = 1, xpd = TRUE)
text(day[length(day)] + 3, 110, "`Black Lives Matter`", col = cols[2], 
     font = 2, cex = 1.5, adj = 1, xpd = TRUE)
text(day[length(day)] + 3, 102, "`Federal`", col = cols[3], font = 2, 
     cex = 1.5, adj = 1, xpd = TRUE)


par(new = TRUE)
plot(cday, cans[ , 2], type = "h", lwd = 1, col = 0, las = 1, 
     xlab = "", ylab = "Hexachloroethane Cans Deployed",
     xlim = range(day), ylim = c(0, 6), cex.lab = 1.5, cex.axis = 1.25)
for(i in 1:nrow(cans)){
  rect(cday[i] - 0.4, 0, cday[i] + 0.4, cans[i, 2], border = grey(0.3, 0.3), 
       col = grey(0.6, 0.3), lwd = 2)
}