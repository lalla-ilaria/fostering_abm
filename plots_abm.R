
#STORE DEMOGRAPHIC VARIABLES
#####
p_reproduction_simple <- c(rep (0, 15),          #pre-reproductive
                           rep (0.1, 45-15),     #reproductive ages
                           rep (0, 80-45))      #post-reproductive

#fertility as used in white 2015 ABM
p_reproduction_white2015 <- c( rep(0, 10), #1-10 years
                               rep(0.01, 5),#11-15
                               rep(0.15, 5),#16-20
                               rep(0.25, 5),#21-25
                               rep(0.28, 10),#26-35
                               rep(0.25, 5),#36-40
                               rep(0.15, 5),#41-45
                               rep(0.08, 5),#46-50
                               rep(0.01, 5),#51-55
                               rep(0, 80-55)#56-80
)
p_death_simple <- c(rep (0.015, 79), 1) #constant p death until 100
p_death_white2015 <- c(0.07,0.06,0.05,0.04,0.03,#1-5 years
                       rep(0.02,5),#6-10
                       rep(0.015, 30),#11-40
                       rep(0.018, 5),#41-45
                       rep(0.02, 5),#46-50
                       rep(0.03, 5),#51-55
                       rep(0.04, 5),#56-60
                       rep(0.08, 5),#61-65
                       rep(0.12, 5),#66-70
                       rep(0.20, 5),#71-75
                       rep(0.30, 4),#76-79
                       1# certain death at 80, like ballad of Narayama (1958)
)
net_production_simple <- as.integer(c( seq(-9,10, 1), rep(10, 40), seq(10,-9, -1) ))

net_production_ache <- c(-1141.0, -1405.0, -1668.0, -1961.0, -2078.0, -2137.0, -2166.0, -2137.0, -2078.0, -2020.0, -2020.0, -2049.0, -2078.0, -2166.0, -2224.0, -2283.0, -1726.5, -907.5, -439.0, -176.0, 29.0, 234.0, 410.0, 556.0, 790.0, 966.0, 1171.0, 1346.0, 1551.0, 1727.0, 1902.0, 2078.0, 2107.0, 2078.0, 2020.0, 1990.0, 1932.0, 1844.0, 1785.0, 1756.0, 1698.0, 1668.0, 1580.0, 1522.0, 1472.0, 1422.0, 1372.0,  1322.0, 1272.0,  1222.0, 1172.0, 1122.0, 1072.0, 1022.0, 972.0, 922.0, 872.0, 822.0, 772.0, 722.0, 672.0, 622.0, 572.0, 522.0, 472.0, 422.0, 372.0, 322.0, 272.0, 222.0, 172.0, 122.0, 72.0, 22.0, -28.0, -78.0, -128.0, -178.0, -228.0, -278.0)
#####

#PLOT DEMOGRAPHIC VARIABLES
#######

simp_col <- "#B0511e"
real_col <- "#1eb094"

#death risk
png("../images/p_death.png", width = 15, height = 10, units = "cm", res = 500)
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(1:80, p_death_simple, type = "l", xlab = "Age", ylab = "p death", col = simp_col, ljoin = 2, lty = 2, lwd = 3)
lines(1:80, p_death_white2015, col = real_col, ljoin = 2, lty = 1, lwd = 3)
abline(h =0, col = "grey80", ljoin = 2, lty = 3, lwd = 2)
legend(1,0.98, legend=c("Simple", "Realistic (White2015)"),
       col=c(simp_col, real_col), lty=2:1, lwd = 3, cex=0.9,
       box.lty=0)
dev.off()

#fertility
png("../images/p_repro.png", width = 15, height = 10, units = "cm", res = 500)
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(1:80, p_reproduction_simple, ylim = c(0, 0.3), type = "l", xlab = "Age", ylab = "p reproduction", col = simp_col, ljoin = 2, lty = 2, lwd = 3)
abline(h =0, col = "grey80", ljoin = 2, lty = 3, lwd = 2)
lines(1:80, p_reproduction_white2015, col = real_col, ljoin = 2, lty = 1, lwd = 3)
legend(48,0.29, legend=c("Simple", "Realistic (White2015)"),
       col=c(simp_col, real_col), lty=2:1, lwd = 3, cex=0.9,
       box.lty=0)
dev.off()

#net production
png("../images/net_production.png", width = 15, height = 10, units = "cm", res = 500)
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(1:80, net_production_simple, ylim = c(-22, 22), type = "l", xlab = "Age", ylab = "net production", col = simp_col, ljoin = 2, lty = 2, lwd = 3)
abline(h =0, col = "grey80", ljoin = 2, lty = 3, lwd = 2)
lines(1:80, net_production_ache/100, col = real_col, ljoin = 2, lty = 1, lwd = 3)
lines(46:80, (net_production_ache/100)[46:80], col = '#67dbe4', ljoin = 2, lty = 1, lwd = 3)
legend(35,-14, legend=c("Simple", "Realistic (Ache, Kaplan 1996)"),
       col=c(simp_col, real_col), lty=2:1, lwd = 3, cex=0.9,
       box.lty=0)
dev.off()
######


