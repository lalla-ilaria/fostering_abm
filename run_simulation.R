#####
#ABM runs
#####
source("simulation_families.R")
#
my_families_simple <- families(foster = 2, N_lineages = 40, N_years = 300, plot_name = "_simple", save_plot = TRUE)
my_families_real <- families(foster = 2, N_lineages = 40, N_years = 300, life_history = "_real", plot_name = "_real", save_plot = TRUE)

list2env(my_families_simple,globalenv())

#plot all descendants for lineage
png("../images/n_ppl_lineage_simple.png", width = 15, height = 10, units = "cm", res = 500)
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', xlim = c(0,N_years), ylim = c(0, max(ppl_lineage[,N_years])+5) , xlab = "years", ylab = "members lineage")
legend(1,max(ppl_lineage[,N_years]), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)
lines(1:N_years, apply(ppl_lineage[1:N_lineages/2,], 2, mean),col = "#46bb29", lwd = 2)
lines(1:N_years, apply(ppl_lineage[(1+N_lineages/2):N_lineages,],2 , mean), col =  "#ff2990", lwd = 2)

for (l in 1:N_lineages) {
  lines(1:N_years, ppl_lineage[l,], col = ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), lwd = 0.3)
  
}
dev.off()

#realistic
list2env(my_families_real,globalenv())

#plot all descendants for lineage
png("../images/n_ppl_lineage_real.png", width = 15, height = 10, units = "cm", res = 500)
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', xlim = c(0,N_years), ylim = c(0, max(ppl_lineage[,N_years])+5) , xlab = "years", ylab = "members lineage")
legend(1,max(ppl_lineage[,N_years]), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)
lines(1:N_years, apply(ppl_lineage[1:N_lineages/2,], 2, mean),col = "#46bb29", lwd = 2)
lines(1:N_years, apply(ppl_lineage[(1+N_lineages/2):N_lineages,],2 , mean), col =  "#ff2990", lwd = 2)

for (l in 1:N_lineages) {
  lines(1:N_years, ppl_lineage[l,], col = ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), lwd = 0.3)
  
}
dev.off()



#####
#FOR THE SERVER
#####

#prep simulation run
N_lineages <- 40
N_years <- 500
N_runs <- 20

ppl_lineage_simp_ptr <- list()
ppl_lineage_real_ptr <- list()


#POOR TO RICH
#####
#run ABM
for( r in 1:N_runs){
  my_families_simple_ptr <- families(foster = 2, N_lineages = N_lineages, N_years = N_years, plot_name = "_simple", save_plot = FALSE)
  my_families_real_ptr <- families(foster = 2, N_lineages = N_lineages, N_years = N_years, life_history = "_real", plot_name = "_real", save_plot = FALSE)
  ppl_lineage_simp_ptr[[r]] <- my_families_simple[["ppl_lineage"]]
  ppl_lineage_real_ptr[[r]] <- my_families_real[["ppl_lineage"]]
  
}

#plot
#plot all descendants for lineage -  simple
png("images/n_ppl_lineage_simple_ptr.png", width = 15, height = 10, units = "cm", res = 500, type="cairo")
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', 
     xlim = c(0,N_years), ylim = c(0, max(unlist(ppl_lineage_simp_ptr))+5) , 
     xlab = "years", ylab = "members lineage",
     main = "Lineage growth with simple life history traits, poor to rich")
legend(1,max(unlist(ppl_lineage_simp_ptr)), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)

#store means for shading
means_foster <- matrix(nrow = N_runs, ncol = N_years)
means_nofoster <- matrix(nrow = N_runs, ncol = N_years)

#calculate means and plot lines
for( r in 1:N_runs){
  means_foster[r,] <- apply(ppl_lineage_simp_ptr[[r]][1:N_lineages/2,], 2, mean)
  means_nofoster[r,] <- apply(ppl_lineage_simp_ptr[[r]][(1+N_lineages/2):N_lineages,],2 , mean)
  
  # for (l in 1:N_lineages) {
  #   lines(1:N_years, ppl_lineage_simp_ptr[[r]][l,], col = col.alpha(ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), 0.3), lwd = 0.4)
  #   
  # }#l
}#r

#shade
x <- c(1:N_years, N_years:1)
y <- c(apply(means_foster, 2, max), rev(apply(means_foster, 2, min)))
polygon(x, y, col = col.alpha("#46bb29", 0.2), lty = 0)
y <- c(apply(means_nofoster, 2, max), rev(apply(means_nofoster, 2, min)))
polygon(x, y, col = col.alpha("#ff2990", 0.2), lty = 0)

#add mean of means
lines(1:N_years, apply(means_foster, 2, mean),col = col.alpha("#46bb29", 0.8), lwd = 2)
lines(1:N_years, apply(means_nofoster,2 , mean), col = col.alpha("#ff2990", 0.8) , lwd = 2)

dev.off()

#plot all descendants for lineage -  realistic
png("images/n_ppl_lineage_real_ptr.png", width = 15, height = 10, units = "cm", res = 500, type="cairo")
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', 
     xlim = c(0,N_years), ylim = c(0, max(unlist(ppl_lineage_real_ptr))+5) , 
     xlab = "years", ylab = "members lineage",
     main = "Lineage growth with realistic life history traits, poor to rich")
legend(1,max(unlist(ppl_lineage_real_ptr)), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)

#store means for shading
means_foster <- matrix(nrow = N_runs, ncol = N_years)
means_nofoster <- matrix(nrow = N_runs, ncol = N_years)

#calculate means and plot lines
for( r in 1:N_runs){
  means_foster[r,] <- apply(ppl_lineage_real_ptr[[r]][1:N_lineages/2,], 2, mean)
  means_nofoster[r,] <- apply(ppl_lineage_real_ptr[[r]][(1+N_lineages/2):N_lineages,],2 , mean)
  
  # for (l in 1:N_lineages) {
  #   lines(1:N_years, ppl_lineage_real_ptr[[r]][l,], col = col.alpha(ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), 0.3), lwd = 0.4)
  #   
  # }#l
}#r

#shade
x <- c(1:N_years, N_years:1)
y <- c(apply(means_foster, 2, max), rev(apply(means_foster, 2, min)))
polygon(x, y, col = col.alpha("#46bb29", 0.2), lty = 0)
y <- c(apply(means_nofoster, 2, max), rev(apply(means_nofoster, 2, min)))
polygon(x, y, col = col.alpha("#ff2990", 0.2), lty = 0)

#add mean of means
lines(1:N_years, apply(means_foster, 2, mean),col = col.alpha("#46bb29", 0.8), lwd = 2)
lines(1:N_years, apply(means_nofoster,2 , mean), col = col.alpha("#ff2990", 0.8) , lwd = 2)

dev.off()


#####

#RICH TO POOR
#####
#run ABM
for( r in 1:N_runs){
  my_families_simple <- families(foster = 2, rich_to_poor = TRUE, N_lineages = N_lineages, N_years = N_years, plot_name = "_simple")
  my_families_real <- families(foster = 2, rich_to_poor = TRUE, N_lineages = N_lineages, N_years = N_years, life_history = "_real", plot_name = "_real")
  ppl_lineage_simp_rtp[[r]] <- my_families_simple[["ppl_lineage"]]
  ppl_lineage_real_rtp[[r]] <- my_families_real[["ppl_lineage"]]
  
}


ppl_lineage_simp_rtp <- list()
ppl_lineage_real_rtp <- list()

#plot
#plot all descendants for lineage -  simple
png("images/n_ppl_lineage_simple_rtp.png", width = 15, height = 10, units = "cm", res = 500, type="cairo")
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', 
     xlim = c(0,N_years), ylim = c(0, max(unlist(ppl_lineage_simp_rtp))+5) , 
     xlab = "years", ylab = "members lineage",
     main = "Lineage growth with simple life history traits, rich to poor ")
legend(1,max(unlist(ppl_lineage_simp_rtp)), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)

#store means for shading
means_foster <- matrix(nrow = N_runs, ncol = N_years)
means_nofoster <- matrix(nrow = N_runs, ncol = N_years)

#calculate means and plot lines
for( r in 1:N_runs){
  means_foster[r,] <- apply(ppl_lineage_simp_rtp[[r]][1:N_lineages/2,], 2, mean)
  means_nofoster[r,] <- apply(ppl_lineage_simp_rtp[[r]][(1+N_lineages/2):N_lineages,],2 , mean)
  
  # for (l in 1:N_lineages) {
  #   lines(1:N_years, ppl_lineage_simp_rtp[[r]][l,], col = col.alpha(ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), 0.3), lwd = 0.4)
  #   
  # }#l
}#r

#shade
x <- c(1:N_years, N_years:1)
y <- c(apply(means_foster, 2, max), rev(apply(means_foster, 2, min)))
polygon(x, y, col = col.alpha("#46bb29", 0.2), lty = 0)
y <- c(apply(means_nofoster, 2, max), rev(apply(means_nofoster, 2, min)))
polygon(x, y, col = col.alpha("#ff2990", 0.2), lty = 0)

#add mean of means
lines(1:N_years, apply(means_foster, 2, mean),col = col.alpha("#46bb29", 0.8), lwd = 2)
lines(1:N_years, apply(means_nofoster,2 , mean), col = col.alpha("#ff2990", 0.8) , lwd = 2)

dev.off()

#plot all descendants for lineage -  realistic
png("images/n_ppl_lineage_real_rtp.png", width = 15, height = 10, units = "cm", res = 500, type="cairo")
par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
plot(0, type = 'n', 
     xlim = c(0,N_years), ylim = c(0, max(unlist(ppl_lineage_real_rtp))+5) , 
     xlab = "years", ylab = "members lineage",
     main = "Lineage growth with realistic life history traits, rich to poor")
legend(1,max(unlist(ppl_lineage_real_rtp)), legend=c("With fostering", "Without fostering"),
       col=c("#46bb29", "#ff2990"), lwd = 3, cex=0.9,
       box.lty=0)

#store means for shading
means_foster <- matrix(nrow = N_runs, ncol = N_years)
means_nofoster <- matrix(nrow = N_runs, ncol = N_years)

#calculate means and plot lines
for( r in 1:N_runs){
  means_foster[r,] <- apply(ppl_lineage_real_rtp[[r]][1:N_lineages/2,], 2, mean)
  means_nofoster[r,] <- apply(ppl_lineage_real_rtp[[r]][(1+N_lineages/2):N_lineages,],2 , mean)
  
  # for (l in 1:N_lineages) {
  #   lines(1:N_years, ppl_lineage_real_rtp[[r]][l,], col = col.alpha(ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), 0.3), lwd = 0.4)
  #   
  # }#l
}#r

#shade
x <- c(1:N_years, N_years:1)
y <- c(apply(means_foster, 2, max), rev(apply(means_foster, 2, min)))
polygon(x, y, col = col.alpha("#46bb29", 0.2), lty = 0)
y <- c(apply(means_nofoster, 2, max), rev(apply(means_nofoster, 2, min)))
polygon(x, y, col = col.alpha("#ff2990", 0.2), lty = 0)

#add mean of means
lines(1:N_years, apply(means_foster, 2, mean),col = col.alpha("#46bb29", 0.8), lwd = 2)
lines(1:N_years, apply(means_nofoster,2 , mean), col = col.alpha("#ff2990", 0.8) , lwd = 2)

dev.off()

#####