library(dplyr)
library(rethinking)

families <- function(foster, #foster= 0-no foster, 1-foster every year, 2-foster when any household$energy <0
                     rich_to_poor = FALSE, #direction of fostering: from rich household to poor or vice versa
                     min_energy_rich = 0, #minimum energy of family to be considered rich
                     max_energy_poor = 0, #max energy of family to cbe considered poor
                     #sizes
                     N_lineages = 10, #n lineages
                     N_years = 100, #n years (rounds in simulation)
                     #life history stuff
                     life_history = "simple", #change this to anything else to dset a more realistic life history
                     simple_fertility = 0.1,
                     simple_mortality = 0.015,
                     #plotting
                     hh_color = "lineage", #lineage to have all households of a lineage same color, anything else for random
                     plot_name = "",
                     save_plot = FALSE
){
  l_colors <- rainbow(N_lineages, alpha = 0.8)
  #objects to store info
  ppl_lineage <- matrix(NA, nrow = N_lineages, ncol = N_years)
  households_info <- list()
  moves <- data.frame(matrix(nrow = 0, ncol = 6, dimnames = list(NULL, c("person_move", "year", "hh_origin", "hh_dest", "energy_origin", "energy_dest"))))
  
  #create data frame with individual measures and original individuals
  individuals <- data.frame(ID = 1:N_lineages,
                            dead = rep(0, N_lineages),
                            year_birth = rep(0, N_lineages),
                            year_death = rep(NA, N_lineages),
                            year_move = rep(NA, N_lineages),
                            parent = rep(0, N_lineages),
                            household= rep(1, N_lineages),
                            lineage = 1:N_lineages)
  
  #create households dataframe (current state of households)
  households <- data.frame(h_ID = 1,
                           n_members = NA,
                           total_energy = 0,
                           lineage = 0,
                           color = rgb(255, 255, 255, max = 255, alpha = 0))
  
  
  
  #create fertility, mortality schedules
  #simple model with uniform fertility between 16 and 45 years
  p_reproduction_simple <- c(rep (0, 15),          #pre-reproductive
                             rep (simple_fertility, 45-15),     #reproductive ages
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
  p_death_simple <- c(rep (simple_mortality, 79), 1) #constant p death until 100
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
  net_production_simple <- as.integer(c( seq(-10,10, 1), rep(10, 40), seq(10,-10, -1) ))
  
  net_production_ache <- c(-1141.0, -1405.0, -1668.0, -1961.0, -2078.0, -2137.0, -2166.0, -2137.0, -2078.0, -2020.0, -2020.0, -2049.0, -2078.0, -2166.0, -2224.0, -2283.0, -1726.5, -907.5, -439.0, -176.0, 29.0, 234.0, 410.0, 556.0, 790.0, 966.0, 1171.0, 1346.0, 1551.0, 1727.0, 1902.0, 2078.0, 2107.0, 2078.0, 2020.0, 1990.0, 1932.0, 1844.0, 1785.0, 1756.0, 1698.0, 1668.0, 1580.0, 1522.0, 1472.0, 1422.0, 1372.0,  1322.0, 1272.0,  1222.0, 1172.0, 1122.0, 1072.0, 1022.0, 972.0, 922.0, 872.0, 822.0, 772.0, 722.0, 672.0, 622.0, 572.0, 522.0, 472.0, 422.0, 372.0, 322.0, 272.0, 222.0, 172.0, 122.0, 72.0, 22.0, -28.0, -78.0, -128.0, -178.0, -228.0, -278.0)
  
  if(life_history == 'simple'){
    p_reproduction <- p_reproduction_simple
    p_death <- p_death_simple
    net_production <- net_production_simple
  }else{
    p_reproduction <- p_reproduction_white2015
    p_death <- p_death_white2015
    net_production <- net_production_ache
  }
  #simulate life histories
  #set counter time
  year <- 1
  
  if(save_plot == TRUE) png(paste("../images/energy_cycles", plot_name, ".png", sep = ""), width = 15, height = 10, units = "cm", res = 500)
  
  #draw plot to keep track of some stuff
  par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
  y_lim <- c(2 * min(net_production), 4 * max(net_production))
  plot(0, type = 'n', xlim = c(15,N_years), ylim = y_lim , xlab = "years", ylab = "total labour")
  abline(h =0, col = "grey80")
  
  for (y in 1:N_years){
    #PLOTTING - draw lines for energy from current to previous year 
    if(year >= 2) { 
      for( h in 1:nrow(households)) {
        if(households$n_members[h] > 0) {
          if(foster == 0){
            lines( c(year-1, year), c(last_energy[h], households$total_energy[h]), col = households$color[h], ljoin = 2, lty = 1, lwd = 0.8)
          }else{
            lines( c(year-1, year), c(last_energy[h], households$total_energy[h]), col = households$color[h], ljoin = 2, lty = 2, lwd = 0.3)
          }#alternatively draw continuous lines for energy or light colored dashed lines if in combination with fostering lines
        }#if enough household members
      }#h
    }#starting from year 2
    
    #INDIVIDUALS-LOOP
    #for each individual
    for (i in 1:nrow(individuals)) {
      if(individuals$dead[i] == 0){ #if individual is alive
        #individual reproduces
        if(households$total_energy[individuals$household[i]] >= 0){ #if household has positive energy
          if(rbinom(1,1,p_reproduction[year - individuals$year_birth[i]]) == 1){ #with age specific probability
            #if it's first offspring,they move out and establishes new household
            if(any(individuals$parent == individuals$ID[i]) == FALSE) {
              individuals$household[i] <- nrow(households) + 1
              households <- rbind(households, list(h_ID = nrow(households)+1, 
                                                   n_members = 2,
                                                   total_energy = net_production[year - individuals$year_birth[i]],
                                                   lineage = individuals$lineage[i],
                                                   color = ifelse(hh_color == "lineage", 
                                                                  col.alpha(l_colors[individuals$lineage[i]],alpha = inv_logit(- (nrow(households)-4*N_lineages) * 0.01)), 
                                                                  sample(rainbow(40, alpha = inv_logit(- (nrow(households)-4*N_lineages) * 0.1)),1 )))# PLOTTING - weird stuff to increase transparency for later formed families so the plot remains visible. note that transparency is defined as inverted logistic curve going down from 1, hitting median point at a multiplication of N_lineages (e.g. 2) and decreasing with a certain rate (e.g. 0.1)
              )
            }
            #add offspring
            offspring <- list(ID = individuals$ID[nrow(individuals)] + 1,
                              dead = 0,
                              year_birth = year,
                              year_death = NA,
                              year_move = NA,
                              parent = individuals$ID[i],
                              household = individuals$household[i],
                              lineage = individuals$lineage[i])
            individuals <- rbind(individuals, offspring)
            #PLOTTING - mark the birth of baby in plot
            points(year, households$total_energy[individuals$household[i]], col = households$color[individuals$household[i]], pch = 16)
          }
        }
        
        #individuals die
        if(year > 20){
          individuals$dead[i] <- rbinom(1, 1, p_death[year - individuals$year_birth[i]]) #dies with age specific probability
        individuals$year_death[i] <- ifelse(individuals$dead[i] == 1, year, NA)                                        #and year of death is recorded
        #PLOTTING - dead individual
        if(individuals$dead[i] == 1) points(year, households$total_energy[individuals$household[i]], cex = 0.8, col =  households$color[individuals$household[i]])
        }#nobody dies in the first 20 years
        
      }#if individual alive, do the above
      
    }#i
    
    #FOSTER - LOOP LINEAGES
    #do the following according to the commands provided
    if(foster != 0){ #run the foster section only if fostering is allowed
      for(l in 1:(N_lineages/2)){ #foster only in the first half of households
        #define households involved
        hh_lineage <- households[which(households$lineage == l & households$n_members >= 1),] #all households in the lineage with more than one person (i.e. grandmas are not involved, TEST this as well?)
        if (sum(households$lineage == l) > 1 & #foster if there is more than one household in lineage
            ifelse(foster == 2, any(hh_lineage$total_energy < max_energy_poor, na.rm = TRUE) & any( hh_lineage$total_energy > min_energy_rich, na.rm = TRUE) , TRUE) #foster only if any household has negative energy when fostering=2
        ) {
          rich <- hh_lineage$h_ID[which.max(hh_lineage$total_energy) ]
          poor <- hh_lineage$h_ID[which.min(hh_lineage$total_energy) ]
          #depending on direction of fostering (from rich to poor or vice versa)
          if(rich_to_poor == TRUE ) {
            hh_origin <- rich
            hh_dest <- poor
            movable_people <- individuals[which(individuals$household == rich & individuals$dead == 0 & !individuals$ID %in% individuals$parent & year - individuals$year_birth > 10),]#people in the richest family who are older than 10 and are not parents can move to poorest
          } #rich > poor
          if(rich_to_poor == FALSE ) {
            hh_origin <- poor
            hh_dest <- rich
            movable_people <- individuals[which(individuals$household == poor & individuals$dead == 0 & year - individuals$year_birth < 10),]#people in the poorest family who are younger than 10 can move to richest
          } #poor > rich
          if(nrow(movable_people) >= 1 ){ 
            person_move <- movable_people$ID[sample(length(movable_people$ID), 1)]
            individuals$household[which(individuals$ID == person_move )] <- hh_dest #one random movable person gets assigned to poorest house
            
            #PLOTTING - show move, x is in household that looses individual * in household that receives
            
            lines( c(year, year), c(households$total_energy[hh_dest], households$total_energy[hh_origin]), col = households$color[hh_dest], ljoin = 2)
            points(year, households$total_energy[hh_origin], pch = 4, col = ifelse( hh_color == "lineage", l_colors[l], households$color[hh_origin]))
            points(year, households$total_energy[hh_dest], pch = 8,  col = ifelse( hh_color == "lineage", l_colors[l], households$color[hh_origin]))
            #record movement
            individuals$year_move[which(individuals$ID == person_move)] <- year
            moves <- rbind(moves, list(person_move = person_move,
                                       year = year,
                                       hh_origin = hh_origin,
                                       hh_dest = hh_dest,
                                       energy_origin = households$total_energy[hh_origin],
                                       energy_dest = households$total_energy[hh_dest]))
            #remove info as a safety check
            rm(hh_lineage, rich, poor, hh_origin, hh_dest, movable_people, person_move)
          }#if there are people to move
        }#if fostering can happen
        
      }#l    
      
    }#foster?
    
    
    #HOUSEHOLDS - LOOP
    #calculate energy in household
    last_energy <- households$total_energy #store last energy for plotting
    for(h in 1:nrow(households)){
      households$n_members[h] <- sum(individuals$household == households$h_ID[h] & individuals$dead == 0)
      if(households$n_members[h] > 0){
        hh_members <- individuals[which(individuals$household == households$h_ID[h] & individuals$dead == 0),]
        households$total_energy[h] <- sum(net_production[year - hh_members$year_birth +1 ]) #calculate total energy for the following year
      } else {
        households$total_energy[h] <- NA
      }
    }#h
    
    #store people who are alive per lineage
    ppl_lineage[,year] <- count(individuals,lineage, wt = !dead, .drop = FALSE)[,2]
    #store household info for that year
    households_info[[year]] <- households
    #time passes
    year <- year + 1
    
  }
  if(save_plot == TRUE) dev.off()
  
  my_families <- list(N_lineages = N_lineages, 
                      N_years = N_years, 
                      individuals = individuals, 
                      households = households, 
                      households_info = households_info, 
                      ppl_lineage = ppl_lineage,
                      moves = moves)
  return(my_families)
}

# my_families <- families(foster = 2, N_lineages = 4, N_years = 130, save_plot = TRUE)
# list2env(my_families,globalenv())
# 
# #plot all descendants for lineage
# #png("../images/n_ppl_lineage.png", width = 15, height = 10, units = "cm", res = 500)
# plot(0, type = 'n', xlim = c(0,N_years), ylim = c(0, sum(ppl_lineage[,N_years])/2) , xlab = "years", ylab = "members lineage")
# par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) )
# lines(1:N_years, apply(ppl_lineage[1:N_lineages/2,], 2, mean),col = "#46bb29", lwd = 2)
# lines(1:N_years, apply(ppl_lineage[(1+N_lineages/2):N_lineages,],2 , mean), col =  "#ff2990", lwd = 2)
# 
# for (l in 1:N_lineages) {
#   lines(1:N_years, ppl_lineage[l,], col = ifelse(l <= N_lineages/2, "#46bb29", "#ff2990" ), lwd = 0.3)
# 
# }
# 
# plot(moves$energy_origin, moves$energy_dest)
#dev.off()
# lineage <- 1
# 
# png("offspring.png", width = 15, height = 10, units = "cm", res = 500)
# plot(0, type = 'n', xlim = c(15,N_years), ylim = c(0, 10) , xlab = "years", ylab = "n children")
# parents_lineage <- unique(individuals$parent[which(individuals$lineage == lineage)])[-1]
# 
# for (i in 1:length(parents_lineage)){
#   points(individuals$year_birth[which(individuals$parent == parents_lineage[i])], 1: sum (individuals$parent == parents_lineage[i]), col = households$color[individuals$household[parents_lineage[i]]], pch = 16)
# 
# }
# 
# 
# points(individuals$year_birth[which(individuals$lineage == lineage)], 1: sum (individuals$parent == lineage), col = households$color[individuals$household[lineage]], pch = 16)
# 
# # plot(0, type = 'n', xlim = c(15,80), ylim = c(0, 10) , xlab = "years", ylab = "n children")
# # points(individuals$year_birth[which(individuals$parent == lineage)], 1: sum (individuals$parent == lineage), col = households$color[individuals$household[lineage]], pch = 16)
# # 
# # points(individuals$year_birth[which(individuals$lineage == lineage)], 1: sum (individuals$parent == lineage), col = households$color[individuals$household[lineage]], pch = 16)
# dev.off()
# 
# 
# for (i in 1:sum (individuals$parent == lineage)){
#   lines( individuals$year_birth[which(individuals$parent == lineage)][c(i, i+1)], 1:sum (individuals$parent == lineage), col = households$color[h], ljoin = 2)
# }
# 
# for(p in 1:length(unique(individuals$parent))){
#   cumulative
# }
# 
# # #draw plot to keep track of some stuff
# households_energy <- households_energy[,-1]
# plot(0, type = 'n', xlim = c(15,N_years), ylim = c(-5, 5) , xlab = "years", ylab = "energy")
# for (l in 2:nrow(households)){
#   lines(1:N_years, households_energy[l,], col =  h_colors[l])
# }#y
# for (i in 11:nrow(individuals)){
#   points(individuals$year_birth[i], households_energy[individuals$household[individuals$parent[i]], individuals$year_birth[i]], pch = 16, col = h_colors[individuals$household[individuals$parent[i]]])
# }
# points(individuals$year_death, households_energy[individuals$household, individuals$year_death -1][,1], pch = 16, size = 0.3)

#useful counting
#n offspring per parent
# n_off <- individuals %>% count(parent)
# hist(n_off$n)
# hist(individuals$year_death - individuals$year_birth)
