library(tcltk)
pb <- txtProgressBar(min = 1, max = 486, style = 3)
pb_temp <- 0

sim_cycle <- 100
sim_date <- 200
start_infV <- 0.05
pop <- 1000000

dispersion_list <- c(0.01,0.25,1.0)
r0_list <- c(0.67,1.0,1.5)
var_list <- c(1.0,1.5,2.0)
start_infT_list <- c(100/500/2500)
cross_list <- c(0/1)
rec_per_list <- c(0/0.05/0.25)




inf_array <- c()
infV_array <- c()
rec_array <- c()
recV_array <- c()
sim_array <- c()
day_array <- c()


dispersion_array <- c()
r0_array <- c()
var_array <- c()
start_infT_array <- c()
cross_array <- c()
rec_per_array <- c()


for (dispersion in dispersion_list) {
  for (r0 in r0_list) {
    for (var in var_list) {
      for (start_infT in start_infT_list) {
        for (cross in cross_list) {
          for (rec_per in rec_per_list) {

          
pb_temp <- pb_temp + 1
        




for (i in 1:sim_cycle) {
  r0_var <- r0 * var
  
  inf <- start_infT * (1 - start_infV)
  infV <- start_infT * start_infV
  rec <- pop * rec_per
  recV <- 0
  
  inf_array <- c(inf_array, inf)
  infV_array <- c(infV_array, infV)
  rec_array <- c(rec_array, rec)
  recV_array <- c(recV_array, recV)

  for (ii in 1:sim_date) {
    
      new_inf <- 0
      new_infV <- 0
      rec_inf <- 0
      rec_infV <- 0

    if (infV >= 1) {
      new_infV_temp <- sum(rnbinom(n=infV, size=dispersion, mu=r0_var/5))
      new_infV <- round(new_infV_temp * (1 - ((infV + (inf*cross) + recV + (rec*cross))/pop)))
    } else {
      new_infV <- 0  
    }
    if (new_infV < 0) {
      new_infV <- 0
    }
      
    if (inf >= 1) {
    new_inf_temp <- sum(rnbinom(n=inf, size=dispersion, mu=r0/5))
    new_inf <- round(new_inf_temp * (1 - ((inf + (infV*1) + rec + (recV*cross) + (new_infV*1))/pop)))
    } else {
    new_inf <- 0  
    }
    if (new_inf < 0) {
      new_inf <- 0
    }

    
    rec_inf <- 0
    for (iii in 1:inf) {
      if (runif(1) < 1/5) {
        rec_inf <- rec_inf + 1
      }
    }

    rec_infV <- 0
    for (iii in 1:infV) {
      if (runif(1) < 1/5) {
        rec_infV <- rec_infV + 1
      }
    }

    

    
    
    inf <- inf + new_inf - rec_inf - round((new_infV_temp * ((inf*cross)/pop)))
    infV <- infV + new_infV - rec_infV
    rec <- rec + rec_inf - round((new_infV_temp * ((rec*cross))/pop))
    recV <- recV + rec_infV - round((new_inf_temp * ((recV*cross))/pop))
    
    
    
    if (inf < 0) {
      inf <- 0
    }
    if (infV < 0) {
      infV <- 0
    }
    if (rec < 0) {
      rec <- 0
    }
    if (recV < 0) {
      recV <- 0
    }
    
        
    inf_array <- c(inf_array, inf)
    infV_array <- c(infV_array, infV)
    rec_array <- c(rec_array, rec)
    recV_array <- c(recV_array, recV)
    
    
  } # day end
  
  dispersion_array_temp <- c(rep(dispersion,sim_date+1))
  dispersion_array <- append(dispersion_array, dispersion_array_temp)
  r0_array_temp <- c(rep(r0,sim_date+1))
  r0_array <- append(r0_array, r0_array_temp)
  var_array_temp <- c(rep(var,sim_date+1))
  var_array <- append(var_array, var_array_temp)
  start_infT_array_temp <- c(rep(start_infT,sim_date+1))
  start_infT_array <- append(start_infT_array, start_infT_array_temp)
  cross_array_temp <- c(rep(cross,sim_date+1))
  cross_array <- append(cross_array, cross_array_temp)
  rec_per_array_temp <- c(rep(rec_per,sim_date+1))
  rec_per_array <- append(rec_per_array, rec_per_array_temp)
  
  
  

  
    
  sim_array_temp <- c(rep(i,sim_date+1))
  sim_array <- append(sim_array, sim_array_temp)
  day_array_temp <- c(0:sim_date)
  day_array <- append(day_array, day_array_temp)
  
} # sim end






setTxtProgressBar(pb, pb_temp)
          } # rec_per end
        } # cross end
      } # start_infT end
    } # var end
  } # R0 end
} # dispersion end



df <- data.frame(dispersion_array, r0_array, var_array, start_infT_array, cross_array, rec_per_array, sim_array,day_array,inf_array,infV_array,rec_array,recV_array)


write.csv(df, "output.csv")

remove(list = ls())
