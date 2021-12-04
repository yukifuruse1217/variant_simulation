library(tcltk)
pb <- txtProgressBar(min = 1, max = 243, style = 3)
pb_temp <- 0

sim_cycle <- 100
sim_date <- 200 #200
start_infV <- 0.05
pop <- 1000000

inf_period <- 5
cross <- 0.7 #actually, cross_eff

dispersion_list <- c(0.3)
r0_list <- c(0.67,1.0,1.5) # c(0.67,1.0,1.5)
var_list <- c(1.0,1.5,2.0) # c(1.0,1.5,2.0)
start_infT_list <- c(100,500,2500) #100/500/2500
cross_list <- c(0.25,0.5,0.75) #c(0.25,0.5,0.75), cross0 is complete escape, cross1 is unable to escape
rec_per_list <- c(0,0.4,0.8) #c(0,0.4,0.8)




inf_array <- c()
infV_array <- c()

recW_array <- c()
recV_array <- c()
recB_array <- c()

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
              
              
              recB <- pop * rec_per * cross
              recW <- pop * rec_per * (1 - cross)
              recV <- 0
              
              
              inf_array <- c(inf_array, inf)
              infV_array <- c(infV_array, infV)
              
              recW_array <- c(recW_array, recW)
              recV_array <- c(recV_array, recV)
              recB_array <- c(recB_array, recB)
              
              for (ii in 1:sim_date) {
                
                #    if (inf + infV+ rec > 1000000000) {
                new_inf <- 0
                new_infV <- 0
                
                recW_from_inf <- 0
                recV_from_infV <- 0
                recB_from_inf <- 0
                recB_from_infV <- 0
                
                #    } else {

                
                
                
                
                
                
 
                  if (infV >= 1) {
                    new_infV_temp <- sum(rnbinom(n=infV, size=dispersion, mu=r0_var/inf_period))
                    new_infV <- round(new_infV_temp * (1 - ((infV + inf + recV + recB)/pop)))
                  } else {
                    new_infV <- 0  
                  }
                  if (new_infV < 0) {
                    new_infV <- 0
                  }
                  
                  if (inf >= 1) {
                    new_inf_temp <- sum(rnbinom(n=inf, size=dispersion, mu=r0/inf_period))
                    new_inf <- round(new_inf_temp * (1 - ((inf + infV + recW + recB + new_infV)/pop)))
                  } else {
                    new_inf <- 0  
                  }
                  if (new_inf < 0) {
                    new_inf <- 0
                  }
                  
                  
                  
                  for (iii in 1:inf) {
                    if (runif(1) < 1/inf_period) {
                      recW_from_inf <- recW_from_inf + 1
                      
                      if (runif(1) < cross) {
                        recW_from_inf <- recW_from_inf - 1
                        recB_from_inf <- recB_from_inf + 1
                      }
                      
                    }
                  }
                  
                  for (iii in 1:infV) {
                    if (runif(1) < 1/inf_period) {
                      recV_from_infV <- recV_from_infV + 1
                      
                      if (runif(1) < cross) {
                        recV_from_infV <- recV_from_infV - 1
                        recB_from_infV <- recB_from_infV + 1
                      }
                      
                    }
                  }
                
                
                
                
                
                
                
##############################################################                
                #    } # 1000000000 end
                
                
                
                inf <- inf + new_inf - recW_from_inf - recB_from_inf
                infV <- infV + new_infV - recV_from_infV - recB_from_infV
                
                recW <- recW + recW_from_inf - round(new_infV_temp * (recW/pop))
                recV <- recV + recV_from_infV - round(new_inf_temp * (recV/pop))
                recB <- recB + recB_from_inf + recB_from_infV

                
                
                if (inf < 0) {
                  inf <- 0
                }
                if (infV < 0) {
                  infV <- 0
                }
                if (recW < 0) {
                  rec <- 0
                }
                if (recV < 0) {
                  recV <- 0
                }
                if (recB < 0) {
                  recB <- 0
                }
                
                
                inf_array <- c(inf_array, inf)
                infV_array <- c(infV_array, infV)
                recW_array <- c(recW_array, recW)
                recV_array <- c(recV_array, recV)
                recB_array <- c(recB_array, recB)
                
                
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



df <- data.frame(dispersion_array, r0_array, var_array, start_infT_array, cross_array, rec_per_array, sim_array,day_array,inf_array,infV_array,recW_array,recV_array,recB_array)
