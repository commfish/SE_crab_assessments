#K.Palof
# graphing function for crab CSA bootstrap results 
## load these into the workspace after bootstrap is complete


############LEGAL GRAPH -----------------------------------
crabboot.Legal.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                                  min = 50000 ){
  par(mfrow = c(1, 1))
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Legal Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,2])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
}

############## Just mature ############################
crabboot.Mature.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                                   min = 50000 ){
  par(mfrow = c(1, 1))
  plot(CSAout$est[,1], CSAout$est[,12], xlab = "Year", 
       ylab = "Mature Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,3],rev(bootout$quantCI[,4])), 
          col="blue", border = "blue", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,12], lwd=2)
}

############Graph with BOTH LEGAL AND MATURE------------------------------
crabboot.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                            min = 50000 ){
  ### set up output to file
  #pdf("C:/Users/kjpalof/Documents/R/tanner crab/SP_TCS_CIplot.pdf")
  par(mfrow = c(2, 1))
  
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "b", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])))
  #lines for legal biomass
  lines(x=CSAout$est[,1], y =bootout$quantCI[,1], 
        col = "red", type = "l", lty="dashed")
  lines(x=CSAout$est[,1], y =bootout$quantCI[,2], 
        col = "red", type = "l", lty="dashed")
  
  lines(x=CSAout$est[,1], y =CSAout$est[,12], 
        col = "black", type = "b", pch= 1)
  lines(x=CSAout$est[,1], y =bootout$quantCI[,3], 
        col = "blue", type = "l", lty="dashed")
  lines(x=CSAout$est[,1], y =bootout$quantCI[,4], 
        col = "blue", type = "l", lty="dashed")
  
  ### attempt at shading Both legal and mature biomass
  
  ##### legal biomass first
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
  #Mature biomass
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,3],rev(bootout$quantCI[,4])), 
          col="blue", border = "blue", lty= "dashed", density=25,
          angle=110)
  lines(CSAout$est[,1], CSAout$est[,12], lwd=2)
  #dev.off()
}


############## Legal and mature - larger ---------------------------------
crabboot.LM.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                               min = 50000 ){
  ### set up output to file
  #pdf("C:/Users/kjpalof/Documents/R/tanner crab/SP_TCS_CIplot.pdf")
  par(mfrow = c(1, 1))
  
  ### attempt at shading Both legal and mature biomass
  
  ##### legal biomass first
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
  #Mature biomass
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,3],rev(bootout$quantCI[,4])), 
          col="blue", border = "blue", lty= "dashed", density=25,
          angle=110)
  lines(CSAout$est[,1], CSAout$est[,12], lwd=2)
  #dev.off()
}
