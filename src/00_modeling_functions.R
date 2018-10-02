###
### Claire Kelling
### Creating functions for the modeling used in 20_sensitivity_analysis.R
### Includes functions for the modeling procedure and storage of output.
###
### Created       9/9/18
### Last Modified 9/9/18
### 


## Output storage function:
mod_out <- function(mod, sglmm){
    if(sglmm == T){
      dic <- mod$dic
      pd <- mod$pD #just number of effective parameters, not perc dev
      waic <- NA
    }else{
      dic <- as.numeric(mod$modelfit[1]) #dic
      pd <- as.numeric(mod$modelfit[7]) #perc dev
      waic <- as.numeric(mod$modelfit[3]) #waic
    }
  return(c(dic,pd, waic))
}

## Modeling Procedure function

model_func <- function(W_m_geog, W_m_soc){
  #############
  ###  Combining adjacency matrices
  #############
  #addition
  add_W <- W_m_geog + W_m_soc
  #binary
  bin_W <- ifelse(add_W == 2, 1, add_W)
  
  ####
  #### Geographic modeling
  ####
  rownames(W_m_geog) <- NULL #need this for test if matrix is symmetric
  model.bym.geog <- S.CARbym(formula=form, data=det_bg_geog@data,
                             family="poisson", W=W_m_geog, burnin=20000, n.sample=150000, thin=10)
  model.ler.geog <- S.CARleroux(formula=form, data=det_bg_geog@data,
                                family="poisson", W=W_m_geog, burnin=20000, n.sample=150000, thin=10)
  sp.sglmm.fit.geog <- sparse.sglmm(formula = form,data=det_bg_geog@data, family = poisson, A = W_m_geog,
                                    verbose = TRUE) #tune = list(sigma.s = 0.02)
  ####
  #### Social AND Geographic modeling
  ####
  rownames(bin_W) <- NULL #need this for test if matrix is symmetric
  model.bym.soc <- S.CARbym(formula=form, data=det_bg_geog@data,
                            family="poisson", W=bin_W, burnin=20000, n.sample=150000, thin=10)
  model.ler.soc <- S.CARleroux(formula=form, data=det_bg_geog@data,
                               family="poisson", W=bin_W, burnin=20000, n.sample=150000, thin=10)
  sp.sglmm.fit.soc <- sparse.sglmm(formula = form,data=det_bg_geog@data, family = poisson, A = bin_W,
                                   verbose = TRUE) #tune = list(sigma.s = 0.02)

  
  output <- cbind(c("BYM Geog", mod_out(model.bym.geog, sglmm = F)),
                  c("Ler Geog", mod_out(model.ler.geog, sglmm = F)),
                  c("SGLMM Geog", mod_out(sp.sglmm.fit.geog, sglmm = T)),
                  c("BYM Soc", mod_out(model.bym.soc, sglmm = F)),
                  c("Ler Soc", mod_out(model.ler.soc, sglmm = F)),
                  c("SGLMM Soc", mod_out(sp.sglmm.fit.soc, sglmm = T)))
  return(output)
  
}


W_geog_setup <- function(shapefile){
  W.nb <- poly2nb(shapefile, row.names = rownames(shapefile@data)) 
  W_geog <- nb2mat(W.nb, style="B")
  return(W_geog)
}


W_soc_setup <- function(subs_lodes){
  mi_lodes_det_agg <- subs_lodes
  
  edgelist <- mi_lodes_det_agg[,c(1,2)]
  edgelist <- as.matrix(edgelist)
  
  # I need to rescale the block groups so that their ID's are not so large
  bg_dat <- det_bg@data
  new_geoid <- cbind(1:1822,bg_dat$GEOID)
  new_geoid[,1] <- as.numeric(new_geoid[,1])
  new_geoid[,2] <- as.numeric(new_geoid[,2])
  
  new_el <- matrix(NA,nrow = nrow(edgelist),ncol = ncol(edgelist))
  
  for(i in 1:nrow(edgelist)){
    new_el[i,1] <- as.numeric(new_geoid[which(edgelist[i,1] == new_geoid[,2]),1])
    new_el[i,2] <- as.numeric(new_geoid[which(edgelist[i,2] == new_geoid[,2]),1])
  }
  
  new_el[,1] <- as.integer(new_el[,1])
  new_el[,2] <- as.integer(new_el[,2])
  
  mat <- matrix(0, 1822, 1822)
  mat[new_el] <- 1
  sum(mat)
  
  #need to make this matrix symmetric
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(mat[i,j] == 1){
        mat[j,i] <- 1
      }
    }
    #need to make diagonal null (not own neighbor, just establishing links)
    mat[i,i] <- 0
  }
  
  #I need to remove those entries such that there are no ACS information, in order for modeling to work
  det_bg$id <- row.names(det_bg)
  det_bg@data <- left_join(det_bg@data, agg_domv_dat_comp, by = (GEOID = "GEOID"))
  no_acs <- which(is.na(det_bg$median_income))
  na_dat<- det_bg[which(is.na(det_bg$median_income)),]
  det_bg <- det_bg[-which(is.na(det_bg$median_income)),]
  
  #they are equal (checked!)
  #View(cbind(det_bg$GEOID, new_geoid[,2]))
  mat <- mat[-no_acs,-no_acs] #this is my new adjacency matrix where all info is complete
  dim(mat)
  W.soc <- mat
  return(W.soc)
}
