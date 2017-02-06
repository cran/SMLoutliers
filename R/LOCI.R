LOCI <-
function(data,alpha){
  try(if(alpha>1 |alpha <0) stop("alpha must be in (0,1)"))
  '%out%' <- function(a,b) ! a %in% b
  try(if(any((sapply(data,class) %out% c("integer","numeric")))) stop("data class must be numeric or integer"))
  try(if(any(is.na(data))) stop("data contains NA values"))
  
  sl_no <- 1:nrow(data)
  DM <- as.matrix(dist(data))
  rp <- max(DM)/alpha
  rprange <- seq(rp/25,rp/16,length.out=10)
  Outlier_Mat <- matrix(0,nrow=nrow(data),ncol=length(rprange))
  Outlier_Ind <- NULL
  
  MDEF_Ind <- function(obs_no,DM,r,alpha){
    points_npr=NULL
    DM_row = DM[obs_no,]
    points_npr = as.numeric(which(DM_row<=r))
    alpha_points <- NULL
    for (i in  1:length(points_npr)){
      DM_arow = DM[points_npr[i],]
      points_alpha = as.numeric(which(DM_arow<=alpha*r))
      no_of_points_alpha <- length(points_alpha)
      alpha_points <- c(alpha_points,no_of_points_alpha )
    }
    npr_hat <- mean(alpha_points)
    stnd_dev <- ifelse(length(alpha_points)==1,0,sd(alpha_points))
    stnd_dev_normalised <- stnd_dev/npr_hat
    mdef <- 1-(alpha_points[points_npr==obs_no]/npr_hat)
    mdef_critical <- 3*stnd_dev_normalised
    obs_ind <- ifelse(mdef >= mdef_critical,"Outlier","not outlier")
    return(obs_ind)
  }
  
  colnames(Outlier_Mat) = paste("R",rprange)
  for(i in 1:length(rprange)){
    Outlier_Mat[,i] <- sapply(sl_no,MDEF_Ind,DM,alpha,r=rprange[i])
  }
  
  for(i in 1:nrow(Outlier_Mat)) {
    Outlier_Ind[i] <-  ifelse(sum(Outlier_Mat[i,]=="Outlier") ==10, "Outlier","Normal")
  }
  return(Outlier_Ind)
}
