library(raster)

try.env.traits<-function(try_data,amb_rst){
  try_data$TraitID[is.na(try_data$TraitID)]<-0
  try_data$ObservationID[is.na(try_data$ObservationID)]<-0
  
  obsIDs<-na.omit(unique(try_data$ObservationID))[-1]
  traitIDs<-sort(na.omit(unique(try_data$TraitID)))[-1]
  
  try_trait_matrix<-data.frame(matrix(NA, nc = length(traitIDs), nr=length(obsIDs)))
  try_env_matrix<-matrix(NA, nc = 3+nlayers(amb_rst), nr=length(obsIDs))
  sps<-data.frame(species=NA)
  
  a<-try_data[!duplicated(try_data$TraitID),c("TraitID","OriglName")]
  colnames(try_trait_matrix)<-as.character(a[match(traitIDs,a$TraitID),2])
  
  for(i in 1:nrow(try_trait_matrix)){
    try_obs<-try_data[try_data$ObservationID==obsIDs[i],]
    if(length(which(traitIDs%in%try_obs$TraitID))==0) next
    for(j in which(traitIDs%in%try_obs$TraitID)){
      try_trait_matrix[i,j]<-mean(as.numeric(as.character(try_obs[try_obs$TraitID==traitIDs[j],"OrigValueStr"])))
    }
    
    try_env_matrix[i,1:3]<-c(obsIDs[i],as.numeric(as.character(try_obs[match(c(59:60),try_obs$DataID) ,"OrigValueStr"])))
    sps[i,1]<-as.character(try_obs[1,"AccSpeciesName"])
  }
  cells<-cellFromXY(amb_rst, try_env_matrix[,3:2])
  for(k in 1:nlayers(amb_rst)){
    try_env_matrix[,k+3]<-amb_rst[[k]][cells]
  }
  colnames(try_env_matrix)<-c("ObsID","lat","long",names(amb_rst))
  
  try_trait_matrix<-try_trait_matrix[,apply(try_trait_matrix,2,function(x) any(!is.na(x)))]
  try_matrix<-data.frame(sps,try_env_matrix,try_trait_matrix)
  return(try_matrix) 
}
