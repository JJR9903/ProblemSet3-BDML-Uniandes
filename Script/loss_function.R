metrica<-function(data,lev=NULL,model=NULL){
  menor<-function(pred,obs)
    delta=(obs-pred)/obs
    punto_crit=0.8*obs
  if(delta<(-1*punto_crit)){
    error=9999999999
  } else if(delta<0){
    error=delta^2
  } else if (delta<(punto_crit/3)){
    error=(delta^2)+delta
  } else {
    error=9999999999
  }
  return (error)
}



