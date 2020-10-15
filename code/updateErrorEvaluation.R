#updateErrorEvaluation
#this file contains a historic of predictions for Rt, when they were made.
#within a few weeks, we may be able to compare with more mature estimates.


updateErrorEvaluation<-function (state, estimate.data, data.dir) {
  #data.dir = 'E:/Documents/informs/research/2020.09.20 Rt Web/data/error/'
  
  if (state == 0) {
    prefix = "MX"
    
  } else {
    #extract the state name
    nombre = estados$ENTIDAD_FEDERATIVA[estados$CLAVE_ENTIDAD == state]
    
    #print(droplevels(nombre))
    #extract the state name
    prefix = estados$ABREVIATURA[estados$CLAVE_ENTIDAD == state]
    #print(droplevels(nombre))
  }  
  filename = paste(data.dir, prefix,"_Error.csv", sep = "")
  
  #this will draw the infectious cases graph
  if (file.exists(filename)) {
    #band = suppressMessages(read_excel(filename))
    #band = as.data.frame(band)
    band = read.csv(filename)
    band$days = as.Date(band$days)
    #names(band)[1]<- c("num")
    num.error = dim(band)[1]
  
    num.estimate = dim(estimate.data)[1]
    #if there is data to update
    if ( max(estimate.data$days) > max(band$days)) {
      record = data.frame(#num = band$num[num.error]+1, 
                          days = estimate.data$days[num.estimate], 
                          R_p = estimate.data$R[num.estimate],
                          sd_p = estimate.data$sd[num.estimate])
      
      band = rbind(band, record)
      filename = paste(data.dir, prefix,"_Error.csv", sep = "")
      write.csv(band, filename, row.names = FALSE)
    }
  } 
  
}  