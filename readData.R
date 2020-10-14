#ReadDataDateSelect
#This is derive from ReadDataDate
#In ReadDataSelect, we construct a classifier to distinguish between confirmed and discarted based on a 
#set of features with medical meaning.
#2020.04.10
#Joaquin Salas. salas@ieee.org





#this function reads the data from a csv file and filters out 
#data with too many missing values

readData <- function (filename) {
  
  #leer el archivo csv
  data <- read.csv(file = filename,  header=TRUE, stringsAsFactors = TRUE)
  
  dim(data)
  
  
  #keep this variables
  keep = c(
    #"ORIGEN",             
    #"SECTOR",            
    #"ENTIDAD_UM",         
    "SEXO",              
    #"ENTIDAD_NAC",   
    "ENTIDAD_RES",         
    "MUNICIPIO_RES",      
    "TIPO_PACIENTE",       
    #"FECHA_INGRESO",  
    "FECHA_SINTOMAS",                
    "NEUMONIA",       
    "EDAD",               
    #"NACIONALIDAD",       
    "EMBARAZO",              "DIABETES",            "EPOC",               
    "ASMA",                "INMUSUPR",            "HIPERTENSION",        "OTRA_COM",           
    "CARDIOVASCULAR",      "OBESIDAD",            "RENAL_CRONICA",       "TABAQUISMO",         
    "OTRO_CASO",           "RESULTADO"        
    #"MIGRANTE",          
    #"PAIS_NACIONALIDAD",  
    #"PAIS_ORIGEN"          
  )
  
  resultado = data$CLASIFICACION_FINAL == 1 | data$CLASIFICACION_FINAL == 2 | data$CLASIFICACION_FINAL == 3
  resultado <- as.numeric(resultado)
  res.column = data.frame(RESULTADO = resultado)
  data = cbind(data, res.column)
  
  no.factors = c( "FECHA_SINTOMAS")
  factors = keep[!(keep %in% no.factors)]
  
  
  ## remove the variables that for now will not be taken into account
  data.keep = data[, (names(data) %in% c(keep))]
  
  #make sure these variables are factors
  for (factor in factors) {
    
    data.keep[,factor] = as.factor(data.keep[,factor])
    
  }
  
  
  #dates are converted in relative days
  data.keep$FECHA_SINTOMAS = as.Date(data.keep$FECHA_SINTOMAS)
   
  #dates are converted in relative days
  data.keep$EDAD = as.numeric(data.keep$EDAD)
  
  
  
  
  return (data.keep)
  
  
}




