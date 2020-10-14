
suppressMessages(library(latex2exp)) #include latex
suppressMessages(library(ggplot2)) #ggplot
suppressMessages(library(RandomFieldsUtils)) #file.exists
suppressMessages(library(tidyverse)) #stringr



updateDelaysMetro <- function (zona.metro, code.dir, data.dir, common.data.dir) { 
  
  

  #directorios con datos o codigo comunes
  common.data = 'E:\\Documents\\informs\\research\\covid-common\\'
  
  
  #archivos auxiliares para leer datos
  filename = paste(code.dir, "readData.R", sep = "")
  source(filename)
  
  
  #archivos auxiliares para leer datos
  filename = paste(data.dir, "zonas_metropolitanas_2015.csv", sep = "")
  zonas.metro.data = read.csv(filename)
  
  #extract the state name
  prefix = str_pad(zona.metro,5,pad="0")
  
  
  
  #clave.entidad = estados$CLAVE_ENTIDAD[(estados$ABREVIATURA %in% prefix)]
  
  #extract the metropolitan zone name
  ciudades = zonas.metro.data[zonas.metro.data$CVE_ZM %in% zona.metro,]
  nombre = ciudades[1,"NOM_ZM"] #extract the first name

  print(droplevels(nombre))
  
  #clave.entidad = estados$CLAVE[(estados$ENTIDAD_FEDERATIVA %in% nombre)]
  
  
  
  #files in the data.dir directory corresponding to the Health Ministery data
  files.pattern = "*COVID19MEXICO.csv"
  files <- list.files(path = common.data.dir, pattern =  files.pattern)
  
  
  #este archivo contiene los datos mas actualizados de positivos
  master.filename = paste("updated_delays_",prefix,".csv", sep = "")
 
  filename = paste(data.dir,master.filename , sep = "")
  
  #existe el archivo de observaciones acumuladas
  if (file.exists(filename)) {
    positives = read.csv(filename)
    #column names cannot start with a number so I need to remove the X that R included
    names(positives) = sub("X", "", names(positives))
    col.names = as.Date(names(positives), "%Y.%m.%d")
    
    #find our the last date in the accumulated cases file
    columns = ncol(positives)
    last.date = names(positives)[columns]
    last.date = as.Date(last.date,"%Y.%m.%d")
    
    #transform the dataframe into a matrix for numeric manipulation
    pos.matrix = data.matrix(positives)
    
    
    adding = FALSE
    
    #add information from the files as needed  
    for (file in files) {
      
      fecha = substring(file,1,6)
      file.date = as.Date(fecha,format = "%y%m%d")
      
      
      #found a file to add
      if (file.date > last.date) {
        adding = TRUE
        print("adding...")
        print(file.date)
        
        #add this name to the columns' names
        col.names[length(col.names)+1]= file.date 
        
        #read this file  
        filename = paste(common.data.dir,file , sep = "")
        covid = readData(filename)
        
        #update these dates
        last.date.current = max(covid$FECHA_SINTOMAS)
        dates = seq(from = as.Date("2020-02-28"), 
                    to = last.date.current, by= "day")
        num.dates = length(dates) 
        
        if (prefix == "MX") {
          #filter in the positives
          covid.pos = covid[covid$RESULTADO==1,]
        } else {
          for (i in seq(1, dim(ciudades)[1])) {
            clave.entidad = ciudades[i,"CVE_ENT"]
            cadena = ciudades[i, "CVE_MUN"]
            #extract the last three characters of the string
            clave.municipio = substr(cadena, nchar(cadena)-2, nchar(cadena))
            covid.pos.ciudad = covid[covid$RESULTADO==1 & 
                                       covid$ENTIDAD_RES==clave.entidad & 
                                       covid$MUNICIPIO_RES == as.numeric(clave.municipio),]
            if (i == 1) {
              covid.pos = covid.pos.ciudad
            } else {
              covid.pos = rbind(covid.pos,covid.pos.ciudad)
            }
          }
        }
        #initially the dates will have zero cases
        count = matrix(0, nrow=num.dates, ncol=1)
        i = 1
        for (date in dates) {
          #sum the number of cases for this date
          count[i] = sum(covid.pos$FECHA_SINTOMAS == date)
          i = i + 1
        }
        
        #add additional rows
        r.positives = nrow(pos.matrix)
        r.count = nrow(count)
        if (r.count > r.positives) {
          diff = r.count - r.positives
          #init a vector with zeros
          v = matrix(0,nrow=diff, ncol = ncol(pos.matrix))
          #add it to the accumulated
          pos.matrix= rbind(pos.matrix,v)
        }
        
        #add the new column
        pos.matrix= cbind(pos.matrix, count) #2020.09.11 check this
        
        
        
      }#if (file.date
      
      
    }
    if (adding== FALSE) {
      file = files[length(files)]
      fecha = substring(file,1,6)
      file.date = as.Date(fecha,format = "%y%m%d")
      
      
      #read this file  
      filename = paste(common.data.dir,file , sep = "")
      covid = readData(filename)
      
      #update these dates
      last.date.current = max(covid$FECHA_SINTOMAS)
      dates = seq(from = as.Date("2020-02-28"), 
                  to = last.date.current, by= "day")
      
      
    }
    #save results
    positives = data.frame(pos.matrix[,2:ncol(pos.matrix)])
    #fill the column and row names
    names(positives) = col.names[2:length(col.names)]
    rownames(positives) = dates
    #save csv file
    filename = paste(data.dir,master.filename, sep = "")
    write.csv(positives, filename)
    
    
  } else {
    
    #read the last file 
    filename = paste(common.data.dir,files[length(files)] , sep = "")
    covid.last = readData(filename)
    
    last.date = max(covid.last$FECHA_SINTOMAS) #ultima fecha reportada en el ultimo reporte
    
    
    dates = seq(from = as.Date("2020-02-28"), 
                to = last.date, by= "day")   #secuencia de fechas de la infeccion
    
    num.dates = length(dates)  
    
    current.date = as.Date("2020-04-12") #first day of reports
    j = 1
    #visit all the files
    for (file in files) {
      print("adding...")
      print(current.date)
      
      #read the current file to process
      filename = paste(common.data.dir,file, sep = "")
      covid = readData(filename)
      
      if (prefix == "MX") {
        #filter in the positives
        covid.pos = covid[covid$RESULTADO==1,]
      } else {
        
        #extract the records for the metropolitan area
        for (i in seq(1, dim(ciudades)[1])) {
          clave.entidad = ciudades[i,"CVE_ENT"]
          cadena = ciudades[i, "CVE_MUN"]
          #extract the last three characters of the string
          clave.municipio = substr(cadena, nchar(cadena)-2, nchar(cadena))
          covid.pos.ciudad = covid[covid$RESULTADO==1 & 
                                     covid$ENTIDAD_RES==clave.entidad & 
                                     covid$MUNICIPIO_RES == as.numeric(clave.municipio),]
          if (i == 1) {
            covid.pos = covid.pos.ciudad
          } else {
            covid.pos = rbind(covid.pos,covid.pos.ciudad)
          }
        }
        
      }
      
      
      #initially the dates will have zero cases
      count = matrix(0, nrow=num.dates, ncol=1)
      i = 1
      for (date in dates) {
        #sum the number of cases for this date
        count[i] = sum(covid.pos$FECHA_SINTOMAS == date)
        i = i + 1
      }
      #create the history of observations
      count = data.frame(count)
      names(count) = as.character(current.date)
      if (j==1) {
        
        
        df = count
        
        rownames(df) <-dates
      }  else {
        df = cbind(df, count)
        
      }
      j = j +  1
      current.date = current.date + 1
      
    }
    
    #names(df) <-dates
    filename = paste(data.dir,master.filename , sep = "")
    write.csv(df, filename)
    
    
  }
  
  
}

