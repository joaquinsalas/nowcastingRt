
library(EpiEstim)
library(incidence) #incidence object
library(distcrete) #discretise a pdf
library(epitrix) #from mu, cv to scale and shape in the Gamma distribution
library(latex2exp) #include latex
library(ggplot2) #ggplot


library(matrixStats)
library(readxl)
library(xlsx)


nowcasting <- function (prefix) { 
  
  #directorios de trabajo
  code.dir = 'E:/Documents/informs/research/2020.06.05 delays/code/'
  #setwd(code.dir)
  data.dir = 'E:/Documents/informs/research/2020.06.05 delays/data/'
  
  
  #directorios con datos o codigo comunes
  common.data.dir = 'E:\\Documents\\informs\\research\\covid-common\\data\\'
  common.code.dir = 'E:\\Documents\\informs\\research\\covid-common\\code\\'
  common.data = 'E:\\Documents\\informs\\research\\covid-common\\'
  
  
  filename = paste(common.code.dir, "readData.R", sep = "")
  source(filename)
  
  

  #############
  #read the data from the last file of cases
  filename = paste(data.dir, "infectious_samples",prefix,".xlsx", sep = "")
  samples = suppressMessages(read_excel(filename))
  samples = as.data.frame(samples)
  #covid.before = readData(filename)
  
  #filename = paste(common.data.dir, files[length(files)], sep = "")
  #covid = readData(filename)
  
  
  
  #############
  #read the data from the last file of cases
  
  #files in the data.dir directory corresponding to the Health Ministery data
  files.pattern = "*COVID19MEXICO.csv"
  files <- list.files(path = common.data.dir, pattern =  files.pattern)
  
  filename = paste(common.data.dir, files[length(files)], sep = "")
  covid = readData(filename)
  
  
  #filter in positives
  covid.pos = covid[covid$RESULTADO==1,]
  
  
  #############
  #read the list of states
  
  filename = paste(common.data, "estados.csv", sep = "")
  estados = read.csv(filename)
  
  
  #for the states of Mexico
  #unique.states = sort(unique(covid.pos$ENTIDAD_RES))
  
  #for (state in unique.states) {
  
  state = estados$CLAVE_ENTIDAD[(estados$ABREVIATURA %in% prefix)]
  
  #extract the state name
  nombre = estados$ENTIDAD_FEDERATIVA[estados$CLAVE_ENTIDAD == state]
  #print(droplevels(nombre))
  
  #confirmados 
  confirmed.state = covid.pos[covid.pos$ENTIDAD_RES ==  state,]
  
  #we create an incidence object 
  q <- incidence(confirmed.state$FECHA_SINTOMAS)
  
  #q = i + theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 6))# full outbreak
  
  
  
  
  
  
  
  #we subtract three because there is a latent period
  incubation.period = 5
  latent.period = 3
  dates = as.Date(samples[,1], format="%d/%m/%Y")-(incubation.period-latent.period)
  
  Rt = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
  
  for (sample in seq(2,ncol(samples))) {
    # print(sample)
    #create a data frame to feed the routine
    incid = data.frame(dates = dates, I=samples[,sample])
    
    #compute R
    
    #parameters for the Gamma prior distribution
    mu = 2.6
    sigma = 1.5
    res_parametric_si <- suppressWarnings(suppressMessages(estimate_R(incid, 
                                                                      method="parametric_si",
                                                                      config = make_config(list(
                                                                        mean_si = mu, 
                                                                        std_si = sigma)))))
    
    Rt[8:nrow(Rt),sample] = res_parametric_si$R$`Mean(R)`  
  }
  
  
  mean.values = rowMeans (Rt)
  
  sd.values = rowSds(Rt)
  lim.sup = mean.values + sd.values
  lim.inf = mean.values - sd.values
  
  
  
  
  RtNow = data.frame(days = dates[8:(length(dates))], 
                     R =  mean.values[8:(length(dates))],
                     sd =  sd.values[8:(length(dates))],
                     lower = lim.inf[8:(length(dates))],
                     upper = lim.sup[8:(length(dates))])
  
  
  
  
  
  Rt.mu = RtNow$R[length(RtNow$R)]
  Rt.sd = RtNow$sd[length(RtNow$R)]
  fecha = RtNow$days[length(RtNow$days)]
  
  
  p<-suppressWarnings(ggplot() + 
                        geom_line(data=RtNow, 
                                  aes(x=days, 
                                      y=R, 
                                      color= "blue", linetype = "dashed"))+
                        
                        geom_ribbon(data=RtNow, 
                                    aes(x=days,
                                        y=R, 
                                        ymin=lower, ymax=upper),
                                    linetype=2, alpha=0.4)+
                        
                        
                        geom_hline(yintercept = 1,
                                   linetype = "solid", 
                                   color = "blue", size=0.3) +
                        theme (legend.position = "none",
                               axis.text.x = element_text(angle = 20, hjust = 1)) +
                        ggtitle(nombre))+
    ylim(0,2.75)
  #suppressWarnings(plot(p))  
  
  estimate = data.frame(Rt = Rt.mu, sd = Rt.sd, fecha = fecha)
  #estimate
  result = list(p = p, q = q, estimate = estimate)
  return (result)
  
}


