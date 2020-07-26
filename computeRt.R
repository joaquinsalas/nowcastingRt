computeRt <- function(state) {
  
  time.back = 30
  #extract the state name
  nombre = estados$ENTIDAD_FEDERATIVA[estados$CLAVE_ENTIDAD == state]
  #print(droplevels(nombre))
  
  
  #extract the state name
  prefix = estados$ABREVIATURA[estados$CLAVE_ENTIDAD == state]
  #print(droplevels(nombre))
  
  
  #confirmados 
  confirmed.state = covid.pos[covid.pos$ENTIDAD_RES ==  state,]
  
  #we create an incidence object 
  i <- incidence(confirmed.state$FECHA_SINTOMAS)
  
  
  filename = paste(data.dir, "nowcasting",prefix,".xlsx", sep = "")
  
  
  
  if (file.exists(filename)) {
    band = suppressMessages(read_excel(filename))
    band = as.data.frame(band)
    
    suppressWarnings(suppressMessages(print(plot(i) + 
        #    geom_line(data=band, 
        #              aes(x=as.Date(fecha), 
        #                  y=mean, 
        #                  color= "red", linetype = "dashed"))+
            ylab("Incidencia Diaria")+ xlab("")+
            geom_ribbon(data=band, 
                        aes(x=as.Date(fecha),
                            y=mean, 
                            ymin=r0.025, ymax=r0.975, alpha = 0.7, color = "red"),
                        linetype=0, alpha=0.4)+
          xlim(Sys.Date()-time.back, Sys.Date()) +
          scale_color_manual(values=c( "#E69F00"))+
            theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1, size = 6)))))# full outbreak
    
  }
  
  
  
  #############
  #read the data from the last file of cases
  filename = paste(data.dir, "infectious_samples",prefix,".xlsx", sep = "")
  if (file.exists(filename)) {
    
    
    
    
    
    samples = suppressMessages(read_excel(filename))
    samples = as.data.frame(samples)
    #covid.before = readData(filename)
    
    
    
    
    #we subtract three because there is a latent period
    incubation.period = 5
    latent.period = 3
    dates = as.Date(samples[,1], format="%d/%m/%Y")-(incubation.period-latent.period)
    
    Rt = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
    
    for (sample in seq(2,ncol(samples))) {
      
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
    
    
    
    
    RtNow = data.frame(days = dates[8:(length(dates)-2)], 
                       R =  mean.values[8:(length(dates)-2)],
                       sd =  sd.values[8:(length(dates)-2)],
                       lower = lim.inf[8:(length(dates)-2)],
                       upper = lim.sup[8:(length(dates)-2)])
    
    
    
    
    
    Rt.mu = RtNow$R[length(RtNow$R)]
    Rt.sd = RtNow$sd[length(RtNow$R)]
    fecha = RtNow$days[length(RtNow$days)]
    
    #nombre = "Ciudad de Mexico"
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
                          ggtitle(nombre)+
                          xlab("")+ ylab("Rt")+
                          xlim(Sys.Date()-time.back, Sys.Date()) +
                          
                          ylim(0,2))
    suppressWarnings(plot(p))  
    
    estimate = data.frame(Rt = Rt.mu, sd = Rt.sd, fecha = fecha)
    options(digits = 3)
    print(estimate)
    
    
    
    
    
    
  }
  
}