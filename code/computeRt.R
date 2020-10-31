
#state, state number 
#covid.pos, dataset with confirmed positives
computeRt <- function(state, covid.pos) {
  
  
  
  RtNow <- data.frame(Date=as.Date(character()),
                      R= as.numeric(), 
                      sd = as.numeric(),
                      lower = as.numeric(),
                      upper = as.numeric())
  
  
  time.back = 30
  if (state == 0) {
    nombre = "MEXICO"
    prefix = "MX"
    confirmed.state = covid.pos
    
  } else {
    #extract the state name
    nombre = estados$ENTIDAD_FEDERATIVA[estados$CLAVE_ENTIDAD == state]
    
    #print(droplevels(nombre))
    #extract the state name
    prefix = estados$ABREVIATURA[estados$CLAVE_ENTIDAD == state]
    #print(droplevels(nombre))
    #confirmados 
    confirmed.state = covid.pos[covid.pos$ENTIDAD_RES ==  state,]
    
  }
  
  
  
  
  #we create an incidence object 
  i <- incidence(confirmed.state$FECHA_SINTOMAS)
  
  
  filename = paste(data.dir, "nowcasting",prefix,".xlsx", sep = "")
  
  
  #this will draw the infectious cases graph
  if (file.exists(filename)) {
    band = suppressMessages(read_excel(filename))
    band = as.data.frame(band)
    
    
    
    
    suppressWarnings(suppressMessages(print(plot(i) + 
                                              geom_line(data=band, 
                                                        color= "red", 
                                                        linetype = "twodash",size=0.25,
                                                        aes(x=as.Date(fecha), 
                                                            y=mean))+
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
    #Q0.05 = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
    #Q0.25 = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
    #Q0.75 = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
    #Q0.95 = matrix(0, nrow = nrow(samples), ncol = ncol(samples))
    
    for (sample in seq(2,ncol(samples))) {
      
      #create a data frame to feed the routine
      incid = data.frame(dates = dates, I=samples[,sample])
      
      #compute R
      
      #parameters for the Gamma prior distribution
      #influenza, Cori et al. (2013)
      #mu = 2.6
      #sigma = 1.5
      
      #nishiura et al. (2020)
      mu = 5.75 #4.0
      sigma = 3.87 #4.6
      res_parametric_si <- suppressWarnings(suppressMessages(estimate_R(incid, 
                                                                        method="parametric_si",
                                                                        config = make_config(list(
                                                                          mean_si = mu, 
                                                                          std_si = sigma)))))
      
      Rt[8:nrow(Rt),sample] = res_parametric_si$R$`Mean(R)`  
      #Q0.05[8:nrow(Rt),sample] = res_parametric_si$R$`Quantile.0.05(R)`
      #Q0.25[8:nrow(Rt),sample] = res_parametric_si$R$`Quantile.0.25(R)`
      #Q0.75[8:nrow(Rt),sample] = res_parametric_si$R$`Quantile.0.75(R)`
      #Q0.95[8:nrow(Rt),sample] = res_parametric_si$R$`Quantile.0.95(R)`
    }
    
    #mean.05 = rowMeans (Q0.05)
    #mean.25 = rowMeans (Q0.25)
    #mean.75 = rowMeans (Q0.75)
    #mean.95 = rowMeans (Q0.95)
    
    mean.values = rowMeans (Rt)
    
    sd.values = rowSds(Rt)
    lim.sup = mean.values + sd.values
    lim.inf = mean.values - sd.values
    
    
    
    
    RtNow = data.frame(days = dates[8:(length(dates))], 
                       R =  mean.values[8:(length(dates))],
                       sd =  sd.values[8:(length(dates))],
                       lower = lim.inf[8:(length(dates))],
                       upper = lim.sup[8:(length(dates))]#,
                       #mean.05 = mean.05[8:(length(dates))],
                       #mean.25 = mean.25[8:(length(dates))],
                       #mean.75 = mean.75[8:(length(dates))],
                       #mean.95 = mean.95[8:(length(dates))]
                       )
    #2020.08.12, there was a subtraction by two. That may be wrong. I already subtract two a few lines ago.
    
    
    
    longitud = length(RtNow$R)
    
    Rt.mu = RtNow$R[longitud]
    Rt.sd = RtNow$sd[longitud]
   # Rt.05 = RtNow$mean.05[longitud]
  #  Rt.25 = RtNow$mean.25[longitud]
  #  Rt.75 = RtNow$mean.75[longitud]
   # Rt.95 = RtNow$mean.95[longitud]
    fecha = RtNow$days[length(RtNow$days)]
    y.inf = min(0.75, min(RtNow$R[(longitud-time.back):longitud] - 1.01*RtNow$sd[(longitud-time.back):longitud]))
    y.sup = max(1.25, max(RtNow$R[(longitud-time.back):longitud] + 1.01*RtNow$sd[(longitud-time.back):longitud]))
    
    
    #nombre = "Ciudad de Mexico"
    p<-suppressWarnings(ggplot() + 
                          geom_line(data=RtNow, color= "red", 
                                    linetype = "twodash",size=0.25,
                                    aes(x=days, y=R))+
                          # geom_line(data=RtNow, color= "red", 
                          #                linetype = "dashed",
                          #           aes(x=days, y=mean.95)) + 
                          # geom_line(data=RtNow, color= "red", 
                          #                linetype = "dashed",
                          #           aes(x=days, y=mean.05)) +
                          # 
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
                          
                          ylim(y.inf,y.sup))
    suppressWarnings(plot(p))  
    
    # #nombre = "Ciudad de Mexico"
    # #p<-suppressWarnings(ggplot() + 
    # #                      geom_line(data=RtNow, 
    # #                                aes(x=days, 
    # #                                    y=R, 
    #                                     color= "blue", linetype = "dashed"))+
    #                       
    #                       geom_ribbon(data=RtNow, 
    #                                   aes(x=days,
    #                                       y=R, 
    #                                       ymin=lower, ymax=upper),
    #                                   linetype=2, alpha=0.4)+
    #                       
    #                       
    #                       geom_hline(yintercept = 1,
    #                                  linetype = "solid", 
    #                                  color = "blue", size=0.3) +
    #                       theme (legend.position = "none",
    #                              axis.text.x = element_text(angle = 20, hjust = 1)) +
    #                       ggtitle(nombre)+
    #                       xlab("")+ ylab("Rt")+
    #                       xlim(Sys.Date()-time.back, Sys.Date()) +
    #                       
    #                       ylim(y.inf,y.sup))
    # suppressWarnings(plot(p))  
    # 
    estimate = data.frame(Rt = Rt.mu, sd = Rt.sd, fecha = fecha)
    
    #estimate = data.frame(Rt = Rt.mu, sd = Rt.sd, i0.05= Rt.05, i0.25 = Rt.25, i0.75=Rt.75, i0.95=Rt.95,  fecha = fecha)
    options(digits = 3)
    print(estimate, row.names = FALSE)
    #write.csv(RtNow, "../data/RtNow.csv")
    
    
    
    
    
  }
  return (RtNow)
  
}