analisis_var<-function(path_datos,path){
  
  # Only when "resumen_[variable].txt" exists
  
  # Read parameters
  parametros=read_delim(paste0(path,"/parametros.txt"),
                        skip=2,
                        skip_empty_rows = TRUE,
                        col_names = c("Par","valor"),
                        col_types = cols(),
                        delim="=",progress = FALSE)

  fecha1=dmy(parametros[which(parametros$Par == "fecha1"),2])
  fecha2=dmy(parametros[which(parametros$Par == "fecha2"),2])
  
  y1=year(fecha1)
  y2=year(fecha2)
  
  fecha1=format(fecha1, "%Y%m%d")
  fecha2=format(fecha2, "%Y%m%d")
  
  var=as.character(parametros[which(parametros$Par == "var"),2])
  unidad=as.character(parametros[which(parametros$Par == "unidad"),2])
  
  path_proc=as.character(parametros[which(parametros$Par == "path"),2])
  
  series=paste0("Series_",fecha1,"_",fecha2,"_",var) 
  
  if (dir.exists(paste0(path_proc,'/',series,'/estadisticos'))){
    print("Todo Ok")
  } else {
    dir.create(paste0(path_proc,'/',series,'/estadisticos'))
    dir.create(paste0(path_proc,'/',series,'/analisis'))
  }
  
  datos_estaciones=NULL  
  
  totales=read_delim(paste0(path_proc,"/",series,"/resumen_",var,".txt"),
                     delim=";",col_names = TRUE, col_types =cols())
  
  par(mar=c(1,1,1,1)) # Avoiding "margins too large" error
  
  estad=colnames(totales)
  
  for (ee in 3:length(estad)) { # Histogram for each statistical value involved
    
    tryCatch({
      
      variable=estad[ee]
      
      par(mar=c(1,1,1,1)) 
      
      histograma=ggplot(totales,aes(get(variable))) + 
        geom_bar(colour="red",fill="red", alpha=0.5) +
        scale_x_binned()+
        ggtitle(paste0("Histograma de ",variable," de ",var," para el Periodo ",y1," - ",y2)) +
        ylab("Frecuencia") +
        xlab(variable)
  
      name_histo=paste0(path_proc,"/",series,'/estadisticos/',"Histograma_",var,"_",variable,".jpg")
      
      ggsave(name_histo,plot = histograma,dpi=300, width = 30, height = 15,units="cm")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }  
  
  # Esta parte es para la generacíon de la matriz de correlación y su gráfico
  # Idea genial extraída de http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
  
  # Data that I am interested in correlate
  totales2=totales[,c("N","avg","stdv","amp","mes_max","mes_min","Lat","Long","h[m]")] 

  source("http://www.sthda.com/upload/rquery_cormat.r")
  
  par(mar=c(1,1,1,1)) 
  
  tryCatch({
    jpeg(paste0(path_proc,"/",series,'/estadisticos/',"correlaciones_mega.jpg"),width=20, height=20,units="cm",res=300)
    M=rquery.cormat(totales2)
    dev.off()
    
    par(mar=c(1,1,1,1)) 
    
    datos_corr=rquery.cormat(totales2, type="flatten", graph=FALSE)
    corr=datos_corr$r
    
    write.table(corr,file=paste0(path_proc,"/",series,'/estadisticos/',"correlaciones.txt"),
                row.names = FALSE, col.names = TRUE,quote = FALSE)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  
  par(mar=c(1,1,1,1)) 
  
  datos_estaciones=totales
  
  datos_estaciones$season_max=month.abb[datos_estaciones$mes_max]
  datos_estaciones$season_min=month.abb[datos_estaciones$mes_min]
  
  ## DIFFERENT MULTIVARIABLE PLOTS
  #------Lat horizontal---------x=Lat,y=avg,colour=`h[m]`,size=stdv--------------------------
  par(mar=c(1,1,1,1)) 
  P1=ggplot(datos_estaciones)+ 
    xlab("Latitude") + 
    ylab("Mean") +
    scale_color_viridis()+
    geom_point(aes(x=Lat,y=avg,colour=`h[m]`,size=stdv))+
    ggtitle(paste0("Promedio de ",var," vs Latitud de las estaciones SIRGAS")) 

    NAME1=paste0(path_proc,"/",series,"/analisis/lat_vs_avg.jpg")
    ggsave(NAME1,plot = P1,dpi=600, width = 30, height = 15,units="cm")
    
  #-------Lat vertical----------y=Lat,x=avg,colour=`h[m]`,size=stdv-------------------------    
    par(mar=c(1,1,1,1)) 
  P11=ggplot(datos_estaciones)+
    xlab("Latitude") + 
    ylab("Mean") +
    scale_color_viridis()+
    geom_point(aes(y=Lat,x=avg,colour=`h[m]`,size=stdv)) +
    theme(
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 22, face = "bold"))+
    ggtitle(paste0("Promedio de ",var," vs Latitud de las estaciones SIRGAS")) 
    
    NAME11=paste0(path_proc,"/",series,"/analisis/lat_vs_avg_vertical.jpg")
    ggsave(NAME11,plot = P11,dpi=500, width = 25, height = 35,units="cm")  
    
  #-----y=`h[m]`,x=avg,colour=Lat-----------------------------------------------------------  
  par(mar=c(1,1,1,1)) 
  P2=ggplot(datos_estaciones)+     
    xlab("Latitude") + 
    ylab("Mean") +
    scale_color_viridis(option = "inferno",begin = 1, end = 0)+
    geom_point(aes(y=`h[m]`,x=avg,colour=Lat),size=3) +
    ggtitle(paste0("Promedio de ",var," vs Altura de las estaciones SIRGAS")) 
  
    NAME2=paste0(path_proc,"/",series,"/analisis/avg_vs_alt.jpg")
    ggsave(NAME2,plot = P2,dpi=600, width = 30, height = 15,units="cm")
  
  #----y=Lat,x=avg,colour=`h[m]`,size=amp-------------------------------------------
    par(mar=c(1,1,1,1)) 
  P3=ggplot(datos_estaciones)+ 
    xlab("Latitude") + 
    ylab("Mean") +
    scale_color_viridis()+
    geom_point(aes(y=Lat,x=avg,colour=`h[m]`,size=amp)) +
    ggtitle(paste0("Relation between Latitude, Height and ",var," mean"))+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title=element_text(size=14,face="bold"))+
    labs(colour = "Height [m]", size="Amplitude")
  
  NAME1=paste0(path_proc,"/",series,"/analisis/lat_vs_avg_amp.jpg")
  ggsave(NAME1,plot = P3,dpi=600, width = 25, height = 25,units="cm")
  
  #------Scaterplot: Latitud vs amplitud---------------------------------------------
  par(mar=c(1,1,1,1)) 
  P6=ggplot(datos_estaciones,aes(y=abs(Lat),x=amp))+ 
    ylab("abs(Latitud)") + 
    xlab("Amplitude") +
    geom_point(size=2) +
    scale_color_viridis()+
    ggtitle("Scatter plot Latitud vs Amplitude")+
    scale_y_continuous(breaks= seq(0,90,10)) +
    scale_x_continuous(breaks= seq(0,100,5))+
    geom_hline(yintercept=0, linetype="dashed", color = "yellow") + 
    #  geom_text(aes(35,-3,label = "Equator", vjust = -0.5))+
    geom_hline(yintercept=23.27, linetype="dashed", color = "orange")+
      geom_text(aes(35,10,label = "Tropic zone", vjust = -0.5))+
    geom_hline(yintercept=35, linetype="dashed", color = "red")+
      geom_text(aes(35,27,label = "Sub-Tropic zone", vjust = -0.5))+
    geom_hline(yintercept=66, linetype="dashed", color = "blue")+
      geom_text(aes(35,50,label = "Temperate zone", vjust = -0.5))+
      geom_text(aes(35,67,label = "Frigid zone", vjust = -0.5))
  
  
  NAME1=paste0(path_proc,"/",series,"/analisis/corr_lat_vs_amp.jpg")
  ggsave(NAME1,plot = P6,dpi=600, width = 15, height = 15,units="cm")
  
  #------Scaterplot: Latitud vs avg---------------------------------------------
  par(mar=c(1,1,1,1)) 
  P7=ggplot(datos_estaciones,aes(y=abs(Lat),x=avg))+ 
    ylab("abs(Latitud)") + 
    xlab("Average") +
    geom_point(size=2) +
    scale_color_viridis()+
    ggtitle("Scatter plot Latitud vs Mean")+
    scale_y_continuous(breaks= seq(0,90,10)) +
    scale_x_continuous(breaks= seq(0,100,5))+
    geom_hline(yintercept=0, linetype="dashed", color = "yellow") + 
    #  geom_text(aes(35,-3,label = "Equator", vjust = -0.5))+
    geom_hline(yintercept=23.27, linetype="dashed", color = "orange")+
    geom_text(aes(50,17,label = "Tropic zone", vjust = -0.5))+
    geom_hline(yintercept=35, linetype="dashed", color = "red")+
    geom_text(aes(50,27,label = "Sub-Tropic zone", vjust = -0.5))+
    geom_hline(yintercept=66, linetype="dashed", color = "blue")+
    geom_text(aes(50,50,label = "Temperate zone", vjust = -0.5))+
    geom_text(aes(50,67,label = "Frigid zone", vjust = -0.5))
  
  NAME1=paste0(path_proc,"/",series,"/analisis/corr_lat_vs_avg.jpg")
  ggsave(NAME1,plot = P7,dpi=600, width = 15, height = 15,units="cm")
  
  print(paste0("Plots saved in ",path_proc,"/",series))
  
}
