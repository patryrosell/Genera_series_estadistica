compilador_series<-function(path){
  
  suppressPackageStartupMessages({
    library(readr)
    library(tidyr)
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(lubridate)
    library(RColorBrewer)
    library(viridis)
    library(ggpubr)
    library(reshape2)
    library(ggpmisc)
    library(corrplot)
    #library(tidyverse)
  })

  # Read parameters
  parametros=read_delim(paste0(path,"/parametros.txt"),
                        skip=2,
                        skip_empty_rows = TRUE,
                        col_names = c("Par","valor"),
                        col_types = cols(),
                        delim="=",progress = FALSE)
  
  path_prog=as.character(parametros[which(parametros$Par == "path_prog"),2])
  
  # Load functions
  source(paste0(path_prog,"/genera_series_txt.R"))
  source(paste0(path_prog,"/genera_graficos_series.R"))
  source(paste0(path_prog,"/analisis_var.R"))
  source(paste0(path_prog,"/genera_acumulado.R"))
  source(paste0(path_prog,"/outliers.R"))
  
  genera_acum=as.numeric(parametros[which(parametros$Par == "genera_acumulado"),2])
  
  genera_graficos=as.numeric(parametros[which(parametros$Par == "genera_graficos_series"),2])
  
  analisis=as.numeric(parametros[which(parametros$Par == "analisis_var"),2])
  
  var=as.character(parametros[which(parametros$Par == "var"),2])
  path_proc=as.character(parametros[which(parametros$Par == "path"),2])
  
  path_datos=as.character(parametros[which(parametros$Par == "path_datos"),2])
  
  # Dates
  fecha1=format(dmy(parametros[which(parametros$Par == "fecha1"),2]), "%Y%m%d")
  fecha2=format(dmy(parametros[which(parametros$Par == "fecha2"),2]), "%Y%m%d")

  series=paste0("Series_",fecha1,"_",fecha2,"_",var) #Name of output folder
  
  if (dir.exists(paste0(path_proc,'/',series))){
    print("All good, let's begin....")
  } else {
    dir.create(paste0(path_proc,'/',series))
    print(paste0("Dir created: ",path_proc,'/',series))
  }
  
  if (genera_acum == 1){ # Do we want the accumulated data? Maybe we just want graphics..
    genera_series_txt(path,path_datos)
  }
  
  if (genera_graficos == 1){ # Do we want graphics?
    genera_graficos_series(path_datos,path) 
  }
  
  if (analisis == 1){ # Do we want statistical analysis?
    analisis_var(path_datos,path) 
  }
  
  print("All good things must come to an end..")

}
