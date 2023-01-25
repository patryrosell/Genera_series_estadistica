genera_acumulado<-function(path_datos,name_stat,fileConn){
  
  # Read parameters
  parametros = read_delim(
    paste0(path, "/parametros.txt"),
    skip = 2,
    skip_empty_rows = TRUE,
    col_names = c("Par", "valor"),
    col_types = cols(),
    delim = "=",
    progress = FALSE
  )
  
  # Fechas
  fecha1 = dmy(parametros[which(parametros$Par == "fecha1"), 2])
  fecha2 = dmy(parametros[which(parametros$Par == "fecha2"), 2])
  
  y1 = year(fecha1)
  y2 = year(fecha2)
  
  var = as.character(parametros[which(parametros$Par == "var"), 2])
  unidad = as.character(parametros[which(parametros$Par == "unidad"), 2])
  
  flag_n = as.numeric(parametros[which(parametros$Par == "flag_n"), 2])
  
  stdv = as.numeric(parametros[which(parametros$Par == "stdv"), 2])
  
  n_datos = as.numeric(parametros[which(parametros$Par == "n_datos"), 2])
  
  guardar_serie = as.numeric(parametros[which(parametros$Par == "guardar_serie"), 2])
  
  path_proc = as.character(parametros[which(parametros$Par == "path"), 2])
  
  error = as.numeric(parametros[which(parametros$Par == "RMS"), 2])
  
  intervalo = as.numeric(parametros[which(parametros$Par == "HH"), 2])
  
  ncentros = as.numeric(parametros[which(parametros$Par == "centros"), 2])
  
  fecha11 = format(fecha1, "%Y%m%d")
  fecha22 = format(fecha2, "%Y%m%d")
  series = paste0("Series_", fecha11, "_", fecha22, "_", var) 
  
  per = NULL
  anual = NULL
  enull = NULL
  out = NULL
  
  for (i in y1:y2) {
    nn = 2
    xx = as.character(i)
    
    yy = substring(xx, seq(1, nchar(xx), nn), seq(nn, nchar(xx) + nn - 1, nn))
    path_data = paste0(path_datos, "/anual_", i, "/", name_stat, "_", yy[2], ".csv")
    
    if (file.exists(path_data)) {
      datos_var <- read_csv(path_data,
                              col_names = FALSE,
                              col_types = cols(),
                              progress = FALSE
                            )
      colnames(datos_var) = c("Date",
                              "Pressure",
                              "Temperature",
                              "IWV",
                              "ZTD",
                              "RMS",
                              "centros")
      
      peri = rbind(per, i)
      per = peri
    } else {
      datos_var = NULL
      print(paste0("   No data for year ", i))
    }
    
    total_raw <- rbind(out, datos_var)
    out = total_raw
    
  }
  
  
  if (is.null(out)) {
    out = 0
    longanizmo = 0
  } else {
    longanizmo = (y2 - y1) + 1
  }
  
  ylabb = paste0(var, " ", unidad)
  numcol = which(colnames(out) == var)
  
  if ((out != 0) && (nrow(out) > (n_datos * longanizmo)))  {
    # Filter for ERA5 possible errors
    out = dplyr::filter(out, get(var) > 0)
    
    # Filter by RMS
    out1 = dplyr::filter(out, RMS <= error)
    
    # Filter by amount of solutions involved
    out2 = dplyr::filter(out, centros >= ncentros)
    
    # Do we want data hourly or every two hours?
    if (intervalo == 2) {
      out3 = filter(out2, hour(Date) %in% c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22))
      out1 = out3
    } else {
      out3 = out2
    }
    
    # Removing outliers
    remove_outliers(out3, var, stdv) -> out4
    
    # Saving the amount of data removed in every filter applied.
    if (is.null(fileConn) == FALSE) {
      cat(
        name_stat,
        nrow(out),
        nrow(out1),
        nrow(out4),
        file = fileConn,
        append = TRUE
      )
      cat("\n", file = fileConn, append = TRUE)
    }
    
    out4$Temperature = out4$Temperature - 273.15 # From Kelvin to Celsius
    out4$Pressure = out4$Pressure / 100 # From Pascals to HeptoPascals
    
    data_raw = out4[, c(1:5)]
    
    f1 = as.Date(fecha1, '%Y%m%d')
    f2 = as.Date(fecha2, '%Y%m%d')

    out5 = data_raw %>% dplyr::filter(Date > f1 & Date < f2)
    
    # Do we want to save the accumulated data or we were interested only in the summarized info?
    if (guardar_serie == 1) {
      if (!dir.exists(paste0(path_proc, "/", series, "/data"))) {
        dir.create(paste0(path_proc, "/", series, "/data"))
      }
      
      filtro2 = out5

      write.table(
        filtro2,
        file = paste0(path_proc, "/", series, "/data/", name_stat, "_", var, ".txt"),
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE,
        sep = ";"
      )
    }

    
  } else {
    enull = enull + 1
    out5 = NULL
    print("     Not enough data for time series")
  }
  
  out5
  
}

