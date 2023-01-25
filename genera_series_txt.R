genera_series_txt <- function(path, path_datos) {
  
  options(dplyr.summarise.inform = FALSE)
  
  # Read parameters
  parametros = read_delim(paste0(path, "/parametros.txt"),
                          skip = 2,
                          skip_empty_rows = TRUE,
                          col_names = c("Par", "valor"),
                          col_types = cols(),
                          delim = "=",
                          progress = FALSE
                        )
  
  # Dates
  fecha1 = dmy(parametros[which(parametros$Par == "fecha1"), 2])
  fecha2 = dmy(parametros[which(parametros$Par == "fecha2"), 2])
  
  y1 = year(fecha1)
  y2 = year(fecha2)
  
  fecha1 = format(fecha1, "%Y%m%d")
  fecha2 = format(fecha2, "%Y%m%d")
  
  var = as.character(parametros[which(parametros$Par == "var"), 2])
  unidad = as.character(parametros[which(parametros$Par == "unidad"), 2])
  
  n_datos = as.numeric(parametros[which(parametros$Par == "n_datos"), 2])
  
  path_proc = as.character(parametros[which(parametros$Par == "path"), 2])
  
  series = paste0("Series_", fecha1, "_", fecha2, "_", var) # Folder name

  # List of stations involved
  stats = read_table(paste0(path, "/stats.txt"),
                     col_names = FALSE,
                     col_types = cols())
  stats = unique(stats)
  
  # Read SIRGAS stations list with coordinates
  data_stats = read_csv2(paste0(path, "/SIRGAS_abril_2021.csv"),
                          col_types = cols(),
                          progress = FALSE
                        )
  data_stats = data_stats[, c(-5)] # Remove unnecessary data
  
  estadistica_stats = NULL
  
  # txt for saving the amount of removed observations
  removed <- paste0(path_proc, '/', series, "/n_removed.txt")
  fileConn <- file(removed, open = "wt")
  cat("STAT",
      "n_totales",
      "n_RMS",
      "n_outliers",
      file = fileConn,
      append = TRUE)
  cat("\n", file = fileConn, append = TRUE)
  
  # Let's begin.. Reading annual data and merging all together
  
  for (j in 1:nrow(stats)) {
    out2 = NULL
    
    name_stat = stats[[1]][j]
    print(paste0("Site ", name_stat, " (", j, " of ", nrow(stats), ")"))
    
    data_unique = filter(data_stats, STAT == name_stat)
    data_unique[, c(2:4)] = round(data_unique[, c(2:4)], digits = 3)
    
    # Lets create the accumulated data
    genera_acumulado(path_datos, name_stat, fileConn) -> out2
    
    if (is.null(out2)) {
      longanizmo = 0
    } else {
      longanizmo = (y2 - y1) + 1
      
      res = summarize(out2,
                        N = length(get(var)),
                        avg = mean(get(var), na.rm = TRUE),
                        stdv = sd(get(var), na.rm = TRUE),
                        ster = stdv / sqrt(N),
                        ci = ster * qt(0.95 / 2 + .5, N - 1)
                      )
      
      # Summarizing data
      resumen_mes = out2 %>% group_by(month(Date)) %>% summarize(avg = mean(get(var), na.rm = TRUE))
      
      # Amplitude 
      res$amp = max(resumen_mes$avg, na.rm = TRUE) - min(resumen_mes$avg, na.rm = TRUE)
      
      # Months of maxima and minima
      res$mes_max = resumen_mes$`month(Date)`[which(resumen_mes$avg == max(resumen_mes$avg))]
      res$mes_min = resumen_mes$`month(Date)`[which(resumen_mes$avg == min(resumen_mes$avg))]
      
      res$STAT = name_stat
      
      # Linear fit for long-term tendency of data
      res_lm = out2 %>%
        summarise(ordenada_a = lm(get(var) ~ Date)[1][[1]][[1]],
                  pendiente_b = lm(get(var) ~ Date)[1][[1]][[2]])
      
      res$ordenada_a = res_lm[[1]]
      res$pendiente_b = res_lm[[2]]
      
      # Adding the mean of each year to the summarized data
      
      out_anual = out2 %>% group_by(year(out2$Date)) %>% summarise(avg = mean(get(var)))
      
      tt = as.data.frame(t(out_anual))
      
      names(tt) <- tt %>% slice(1) %>% unlist()
      tt <- tt %>% slice(-1)
      row.names(tt) <- NULL
      
      res = cbind(res, tt)
      
      estadistica_stats = dplyr::bind_rows(estadistica_stats, res)
      
      #} # fin del loop que resume los datos
      
    } # fin del loop que controla si hay datos de salida o no
    
  } # fin del loop por estaciones
  
  # cierro txt
  close(fileConn)
  
  totales = merge(estadistica_stats,
                  data_stats,
                  by = "STAT",
                  all.x = TRUE)
  write.table(
    totales,
    file = paste0(path_proc, "/", series, "/resumen_", var, ".txt"),
    row.names = FALSE,
    col.names = TRUE,
    sep = ";",
    quote = FALSE
  )
  
  print(paste0("Data saved in ", path_proc, "/", series))
  
  # Copy of the parameters file, to always know how the process was made
  file.copy(
    paste0(path, "/parametros.txt"),
    paste0(path_proc, '/', series, "/parametros_usados.txt")
  )
  
}


