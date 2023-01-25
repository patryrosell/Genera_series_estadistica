genera_graficos_series <- function(path_datos, path) {
  options(dplyr.summarise.inform = FALSE)
  
  # Read parameters
  parametros = read_delim(
    paste0(path, '/parametros.txt'),
    skip = 2,
    skip_empty_rows = TRUE,
    col_names = c('Par', 'valor'),
    col_types = cols(),
    delim = '=',
    progress = FALSE
  )

  fecha1 = dmy(parametros[which(parametros$Par == 'fecha1'), 2])
  fecha2 = dmy(parametros[which(parametros$Par == 'fecha2'), 2])
  
  y1 = year(fecha1)
  y2 = year(fecha2)
  
  fecha1 = format(fecha1, '%Y%m%d')
  fecha2 = format(fecha2, '%Y%m%d')
  
  var = as.character(parametros[which(parametros$Par == 'var'), 2])
  
  unidad = as.character(parametros[which(parametros$Par == 'unidad'), 2])
  
  flag_n = as.numeric(parametros[which(parametros$Par == 'flag_n'), 2])
  
  outli = as.numeric(parametros[which(parametros$Par == 'outliers'), 2])
  
  stdv = as.numeric(parametros[which(parametros$Par == 'stdv'), 2])
  
  n_datos = as.numeric(parametros[which(parametros$Par == 'n_datos'), 2])
  
  path_proc = as.character(parametros[which(parametros$Par == 'path'), 2])
  
  G1 = as.character(parametros[which(parametros$Par == 'G1'), 2])
  G2 = as.character(parametros[which(parametros$Par == 'G2'), 2])
  G3 = as.character(parametros[which(parametros$Par == 'G3'), 2])
  G4 = as.character(parametros[which(parametros$Par == 'G4'), 2])
  G5 = as.character(parametros[which(parametros$Par == 'G5'), 2])
  G6 = as.character(parametros[which(parametros$Par == 'G6'), 2])
  G7 = as.character(parametros[which(parametros$Par == 'G7'), 2])
  G8 = as.character(parametros[which(parametros$Par == 'G8'), 2])
  G9 = as.character(parametros[which(parametros$Par == 'G9'), 2])
  G10 = as.character(parametros[which(parametros$Par == 'G10'), 2])
  G11 = as.character(parametros[which(parametros$Par == 'G11'), 2])
  
  series = paste0('Series_', fecha1, '_', fecha2, '_', var) 
  ylabb = paste0(var, ' ', unidad)
  
  if (dir.exists(paste0(path_proc, '/', series, '/anual'))) {
    print('Todo Ok')
  } else {
    dir.create(paste0(path_proc, '/', series, '/anual'))
    dir.create(paste0(path_proc, '/', series, '/decomp'))
    dir.create(paste0(path_proc, '/', series, '/por_hora'))
    dir.create(paste0(path_proc, '/', series, '/serie_yyyy'))
    dir.create(paste0(path_proc, '/', series, '/serie_pormes'))
    dir.create(paste0(path_proc, '/', series, '/norm_estand'))
    dir.create(paste0(path_proc, '/', series, '/mes_hora'))
    dir.create(paste0(path_proc, '/', series, '/serie_365'))
  }
  
  # Read SIRGAS stations list with coordinates
  data_stats = read_csv2(
    paste0(path, '/SIRGAS_abril_2021.csv'),
    col_types = cols(),
    progress = FALSE
  )
  data_stats = data_stats[, c(-5)]
  
  # Do we want to make graphics of all data available or only a few stations?
  if (flag_n == 0) {
    # Graphs to all stations processed
    resumen = read_delim(
      paste0(path_proc, '/', series, '/resumen_', var, '.txt'),
      delim = ';',
      col_types = cols()
    )
    stats = as.data.frame(unique(resumen$STAT))
    colnames(stats) = 'STAT'
  } else {
    # Only stations of the 'stats.txt' file
    stats = read_table(paste0(path, '/stats.txt'),
                       col_names = FALSE,
                       col_types = cols())
    stats = unique(stats)
    colnames(stats) = 'STAT'
  }
  
  G10_tabla = data.frame(matrix(nrow = nrow(stats), ncol = 6))
  colnames(G10_tabla) <- c('STAT', 'Lat', 'Long', 'Alt', 'Mean', 'SD')
  
  total_group = data.frame(matrix(ncol = nrow(stats), nrow = 289))
  
  # Let's plot..
  
  for (dd in 1:nrow(stats)) {
    stat = stats[[1]][dd]
    name_stat = stats[[1]][dd]
    
    print(paste0('>>> Plotting ', name_stat, ' [', dd, ']'))
    
    fileConn = NULL
    genera_acumulado(path_datos, name_stat, fileConn) -> datos
    
    if (is.null(datos)) {
      print(' No hay datos para graficar las series')
    } else {
      datos = datos[, c(1:5)]
      
      # Geographic information 
      
      data_stats2 = filter(data_stats, STAT == stat)
      data_stats2[, c(2:4)] = round(data_stats2[, c(2:4)], digits = 3)
      
      # Subtitle for plotting
      subtitulo = paste0('Lat: ',data_stats2[, 2],' - Long: ',data_stats2[, 3],' - Height [m]= ',data_stats2[, 4])
      
      ### G1 - Time series 
      
      if (G1 == 1) {
        p1 = ggplot(datos, aes(Date, get(var))) + xlab('Time') + ylab(ylabb) +
          geom_point(size = 1,
                     colour = 'blue',
                     alpha = 0.7) +
          theme_bw() +
          #geom_smooth(method = 'loess', span = 0.1)+
          ggtitle(paste0('Time series of ',var,' between ',min(datos$Date),' and ',max(datos$Date),' - Station ',name_stat)) +
          labs(subtitle = subtitulo)
        
        name_serie = paste0(path_proc, '/', series, '/', name_stat, '_', var, '.jpg')
        
        ggsave(
          name_serie,
          plot = p1,
          dpi = 300,
          width = 30,
          height = 15,
          units = 'cm'
        )
        
      }
      
      ### G2 - Monthly mean of the whole period
      
      if (G2 == 1) {
        resumen = datos %>% group_by(month = month(Date)) %>% 
          summarize(N = length(get(var)),
                    avg = mean(get(var), na.rm = TRUE),
                    stdv = sd(get(var), na.rm = TRUE),
                    ster = stdv / sqrt(N),
                    min = min(get(var), na.rm = TRUE),
                    max = max(get(var), na.rm = TRUE),
                    ci = ster * qt(0.95 / 2 + .5, N - 1))
        
        p2 = ggplot(resumen) + 
          geom_line(aes(month, avg)) + 
          geom_hline(yintercept = mean(resumen$avg),
                     color = 'red',
                     linetype = 2) +
          geom_point(aes(month, avg)) + 
          geom_errorbar(aes(x = month,
                            ymin = avg - ci,
                            ymax = avg + ci),
                        colour = 'black',
                        width = .1) +
          ggtitle(paste0('Monthly average of ',var,' between ',fecha1,' and- ',fecha2,' - Site: ',name_stat)) +
          xlab('Months') + 
          ylab(ylabb) + 
          scale_x_continuous(breaks = pretty_breaks()) + 
          scale_y_continuous(breaks = pretty_breaks()) +
          labs(subtitle = subtitulo)
        
        name_resumen = paste0(path_proc,'/',series,'/',name_stat,'_',var,'_resumen.jpg')
        
        ggsave(name_resumen,
               plot = p2,
               dpi = 300,
               width = 30,
               height = 15,
               units = 'cm')
      }
      
      ### G3 - Hourly average
      
      if (G3 == 1) {
        resumen_hh = datos %>% group_by(HH = hour(Date)) %>% summarize(
          N = length(get(var)),
          avg = mean(get(var), na.rm = TRUE),
          stdv = sd(get(var), na.rm = TRUE),
          ster = stdv / sqrt(N),
          min = min(get(var), na.rm = TRUE),
          max = max(get(var), na.rm = TRUE),
          ci = ster * qt(0.95 / 2 + .5, N - 1)
        )
        
        write.table(resumen_hh,
                    file = paste0(path_proc,'/',series,'/por_hora/',name_stat,'_',var,'.txt'),
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
        
        p22 = ggplot(resumen_hh) + geom_line(aes(HH, avg, group = 1)) +
          geom_hline(yintercept = mean(resumen_hh$avg),
                     color = 'red',
                     linetype = 2) +
          geom_point(aes(HH, avg)) +
          geom_errorbar(aes(x = HH,
                            ymin = avg - ci,
                            ymax = avg + ci),
                        colour = 'black',
                        width = .2) +
          ggtitle(paste0('Hourly average of ',var,' between ',fecha1,' and ',fecha2,' - Site: ',name_stat)) +
          xlab('Hour') +
          ylab(ylabb) +
          #scale_x_continuous(breaks= seq(0,24,2))+
          scale_y_continuous(breaks = pretty_breaks()) +
          labs(subtitle = subtitulo)
        
        name_resumen_hh = paste0(path_proc,'/',series,'/por_hora/',name_stat,'_',var,'_resumen.jpg')
        
        ggsave(name_resumen_hh,
               plot = p22,
               dpi = 300,
               width = 30,
               height = 15,
               units = 'cm')
        
      }
      
      ### G4 - Hourly average by year
      
      if (G4 == 1) {
        agru = datos %>% 
          group_by(year(Date), HH = hour(Date)) %>% 
          summarise(avg =mean(get(var)), N = length(get(var)))
        
        colnames(agru) = c('Year', 'Hour', 'Mean', 'N')
        agru$Hour = as.numeric(agru$Hour)
        
        mean = mean(agru$Mean)
        
        agru2 = agru %>% group_by(Year)
        
        agru2 = as.data.frame(agru2)
        
        par(mar = c(1, 1, 1, 1))
        
        plotty2 = ggplot(agru2,
                         aes(x = Hour,
                             y = Mean,
                             group = factor(Year),
                             colour = factor(Year))) +
          geom_point() +
          geom_line() +
          xlab('Hour') +
          ylab(ylabb) +
          #geom_errorbar(aes(x=Hour,ymin=Mean-ci, ymax=Mean+ci), width=.1) +
          theme(legend.position = 'top') + labs(colour = 'Year') +
          geom_point(size = 3) + #scale_x_continuous(breaks = seq(0,24,2)) +
          ggtitle(paste0('Hourly average by year for ', stat))
        
        ggsave(plotty2,
               file = paste0(path_proc, '/', series, '/serie_yyyy/', stat, '.jpg'),
               dpi = 600,
               width = 30,
               height = 15,
               units = 'cm')
        
        # Amount of data by year and hour
        
        total_horas = datos %>% group_by(year(Date), HH = hour(Date)) %>% summarise(N = n())
        colnames(total_horas) = c('Year', 'Hour', 'N')
        
        resumen_hs = spread(total_horas, Year, N)
        
        write.table(
          resumen_hs,
          file = paste0(path_proc, '/', series, '/serie_yyyy/', stat, '.txt'),
          row.names = FALSE,
          col.names = TRUE,
          quote = FALSE
        )
        
      }
      
      ### G5 - Hourly average for every month
      
      if (G5 == 1) {
        tot_horas_mes = datos %>% 
          group_by(year(Date), month(Date), HH = hour(Date)) %>% 
          summarise(Mean = mean(get(var)), N = n())
        
        colnames(tot_horas_mes) = c('Year', 'Month', 'Hour', 'Mean', 'N')
        
        tot_wrap = tot_horas_mes  
        
        tot_wrap$Hour = as.numeric(tot_wrap$Hour)
        
        par(mar = c(1, 1, 1, 1))
        
        plotty3 = ggplot(tot_wrap,
                         aes(Hour,
                             Mean,
                             group = factor(Year),
                             colour = factor(Year) )) + 
          geom_line() +
          geom_point(size = 1) +
          xlab('Hour') +
          ylab(ylabb) +
          labs(colour = 'Year') +
          scale_x_continuous(breaks = seq(0, 24, 2)) +
          ggtitle(paste0('Hourly average by month for ', stat)) +
          facet_wrap( ~ Month, scales = 'free')
        
        ggsave(plotty3,
               file = paste0(path_proc, '/', series, '/serie_pormes/', stat, '.jpg'),
               dpi = 600,
               width = 30,
               height = 15,
               units = 'cm')
        
      }
      
      ### G6 - Monthly average for every hour
      
      if (G6 == 1) {
        tot_horas_mes = datos %>% 
          group_by(year(Date), HH = hour(Date), month(Date)) %>% 
          summarise(Mean = mean(get(var)), N = n())
        colnames(tot_horas_mes) = c('Year', 'Hour', 'Month', 'Mean', 'N')
        
        plotty4 = ggplot(tot_horas_mes, aes(Month, Mean, colour = factor(Year))) + 
          geom_line() +
          xlab('Month') +
          ylab(ylabb) +
          labs(colour = 'Year') +
          #theme(legend.position='top')+labs(colour='Year')+
          geom_point(size = 1) + scale_x_continuous(breaks = seq(0, 24, 2)) +
          ggtitle(paste0('Montly average by hour for ', stat)) +
          facet_wrap( ~ Hour, scales = 'free', ncol = 4)
        
        ggsave(plotty4,
               file = paste0(path_proc,'/',series,'/serie_pormes/',stat,'_mes.jpg'),
               dpi = 600,
               width = 55,
               height = 45,
               units = 'cm')
        
      }
      
      ### G7 - Time Series decomposition (Additive method)
      
      if (G7 == 1) {
        tryCatch({ # Sometimes the time series is not long enough to make the decomposition
          
          numcol = which(colnames(datos) == var)
          out_ts = ts(datos[, numcol],
                      start = c(y1, 0),
                      frequency = 24 * 365)
          #plot.ts(out_ts)
          
          out3 = decompose(out_ts, type = 'additive')
          
          jpeg(paste0(path_proc,'/',series,'/decomp/',name_stat,'_decomp_',var,'.jpg'),
               width = 25,
               height = 25,
               units = 'cm',
               res = 300)
          
          p = plot(out3, col = 'dark red')
          dev.off()
          
        }, error = function(e) {
          cat('ERROR :', conditionMessage(e), '\n')
        })
        
      }
      
      ### G8 - Annual average
      
      if (G8 == 1) {
        par(mar = c(1, 1, 1, 1)) 
        
        datos_yyyy = datos %>% 
          group_by(year(Date)) %>% 
          summarise(avg = mean(get(var)))
        colnames(datos_yyyy) = c('YYYY', 'Mean')
        
        mean = mean(datos_yyyy$Mean)
        
        plotty = ggplot(datos_yyyy, aes(x = YYYY, y = Mean)) + 
          geom_line(aes(x = YYYY, y = Mean)) +
          xlab('Year') +
          ylab(ylabb) + #geom_smooth(method = 'loess',formula= y~x,se=F)+
          geom_hline(yintercept = mean,
                     linetype = 'dashed',
                     color = 'red') +
          geom_point(size = 3) +
          ggtitle(paste0('Annual mean value of IWV in ', name_stat))
        
        ggsave(
          plotty,
          file = paste0(path_proc, '/', series, '/anual/', name_stat, '.jpg'),
          dpi = 600,
          width = 30,
          height = 15,
          units = 'cm'
        )
        
      }
      
      ### G9 - Time series with linear fit
      
      if (G9 == 1) {
        p1 = ggplot(datos, aes(Date, get(var))) + 
          xlab('Time') + 
          ylab(ylabb) +
          geom_point(size = 1, colour = 'lightblue') +
          theme_bw() + 
          geom_smooth(method = 'lm', formula = y ~ x) + 
          stat_regline_equation() +
          ggtitle(paste0('Time series of ',var,' between ',fecha1,' and ',fecha2,' - Station ',name_stat)) +
          labs(subtitle = subtitulo)
        
        name_serie = paste0(path_proc,'/',series,'/',name_stat,'_',var,'_ajuste.jpg')
        
        ggsave(
          name_serie,
          plot = p1,
          dpi = 300,
          width = 30,
          height = 15,
          units = 'cm'
        )
        
      }
      
      # G10 - Residuals wrt the mean value
      if (G10 == 1) {
        meann = data.frame(matrix(nrow = 1, ncol = 2))
        meann[1, 1] = '0_0'
        meann[1, 2] = datos %>% summarize(mean(get(var), na.rm = TRUE))
        colnames(meann) <- c('Month_hour', 'avg')
        
        resumen2 = datos %>%
          group_by(month = month(Date), hour = hour(Date)) %>%
          summarize(avg = mean(get(var), na.rm = TRUE)) %>%
          unite('Month_hour', month:hour, remove = TRUE)
        
        ress = rbind(meann, resumen2)
        colnames(ress) <- c('MH', name_stat)
        
        write.table(ress,
                    file = paste0(path_proc,'/',series,'/mes_hora/',name_stat,'_data.txt'),
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
        
        resumen3 = datos %>% 
          group_by(month = month(Date), hour = hour(Date)) %>% 
          mutate(avg_hm = mean(get(var), na.rm = TRUE))
        
        datos_new = resumen3 %>% mutate(val = get(var) - avg_hm)
        
        G10_tabla[dd, 1] = name_stat
        G10_tabla[dd, 2] = data_stats2[, 2]
        G10_tabla[dd, 3] = data_stats2[, 3]
        G10_tabla[dd, 4] = data_stats2[, 4]
        G10_tabla[dd, 5] = mean(datos_new$val)
        G10_tabla[dd, 6] = sd(datos_new$val)
        
        p10 = ggplot(datos_new, aes(Date, val)) + 
          xlab('Time') + 
          ylab(ylabb) +
          geom_point(size = 1, colour = 'blue') +
          theme_bw() +
          ggtitle(paste0(var,' residuals wrt the mean ',var,' per hour - Station ',name_stat)) +
          labs(subtitle = subtitulo)
        
        name_serie = paste0(path_proc, '/', series, '/mes_hora/', name_stat, '.jpg')
        
        ggsave(
          name_serie,
          plot = p10,
          dpi = 300,
          width = 30,
          height = 15,
          units = 'cm'
        )
        
        write.table(G10_tabla,
                    file = paste0(path_proc, '/', series, '/mes_hora/resumen.txt'),
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
        
      }
      
      ### G11 - Daily average
      
      if (G11 == 1) {
        resumen_dia = datos %>% 
          group_by(month(Date), day(Date)) %>% 
          summarize(avg = mean(get(var), na.rm = TRUE))
        
        resumen_dia$JJ <- 1:nrow(resumen_dia)
        resumen_dia = resumen_dia[-c(1:2)]
        resumen_dia = resumen_dia[c(2, 1)]
        
        write.table(resumen_dia,
                    file = paste0(path_proc,'/',series,'/serie_365/',name_stat,'_',var,'.txt'),
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
        
        p22 = ggplot(resumen_dia) + 
          geom_point(aes(JJ, avg)) +
          geom_hline(
            yintercept = mean(resumen_dia$avg),
            color = 'red',
            linetype = 2
          ) +
          ggtitle(paste0('Daily average of ',var,' between ',fecha1,' and ',fecha2,' - Site: ',name_stat)) +
          xlab('Julian Day') +
          ylab(ylabb) +
          #scale_x_continuous(breaks= seq(0,24,2))+
          scale_y_continuous(breaks = pretty_breaks()) +
          labs(subtitle = subtitulo)
        
        name_resumen_hh = paste0(path_proc,'/',series,'/serie_365/',name_stat,'_',var,'_resumen.jpg')
        
        ggsave(
          name_resumen_hh,
          plot = p22,
          dpi = 300,
          width = 30,
          height = 15,
          units = 'cm'
        )
        
      }
      
    }
    
  }
  
  print('<> <> <>')
  
}

