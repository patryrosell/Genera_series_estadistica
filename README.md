# Genera_series_estadistica

This project is part of the [Geodesy and Georeferencing Research Group](https://ingenieria.uncuyo.edu.ar/grupo-de-investigacion-aplicado-a-la-geodesia-y-georreferenciacion) of the SIRGAS Analysis Centre for the Neutral Atmosphere (CIMA) and the Remote Sensing Division of [Instituto de Capacitación Especial y Desarrollo de Ingeniería Asistida por Computadora (CEDIAC)](https://cediac.ingenieria.uncuyo.edu.ar/isat.html)

## Status

Active

## Objetive

This project aims to analyze time series from the GNSS tropospheric products of [Geodetic Reference System for the Americas (SIRGAS)](https://sirgas.ipgh.org/). It read data between two dates, generates an accumulated file, plots the results, and summarizes its data into basic statistics values.

## Institutions involved

- Facultad de Ingeniería ([FING](https://ingenieria.uncuyo.edu.ar/)) - Univesidad Nacional de Cuyo (Mendoza, Argentina)
- Facultad de Ingeniería y Enología ([INE](https://www.umaza.edu.ar/facultad-de-INE)) - Universidad Juan Agustín Maza (Mendoza, Argentina)
- Consejo Nacional de Investigaciones Científicas y técnicas ([CONICET](https://www.conicet.gov.ar/))

## Requirements

Download and Install [R](https://cran.r-project.org/)
Install the following libraries: readr, tidyr, dplyr, ggplot2, scales, lubridate, RColorBrewer, viridis, ggpubr, reshape2, ggpmisc, and corrplot.

## Getting started

1. Clone this repo
2. Fill the parameters file (see below)
3. Complete the "stats.txt" file with the name of the stations (4 letters in one column, no header)
4. Within an R terminal, set the working directory to the location of step 1.: setwd(path)
5. Load the "compilador_series.R": source("compilador_series.R")
6. Run the function "compilador_series" with the path to the parameters file "parametros.txt" as the only argument: compilador_series(path_to_parameters_file)

## How to fill the parameters file (parametros.txt)

* path= Absolute path to the output folder

* path_datos= Absolute path to data. Data should be structured as follow:\
-- "anual_YY", with YY being the last two numbers of the year\
---"STAT_YY.csv", with STAT being the name of a SIRGAS station (4 letters).\
--- Each file, without a header, with the following data:
  * Date: YYYY-MM-DD HH:MM:SS
  * Pressure: In Pascals [Pa]
  * Temperature: In Kelvin [K]
  * IWV (Integrated Water Vapour): In [Kg/m^2] or millimeters [mm] (equivalents)
  * ZTD (Zenith Total Delay): In meters [m]
  * RMS: Root-mean-square of ZTD, in meters [m]
  * centros: Amount of solutions involved in the ZTD adjust
	
* path_prog= Absolute path to programs (step 1.)
* fecha1= Initial date "DD-MM-YYYY" (between quotation marks)
* fecha2= Final date "DD-MM-YYYY" (between quotation marks)
* var: Variable to compute. Available Options: IWV, Temperature, Pressure, ZTD (between quotation marks).
* unidad: Unit of measurement between quotation marks. Only for labels. Changing it doesn't affect the final values of data. The outputs can be: 
  * IWV= "[kg/m²]" o "[kg/m^2]"
  * Temperature= "[ºC]"
  * Pressure= "[hPa]"
  * ZTD= "[m]"
* n_datos= Amount of data per year. This value will be multiplied by the number of years involved and will be used as a filter. If the stations don't present this number of data, will be rejected. Use this value with caution. If you do not want to filter by the amount of data, set this value to 0.
* stdv= Amount of standard deviation used for removing outliers. Ex. 3. The program will compute the monthly mean of each time series and will remove the observations that exceed 3 stdv wrt the mean value. 
* RMS= Threshold of the Root-mean-square of ZTD. For more information about this value [here]()
* HH= Hourly interval. Available options: 1 or 2 hours. 
* centros= Amount of centers involved in the ZTD adjustment. 

* genera_acumulado= Do you want summarized information on each station? Set 1 for yes, and 0 for no. IF 1, a file named "resumen_variable.txt" will be created with the following information: 
	* STAT= Station Name
	* N= Amount of data
	* avg= Average
	* stdv= Standard deviation
	* ster= Standard deviation error
	* ci= Confidence interval
	* amp= Amplitude
	* mes_max= Month of maxima value
	* mes_min= Month of minima value
	* ordenada_a= Y interception of the linear regression model (meaningless)
	* pendiente_b= Slope of linear regression model
	* AÑOS= Mean by every year involved
	* Lat= Latitude
	* Long= Longitude
	* h[m]= Ellipsoidal height

* guardar_serie= Do you want to save the accumulated data? Set 1 for yes, and 0 for no.
	
* genera_graficos_series= Do you want graphics? Set 1 for yes, and 0 for no.
  
* flag_n= If genera_graficos_series is 1, which list of stations do you want to read? If you read the total amount of stations you will have a plot for each station (Set this value to 0). If you want just a few stations, set this value to 1 and indicate the stations in the "stats.txt" file with no header. 

* Possible graphs: Set 1 for plotting, and 0 for not plotting.
	* G1  - Time series. Output folder: see Step 1.
	* G2  - Monthly mean of the whole period. Output folder: see Step 1. "STAT_resumen.jpeg"
	* G3  - Hourly average. Output folder: "por_hora"
	* G4  - Hourly average by year. Output folder: "serie_poraño"
	* G5  - Hourly average for every month. Output folder: "serie_pormes"
	* G6  - Monthly average for every hour. Output folder: "serie_pormes". "STAT_mes.jpeg"
	* G7  - Time Series decomposition (Additive method). Output folder: "decomp"
	* G8  - Annual average. Output folder: "anual"
	* G9  - Time series with a linear fit. Output folder: see Step 1. "STAT_ajuste.jpeg"
	* G10 - Residuals wrt the mean value. Output folder: "mes_hora"
	* G11 - Daily average. Output folder: "serie_365".

* analisis_iwv= Do you want extra statistical analysis? Set 1 for yes, and 0 for no. If 1, the program will compute histograms and correlations plots within the variables involved in the "resumen" file (therefore, it should exist). It also generates some multivariable scatter plots.  

## Last update: 

01-25-2013

## Author

Patricia A. Rosell, Ph.D.

## Contact info

For any doubts or more information, please contact me at:\
patricia.rosell[at]ingenieria.uncuyo.edu.ar\
prosell[at]profesores.umaza.edu.ar\
(to both emails to ensure you an answer)
