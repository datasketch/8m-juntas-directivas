library(highcharter)
library(readr) #Leer archivos
library(dplyr) #Hacer operaciones sobre data
library(tidyr) #Organizar data sin hacer operaciones
library(purrr) #programación funcional
library(htmlwidgets)

#Leer y limpiar datos
data <- read_csv("data/graficos1y2.csv")
data_clean <- data %>% select(Pais, '2019' = `Porcentaje 2019`, '2020' = `Porcentaje 2020`)
data_clean <- data_clean %>% gather('Año', 'Porcentaje', -Pais)
data_clean$Porcentaje <- gsub('%', '', data_clean$Porcentaje)
data_clean_final <- transform(data_clean, Porcentaje = as.numeric(Porcentaje))

#Construir la serie para graficar
serie <- map(unique(data_clean_final$Pais), function(categoria) {
  d_filter <- data_clean_final %>% filter(Pais %in% categoria)
  d_end <- list(name = unique(d_filter$Pais), data = d_filter$Porcentaje
  )
})

#Definir el tema de la grafica
#thm <- hc_theme(colors = c('#fe5f55', '#30d4c8', '#5c30ff',
#                           '#b892ff', '#993ddf', '#5bd573',
#                           '#f3516b', '#ffdd4c', '#5c8aff',
#                           '#cff473', '#ffccaa', '#ff9d28',
#                           '#b4e9ff', '#136f63', '#f678d6'),
thm <- hc_theme(colors = c('#ffccaa','#ff9d28', '#136f63', '#5c8aff',
                           '#ed4743', '#ff99e5', '#993ddf',
                           '#ffdd4c', '#ff4a94', '#cff473',
                           '#b892ff', '#aa1d53', '#30d4c8',
                           '#b4e9ff', '#5c30ff', '#5bd573'),
                chart = list(backgroundColor = "transparent"),
                title = list(style = list(color ='#333333')),
                subtitle = list(style = list(color ='#666666')),
                legend = list(itemStyle = list(fontFamily ="Lato",
                                               color = '#333333'),
                              itemHoverStyle = list(color ='#666666')))

#Construir la grafica
highchart() %>% 
  hc_chart(type="line",
           spacingLeft = 5,
           spacingRight = 5) %>% 
  hc_add_series_list(serie) %>%
  hc_title(text = "Porcentaje de mujeres en juntas directivas de América Latina",
           margin = 50,
           style = list(fontSize = "20px",
                        fontFamily = "Lato",
                        fontWeight = "bold")) %>%
  hc_xAxis(type = "category", 
           categories = map(unique(data_clean_final$Año), function(z) z),
           lineWidth = 0,
           tickWidth = 0,
           opposite = TRUE,
           tickmarkPlacement = 'on',
           tickInterval = 1,
           gridLineWidth = 2,
           minorGridLineColor = '#666666',
           labels = list( style = list( fontSize = "18px",
                                        fontFamily = "Lato"))
  ) %>% 
  hc_yAxis(visible = FALSE,
           #endOnTick = TRUE,
           maxPadding = 0,
           #ceiling = 24,
           softThreshold = TRUE,
           softMin = 0,
           min = 0) %>%
  hc_plotOptions(
    line = list(dataLabels = list(enabled = TRUE,
                                  reserveSpace = TRUE,
                                  allowOverlap = TRUE,
                                  padding = 0,
                                  useHTML = TRUE,
                                  align = 'right',
                                  #x = -10,
                                  #y = 13,
                                  formatter = JS("function() { var label = this.series.name + \": \" + this.point.y; var len = label.length + 5; if(this.point.x == '0') {return '<span style=\"position: relative; top: 7px; left: -15px;\">' + this.point.y + '%</span>';} else {return '<span style=\"position: relative; left: calc(100% + 25px); top: 7px;\"><b>' + this.series.name + ':</b> ' + this.point.y + '%</span>';} }"),
                                  style = list(fontFamily = 'Lato',
                                               color = '#333333',
                                               fontSize = '14px',
                                               fontWeight = 'light')),
                marker = list(symbol = 'circle',
                              radius = 7,
                              states = list(hover = list(radius = 10))),
                lineWidth = '5px',
                shadow = TRUE,
                states = list(hover = list(lineWidth = '10px')))
  ) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "{point.series.name}: {point.y}%") %>%
  hc_add_theme(thm)






