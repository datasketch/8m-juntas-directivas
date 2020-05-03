library(highcharter)
library(readr) #Leer archivos
library(dplyr) #Hacer operaciones sobre data
library(tidyr) #Organizar data sin hacer operaciones
library(purrr) #programación funcional
library(htmlwidgets)

#Leer y limpiar datos
data <- read_csv("data/graficos1y2.csv")
data_clean <- data %>% select(Pais, Codigo, '2019' = `Porcentaje 2019`, '2020' = `Porcentaje 2020`)
data_clean <- data_clean %>% gather('Año', 'Porcentaje', -Pais, -Codigo)
data_clean$Porcentaje <- gsub('%', '', data_clean$Porcentaje)
data_clean_final <- transform(data_clean, Porcentaje = as.numeric(Porcentaje))

#Construir la serie para graficar
serie <- map(unique(data_clean_final$Pais), function(categoria) {
  d_filter <- data_clean_final %>% filter(Pais %in% categoria)
  d_end <- list(name = unique(d_filter$Pais), code = unique(d_filter$Codigo), data = d_filter$Porcentaje)
})

#Definir el tema de la grafica
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
  hc_yAxis(visible = TRUE,
           gridLineDashStyle = 'dot',
           opposite = TRUE,
           gridLineWidth = 1.5,
           offset = -20,
           labels = list( format = '{value}%',
                          align = 'right',
                          style = list( fontSize = "14px",
                                        fontFamily = "Lato"))) %>%
  hc_plotOptions(
    line = list(marker = list(symbol = 'circle',
                              radius = 7,
                              states = list(hover = list(radius = 10))),
                lineWidth = '5px',
                shadow = TRUE,
                states = list(hover = list(lineWidth = '15px'))),
    series = list(events = list(legendItemClick = JS("function(){ return false; }")))
  ) %>%
  hc_legend(enabled = TRUE,
            align = 'left',
            verticalAlign = 'middle',
            layout = 'vertical',
            itemMarginBottom = 5,
            useHTML = TRUE,
            itemStyle = list(fontSize = '14px'),
            labelFormatter = JS("function() {return '<a style=\"color: #333333; text-decoration: none;\" href=\"https://www.quienesquien.wiki/paises/' + this.options.code + '/mujeresenlabolsa\" target=\"_blank\">' + this.name + '</a>'}")) %>%
  hc_tooltip(pointFormat = "<b>{point.series.name}</b>: {point.y}%") %>%
  hc_add_theme(thm)






