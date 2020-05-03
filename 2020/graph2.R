library(highcharter)
library(readr) #Leer archivos
library(dplyr) #Hacer operaciones sobre data
library(tidyr) #Organizar data sin hacer operaciones
library(purrr) #programación funcional

data <- read_csv("data/graficos1y2.csv")

#Limpiar datos para graficar el errorbar chart
d_line <- data %>% select('name' = Pais, Codigo,'low' = `Porcentaje 2020`, 'high' = `Porcentaje ministras`)
df_line <- data.frame(d_line)

#Quitar porcentajes
df_line$name <- gsub('i ', '&#73;',  df_line$name)
df_line$low <- gsub('%', '', df_line$low)
df_line$high <- gsub('%', '', df_line$high)
df_final <- transform(df_line, low = as.numeric(low), high = as.numeric(high))
df_final <-  df_final %>% arrange(-low)

#Limpiar datos para graficar el line chart
d_scatter <- data %>% select('name' = Pais, 'codigo' = Codigo, 'bolsa' = `Porcentaje 2020`, 'ministerios' = `Porcentaje ministras`)
d_scatter$bolsa <- gsub('%', '', d_scatter$bolsa)
d_scatter <- d_scatter %>% arrange(-as.numeric(bolsa))
d_scatter <- d_scatter %>% gather('Cargo', 'Porcentaje', -'name', -'codigo')
d_scatter$Cargo <- gsub('bolsa', 'Bolsa de valores', d_scatter$Cargo)
d_scatter$Cargo <- gsub('ministerios', 'Ministerios', d_scatter$Cargo)

#Agregar informacion de color al line chart
data_color <- data.frame(color = c("#d1ffe2", "#30d4c8"), Cargo = unique(d_scatter$Cargo))
d_scatter <- d_scatter %>% left_join(data_color)
d_scatter$Porcentaje <- gsub('%', '', d_scatter$Porcentaje)
d_scatter_final <- transform(d_scatter, Porcentaje = as.numeric(Porcentaje))

d_code <- df_final
d_code$name <- paste0(df_final$Codigo, df_final$name)

#Construir la serie para el line chart
serie <- map(unique(d_scatter_final$Cargo), function(categoria) {
  d_filter <- d_scatter_final %>% filter(Cargo %in% categoria)
  d_end <- list(name = unique(d_filter$Cargo),
                data = d_filter$Porcentaje,
                color = unique(d_filter$color),
                codigo = unique(d_filter$codigo)
  )
})

#Definir el tema para la gráfica conjunta
thm <- hc_theme(chart = list(backgroundColor = "transparent"),
                title = list(style = list(color ='#ffffff')),
                subtitle = list(style = list(color ='#ffffff')),
                legend = list(itemStyle = list(fontFamily ="Lato",
                                               color = '#ffffff'),
                              itemHoverStyle = list(color ='#ffffff')))

#Construir la gráfica con un errorbar chart y un line chart
highchart() %>% 
  hc_chart(inverted = TRUE,
           errorbar = list(tooltip = list(enabled = FALSE))) %>%
  hc_add_series(df_final, 'errorbar') %>%
  hc_add_series_list(serie) %>%
  hc_title(text = "Porcentaje de mujeres en la bolsa o en ministerios de América Latina",
           style = list(fontSize = "20px",
                        fontFamily = "Lato",
                        fontWeight = "bold")) %>%
  hc_xAxis(type = "category", 
           categories = map(unique(d_code$name), function(z) z),
           lineWidth = 0,
           tickWidth = 0,
           labels = list( style = list( fontSize = "16px",
                                        fontFamily = "Lato",
                                        color = '#ffffff'),
                          useHTML = TRUE,
                          formatter = JS("function() {var pais = this.value + ''; return '<a style=\"color: #ffffff; text-decoration: none;\" href=\"https://www.quienesquien.wiki/paises/' + pais.slice(0,2) + '/mujeresenlabolsa\" target=\"_blank\">' + pais.slice(2) + '</a>'}")
                          )
  ) %>%
  hc_yAxis(title = list(text = "Porcentaje de mujeres (%)",
                        style = list(color = '#ffffff',
                                     fontSize = "16px",
                                     fontFamily = "Lato")),
           tickInterval = 10,
           labels = list( style = list( color = '#ffffff',
                                        fontSize = "16px",
                                        fontFamily = "Lato"))) %>%
  hc_plotOptions(
    series = list(events = list(legendItemClick = JS("function(){ return false; }"))),
    errorbar = list(stemWidth = 10,
                    color = "#785cde",
                    whiskerLength = 0,
                    enableMouseTracking = FALSE),
    line = list(color = "#30d4c8",
                lineWidth = 0,
                marker = list(symbol = 'circle',
                              radius = 7,
                              states = list(hover = list(radius = 10))),
                states = list(hover = list(lineWidth = "0px")))
  ) %>%
  hc_tooltip(formatter = JS("function(){ var pais = this.x + ''; var text = '<b>' + pais.slice(2) + '</b><br>' + this.series.name + \": \" + this.point.y + \" %\"; if(this.series.name != 'Series 1') return text }")) %>% 
  #hc_tooltip(format = '{value}%') %>% 
  hc_legend(verticalAlign = 'top',
            itemStyle = list( fontFamily = "Lato",
                          fontSize = "14px")) %>%
  hc_add_theme(thm)

