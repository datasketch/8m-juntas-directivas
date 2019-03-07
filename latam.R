

# Porcentaje de asientos ocupados por hombres y mujeres en cada país
# (esto es para el mapa, que además incluye un desplegable con la nota
#   de cada país)
#


library(tidyverse)
library(leaflet)
library(geodata)
library(leaflet.minicharts)
library(htmlwidgets)

juntas <- read_csv("data/Todas las mujeres - Consejeres.csv")

total <- juntas %>%
  group_by(`Género (f, m, nb, nd)`, País) %>%
  summarise(total = n())

total <- total[grep("f|m", total$`Género (f, m, nb, nd)`),]
total <- total[-1,]
total <- total %>% plyr::rename(c("País" = "name_esp", "Género (f, m, nb, nd)" = "genero"))
total$genero <- plyr::revalue(total$genero, c("f" = "Mujeres", "m" = "Hombres"))


total <- total %>% group_by(name_esp) %>% mutate(prop = round(total/sum(total)*100, 2))



total$titulo <- "Esto es un título"
total$descripcion <- "Esto es una descripción"
total$link <- "acá va un link"
total$name <- plyr::revalue(total$name_esp,
                            c("México" = "Mexico",
                              "Panamá" = "Panama",
                              "Brasil" = "Brazil",
                              "Perú" = "Peru",
                              "España" = "Spain"))


mujeres <- total %>% filter(genero == 'Mujeres')


## Handle topojson


tj <- geojsonio::topojson_read(geodataTopojsonPath("world_countries"))

countries <- mujeres$name

dat <- tj@data %>% mutate(.id = 0:(nrow(.)-1))
idx <- dat$name %in% countries

tj2 <- tj
tj2@polygons <- tj@polygons[idx]
tj2@data <- tj@data[idx,]
topojson_write(tj2, file="8m-juntas-mujeres.topojson")


dt <- geojsonio::topojson_read("8m-juntas-mujeres.topojson")


dt@data <- dt@data %>% left_join(mujeres, by = "name")
dt@data$name_esp[is.na(dt@data$name_esp)] <- " "
#dt@data$prop[is.na(dt@data$prop)] <- " "
dt@data$titulo[is.na(dt@data$titulo)] <- "Sin información"
dt@data$descripcion[is.na(dt@data$descripcion)] <- " "
dt@data$link[is.na(dt@data$link)] <- " "

pale <- c('#DDA0DD', '#4900a3')
pal <- colorNumeric(pale,
                    domain = dt@data$prop, na.color = '#CCCCCC')


labels <- sprintf(
  paste0('<p>', dt@data$name_esp, '</span></br>','Mujeres en juntas directivas: ',dt@data$prop, '&#37</br>', dt@data$titulo, '</br>',
         dt@data$descripcion, '</br>', dt@data$link ,'</p>'
  )) %>% lapply(htmltools::HTML)


providers <- c(
  #"Hydda.Base",
  #"Stamen.TonerLite",
  "Esri.WorldTerrain"#,
  #"CartoDB.Positron",
  #"HikeBike.HikeBike",
  #"Wikimedia"
)

#for (i in  1:length(providers)){
i <- 1
provider <- providers[i]

mapCan <- leaflet(dt) %>%
  addProviderTiles(provider, options = providerTileOptions(
    zoomControl = FALSE,
    minZoom = 2, maxZoom = 2,
    dragging = FALSE
  ))

m <- mapCan %>%
  addPolygons(#stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor = ~pal(prop),
    popup = labels,
    weight = 1,
    opacity = 1,
    color = '#F2F3F7'
  ) %>%
  addLegend(pal = pal,
            title = "",
            position = c("bottomright"),
            values = ~prop,
            opacity = 1.0,
            labFormat = labelFormat(prefix = "", suffix = "%"))


centroides <- read_csv(system.file("geodata/world/world-countries.csv",package = "geodata"))
dm <- inner_join(mujeres, centroides)
m <- m %>%
  addCircleMarkers(lng = dm$lon,
                   lat = dm$lat,
                   radius = dm$prop/3,
                   color = "#FFCE00",
                   opacity = 0,
                   fillColor = "#FFCE00",
                   popup = labels,
                   fillOpacity = 1)


m
saveWidget(m, paste0("map_",i,".html"))
#}



### Quintales

# Quintales por empresas. Sobre cada quintal qué porcentaje de mujeres tienen,
# es decir: el 20% de las empresas tienen entre 0 y 5% de mujeres.
# Otro 20% tienen entre 5% y 10%, otro 20% de empresas tienen
# entre 10 y 30% de mujeres y así... los números exactos se los vamos a pasar


empresas_f_pct <- juntas %>%
  select(Empresa,`Género (f, m, nb, nd)`) %>%
  group_by(Empresa) %>%
  summarise(pct_f = 100*sum(`Género (f, m, nb, nd)` == "f")/ n()) %>%
  na.omit()

quantile(empresas_f_pct$pct_f)

x <- empresas_f_pct %>%
  mutate(rank = ntile(pct_f, 5)) %>%
  arrange(pct_f) %>%
  group_by(rank) %>%
  summarize(min = min(pct_f), max = max(pct_f))

x <- x %>%
  mutate(rank = recode(rank, "Quintal 1", "Quintal 2", "Quintal 3", "Quintal 4", "Quintal 5"))
x

# empresas_f_pct %>%
#   group_by(Empresa) %>%
#   summarise(pct_f = list(enframe(quantile(pct_f, probs=c(0.25,0.5,0.75))))) %>%
#   unnest







