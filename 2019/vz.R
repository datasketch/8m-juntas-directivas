library(tidyverse)
library(leaflet)
library(geodata)
library(leaflet.minicharts)

juntas <- read_csv("data/Todas las mujeres - Consejeres.csv")

total <- juntas %>% 
          group_by(`Género (f, m, nb, nd)`, País) %>% 
           summarise(total = n()) 

total <- total[grep("f|m", total$`Género (f, m, nb, nd)`),]
total <- total[-1,]
total <- total %>% plyr::rename(c("País" = "name_esp", "Género (f, m, nb, nd)" = "genero"))
total$genero <- plyr::revalue(total$genero, c("f" = "Mujeres", "m" = "Hombres"))


total <- total %>% group_by(name_esp) %>% mutate(prop = round(total/sum(total)*100, 2))

colombia <- total %>%
            filter(name_esp == "Colombia") %>% 
              select(genero, total)
colombia <- colombia[,-1]
library(hgchmagic)


hgch_treemap_CatNum(colombia,
                    title = "<b>Participación de mujeres en juntas directivas en Colombia</b>",
                    colors = c("#00CAB9", "#FFCE00"),
                    showLegend = F,
                    tooltip = list(headerFormat = NULL, pointFormat = "Total de <b>{point.name} </b> en juntas directivas: <b>{point.value}</b>"),
                    theme = tma(background = "#F2F3F7", 
                                fontFamily = "Exo 2",
                                labsData = list(colLabel = "#000000", 
                                                familyLabel = "Exo 2",
                                                sizeLabel = "17px"
                                                )))

# Por cargo
juntas$Cargo <- Hmisc::capitalize(tolower(juntas$Cargo))
cargoCol <- juntas %>% 
             filter(País == "Colombia", `Género (f, m, nb, nd)` == "f") %>% 
              group_by(genero = `Género (f, m, nb, nd)`, Cargo) %>% 
                summarise(total = n()) 
cargoCol$genero <- plyr::revalue(cargoCol$genero, c("f" = "Mujeres"))
cargoCol <- cargoCol[,-1]

hgch_pie_CatNum(cargoCol,
                title = "Total de cargos ocupados por mujeres en juntas directivas",
                tooltip = list(headerFormat = NULL, pointFormat = "Total de mujeres en el cargo <b>'{point.name}' </b>: <b>{point.y}</b>"),
                theme = tma(
                  background = "#F2F3F7", 
                  showText = F,
                  fontFamily = "Exo 2",
                  colores = c("#3700E1", "#FFAA35", "#EB004A", "#00CAB9", "#FFCE00"),
                  showLegend = T))

# Por cargo
cargoCol <- juntas %>% 
  filter(País == "Colombia") %>% 
  group_by(genero = `Género (f, m, nb, nd)`, Cargo) %>% 
  summarise(total = n())  %>% filter(genero != "nd")
cargoCol$genero <- plyr::revalue(cargoCol$genero, c("f" = "Mujeres", "m" = "Hombres"))

hgch_bar_CatCatNum(cargoCol,
                   title = "Cargos ocupados por mujeres y hombres en juntas directivas",
                   verLabel = " ",
                   tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en el cargo <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                   graphType = "stacked", 
                   percentage = TRUE,
                   theme = tma(background = "#F2F3F7", 
                               colores = c("#FFCE00", "#00CAB9"),
                               fontFamily = "Exo 2"))


## Top empresas con participación de mujeres TOTAL

empresasCol <- juntas %>%
  filter(País == "Colombia") %>%
  group_by(genero = `Género (f, m, nb, nd)`, Empresa) %>%
  summarise(total = n())  %>% filter(genero != "nd")
# 
# empresasMujeres <- empresasCol %>% 
#                     filter(genero == "f") %>% 
#                      arrange(-total) %>% slice(1:10) 
# 
# empresasTotal <- empresasCol %>% filter(Empresa %in% empresasMujeres$Empresa)
# empresasTotal$genero <- plyr::revalue(empresasTotal$genero, c("f" = "Mujeres", "m" = "Hombres"))
# 
# hgch_bar_CatCatNum(empresasTotal,
#                    orientation = "hor", 
#                    labelWrapV = c("12", "70"),
#                    horLabel = " ",
#                    order2 = unique(empresasTotal$Empresa),
#                    theme = tma(background = "#F2F3F7", 
#                                colores = c("#00CAB9", "#FFCE00"),
#                                fontFamily = "Exo 2"))

## Top empresas con participación de mujeres PORCENTAJE

empresasPor <-  empresasCol %>% 
                  group_by(Empresa) %>% mutate(prop = round(total/sum(total)*100, 2))

empresasMujeres <- empresasPor %>% 
                    filter(genero == "f") %>% 
                      arrange(-prop) 
empresasMujeres <- empresasMujeres[1:10,]

empresasPor <- empresasPor %>% filter(Empresa %in% empresasMujeres$Empresa)
empresasPor$genero <- plyr::revalue(empresasPor$genero, c("f" = "Mujeres", "m" = "Hombres"))
empresasPor <- empresasPor %>% select(-prop)
hgch_bar_CatCatNum(empresasPor,
                   title = "Top empresas con participación de mujeres",
                   tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en la empresa <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                   orientation = "hor", 
                   labelWrapV = c("12", "70"),
                   horLabel = " ",
                   graphType = "stacked",
                   percentage = TRUE,
                   order2 = unique(empresasPor$Empresa),
                   theme = tma(background = "#F2F3F7", 
                               colores = c("#FFCE00", "#00CAB9"),
                               fontFamily = "Exo 2"))


# MAPAAS

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




dt <- geojsonio::topojson_read(geodataTopojsonPath("world_countries"))

dt@data <- dt@data %>% left_join(mujeres, by = "name")
dt@data$name_esp[is.na(dt@data$name_esp)] <- " "
#dt@data$prop[is.na(dt@data$prop)] <- " "
dt@data$titulo[is.na(dt@data$titulo)] <- "Sin información"
dt@data$descripcion[is.na(dt@data$descripcion)] <- " "
dt@data$link[is.na(dt@data$link)] <- " "

pale <- c('#FFAA35', '#4900a3')
pal <- colorNumeric(pale,
                domain = dt@data$prop, na.color = '#CCCCCC')


labels <- sprintf(
  paste0('<p>', dt@data$name_esp, '</span></br>','Mujeres en juntas directivas: ',dt@data$prop, '&#37</br>', dt@data$titulo, '</br>',
         dt@data$descripcion, '</br>', dt@data$link ,'</p>'
  )) %>% lapply(htmltools::HTML)

mapCan <- leaflet(dt) %>% 
            #addProviderTiles('OpenStreetMap.Mapnik') %>% 
           addTiles() %>% 
              setView(lng = -60, lat = -7, zoom = 2) 


mapCan %>% 
  addPolygons(stroke = FALSE, 
              smoothFactor = 0.3,
              fillOpacity = 1,
              fillColor = ~pal(prop),
              popup = labels
              ) %>% 
  addLegend(pal = pal, 
            title = "",
            position = c("bottomleft"),
            values = ~prop,
            opacity = 1.0,
            labFormat = labelFormat(prefix = "", suffix = "%"))

  
###########################################
centroides <- read_csv(system.file("geodata/world/world-countries.csv",package = "geodata"))

dd <- inner_join(total, centroides)

dd <- dd %>% select(-total) %>% spread(genero, prop)
dd$nb[is.na(dd$nb)] <-  0
dd$nd[is.na(dd$nd)] <-  0

colors <- c("#FA0A34", "#EBA41A", "#FAAC1A", "#EBBAAA")

mapCan %>% 
  addMinicharts(dd$lon,
                dd$lat,type = "pie",
                chartdata = dd[, c("f", "m", "nb", "nd")], 
                colorPalette = colors, 
                width = 15,height = 11,
                legendPosition = "bottomleft") 



mapCan %>% 
  addMinicharts(dd$lon,
                dd$lat,
                chartdata = dd[, c("f", "m", "nb", "nd")], 
                colorPalette = colors,
                width = 15,
                height = 11, 
                legendPosition = "bottomleft") 
