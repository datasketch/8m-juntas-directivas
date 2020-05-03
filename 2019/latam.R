

# Porcentaje de asientos ocupados por hombres y mujeres en cada país
# (esto es para el mapa, que además incluye un desplegable con la nota
#   de cada país)
#


library(tidyverse)
library(leaflet)
library(geodata)
library(leaflet.minicharts)
library(htmlwidgets)
library(htmltools)
library(hgchmagic)

juntas <- read_csv("data/Todas las mujeres - Consejeres.csv")

total <- juntas %>%
  group_by(`Género (f, m, nb, nd)`, País) %>%
  summarise(total = n())

total <- total[grep("f|m", total$`Género (f, m, nb, nd)`),]
total <- total[-1,]
total <- total %>% plyr::rename(c("País" = "name_esp", "Género (f, m, nb, nd)" = "genero"))
total$genero <- plyr::revalue(total$genero, c("f" = "Mujeres", "m" = "Hombres"))

orderTot <- total %>% group_by(name_esp, genero)%>% arrange(-total)
h <- hgch_bar_CatCatNum(orderTot,
                        tooltip = list(headerFormat = NULL,
                                       pointFormat = "Total de <b>{series.name} en juntas directivas </b> en <b>'{point.category}' </b>: <b>{point.y}</b>"),
                  # orientation = "hor",
                  graphType = "stacked",
                  #percentage = TRUE,
                  order1 = c("Mujeres", "Hombres"),
                   order2 = unique(orderTot$name_esp),
                  horLabel = " ",  verLabel = " ",
                  theme =  tma(
                    background = "#FFFFFF",
                    colores = c("#4B08B3",
                                "#008075"),
                    fontFamily = "Lato",
                    fontSize = "13px",
                    plotBorderWidth = 0,
                    bordercolor = "transparent",
                    stylesTitleX = list(color = "#666666", fontSize = "17px"),
                    stylesTitleY = list(color = "#666666", fontSize = "17px"),
                    stylesY = list(gridLineWidth = 0),
                    stylesX = list(gridLineWidth = 0),
                    stylesLabelX = list(color = "#666666",
                                        fontSize = "13px", enabled = TRUE),
                    stylesLabelY = list(enabled = F),
                    labsData = list(colLabel = "contrast", familyLabel = "Lato", sizeLabel = NULL, textDecoration = "none")
                  )) #%>%
  # hc_plotOptions(
  #   column = list(
  #     dataLabels = list(
  #       inside = F
  #       #format= '{y}%'
  #     )))

h
# h <- h %>% hc_plotOptions(
#     column = list(
#       stacking = 'percent'
#   )
#   )

h
saveWidget(h, paste0("",mop::create_slug("allCountries"),"_1.html"), selfcontained = FALSE, libdir = "assets")



total <- total %>% group_by(name_esp) %>% mutate(prop = round(total/sum(total)*100, 2))

popp <- read_csv("data/control_datasets - Hoja 1.csv")
popp$País <- gsub("Guate", "Guatemala", popp$País)
popp$País <- gsub("Peru", "Perú", popp$País)
total <- left_join(total, popp, by = c("name_esp" = "País"))

# total$titulo <- "Esto es un título"
# total$descripcion <- "Esto es una descripción"
# total$link <- "acá va un link"
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
dt@data$Encabezado[is.na(dt@data$Encabezado)] <- " "
dt@data$Encabezado <- gsub("%", "&#37", dt@data$Encabezado)
dt@data$Partner[is.na(dt@data$Partner)] <- " "
dt@data$`Enlace nota`[is.na(dt@data$`Enlace nota`)] <- " "
dt@data$`Enlace nota` <- gsub("%", "&#37", dt@data$`Enlace nota`)
dt@data$nameId <- tolower(gsub(" ", "" ,dt@data$name))
dt@data$nameId <- gsub("brazil", "brasil", dt@data$nameId)
dt@data$nameId <- gsub("spain", "españa", dt@data$nameId)
# dt@data$link[is.na(dt@data$link)] <- " "

pale <- c('#DDA0DD', '#4900a3')
pal <- colorNumeric(pale,
                    domain = dt@data$prop, na.color = '#CCCCCC')

classMap <- ".btn {padding: 0.5rem 1rem;font-family: 'Lato';font-size: 90%;text-transform: uppercase;letter-spacing: .15rem;border: 0;font-weight: bold;transition: 0.3s;}"

labels <- sprintf(
  paste0("<h4>", dt@data$name_esp, "</h4><p><b>",
         dt@data$prop,"&#37</b> de asientos ocupados por consejeras</p>
         <p style='font-size:11px;display:block;margin-bottom:2rem'><b>&quot;",
         dt@data$Encabezado, "&quot; -", dt@data$Partner, "</b></p>
           <a class='btn btnprimary' style='margin-right:0.5rem; text-decoration: none;position: relative;
          color: #4900A3;' href='",dt@data$`Enlace nota`,"'  target='_newtab'>Leer</a>
           <a class='btn btnsecondary' style='margin-right:0.5rem;text-decoration: none;position: relative;
          color: #4900A3;' href='https://www.mujeresenlabolsa.org/pais.html#", tolower(dt@data$nameId),"' target='_top'><b> Ver más</a></b></a>"
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
    scrollWheelZoom = FALSE,
    zoomControl = FALSE,
    minZoom = 3, maxZoom = 3,
    dragging = FALSE
  )) %>%
  setView(lng = -60, lat = -7, zoom = 3)

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




m <- browsable(
  tagList(list(
    tags$head(
      # you'll need to be very specific
      tags$style("
h1, h2, h3, h4, h5, h6 { font-family: 'Exo 2', sans-serif;
                 font-weight: bold;
                 }
                 body {
                 font-family: 'Lato';
                 }
                 a, a:visited {
                 text-decoration: none;
                 position: relative;
                 color: #4900A3;
                 }
                 a:focus, a:hover {
                 text-decoration: none;
                 color: #4900A3;
                 }
                 a:hover:after, a:visited:hover:after {
                 width: 100%;
                 }
                 .btn {
                 padding: 0.5rem 1rem;
                 font-family: 'Lato';
                 font-size: 90%;
                 text-transform: uppercase;
                 letter-spacing: .15rem;
                 border: 0;
                 font-weight: bold;
                 transition: 0.3s;
                 }
                 a.btn:after, a.btn:visited:after {
                 content: none;
                 }
                 .btnprimary {
                 color: #fff !important;
                 background-color: #4900A3;
                 text-align: center;
                 padding-left: 2rem;
                 padding-right: 2rem;
                 }
                 .btnprimary:hover, .btnprimary:focus, .btnprimary:active {
                 background-color: #3700E1 !important;
                 }
                 .btnsecondary {
                 color: #4900A3 !important;
                 background-color: transparent;
                 text-align: center;
                 border: 1px solid #d2d2d2;
                 }
                 .btnsecondary:hover, .btnsecondary:focus, .btnsecondary:active {
                 background-color: #d2d2d2 !important;
                 }
                 ")
      # could also use url
      #tags$link(href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css",rel="stylesheet")
    ),
    m
  ))
)

save_html(m, "mapa.html")

# write_lines(m, "mapa.html")
# saveWidget(m, paste0("countries/",mop::create_slug("mapa"),"_1.html"), selfcontained = FALSE, libdir = "countries/assets")
#
# saveWidget(m, paste0("map_",i,".html"))
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







