

# Generate charts for every country


library(tidyverse)
library(hgchmagic)
library(htmlwidgets)


#+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE

x0 <- read_csv("data/Todas las mujeres - Consejeres.csv")

countries <- unique(x0$País)
x0 <- x0 %>% filter(País %in% countries[c(1:15)])

for (country in countries) {
  #country <- "Argentina"
  x <- x0 %>% filter(País == country) %>%
    rename(Género = `Género (f, m, nb, nd)`)
  x$Género <- plyr::revalue(x$Género, c("f" = "Mujeres", "m" = "Hombres"))
  table(x$Género)
  #x %>% group_by(Género, Empresa) %>% summarise(total = n())
  country <- tolower(gsub(" ", "",country))
  country <- iconv(country,from="UTF-8",to="ASCII//TRANSLIT")
  # x <- x %>%
  #   filter(Género != c("Mujeres", "Hombres"))


  mytheme <- tma(
    background = "#FFFFFF",
    colores = c("#008075", "#4B08B3"),
    # colores = c("#0EA5B8",
    #             "#4B08B3",
    #             "#562BFA",
    #             "#662AAF",
    #             "#1FACC6",
    #             "#2BCEC1"),
    fontFamily = "Lato",
    fontSize = "13px",
    plotBorderWidth = 0,
    bordercolor = "transparent",
    stylesTitleX = list(color = "#666666", fontSize = "17px"),
    stylesTitleY = list(color = "#666666", fontSize = "17px"),
    stylesY = list(gridLineWidth = 0),
    stylesX = list(gridLineWidth = 0),
    stylesLabelX = list(color = "#666666",
                        fontSize = "11px", enabled = TRUE),
    stylesLabelY = list(enabled = F),
    labsData = list(colLabel = "contrast", familyLabel = "Lato", sizeLabel = NULL, textDecoration = "none")
  )



  # Empresas por cantidad de consejeras. Este gráfico serían barras con la cantidad de consejeras,
  # por ejemplo 8, y la cantidad de empresas que tienen esa cantidad de consejeras en ese país.

  d <- x %>%
    select(Género, Empresa) %>%
    group_by(Empresa) %>%
    summarise(n_f = sum(Género == "Mujeres")) %>%
    group_by(n_f) %>%
    summarise(n_emp = n())

  h <- hgch_bar_CatNum(d, verLabel = "Número de empresas", horLabel = "Número de consejeras mujeres",
                       tooltip =list(headerFormat = NULL, pointFormat = "<b>{point.y}</b> empresas</b> tienen <b>{point.name}</b> consejeras"),
                       title = "",
                       theme = tma(
                         fontSize = "17px",
                         bordercolor = "transparent",
                         plotBorderWidth = 0,
                         background = "#FFFFFF",
                         colores = c("#4B08B3"),
                         fontFamily = "Lato",
                         diffColorsBar = F,
                         stylesY = list(gridLineWidth = 0),
                         stylesLabelX = list(color = "#666666",
                                             fontSize = "15px", enabled = TRUE),
                         stylesLabelY = list(enabled = F),
                         stylesTitleX = list(color = "#666666", fontSize = "17px"),
                         stylesTitleY = list(color = "#666666", fontSize = "17px"),
                         labsData = list(colLabel = "#0E0329", familyLabel = "Lato")
                       ))

  h
  saveWidget(h, paste0("countries/",mop::create_slug(country),"_1.html"), selfcontained = FALSE, libdir = "countries/assets")


  # Cantidad de hombres y mujeres,
  # Cantidad de hombres por cada mujer (promedio 7),


  # d0 <- x %>%
  #   select(Género, Empresa) %>%
  #   group_by(Empresa) %>%
  #   summarise(n_h_por_m = sum(Género == "Hombres")/sum(Género == "Mujeres")) %>%
  #   mutate(paridad = ifelse(n_h_por_m <= 1, 0, floor(n_h_por_m))) %>%
  #   mutate(paridad2 = ifelse(paridad == 0, "Mayoría mujeres o paridad",
  #                            ifelse(is.infinite(paridad), "Solo hombres",
  #                                   ifelse(paridad == 1, "Más de 1",
  #                                          paste0("Más de ",paridad, ""))))) %>%
  #   arrange(paridad) %>%
  #   group_by(paridad, paridad2) %>%
  #   summarise(n_emp = n()) %>%
  #   ungroup()
  #
  # d <- d0 %>% select(paridad2, n_emp) %>% ungroup()
  #
  # h <- hgch_bar_CatNum(d, verLabel = "Número de empresas", horLabel = "Número de hombres por mujer",
  #                      title = "",
  #                      order2 = unique(d$paridad2),
  #                      labelWrapV = c(80, 80),
  #                      theme = mytheme)
  #
  # h
  # saveWidget(h, paste0("countries/",mop::create_slug(country),"_2.html"), selfcontained = FALSE, libdir = "countries/assets")


  # cantidad de mujeres en más de una empresa (promedio 4)
  # ranking. Mejores y Peores Empresas



  # Top Empresas

  d0 <- x %>%
    group_by(Género, Empresa) %>%
    summarise(total = n())  %>% filter(Género != "nd") %>%
    group_by(Empresa) %>% mutate(prop = round(total/sum(total)*100, 2))

  cienH <- d0 %>%
        filter(prop == 100) %>%
          arrange(-total)
  cienH$prop <- 0

  empresasTop <- d0 %>% filter(Género == "Mujeres") %>% ungroup() %>%
    arrange(desc(prop)) %>%
    slice(1:10) %>% pull(Empresa)


    n <-  nrow(cienH)
    l <- 10 -  n
    if (n  > 10) {
      empresasBottom <- cienH[1:10,]  %>% pull(Empresa)
    } else if (n > 0 || n < 10) {
      empresasBottom <-  d0 %>% filter(Género == "Mujeres") %>% ungroup() %>%
        arrange(prop) %>%
        slice(1:l) %>% bind_rows(cienH) %>% pull(Empresa)
    } else {
      empresasBottom <-  d0 %>% filter(Género == "Mujeres") %>% ungroup() %>%
        arrange(prop) %>%
        slice(1:n) %>% pull(Empresa)
    }




   #%>% pull(Empresa)

  d <- d0 %>%
    filter(Empresa %in% empresasTop) %>%
    arrange(prop) %>%
    select(-prop)

  h <- hgch_bar_CatCatNum(d,
                          #title = "Top empresas con participación de mujeres",
                          tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en la empresa <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                          orientation = "hor",
                          labelWrapV = c("12", "300"),
                          horLabel = " ",
                          graphType = "stacked",
                          percentage = TRUE,
                          order2 = empresasTop,
                          theme = mytheme) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          format= '{y}%'
      )))


  h
  saveWidget(h, paste0("countries/",mop::create_slug(country),"_3.html"), selfcontained = FALSE, libdir = "countries/assets")



  d <- d0 %>%
    filter(Empresa %in% empresasBottom) %>%
    arrange(prop) %>%
    select(-prop)

  h <- hgch_bar_CatCatNum(d,
                          colors = c("#4B08B3","#008075"),
                          #title = "Bottom empresas con participación de mujeres",
                          tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en la empresa <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                          orientation = "hor",
                          labelWrapV = c("12", "300"),
                          horLabel = " ",
                          graphType = "stacked",
                          percentage = TRUE,
                          order2 = empresasBottom,
                          theme = mytheme) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          format= '{y}%'
        )))

  h
  saveWidget(h, paste0("countries/",mop::create_slug(country),"_4.html"), selfcontained = FALSE, libdir = "countries/assets")

}

