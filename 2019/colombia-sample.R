# # /* DOC
# rmarkdown::render(knitr::spin("generate-charts.R", format = "Rmd", knit = FALSE),output_format = "html_document")
# # END DOC */



#+ echo = FALSE, warning = FALSE,message = FALSE

library(tidyverse)
library(hgchmagic)
library(htmlwidgets)

#+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE

x0 <- read_csv("data/Todas las mujeres - Consejeres.csv")

contries <- unique(x0$País)
country <- "Colombia"

x <- x0 %>% filter(País == country) %>%
  rename(Género = `Género (f, m, nb, nd)`)
x$Género <- plyr::revalue(x$Género, c("f" = "Mujeres", "m" = "Hombres"))
table(x$Género)

x <- x %>%
  filter(Género != "nd")


mytheme <- tma(
  background = "#FFFFFF",
  colores = c("#4B08B3",
              "#0EA5B8",
              "#562BFA",
              "#662AAF",
              "#1FACC6","#2BCEC1"),
  fontFamily = "Lato"
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
                title = "",
                theme = mytheme)
h
saveWidget(h, "countries/colombia_1.html", selfcontained = FALSE, libdir = "countries/assets")


# Cantidad de hombres y mujeres,
# Cantidad de hombres por cada mujer (promedio 7),


d0 <- x %>%
  select(Género, Empresa) %>%
  group_by(Empresa) %>%
  summarise(n_h_por_m = sum(Género == "Hombres")/sum(Género == "Mujeres")) %>%
  mutate(paridad = ifelse(n_h_por_m <= 1, 0, floor(n_h_por_m))) %>%
  mutate(paridad2 = ifelse(paridad == 0, "Mayoría mujeres o paridad",
                           ifelse(is.infinite(paridad), "Solo hombres",
                                  ifelse(paridad == 1, "Más de 1",
                           paste0("Más de ",paridad, ""))))) %>%
  arrange(paridad) %>%
  group_by(paridad, paridad2) %>%
  summarise(n_emp = n()) %>%
  ungroup()

d <- d0 %>% select(paridad2, n_emp) %>% ungroup()

h <- hgch_bar_CatNum(d, verLabel = "Número de empresas", horLabel = "Número de hombres por mujer",
                     title = "",
                     order2 = unique(d$paridad2),
                     labelWrapV = c(80, 80),
                     theme = mytheme)

h
saveWidget(h, "countries/colombia_2.html", selfcontained = FALSE, libdir = "countries/assets")


# cantidad de mujeres en más de una empresa (promedio 4)
# ranking. Mejores y Peores Empresas



# Top Empresas

d0 <- x %>%
  group_by(Género, Empresa) %>%
  summarise(total = n())  %>% filter(Género != "nd") %>%
  group_by(Empresa) %>% mutate(prop = round(total/sum(total)*100, 2))

empresasTop <- d0 %>% filter(Género == "Mujeres") %>% ungroup() %>%
  arrange(desc(prop)) %>%
  slice(1:10) %>% pull(Empresa)
empresasBottom <-  d0 %>% filter(Género == "Mujeres") %>% ungroup() %>%
  arrange(prop) %>%
  slice(1:10) %>% pull(Empresa)

d <- d0 %>%
  filter(Empresa %in% empresasTop) %>%
  arrange(prop) %>%
  select(-prop)

h <- hgch_bar_CatCatNum(d,
                   title = "Top empresas con participación de mujeres",
                   tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en la empresa <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                   orientation = "hor",
                   labelWrapV = c("12", "70"),
                   horLabel = " ",
                   graphType = "stacked",
                   percentage = TRUE,
                   order2 = empresasTop,
                   theme = mytheme)

h
saveWidget(h, "countries/colombia_3.html", selfcontained = FALSE, libdir = "countries/assets")



d <- d0 %>%
  filter(Empresa %in% empresasBottom) %>%
  arrange(prop) %>%
  select(-prop)

h <- hgch_bar_CatCatNum(d,
                        title = "Top empresas con participación de mujeres",
                        tooltip = list(headerFormat = NULL, pointFormat = "Porcentaje de <b>{series.name}</b> en la empresa <b>'{point.category}' </b>: <b>{point.y}%</b>"),
                        orientation = "hor",
                        labelWrapV = c("12", "20"),
                        horLabel = " ",
                        graphType = "stacked",
                        percentage = TRUE,
                        order2 = empresasTop,
                        theme = mytheme)

h
saveWidget(h, "countries/colombia_4.html", selfcontained = FALSE, libdir = "countries/assets")




# mixCats <- function(x){
#   x %>% select(2,1, everything())
# }
#
# # Género
#
# d <- x %>%
#   select(Género) %>%
#   group_by(Género) %>%
#   summarise(n = n())
#
# #+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE
# hgch_bar_CatNum(d, verLabel = "Conteo Consejeros",
#                 title = "Participación de mujeres en Juntas Directivas en Colombia",
#                    theme = mytheme)
#
# #+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE
# hgch_pie_CatNum(d,
#                 title = "Participación de mujeres en Juntas Directivas en Colombia",
#                 verLabel = "Conteo Consejeros",
#                 theme = mytheme)
#
# #+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE
# hgch_donut_CatNum(d,
#                 title = "Participación de mujeres en Juntas Directivas en Colombia",
#                 verLabel = "Conteo Consejeros",
#                 theme = mytheme)
# #+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE
# hgch_treemap_CatNum(d,
#                     showLegend = FALSE,
#                 title = "Participación de mujeres en Juntas Directivas en Colombia",
#                 verLabel = "Conteo Consejeros",
#                 theme = mytheme)
# # Cargo
# #+ echo = FALSE, warning = FALSE,message = FALSE, include = TRUE
#
# d <- x %>%
#   select(Cargo, Género) %>%
#   group_by(Cargo, Género) %>%
#   summarise(n = n())
#
# hgch_bar_CatCatNum(d, verLabel = "Conteo Consejeros",
#                 title = "Consejeros",
#                    theme = mytheme)
# hgch_bar_CatCatNum(d, graphType = "stacked", verLabel = "Total Consejeros",
#                 title = "Consejeros",
#                    theme = mytheme)
# hgch_bar_CatCatNum(d, graphType = "stacked", percentage = TRUE,
#                 title = "Consejeros",
#                     theme = mytheme)
# hgch_bar_CatCatNum(d, verLabel = "Conteo Consejeros", orientation = "hor",
#                 title = "Consejeros",
#                    theme = mytheme)
# hgch_bar_CatCatNum(d, graphType = "stacked", orientation = "hor",
#                 title = "Consejeros",
#                    verLabel = "Total Consejeros",
#                    theme = mytheme)
# hgch_bar_CatCatNum(d, graphType = "stacked",
#                 title = "Consejeros",
#                    orientation = "hor", percentage = TRUE,
#                     theme = mytheme)
# hgch_treemap_CatCatNum(d %>% mixCats(),
#                 title = "Consejeros",
#                        theme = mytheme)


# d0 <- x %>%
#   select(Género,Empresa) %>%
#   group_by(Género, Empresa) %>%
#   summarise(n = n()) %>%
#   spread(Género,n)
# d0$f[is.na(d0$f)] <- 0
# d <- d0 %>%
#   mutate(f2 = 100*f/(f+m), m2 = 100*m/(f+m)) %>%
#   select(Empresa, f = f2, m = m2) %>%
#   arrange(desc(f)) %>%
#   slice(1:10) %>%
#   gather(Género, Porcentaje, -Empresa) %>%
#   mixCats()
# h <- hgch_bar_CatCatNum(d,
#                         title = "Top empresas con participación de mujeres",
#                         verLabel = "Empresa",
#                         horLabel = "Participación de consejeros por Género",
#                         orientation = "hor",
#                         order2 = unique(d$Empresa),
#                         percentage = TRUE,
#                         graphType = "stacked", nDigits = 0,
#                         labelWrapV = c(0, 50),
#                         format = c("%",""),
#                         theme = mytheme)
# h







