
# import market data ------------------------------------------------------

path <- "C:/Users/USER MSI/Documents/R Project/rbd_support/data/"

bases <- path |> dir_ls(regexp = ".csv") |> 
  map(~read.csv(.))
list2env(bases, .GlobalEnv)

base_global <- path |> dir_ls(regexp = ".csv") |> 
  map_df(~read.csv(.))

Benin_tot <- get("C:/Users/USER MSI/Documents/R Project/rbd_support/data/WFP_2023May19_Benin_FoodPricesData.csv")
civ_tot <- get("C:/Users/USER MSI/Documents/R Project/rbd_support/data/WFP_2023May19_Côted'Ivoire_FoodPricesData.csv")
ghana_tot <- get("C:/Users/USER MSI/Documents/R Project/rbd_support/data/WFP_2023May19_Ghana_FoodPricesData.csv")
togo_tot <- get("C:/Users/USER MSI/Documents/R Project/rbd_support/data/WFP_2023May19_Togo_FoodPricesData.csv")

Benin <- Benin_tot |> 
  filter(Year %in% c(2021,2022,2023))

civ <- civ_tot |> 
  filter(Year %in% c(2021,2022,2023))


ghana <- ghana_tot |> 
  filter(Year %in% c(2021,2022,2023))

togo <- togo_tot |> 
  filter(Year %in% c(2020,2021,2022))

base_last3year <- rbind(Benin, civ, ghana, togo)

nlevels(as.factor(Benin$Market))
nlevels(as.factor(Benin$Commodity))

nlevels(as.factor(civ$Market))
nlevels(as.factor(civ$Commodity))

nlevels(as.factor(ghana$Market))
nlevels(as.factor(ghana$Commodity))

nlevels(as.factor(togo$Market))
nlevels(as.factor(togo$Commodity))

# Benin since 2002 we collect their data
# Côte d'ivoire since 2005 we collect their data
# Ghana since 2006 we collect their data
# Togo since 2001 we collect their data

# Tabulation et graphique -------------------------------------------------

markets <- base_global |> dplyr::group_by(Country) |> 
  summarise(n_market = n_distinct(Market))


markets <- markets |> arrange(n_market)
  

markets |> e_chart(Country) |> 
  e_bar(n_market) |> 
  e_legend(show = FALSE) |> 
  e_labels(position = 'insideRight') |> 
  e_title("Nombre de Marchés par Pays", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_flip_coords() |> 
  e_y_axis(axisLabel = list(textStyle = list(fontWeight = "bold"))) |> 
  e_toolbox_feature("saveAsImage") 

commodity <- base_global |> dplyr::group_by(Country) |> 
  summarise(n_commodity = n_distinct(Commodity))

commodity <- commodity |> arrange(n_commodity)

commodity |> e_chart(Country) |> 
  e_bar(n_commodity) |> 
  e_legend(show = FALSE) |> 
  e_labels(position = 'insideRight',fontWeight = "bold") |> 
  e_title("Nombre de Produits collectés par Pays", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_flip_coords() |> 
  e_y_axis(axisLabel = list(textStyle = list(fontWeight = "bold"))) |> 
  e_toolbox_feature("saveAsImage")


# Tabulation et graphique 3 last year -------------------------------------

markts3years <- base_last3year |> dplyr::group_by(Country) |> 
  summarise(n_market = n_distinct(Market))

markts3years <- markts3years |> arrange(n_market)

markest_final <- markets |> left_join(markts3years, by = "Country")
markest_final <- markest_final |> rename(
  debut = "n_market.x",
  troisan = "n_market.y"
)

p1 <- markest_final |> e_chart(Country) |> 
  e_bar(debut, name = "2006") |> 
  e_bar(troisan, name = "Last 3 years") |> 
  e_legend(bottom = 0,textStyle = list(fontWeight = "bold")) |>
  e_labels(position = 'insideRight',fontWeight = "bold") |> 
  e_title("Number of Markets by country", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_flip_coords() |> 
  e_y_axis(axisLabel = list(textStyle = list(fontWeight = "bold"))) |> 
  e_toolbox_feature("saveAsImage")

annee <- base_global |> group_by(
  Country, Year
) |> count()


p2 <- annee |> e_charts(Country) |> 
  e_bar(Year, stack = "grp") |> 
  e_labels(position = "inside") |> 
  e_legend(show = FALSE) |> 
  e_title("Année de collect de données par pays", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |> 
  e_x_axis(axisLabel = list(textStyle = list(fontWeight = "bold"))) 
# |>
#   e_toolbox_feature("saveAsImage")



e_arrange(p1, p2, cols = 2) 


commodity3years <- base_last3year |> dplyr::group_by(Country) |> 
  summarise(n_commodity = n_distinct(Commodity))
commodity3years <- commodity3years |> arrange(n_commodity)


commodity_final <- commodity |> left_join(commodity3years, by = "Country")

commodity_final <- commodity_final |> rename(
  debut = "n_commodity.x",
  troisan = "n_commodity.y"
)

p3 <- commodity_final |> e_chart(Country) |> 
  e_bar(debut, name = "2006") |> 
  e_bar(troisan, name = "Last 3 years") |> 
  e_legend(bottom = 0,textStyle = list(fontWeight = "bold")) |>
  e_labels(position = 'insideRight',fontWeight = "bold") |> 
  e_title("Number of Commodity by Country", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_flip_coords() |> 
  e_y_axis(axisLabel = list(textStyle = list(fontWeight = "bold"))) |> 
  e_toolbox_feature("saveAsImage")

e_arrange(p3, p2, cols = 2) 

# Partie données CH -------------------------------------------------------

data_ch <- read_xlsx("data/CH/cadre_harmonise_caf_ipc.xlsx")
golfguine <- data_ch |> filter(
  adm0_name %in% c("Benin", "Cote d'Ivoire","Ghana","Togo")
)

golfguine <- golfguine |> filter(
  usethisperiod == "Y"
) |> mutate_at(vars(population:phase35), replace_na, 0)

golfguine_national <- golfguine |> 
  group_by(adm0_name,reference_label, reference_year) |> 
  summarise(phase35 = sum(phase35))

mars <- golfguine_national |> 
  filter(reference_label == "Jan-May") |> 
  mutate(phase35_2 = round((phase35 / 1000000),1))

mars <- mars |> select(
  - reference_label
)

octobre <- golfguine_national |> 
  filter(reference_label == "Sep-Dec") |> 
  mutate(phase35_2 = round((phase35 / 1000000),1))

juin <- golfguine_national |> 
  filter(reference_label == "Jun-Aug") |> 
  mutate(phase35_2 = round((phase35 / 1000000),1))

 mars |> 
 echarts4r::group_by(adm0_name) |>
  e_charts( x = reference_year) |> 
  e_x_axis(min = 2020) |> 
  e_line(phase35_2) |> 
  e_labels(fontSize = 12, fontWeight = "bold") |>
  e_legend(bottom = 0, textStyle = list(fontWeight = "bold")) |> 
  e_title("Population en insécurité alimentaire période Jan-Mai (Current)",
          subtext = "en million",
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
   e_y_axis(show = FALSE) |> 
   
  # e_toolbox_feature("dataZoom") %>%
  # e_toolbox_feature(feature = "reset") %>%
  # e_toolbox_feature("saveAsImage") |>
  # e_facet(rows = 1, cols = 4,legend_space = 16) |> 
  e_tooltip()

octobre |> 
  echarts4r::group_by(adm0_name) |>
  e_charts( x = reference_year) |> 
  e_x_axis(min = 2020) |> 
  e_line(phase35_2) |> 
  e_labels(fontSize = 12, fontWeight = "bold") |>
  e_legend(bottom = 0, textStyle = list(fontWeight = "bold")) |> 
  e_title("Population en insécurité alimentaire période Sep-Dec (Current)",
          subtext = "en million",
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |>
  # e_toolbox_feature("dataZoom") %>%
  # e_toolbox_feature(feature = "reset") %>%
  # e_toolbox_feature("saveAsImage") |>
  # e_facet(rows = 1, cols = 4,legend_space = 16) |> 
  e_tooltip()

juin |> 
  echarts4r::group_by(adm0_name) |>
  e_charts( x = reference_year) |> 
  e_x_axis(min = 2017) |> 
  e_line(phase35_2) |> 
  e_labels(fontSize = 12, fontWeight = "bold") |>
  e_legend(bottom = 0, textStyle = list(fontWeight = "bold")) |> 
  e_title("Population in Food Insecure Jun-Aug (Projected)", 
          subtext = "en million",
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |>
  # e_toolbox_feature("dataZoom") %>%
  # e_toolbox_feature(feature = "reset") %>%
  # e_toolbox_feature("saveAsImage") |>
  # e_facet(rows = 1, cols = 4,legend_space = 16) |> 
  e_tooltip()

e_arrange(graph1, graph2, graph3,cols = 2, rows = 2) 


# anne d'intervention -----------------------------------------------------

mars2 <- mars |> mutate(
  annee = case_when(
    phase35 == 0 ~ NA,
    TRUE~ reference_year
  )
)

octobre2 <- octobre |> mutate(
  annee = case_when(
    phase35 == 0 ~ NA,
    TRUE~ reference_year
  )
)

juin2 <- juin |> mutate(
  annee = case_when(
    phase35 == 0 ~ NA,
    TRUE~ reference_year
  )
)

mars3 <- mars2 |> dplyr::group_by(adm0_name, annee) |> 
  count()

juin3 <- juin2 |> dplyr::group_by(adm0_name, annee) |> 
  count()

octobre3 <- octobre2 |> dplyr::group_by(adm0_name, annee) |> 
  count()


mars3 |> e_charts(adm0_name) |> 
  e_bar(annee, stack = "grp") |> 
  e_labels(position = "inside") |> 
  e_legend(show = FALSE) |> 
  e_title("Année de collect de données CH par pays periode  Jan-May", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |> 
  e_x_axis(axisLabel = list(textStyle = list(fontWeight = "bold")))


juin3 |> e_charts(adm0_name) |> 
  e_bar(annee, stack = "grp") |> 
  e_labels(position = "inside") |> 
  e_legend(show = FALSE) |> 
  e_title("Année de collect de données CH par pays periode  Jun-Aug", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |> 
  e_x_axis(axisLabel = list(textStyle = list(fontWeight = "bold")))


octobre3 |> e_charts(adm0_name) |> 
  e_bar(annee, stack = "grp") |> 
  e_labels(position = "inside") |> 
  e_legend(show = FALSE) |> 
  e_title("Année de collect de données CH par pays periode  Sep-Dec", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |> 
  e_x_axis(axisLabel = list(textStyle = list(fontWeight = "bold")))

# partie données supplémentaire -------------------------------------------

urbanisation <- data.frame(
  stringsAsFactors = FALSE,
                           country = c("Benin","Côte d'Ivoire","Ghana",
                                       "Togo"),
             x2015 = c(45.7, 49.4, 54.1, 40.1),
             x2020 = c(48.4, 51.7, 57.3, 42.8),
             x2025 = c(51.2, 54.1, 60.5, 45.6),
             x2030 = c(54.1, 56.7, 63.4, 48.6)
                )

urbanisation <- urbanisation |> 
  pivot_longer(!country, names_to = "Year", values_to = "urban_percentage")

urbanisation <- urbanisation |> mutate(
 annee = case_when( Year == "x2015" ~ 2015,
             Year == "x2020" ~ 2020,
             Year == "x2025" ~ 2025,
             Year == "x2030" ~ 2030)
)

urbanisation <- urbanisation |> select(-Year)
urbanisation <- urbanisation |> 
  mutate(annee = as.Date(annee))

urbanisation |>
  group_by(country) |> 
  e_charts(annee) |> 
  e_line(urban_percentage) |> 
  e_x_axis(min =2015, max = 2030, interval = 5) |>
  e_legend(bottom = 0, textStyle = list(fontWeight = "bold")) |> 
  e_labels(fontSize = 12, fontWeight = "bold") |> 
  e_legend(show = TRUE) |> 
  e_title("Level of urbanization (%)", 
          left = "center", top = 5, 
          textStyle = list(fontSize = 24)) |> 
  e_y_axis(show = FALSE) |>
  e_x_axis(axisLabel = list(textStyle = list(fontWeight = "bold")))

prop_urban <- 
  
  
prop_urban <- prop_urban |> 
  pivot_longer(!Country, names_to = "Year", values_to = "urban_percentage")

