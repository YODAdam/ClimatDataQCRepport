library(tidyverse)
data <- read_csv(file = "data/_SELECT_vd_EG_GH_ID_g_GH_ID_g_NAME_vd_EG_EL_ABBREVIATION_vd_YEAR_202605221359.csv")

data <- data |> 
  select(GH_ID, Station_name = NAME, Parameter = EG_EL_ABBREVIATION, Year = YEAR, Month = MONTH, Day = DAY, Value =VALUE ) |> 
  mutate(
    Station_type = str_sub(string = GH_ID, start = -1, end = -1),
    Station_type = case_when(
      Station_type == 'S' ~ 'Synoptique',
      Station_type == 'P' ~ 'Pluviometrique',
      Station_type == 'A' ~ 'Agrometeorologique',
      Station_type == 'C' ~ 'Climatologique',
      Station_type == 'W' ~ 'Automatique',
      .default =  'Inconnue'
    )
  ) |> 
  select(!GH_ID) 

data |> 
  write_csv(file = 'data/ANAM_data')
