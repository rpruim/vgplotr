library(tidyverse)
library(httr2)
library(jsonlite)

# 1. Download Main Visitation Data from NPS
nps_url <- "https://irma.nps.gov/DataStore/DownloadFile/756960"
main_data <-
  read_csv(nps_url, show_col_types = FALSE) |>
  rename(Stat = Statistic) |>

  # 2. Recode values into a new 'Statistic' column
  mutate(
    Statistic = replace_values(
      Stat,
      "TRV" ~ "RecreationVisits",
      "TNRV" ~ "NonRecreationVisits",
      "TV" ~ "TotalVisits",
      "TRVH" ~ "RecreationVisitorHours",
      "TH" ~ "TotalVisitorHours",
      "CL" ~ "ConcessionerLodgingOvernights",
      "CCG" ~ "ConcessionerCampingOvernights",
      "TT" ~ "TentOvernights",
      "TRVS" ~ "RVCampingOvernights",
      "TTRV" ~ "TotalRecreationOvernights",
      "BC" ~ "BackcountryOvernights",
      "MISC" ~ "MiscOvernights",
      "NROS" ~ "NonRecreationOvernights",
      "TOS" ~ "TotalOvernights",
      "TNRVH" ~ "NonRecreationVisitorHours"
    )
  )

# 1a. Pivot and rename

main_data_wide <-
  main_data |>
  select(-Stat) |>
  pivot_wider(names_from = "Statistic", values_from = "Value")

# 2. Query the official NPS Developer API for all park unit locations
# Using DEMO_KEY (provided by developer.nps.gov for public access)
nps_api_url <- "https://developer.nps.gov/api/v1/parks?limit=700&api_key=DEMO_KEY"

res <- request(nps_api_url) |> req_perform()
raw_json <- resp_body_string(res) |> fromJSON()

# 3. Extract Unit Code, Name, State, Latitude, and Longitude
meta_data <- raw_json$data |>
  transmute(
    UnitCode = toupper(parkCode),
    UnitName = fullName,
    State = states,
    Latitude = as.numeric(latitude),
    Longitude = as.numeric(longitude),
    Designation = designation
  ) |>
  filter(!is.na(Latitude) & !is.na(Longitude) & Latitude != 0) |>
  distinct(UnitCode, .keep_all = TRUE)

# 4. Join metadata with Main Visitation Statistics
NPSvisits <- main_data_wide |>
  left_join(meta_data, by = "UnitCode") |>
  relocate(UnitName, State, Latitude, Longitude, .after = UnitCode)

NPSvisits <- NPSvisits |>
  filter(grepl("National Park", Designation)) |>
  select(-Designation)

# 6. Export to CSV
NPSVisits |> write_csv("NPSvisits.csv")

# NPSvisits <- read_csv("NPSvisits.csv")

NPSvisits |>
  group_by(UnitCode, UnitName, State) |>
  summarise(
    visits = sum(TotalVisits),
    hours = sum(TotalVisitorHours),
    overnights = sum(TotalOvernights)
  ) |>
  ungroup() |>
  arrange(desc(visits))

meta_data |> pull(Designation) |> table()

meta_data |> filter(Designation == "National Parks")

usethis::use_data(NPSvisits)
