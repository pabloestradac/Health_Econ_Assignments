library(tidyverse)
library(fixest)
options(dplyr.summarise.inform = FALSE) ## Add to .Rprofile to make permanent

# Questions for bestie Ian
# 1. How do I get total number of claims billed to an ambulatory surgery center?

data_dir = "/Users/pablo/Dropbox/2_PhD/3rd_year/771_Health_II/Health_Econ_Assignments/Data"

name_puf_yearly = list.files(path=paste0(data_dir, "/PUF"), pattern="*.txt", recursive=TRUE)
name_mdppas_yearly = list.files(path=paste0(data_dir, "/MDPPAS"))
datalist = vector("list", length = length(name_puf_yearly))

for (y in 1:length(name_puf_yearly)){

  name_puf = name_puf_yearly[y]
  year = as.numeric(strsplit(name_puf, "/")[[1]][1])
  print(paste0("Year: ", year))

  # Read PUF for each year
  puf_yearly = read_tsv(paste0(data_dir, "/PUF/", name_puf))
  names(puf_yearly) = tolower(names(puf_yearly))

  # For years > 2013, use only MD not integrated in 2012
  if (year != 2012){
    puf_yearly = puf_yearly %>% filter(npi %in% MD_not_int$npi)
  }

  # Restrict to MD only
  if (year == 2012){
    puf_yearly = puf_yearly %>%
      filter(
        grepl("MD", nppes_credentials) |
          grepl("M.D", nppes_credentials) |
          grepl("M..D", nppes_credentials))
  }

  # Calculate physician-level data
  MD_data = puf_yearly %>%
    select(average_medicare_allowed_amt, bene_day_srvc_cnt, bene_unique_cnt, npi) %>%
    group_by(npi) %>%
    summarise(
      medicare_spending = sum(average_medicare_allowed_amt * bene_day_srvc_cnt),
      claims = sum(bene_day_srvc_cnt),
      patients = sum(bene_unique_cnt)) %>%
    mutate(year = year)

  # Calculate integration of physicians
  MD_int = puf_yearly %>%
    # Total claims by physician
    select(npi, place_of_service, bene_day_srvc_cnt) %>%
    group_by(npi) %>%
    mutate(total_claims = sum(bene_day_srvc_cnt)) %>%
    # Percentage of claims Facility vs Office
    group_by(npi, place_of_service) %>%
    summarise(place_claims = sum(bene_day_srvc_cnt),
              total_claims = last(total_claims)) %>%
    mutate(per_claims = place_claims/total_claims) %>%
    # Calculate integration
    group_by(npi) %>%
    pivot_wider(
      id_cols = npi,
      names_from = place_of_service,
      values_from = c(per_claims),
      values_fill = 0) %>%
    mutate(integration = (F>=0.75)*1) %>%
    select(npi, integration)

  # Merge physician claims and integration and add it to the list
  if (year == 2012){
    MD_not_int = MD_int %>% filter(integration == 0)
    MD_data = MD_data %>% filter(npi %in% MD_not_int$npi)
    datalist[[y]] = left_join(MD_data, MD_not_int, by="npi")
  } else {
    datalist[[y]] = left_join(MD_data, MD_int, by="npi")
  }

}

# Remove and append all datasets
puf <- bind_rows(datalist)
rm(datalist, MD_data, MD_int, puf_yearly, year)

# puf %>% group_by(NPI, PLACE_OF_SERVICE) %>% summarise(sum(BENE_DAY_SRVC_CNT))

puf$lclaims = log(puf$claims)
res = feols(lclaims ~ integration | npi + year, puf)
etable(res)





