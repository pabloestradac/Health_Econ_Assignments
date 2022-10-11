library(tidyverse)
options(dplyr.summarise.inform = FALSE) ## Add to .Rprofile to make permanent

# Questions for bestie Ian


data_dir = "/Users/pablo/Dropbox/2_PhD/3rd_year/771_Health_II/Health_Econ_Assignments/Data"

# PUF: Medicare spending of each physician and service
name_puf_yearly = list.files(path=paste0(data_dir, "/PUF"), pattern="*.txt", recursive=TRUE)
datalist = vector("list", length = length(name_puf_yearly))
# MD-PPAS: The Medicare Data on Provider Practice and Specialty
name_mdppas_yearly = list.files(path=paste0(data_dir, "/MDPPAS"))[c(4:9)]   # 2012 - 2017
# PFS: Physician Fee Schedule 2010 Update
pfs = read_tsv(paste0(data_dir, "/PFS_update_data.txt"))
# MD-PPAS 2009: baseline measure of the practice before the price shock takes effect
tax_base = read_csv(paste0(data_dir, "/MDPPAS/PhysicianData_2009.csv")) %>%
  select(npi, tax_id=group1)
tax_base$npi = as.character(tax_base$npi)


for (y in 1:length(name_puf_yearly)){

  # Get file names and year
  name_mdppas = name_mdppas_yearly[y]
  name_puf = name_puf_yearly[y]
  year_num = as.numeric(strsplit(name_puf, "/")[[1]][1])
  print(paste0("*** Analyzing year: ", year_num))

  # Read MDPPAS for every year
  mdppas_yearly = read_csv(paste0(data_dir, "/MDPPAS/", name_mdppas))

  # Read PFS for every year
  pfs_yearly = pfs %>% filter(year == year_num)

  # Read PUF for each year
  puf_yearly = read_tsv(paste0(data_dir, "/PUF/", name_puf))
  names(puf_yearly) = tolower(names(puf_yearly))

  # For years > 2013, use only MD not integrated in 2012
  if (year_num == 2012){
    puf_yearly = puf_yearly %>%
      filter(
        grepl("MD", nppes_credentials) |
          grepl("M.D", nppes_credentials) |
          grepl("M..D", nppes_credentials))
  } else {
    puf_yearly = puf_yearly %>% filter(npi %in% MD_not_int$npi)
  }

  # Calculate physician-level data
  MD_data = puf_yearly %>%
    select(average_medicare_allowed_amt, bene_day_srvc_cnt, bene_unique_cnt, npi) %>%
    group_by(npi) %>%
    summarise(
      medicare_spending = sum(average_medicare_allowed_amt * bene_day_srvc_cnt),
      claims = sum(bene_day_srvc_cnt),
      patients = sum(bene_unique_cnt)) %>%
    mutate(year = year_num)

  # Calculate integration of physicians
  MD_int = mdppas_yearly %>%
    select(npi, pos_opd, pos_office, pos_asc) %>%
    mutate(integration = ((pos_opd / (pos_opd + pos_office + pos_asc)) > 0.75) * 1) %>%
    select(npi, integration)
  MD_int$npi = as.character(MD_int$npi)

  # Create instrument
  price_shock <- puf_yearly  %>%
    inner_join(tax_base, by="npi") %>%
    inner_join(pfs_yearly %>%
                 select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007),
               by=c("hcpcs_code" = "hcpcs")) %>%
    mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
    mutate(price_shock = case_when(
      year_num <= 2013 ~ ((year_num-2009)/4)*dprice_rel_2010,
      year_num > 2013  ~ dprice_rel_2010),
      denom = line_srvc_cnt*price_nonfac_orig_2010,
      numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
    group_by(npi) %>%
    summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
    ungroup() %>%
    mutate(phy_rev_change=phy_numer/phy_denom) %>%
    group_by(tax_id) %>%
    summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
    ungroup() %>%
    inner_join(tax_base, by="tax_id") %>%
    select(practice_rev_change, npi)

  # Merge physician claims and integration and add it to the list
  if (year_num == 2012){
    MD_not_int = MD_int %>% filter(integration == 0)
    MD_data = MD_data %>% filter(npi %in% MD_not_int$npi)
    datalist[[y]] = MD_data %>%
      left_join(MD_not_int, by="npi") %>%
      left_join(price_shock, by="npi")
  } else {
    datalist[[y]] = MD_data %>%
      left_join(MD_int, by="npi") %>%
      left_join(price_shock, by="npi")
  }

}

# Append all datasets, remove missing values, and remove other stuff
puf <- bind_rows(datalist)
# puf = puf %>% filter(!is.na(integration))
rm(datalist, MD_data, MD_int, puf_yearly, mdppas_yearly, year_num)

# OLS: log(claims) ~ integration
library(fixest)
puf$lclaims = log(puf$claims)
res = feols(lclaims ~ integration | npi + year, puf)
etable(res)

# Robustness Checks for Omitted Variable Bias
library(robomit)
delta_list = c(0, 0.5, 1, 1.5, 2)
R2max_list = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

# puf_dummy = puf %>%
#   mutate(var = 1) %>%
#   spread(npi, var, fill = 0, sep = "_") %>%
#   left_join(puf)

o_beta("lclaims", "integration", con="year", id="npi", time="year",
       delta=1, R2max=1, type="lm", data=puf)

# o_beta("lclaims", "integration", con="npi + year", id="npi", time="year",
#        delta=1, R2max=1, type="lm", data=puf)
#
# for (delta in delta_list){
#   for (R2max in R2max_list){
#     o_beta("lclaims", "integration", con="npi + year", id="npi", time="year",
#            delta=delta, R2max=R2max, type="lm", data=puf)
#   }
# }



# IV: log(claims) ~ (integration ~ rev_change)
est_iv = feols(lclaims ~ 1 | npi + year | integration ~ practice_rev_change, puf)
est_iv


# Durbin-Wu-Hausman test with an augmented regression
res_fs = feols(integration ~ practice_rev_change | npi + year, puf)$residuals
est_dwh = feols(lclaims ~ integration + res_fs | npi + year, puf)
est_dwh















