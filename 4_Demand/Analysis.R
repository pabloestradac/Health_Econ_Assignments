if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, fixest)
options(dplyr.summarise.inform = FALSE) ## Add to .Rprofile to make permanent



### 0) Read and Clean Data

data_dir = "../Data"

# HCRIS: Hospital Cost Report Information System
hcris = read_tsv(paste0(data_dir, "/HCRIS_cleaned/data/output/HCRIS_Data.txt"))
counties = read_csv(paste0(data_dir, "/Market_Data/zcta-to-county.csv"))
market_hrr = read_csv(paste0(data_dir, "/Market_Data/Zip_Hsa_Hrr.csv"))
market_cd = read_rds(paste0(data_dir, "/Market_Data/hospital_markets.rds"))



### 1) Calculate hospital market shares by ZIP code
hcris = hcris %>% 
  filter(year >= 2000 & year <= 2017) %>% 
  mutate(zip = str_sub(zip, 1, 5)) %>% 
  filter(zip != "00000") %>% 
  group_by(zip, year) %>% 
  mutate(zip_share = tot_discharges / sum(tot_discharges, na.rm = TRUE))

ggplot(data = hcris, aes(x = year, y = zip_share, group = year)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  theme_classic()



### 2) Market concentration and price
hcris = hcris %>% 
  mutate(lprice = log((tot_charges-tot_discounts)/tot_discharges)) %>% 
  group_by(zip, year) %>% 
  mutate(hhi_zip = sum(zip_share^2, na.rm = TRUE), 
         tot_hosp_zip = n())

reg_price = feols(lprice ~ hhi_zip + beds + cash + tot_hosp_zip | provider_number + year, data=hcris)
etable(reg_price, order = "f", drop="Int") ## , file='did_table.tex')



### 3) Market concentration by HRR
market_hrr = market_hrr %>% select(zip=zipcode15, hsa=hsanum, hsa_city=hsacity, hsa_state=hsastate, 
                           hrr=hrrnum, hrr_city=hrrcity, hrr_state=hrrstate)
hcris = left_join(hcris, market_hrr) %>% 
  group_by(hrr, year) %>% 
  mutate(hrr_share = tot_discharges / sum(tot_discharges, na.rm = TRUE))

ggplot(data = hcris, aes(x = year, y = hrr_share, group = year)) +
  # geom_violin(trim=FALSE, outlier.shape=NA) +
  geom_boxplot(width=0.3, outlier.shape=NA) +
  coord_cartesian(ylim=c(0, 1)) +
  theme_classic()



### 4) Market concentration and price with HRR
hcris = hcris %>% 
  group_by(hrr, year) %>% 
  mutate(hhi_hrr = sum(hrr_share^2, na.rm = TRUE), 
         tot_hosp_hrr = n())

reg_price = feols(lprice ~ hhi_hrr + beds + cash + tot_hosp_hrr | provider_number + year, data=hcris)
etable(reg_price, order = "f", drop="Int") ## , file='did_table.tex')



### 5) Market concentration by Community detection
# counties = read_csv(paste0(data_dir, "/Market_Data/zcta-to-county.csv"))
# counties = counties %>% 
#   select(zip=zcta5, fips=county, statefp=state) %>% 
#   mutate(zip = str_pad(zip, 5, pad="0"), 
#          fips = str_pad(fips, 5, pad="0"), 
#          statefp = str_pad(statefp, 2, pad="0"))
# market_cd = market_cd %>% select(fips, community=mkt, statefp)
# hcris = left_join(hcris, counties, by=c("zip")) 
# hcris = left_join(hcris, counties, by=c("zip", "state")) %>% 
#   group_by(hrr, year) %>% 
#   mutate(hrr_share = tot_discharges / sum(tot_discharges, na.rm = TRUE))



### 6) Market concentration and price with Community Detection



### 7) Logit discrete choice model using market share data
reg_price = feols(log(zip_share) ~ lprice + beds + cash + tot_hosp_zip | provider_number + year, data=hcris)
etable(reg_price, order = "f", drop="Int") ## , file='did_table.tex')

reg_price = feols(log(hrr_share) ~ lprice + beds + cash + tot_hosp_hrr | provider_number + year, data=hcris)
etable(reg_price, order = "f", drop="Int") ## , file='did_table.tex')











#  A couple of tests

# test = hcris %>% 
#   select(year, zip, provider_number, tot_discharges, market_share, hhi) %>% 
#   filter(zip == "10467") %>% 
#   arrange(year, zip, provider_number)
#   
# hcris %>% 
#   select(year, zip, provider_number, tot_discharges, market_share) %>% 
#   filter(abs(market_share) > 1)





