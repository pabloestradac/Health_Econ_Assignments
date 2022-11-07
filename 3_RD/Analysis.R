if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, stargazer, rdrobust, rddensity, ggplot2)
options(dplyr.summarise.inform = FALSE) ## Add to .Rprofile to make permanent



### 0) Read and Clean Data

data_dir = "../Data/Ericson_Replication_Data"

data = read_dta(paste0(data_dir, "/Data_main.dta"))
subsidy = read_dta(paste0(data_dir, "/Data_subsidyinfo.dta"))

data = data %>% left_join(subsidy, by="PDPregion")



### 1) Descriptive Statistics

# First, add the cohort (year the plan was introduced)
data = data %>%
  group_by(uniqueID) %>% 
  mutate(cohort = min(year))

# A: Premium and deductible
sum_stats_A = data %>%
  filter(year == cohort) %>% 
  pivot_longer(c(premium, deductible), names_to="variable", values_to="value") %>%
  group_by(variable, year) %>%
  summarize(mean = round(mean(value)), SD = round(sd(value))) %>% 
  pivot_longer(c(mean, SD), names_to="stat", values_to="value") %>%
  pivot_wider(names_from=year, values_from=value) %>% 
  mutate(variable = paste0(variable, " (", stat, ")")) %>% 
  select(-stat)

# B: Fraction enhanced benefit
sum_stats_B = data %>%
  filter(year == cohort) %>% 
  mutate(enhanced = ifelse(benefit=="E", 1, 0)) %>% 
  group_by(year) %>% 
  summarize(enhanced = round(mean(enhanced), 2)) %>%
  pivot_wider(names_from=year, values_from=enhanced) %>% 
  mutate(variable = "Fraction enhanced benefit")

# C.1: Fraction of plans offered by firms already offering a plan in US
sum_stats_C1 = data %>%
  filter(year == cohort) %>% 
  group_by(orgParentCode) %>% 
  mutate(cohort_state = min(year), state_offered = ifelse(year > cohort_state, 1, 0)) %>%
  group_by(year) %>% 
  summarize(state_offered = round(mean(state_offered), 2)) %>%
  pivot_wider(names_from=year, values_from=state_offered) %>% 
  mutate(variable = "Fraction of plans offered in US")

# C.2: Fraction of plans offered by firms already offering a plan in the same state
sum_stats_C2 = data %>%
  filter(year == cohort) %>% 
  group_by(orgParentCode, state) %>% 
  mutate(cohort_state = min(year), state_offered = ifelse(year > cohort_state, 1, 0)) %>%
  group_by(year) %>% 
  summarize(state_offered = round(mean(state_offered), 2)) %>%
  pivot_wider(names_from=year, values_from=state_offered) %>% 
  mutate(variable = "Fraction of plans offered in same state")

# D: Number of unique firms
sum_stats_D = data %>%
  group_by(cohort) %>%
  summarize(firms = length(unique(orgParentCode))) %>%
  pivot_wider(names_from=cohort, values_from=firms) %>% 
  mutate(variable = "Number of unique firms")

# E: Number of plans
sum_stats_E = data %>%
  filter(year == cohort) %>% 
  group_by(year) %>%
  summarize(plans = length(uniqueID)) %>% 
  pivot_wider(names_from=year, values_from=plans) %>% 
  mutate(variable = "Number of unique plans")

sum_stats = bind_rows(sum_stats_A, sum_stats_B, sum_stats_C1, sum_stats_C2, sum_stats_D, sum_stats_E)
stargazer(sum_stats, summary=FALSE, rownames=FALSE, out="tables/summary_stats.tex")


  
### 2) Effect of 2006 Benchmark Status on 2006 Enrollment

data_2006 = data %>% 
  filter(year == 2006 & benefit == "B") %>% 
  group_by(state) %>% 
  mutate(enroll_share = log(enrollmentImpute / sum(enrollmentImpute)), 
         rel_premium = premium - s2006) %>% 
  filter(abs(rel_premium) <= 10)

pdf(file="figures/rd_nbins20.pdf", width=6, height=4)
rd_plot = rdplot(data_2006$enroll_share, data_2006$rel_premium, 
                 h=10, p=4, nbins=20,
                 title="", 
                 x.label="Monthly premium - LIS subsidy, 2006", 
                 y.label="log enrollment share, 2006")
dev.off()


### 3) Same as before with new partitions

pdf(file="figures/rd_nbins10.pdf", width=6, height=4)
rd_plot = rdplot(data_2006$enroll_share, data_2006$rel_premium,
                  h=10, p=4, nbins=10,
                  title="", 
                  x.label="Monthly premium - LIS subsidy, 2006", 
                  y.label="log enrollment share, 2006")
dev.off()

pdf(file="figures/rd_nbins30.pdf", width=6, height=4)
rd_plot = rdplot(data_2006$enroll_share, data_2006$rel_premium,
                  h=10, p=4, nbins=30,
                  title="", 
                  x.label="Monthly premium - LIS subsidy, 2006", 
                  y.label="log enrollment share, 2006")
dev.off()



### 4) Same as before with optimal bins

pdf(file="figures/rd_optimal.pdf", width=6, height=4)
rd_plot = rdplot(data_2006$enroll_share, data_2006$rel_premium,
                  title="", 
                  x.label="Monthly premium - LIS subsidy, 2006", 
                  y.label="log enrollment share, 2006")  
dev.off()
rd_plot$J



### 5) Manipulation tests

# Ho: The values on the right and left at the cutoff are equal 
# Ho: There is systematic manipulation of the running variable
rd_density = rddensity(X = data_2006$rel_premium)
rd_density$test
rdplotdensity(rd_density, X = data_2006$rel_premium)
# there is no statistical evidence of systematic manipulation of the running variable.



### 6) Recreate Table 3
summary(rdrobust(data_2006$enroll_share, data_2006$rel_premium, h = 4))



### 7) Recreate Table 3 with optimal bandwidth
summary(rdrobust(data_2006$enroll_share, data_2006$rel_premium))

  


