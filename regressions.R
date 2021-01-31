# Date Created: 14/01/2021
# Date Updated: 14/01/2021
# Author: Lauren Thomas
# Purpose: run regressions for stats summative

setwd("C:/Users/ltswe/Dropbox/Oxford/Michaelmas Term Classes/Applied Analytical Statistics")
library(data.table)
library(ivpack)
library(stargazer)
library(plm)
library(tidyverse)

# # Read in data from CSV
all_final <- read.csv("C:/Users/ltswe/Dropbox/Oxford/Michaelmas Term Classes/Applied Analytical Statistics/stats_summative/data/all_final.csv")

# Create data frames for each city
setDT(all_final)
boston <- filter(all_final, all_final$city == "Boston")
nyc <- filter(all_final, all_final$city == "NYC")
sf <- filter(all_final, all_final$city == "SF")

# turn off scientific notation except for big numbers
options(scipen = 9)

# For control dataframes, drop any obs where any controls (med_hh_income + gent_flag + room_type_recoded 
#+ host_is_superhost_recoded + host_multi_listings_flag) = NA

boston_control <- boston %>% drop_na(med_hh_income, room_type_recoded,
                                     host_is_superhost_recoded, host_multi_listings_flag)
nyc_control <- nyc %>% drop_na(med_hh_income, gent_flag, room_type_recoded,
                                  host_is_superhost_recoded, host_multi_listings_flag)
sf_control <- sf %>% drop_na(med_hh_income, gent_flag, room_type_recoded,
                               host_is_superhost_recoded, host_multi_listings_flag)


# Create regression functions

iv_reg_nocontrol <-function(df, x_var, z_var, tex_name) {
  # This IV regression does not include controls, used for each city.
  # X-var = endogenous explantory variable; is either listings or bookings here
  # Z-var = instrumental variable. Equals either policy flag or categorical policy flag here.
  
  # Estimate model 
  iv1 = ivreg(num_complaints~x_var|z_var, data = df)
  stargazer(iv1, title = "IV Regression without Controls", 
            out = paste("tex/tables/regressions/",tex_name, "_nocontrol.tex"))
  return(iv1)
}

iv_reg_control_nob <- function(df, x_var, z_var, tex_name) {
  # This IV reg does include controls. For NYC and SF only (Boston has slightly
  # different sets of controls).
  iv2 = ivreg(num_complaints~x_var + med_hh_income + gent_flag + room_type_recoded 
              + host_is_superhost_recoded + host_multi_listings_flag|z_var +
                + med_hh_income + gent_flag + room_type_recoded 
              + host_is_superhost_recoded + host_multi_listings_flag, data = df)
  stargazer(iv2, title = "IV Regression with Controls", 
            out = paste("tex/tables/regressions/",tex_name, "_control.tex"))
  return(iv2)
  
}

iv_reg_control_bos <-function(df, x_var, z_var, tex_name) {
  # This function includes the controls for Boston only. AKA no gent_flag
  iv3 = ivreg(num_complaints~x_var + med_hh_income  + room_type_recoded 
              + host_is_superhost_recoded + host_multi_listings_flag|z_var +
                + med_hh_income + room_type_recoded 
              + host_is_superhost_recoded + host_multi_listings_flag, data = df)
  stargazer(iv3, title = "IV Regression with Controls", 
            out = paste("tex/tables/regressions/",tex_name, "_control.tex"))
  return(iv3)
  
}

# Run Regressions
# NYC: No controls
## z_var = policy_flag
### x_var = listings (id)
iv1 = iv_reg_nocontrol(nyc, nyc$id, nyc$policy_flag, "nyc_listings_policy_flag")
### x_var = bookings
iv2 = iv_reg_nocontrol(nyc, nyc$num_booked_30, nyc$policy_flag, "nyc_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv3 = iv_reg_nocontrol(nyc, nyc$id, nyc$policy_categorical_flag, "nyc_listings_cpolicy_flag")
### x_var = bookings
iv4 = iv_reg_nocontrol(nyc, nyc$num_booked_30, nyc$policy_categorical_flag, "nyc_bookings_cpolicy_flag")

# NYC: controls
## z_var = policy_flag
### x_var = listings (id)
iv5 = iv_reg_control_nob(nyc_control, nyc_control$id, nyc_control$policy_flag, "nyc_listings_policy_flag")
### x_var = bookings
iv6 = iv_reg_control_nob(nyc_control, nyc_control$num_booked_30, nyc_control$policy_flag, "nyc_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv7 = iv_reg_control_nob(nyc_control, nyc_control$id, nyc_control$policy_categorical_flag, "nyc_listings_cpolicy_flag")
### x_var = bookings
iv8 = iv_reg_control_nob(nyc_control, nyc_control$num_booked_30, nyc_control$policy_categorical_flag, "nyc_bookings_cpolicy_flag")


# SF: No controls
## z_var = policy_flag
### x_var = listings (id)
iv9 = iv_reg_nocontrol(sf, sf$id, sf$policy_flag, "sf_listings_policy_flag")
### x_var = bookings
iv10= iv_reg_nocontrol(sf, sf$num_booked_30, sf$policy_flag, "sf_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv11 = iv_reg_nocontrol(sf, sf$id, sf$policy_categorical_flag, "sf_listings_cpolicy_flag")
### x_var = bookings
iv12 = iv_reg_nocontrol(sf, sf$num_booked_30, sf$policy_categorical_flag, "sf_bookings_cpolicy_flag")

# SF: controls
## z_var = policy_flag
### x_var = listings (id)
iv13 = iv_reg_control_nob(sf_control, sf_control$id, sf_control$policy_flag, "sf_listings_policy_flag")
### x_var = bookings
iv14 = iv_reg_control_nob(sf_control, sf_control$num_booked_30, sf_control$policy_flag, "sf_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv15 = iv_reg_control_nob(sf_control, sf_control$id, sf_control$policy_categorical_flag, "sf_listings_cpolicy_flag")
### x_var = bookings
iv16 = iv_reg_control_nob(sf_control, sf_control$num_booked_30, sf_control$policy_categorical_flag, "sf_bookings_cpolicy_flag")


# Boston: No controls
## z_var = policy_flag
### x_var = listings (id)
iv17 = iv_reg_nocontrol(boston, boston$id, boston$policy_flag, "boston_listings_policy_flag")
### x_var = bookings
iv18 = iv_reg_nocontrol(boston, boston$num_booked_30, boston$policy_flag, "boston_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv19 = iv_reg_nocontrol(boston, boston$id, boston$policy_categorical_flag, "boston_listings_cpolicy_flag")
### x_var = bookings
iv20 = iv_reg_nocontrol(boston, boston$num_booked_30, boston$policy_categorical_flag, "boston_bookings_cpolicy_flag")


# Boston: controls
## z_var = policy_flag
### x_var = listings (id)
iv21 = iv_reg_control_bos(boston_control, boston_control$id, boston_control$policy_flag, "boston_listings_policy_flag")
### x_var = bookings
iv22 = iv_reg_control_bos(boston_control, boston_control$num_booked_30, boston_control$policy_flag, "boston_bookings_policy_flag")

## z_var = categorical policy flag
### x_var = listings (id)
iv23 = iv_reg_control_bos(boston_control, boston_control$id, boston_control$policy_categorical_flag, "boston_listings_cpolicy_flag")
### x_var = bookings
iv24 = iv_reg_control_bos(boston_control, boston_control$num_booked_30, boston_control$policy_categorical_flag, "boston_bookings_cpolicy_flag")

clustered_se_output <- function(reg, df, num) {
  print(paste("iv",num))
  cluster.robust.se(reg, df$cluster)
}

# robust clustered SE must be placed in manually...output in the same order as in
# paper
sink(file = paste("stats_summative/log_output/iv_se.txt"), append = F)
clustered_se_output(iv1, nyc, 1)
clustered_se_output(iv2, nyc, 2)
clustered_se_output(iv5, nyc_control, 5)
clustered_se_output(iv6, nyc_control, 6)
clustered_se_output(iv9, sf, 9)
clustered_se_output(iv10, sf, 10)
clustered_se_output(iv13, sf_control, 13)
clustered_se_output(iv14, sf_control, 14)
clustered_se_output(iv17, boston, 17)
clustered_se_output(iv18, boston, 18)
clustered_se_output(iv21, boston_control, 21)
clustered_se_output(iv22, boston_control, 22)


clustered_se_output(iv3, nyc, 3)
clustered_se_output(iv4, nyc, 4)
clustered_se_output(iv7, nyc_control, 7)
clustered_se_output(iv8, nyc_control, 8)
clustered_se_output(iv11, sf, 11)
clustered_se_output(iv12, sf, 12)
clustered_se_output(iv15, sf_control, 15)
clustered_se_output(iv16, sf_control, 16)
clustered_se_output(iv19, boston, 19)
clustered_se_output(iv20, boston, 20)
clustered_se_output(iv23, boston_control, 23)
clustered_se_output(iv24, boston_control, 24)


sink()


# output NYC binary policy flag, SF binary, Boston binary, 
# NYC categorical, SF categorical, Boston categorical together
stargazer(iv1, iv2, iv5, iv6, title = "NYC", 
           out = paste("tex/tables/regressions/nyc_binary.tex" ))

stargazer(iv9, iv10, iv13, iv14, title = "SF", 
          out = paste("tex/tables/regressions/sf_binary.tex" ))

stargazer(iv17, iv18, iv21, iv22, title = "Boston", 
          out = paste("tex/tables/regressions/boston_binary.tex" ))

stargazer(iv3, iv4, iv7, iv8, title = "NYC", 
          out = paste("tex/tables/regressions/nyc_cate.tex" ))

stargazer(iv11, iv12, iv15, iv16, title = "SF", 
          out = paste("tex/tables/regressions/sf_cate.tex" ))

stargazer(iv19, iv20, iv23, iv24, title = "Boston", 
          out = paste("tex/tables/regressions/boston_cate.tex" ))




