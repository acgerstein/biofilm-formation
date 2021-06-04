##########################################
# Load libraries
##########################################
library(tidyverse)
library(growthcurver)

##########################################
# Import the data
##########################################
# To do this through the RStudio interface, click on Import Dataset (under the environment panel on the top right)
# Click on From Text (readr). 
# Click on browse to find the .csv file that contains your plate information, then click open
# In the Import Options pane, change the name to "df" 
# Click Import
# You should be able to see "df" in the Environment Panel 

##########################################
# Clean the data
##########################################
# Create a separate column for strain, drug, concentration, time point and image file extension 
df_clean <- df %>%
  separate(Filename, c("strain", "drug", "concentration",
                       "timepoint"), sep = "_") %>%
  separate(timepoint, c("timepoint", "image_format")) %>% 
  separate(timepoint, c("t", "time"), sep = "t") %>% 
  select(strain, drug, concentration, time, Fungi) %>% 
  rename("area" = "Fungi") %>%
  mutate_each(funs(as.numeric), time)

##########################################
# Prepare the data to fit a logistic equation
##########################################
# Subtract the t0 % area from all the % area from all the time points to make
# the first time point equal to 0 for logistic analysis
t0 <- df_clean %>%
  filter(time == 0)

df_0 <- merge(df_clean, t0, by = c("strain", "drug",
                                   "concentration")) %>%
  select(!time.y) %>%
  mutate(area = area.x - area.y) %>%
  select(c(strain, drug, concentration, time.x, area)) %>%
  rename("time" = "time.x")
##########################################
# Convert the df_0 dataframe from long to wide
##########################################
df_0_wide <- df_0 %>%
  unite(sample, strain, drug, concentration) %>% 
  pivot_wider(names_from = sample, values_from = area)

##########################################
#Fit a logistic equation to data from each well
##########################################
# SummarizeGrowthByPlate will provide the r (growth rate) and k (asymptote) values for each well

logfit_params0 <- SummarizeGrowthByPlate(df_0_wide)
view(logfit_params0)

# Check the note column of logfit_params 
# If it says "cannot fit data", you need to filter out those wells from the df_0_wide dataframe or the next step will not work. For example if this was recorded for well "Calb_FLC_16", use filter() to remove that well.
  df_0_wide <- df_0 %>%
  unite(sample, strain, drug, concentration) %>% 
  filter(!sample == "Calb_FLC_16") %>% 
  pivot_wider(names_from = sample, values_from = area)

# If you want to remove multiple wells, adjust the filter function to include the name of all wells you want to remove replace line 55 with: 
# filter(!sample %in% c(“S2_FLC_1”, "S7_FLC_2", “S11_FLC_1”))
# indicating the names of the wells that failed as appropriate, separated by commas

# Add the subtracted t0 area back to the asymptote (k)
logfit_params <- unite(t0, sample, strain, drug,
                       concentration) %>% 
  merge(logfit_params0) %>% 
  group_by(sample) %>% 
  mutate(k = area + k) %>% 
  select(!time & !area) %>% 
  separate(sample, into = c("strain", "drug",
                            "concentration"), sep = "_")

# Create a new dataframe, df_predicted_plate, that contains the predicted % area of the logistic fit 
# This will be used to plot the predicted fit in Basic Protocol 6. 
models_all <- lapply(df_0_wide[2:ncol(df_0_wide)], 
                     function(x) SummarizeGrowth(df_0_wide$time, x)
)

df_predicted_plate <- data.frame(time = df_0_wide$time)

for (i in names(df_0_wide[2:ncol(df_0_wide)])){
  df_predicted_plate[[i]] <- 
    stats:::predict(models_all[[i]]$model)
  }

# Create a new dataframe, df_0_logfit, that contains Orbit % area and logistic fit predicted % area.
df_0_logfit <- cbind(
  pivot_longer(df_0_wide, !time,
               names_to = "sample",
               values_to = "area"),
  pred_area = pivot_longer(df_predicted_plate, !time,
                           names_to = "sample",
                           values_to = "pred_area")[,3]) %>%
  separate(sample, 
           c("strain", "drug", "concentration"), "_") 
  

# Add the subtracted t0 % area back in to the % area and predicted % area and save this final dataframe, df_final as .csv file.
df_final <- merge(df_0_logfit, t0,
                  by = c("strain", "drug",
                         "concentration")) %>%
  select(!time.y) %>%
  rename("time" = "time.x") %>% 
  group_by(strain, drug, concentration, time) %>% 
  summarise(area = area.x + area.y,
            pred_area = pred_area + area.y)

# Save df_final as a .csv file 
write_csv(df_final, 
          file = "example_data_predicted.csv")

##########################################
# Calculate time to reach the asymptote & correct unreasonable summary metrics 
##########################################
# Create a new dataframe, logfit_corrected, that uses the predicted areas calculated above to determine time to asymptote (t_asym), correct for asymptote when asymptote (k) > 1 and correct for slope (r) when k < 0.15.

logfit_corrected <- logfit_params %>%
  select(strain, drug, concentration, k, r) %>% 
  merge(df_final, by = c("strain", "drug",
                         "concentration")) %>%
  mutate(k_corrected = replace(k, round(k, digits = 2) > 1,
                               pred_area[which(time == "24" )]), # Change 24 to your period of incubation
         r_corrected = replace(r, k < 0.15, 0),  # Change 0.15 to the k cutoff where you consider your cells not growing
         k = round(k, digits = 2),
         pred_area = round(pred_area, digits = 2)) %>% 
  group_by(strain, concentration, drug) %>% 
  summarise(t_asym = time[which.min(abs (pred_area - k))],
            k_corrected = mean(k_corrected),
            r_corrected = mean(r_corrected)) %>% 
  mutate(t_asym = replace(t_asym, r_corrected == 0, 28)) # Change 28 to your period of incubation 

# add the logfit_corrected to the logfit_params dataframe
logfit_params_corrected <- merge(logfit_corrected,
                                 logfit_params,
                                 by = c("strain", "drug", "concentration"))

##########################################
# Save a .csv file that contains all the summary metrics 
##########################################
write_csv(logfit_params_corrected, 
          file = "example_data_logfit.csv")


