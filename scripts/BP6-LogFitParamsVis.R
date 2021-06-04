##########################################
# Load the tidyverse library
##########################################
# Install the tidyverse package if you have not already do so. 
# NOTE: You only need to install the package the first time you use it
# NOTE: Lines of code can be run by putting your cursor on the line to be executed and clicking on "Run" above, or hitting "ctrl + enter" ("cmd + return on a Mac). You can also highlight multiple lines of code and run them all together.
install.packages("tidyverse") 

# This line needs to be done every session to load the library
library(tidyverse)

##########################################################
# Load the .csv output files from Basic Protocol 5
##########################################################
# Read the df_final and logfit_params_corrected .csv files  
# To do this through the RStudio interface, click on Import Dataset (under the environment panel on the top right).
# Click on From Text (readr). 
# Click on browse to find the .csv file, then click open
# Change the name in the Import Options pane (file containing % area and predicted area -> "df_final", and file containing the summary metrices -> "logfit_params_corrected") 
# Click Import
# You should be able to see those dataframes in the Environment Panel

##########################################
# Plot the % area covered by cells for all the time points
##########################################
# Use the predicted % area column to fit a logistic line to your data points 
df_final %>%
  ggplot(aes(x = time, y = area, color = concentration, 
             group = concentration))+
  geom_point() + 
  geom_line(aes(y = pred_area)) +
  scale_x_continuous(breaks = seq(0, 24, 4)) + # Change 24 to your period of incubation 
  facet_wrap(~strain)+
  xlab("Time (hours)") +
  ylab("Area % Covered by Cells") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw()
  

##########################################
# Plot the change in asymptote(k) as the concentration of drug increases
##########################################
logfit_params_corrected %>%
  ggplot(aes(x = concentration, y = k_corrected,
             group = strain)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~strain) +
  xlab("BA Concentration (mg/mL)") +
  ylab("% Area at Asymptote") +
  theme_bw()

##########################################
# Plot the change in  slope (r) as the concentration of drug increases 
##########################################
logfit_params_corrected %>%
  ggplot(aes(x = concentration, y = r_corrected,
             group = strain)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~strain) +
  xlab("BA Concentration (mg/mL)") +
  ylab("Slope") +
  theme_bw()

##########################################
# Plot the change in  time to reach the asymptote as the concentration of drug increases
##########################################
# Since in this example the images were only captured for 24 h; any well that took more than 24 h to reach the asymptote will be indicated with an open circle 
logfit_params_corrected %>% 
  ggplot(aes(x = concentration, y = t_asym, 
             group = strain)) +
  geom_point(aes(shape = t_asym > 24), size = 2) + # Change 24 to your period of incubation
  geom_line() +
  scale_y_continuous(breaks = seq(0, 28, 4), # Change 28 to your period of incubation + 4.
                     labels = c(seq(0 , 24, 4), "> 24"), # Change 24 to your period of incubation
                     limits = c(0, 28)) + # Change 28 to your period of incubation + 4
  scale_shape_manual(values = c(16, 1)) +
  facet_wrap(~strain) +
  xlab("BA Concentration (mg/mL)") +
  ylab("Time to Asymptote (h)") +
  theme_bw() +
  theme(legend.position = "none")
