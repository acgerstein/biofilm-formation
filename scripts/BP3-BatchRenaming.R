##########################################################
# Load the tidyverse library
##########################################################
# Install the tidyverse package if you have not already do so. 
# NOTE: You only need to install the package the first time you use it
# NOTE: Lines of code can be run by putting your cursor on the line to be executed and clicking on "Run" above, or hitting "ctrl + enter" ("cmd + return on a Mac). You can also highlight multiple lines of code and run them all together.
install.packages("tidyverse") 

# This line needs to be done every session to load the library
library("tidyverse")

##########################################################
# Load the .csv file that contains the plate information
##########################################################
# To do this through the RStudio interface, click on Import Dataset (under the environment panel on the top right)
# Click on From Text (readr). 
# Click on browse to find the .csv file that contains your plate information, then click open
# In the Import Options pane, change the name to "plate_layout" 
# Click Import
# You should be able to see "plate_layout" in the Environment Panel

##########################################################
# Read existing names of images
##########################################################

# Set the working directory to the folder containing the files to be renamed 
# You can do that through the RSTudio interface by going to Session -> Set Working Directory -> Choose Directory 
# Open the folder containing your images. 
# NOTE: This folder should only contain these images
# Check that it's the correct files by listing all of the files in the working directory folder
# NOTE: if your images are not in .TIF format, change the image format as appropriate (e.g., "*.jpg") 
list.files(pattern = "*.TIF" ) 


# Create a dataframe, Evos_tbl, that contains a column (EvosNames) with all of the existing file names.  
Evos_tbl <- tibble(EvosNames =
                     unlist(list.files(pattern = "*.TIF" )))

# Split the EvosNames column into 3 columns: well, timepoint, image; save as a new dataframe, Evos_separate
Evos_separate <- Evos_tbl %>%
  separate(EvosNames, 
           into = c("well", "timepoint", "image")) %>% 
  cbind(Evos_tbl)

##########################################################
# Rename the files 
##########################################################
# Merge the Evos_separate dataframe with the plate_layout dataframe. This is done by creating a new column column, NewFullName, that combines the information provided in separate columns in plate_layout into one column (the example here combines strain, drug, concentration, timepoint). This should contain all of the information you  want to include in the new image names, i.e., all of the information about what differentiates what is in each well of your 96 well plate
Full <- Evos_separate %>% 
  merge(plate_layout, by = "well") %>%
  unite(col = "NewFullName", c(strain, drug,
                              concentration, timepoint), 
        na.rm = TRUE ,sep="_") %>% 
  select(EvosNames, NewFullName) %>% 
  na.omit()

# Images with names that match the existing names column (Full$EvosNames) will be renamed with the corresponding new name. Other images (e.g., those from blank wells if they are not included in the plate_layout spreadsheet) will not be renamed. As above, if your images are in a different format, change ".TIF" to the correct format.
file.rename(from = Full$EvosNames,
            to = paste0(Full$NewFullName, ".TIF"))

# Check that your images have been correctly renamed
list.files(pattern = "*.TIF" ) 