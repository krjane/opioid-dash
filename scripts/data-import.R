library(readxl)
library(tidyverse)

# List all excel files in /data directory
file_list <- list.files(("./data"))

# Create empty list
data_list = list()

# Iterate through names in file_list
for(i in file_list){
  data_list[[i]] <- read_excel(paste0("data/",i), 
                               col_types = c("date","guess","guess","numeric","guess","guess","guess")) %>%
    rename(date = 1, city = 2, county = 3, zip = 4, age = 5, gender = 6, race = 7) %>% 
    mutate(type = str_extract(i, "(?<=-)(.+)(?=\\-)"))
}

#Merge 
full_dataset = do.call(rbind,data_list)

#Write excel to csv file
write_csv(full_dataset, "data/full_dataset.csv")
