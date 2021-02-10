# Processing file lipids file "SHALIN PREVIEW_020621.xlsx"

library(tidyverse)
library(metacsv)

setwd("/Users/paul.mcelhany/Downloads")
dRaw <- readxl::read_xlsx("SHALIN PREVIEW_020621.xlsx", col_names = FALSE)

# Clean up raw data. The code below extracts only rows from the original data
# that have unique information. All of the rows in the orignal data file can be
# created using simple math operations on the data in this striped down data
# frame.
dClean <- dRaw %>%
  #select rows with unique data
  slice(c(1,3:7, 9, 10, 12, 24:27, 47)) %>%
  #transpose the data frame ("...1" is the weird way R named the first column...)
  #the tidy world does not have a very intutive way to do a simple transformation
  #it requires this awkward pivot_longer, then pivot_wider operation. Not cool.
  pivot_longer(-...1) %>% 
  pivot_wider(names_from=...1, values_from=value) %>%
  #remove unneeded first column
  select(-c(name)) %>%
  #rename all the columns to something tidy
  rename(sample_id = "Sample ID") %>%
  rename(moats = "MOATs") %>%
  rename(treatment_system = "Treatment System" ) %>%
  rename(hours_fasting = "Hours Fasting") %>%
  rename(nwfsc_mass = "MWFSC Mass (g)") %>%
  rename(lipid_lab_mass = "Lipid Lab Mass (g)") %>%
  rename(nwfsc_count = "NWFSC Count") %>%
  rename(lipid_lab_count = "Lipid Lab Count") %>%
  rename(extract_vol = "Extract Volume (uL)") %>%
  rename(tag_per_sample = "TAG") %>%
  rename(ffa_per_sample = "FFA") %>%
  rename(st_per_sample = "ST") %>%
  rename(polar_lipids_per_sample = "Polar Lipids") %>%
  rename(total_fame = "Total FAME Estimate (ug)") %>%
  #remove empty comments row
  filter(sample_id != "Comments") %>%
  #convert charcter variables to numbers
  mutate(across(c(hours_fasting, nwfsc_mass, lipid_lab_mass, nwfsc_count,
                     lipid_lab_count, extract_vol, tag_per_sample,
                     ffa_per_sample, st_per_sample, polar_lipids_per_sample,
                     total_fame), as.numeric)) %>%
  {.}

#write the csv file with embedded metadata template. These are all the data you
#need to recreate the info in the orginal excel file.
write_meta_template(dClean, "krill_lipid_2020")
#### Now manually enter meta data in krill_lipid_2020_withMeta.csv file. Do not
#run the above write_meta_template() call again in the same folder or you will
#overwrite any manually entered metadata in the file named
# "krill_lipid_2020_withMeta.csv". (need to add feature to write_meta_template to
# check with user about overwriting the file.)


#############
# from here down, work with the archived _withMeta file that has actual metadata.
#read in the cleaned up data file that has embedded metadate
dM <- read_meta("krill_lipid_2020_withMeta.csv")
# get the dataset dataframe from read_meta output
d <- dM$data


# add the the lipid ug per mg wet weight (used the lipid lab measuremnts of mass)
# the per mass measurement seems most relevant
d <- d %>%
  mutate(tag_per_ll_wwt = tag_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(ffa_per_ll_wwt = ffa_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(st_per_ll_wwt = st_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(polar_lipids_per_ll_wwt = polar_lipids_per_sample / 
           lipid_lab_mass / 1000 ) %>%
{.}
  
  
# graph lipid per wwt as a function of moats and lipid type
d %>%
  # make long dataset for graphing lipid types
  pivot_longer(cols = c(tag_per_ll_wwt, ffa_per_ll_wwt, st_per_ll_wwt, 
                 polar_lipids_per_ll_wwt), names_to = "lipid_type",
               values_to = "lipid_ug_per_mg") %>%
  ggplot(aes(moats, lipid_ug_per_mg)) +
   geom_boxplot() +
  #show jitter to look at data points
#  geom_jitter(aes(colour =  moats)) + 
  facet_wrap(vars(lipid_type), ncol = 2, scales = "free") %>%
  {.}










