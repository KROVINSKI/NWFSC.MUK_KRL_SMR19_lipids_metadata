# Processing file lipids file "SHALIN PREVIEW_020621.xlsx"

#install metacsv
devtools::install_git("https://github.com/pmcelhany/metacsv")

library(tidyverse)
library(metacsv)

setwd("/Users/paul.mcelhany/Documents/Github Projects/krill_lipids")
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
dgen <- dM$generalMetadata
dvar <- dM$variableMetadata
# get the dataset dataframe from read_meta output
d <- dM$data


# TODO use across funcion to calc metrics based on _per_sample
sample_cols <- c("total_per_sample", "tag_per_sample", "ffa_per_sample", 
                 "st_per_ll_wwt", "polar_lipids_per_sample")

# add derived variables of interest
d <- d %>%
  # add treatments
  mutate(treatment = case_when(moats %in% c("1", "6") ~ "high_temp",
                               moats %in% c("2", "8", "12") ~ "all_change",
                               moats %in% c("3", "7", "10", "13") ~ "current",
                               moats %in% c("4", "5") ~ 
                                 "ambient",
                               moats == "wild" ~ "wild")) %>%
  #lipid total
  mutate(total_per_sample = tag_per_sample + ffa_per_sample + 
           st_per_sample + polar_lipids_per_sample) %>%
  # lipid ug per mg wet weight (used the lipid lab measuremnts of mass)
  # the per mass measurement seems most relevant
  mutate(tag_per_ll_wwt = tag_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(ffa_per_ll_wwt = ffa_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(st_per_ll_wwt = st_per_sample / lipid_lab_mass / 1000 ) %>%
  mutate(polar_lipids_per_ll_wwt = polar_lipids_per_sample / 
           lipid_lab_mass / 1000 ) %>%
  mutate(total_per_ll_wwt = total_per_sample / lipid_lab_mass / 1000 ) %>%
  # lipid ug per mg wet weight (used the nwfsc measuremnts of mass)
  # the per mass measurement seems most relevant
  mutate(tag_per_nwfsc_wwt = tag_per_sample / nwfsc_mass / 1000 ) %>%
  mutate(ffa_per_nwfsc_wwt = ffa_per_sample / nwfsc_mass / 1000 ) %>%
  mutate(st_per_nwfsc_wwt = st_per_sample / nwfsc_mass / 1000 ) %>%
  mutate(polar_lipids_per_nwfsc_wwt = polar_lipids_per_sample / 
           nwfsc_mass / 1000 ) %>%
  mutate(total_per_ll_wwt = total_per_sample / nwfsc_mass / 1000 ) %>%
  #lipid ug per count (used the lipid lab counts)
  mutate(tag_per_ll_count = tag_per_sample / lipid_lab_count ) %>%
  mutate(ffa_per_ll_count = ffa_per_sample / lipid_lab_count ) %>%
  mutate(st_per_ll_count = st_per_sample / lipid_lab_count ) %>%
  mutate(polar_lipids_per_ll_count = polar_lipids_per_sample / 
           lipid_lab_count ) %>%
  mutate(total_per_ll_count = total_per_sample / lipid_lab_count ) %>%
  # lipid ug per count (used the nwfsc counts)
  mutate(tag_per_nwfsc_count = tag_per_sample / nwfsc_count ) %>%
  mutate(ffa_per_nwfsc_count = ffa_per_sample / nwfsc_count ) %>%
  mutate(st_per_nwfsc_count = st_per_sample / nwfsc_count ) %>%
  mutate(polar_lipids_per_nwfsc_count = polar_lipids_per_sample / 
           nwfsc_count ) %>%
  mutate(total_per_nwfsc_count = total_per_sample / nwfsc_count ) %>%
  # TAG/ST ratio
  mutate(tag_st = tag_per_sample / st_per_sample) %>%
  # TAG/Polar lipids ratio
  mutate(tag_polar = tag_per_sample / polar_lipids_per_sample) %>%
{.}

  
# graph lipid per wwt as a function of moats and lipid type
d %>%
  # make long dataset for graphing lipid types
  pivot_longer(cols = c(tag_per_ll_wwt, ffa_per_ll_wwt, st_per_ll_wwt, 
                 polar_lipids_per_ll_wwt, total_per_ll_wwt), 
               names_to = "lipid_type", values_to = "lipid_ug_per_mg") %>%
#  ggplot(aes(moats, lipid_ug_per_mg)) +
  ggplot(aes(treatment, lipid_ug_per_mg)) +
   geom_boxplot() +
  #show jitter to look at data points
  geom_jitter(aes(colour =  moats)) + 
#  geom_text(aes(label = sample_id), size = 3, check_overlap = TRUE,
#           colour = "red") +
  facet_wrap(vars(lipid_type), ncol = 2, scales = "free") +
  geom_blank() %>%
  {.}


calc_z_outlier <- function(x){
  d <- data.frame(x = x)
  d$x_mean <- mean(x)
  d$x_sd <- sd(x)
  d$z_abs <- abs((d$x - d$x_mean) / d$x_sd)
  d$is_outlier <- FALSE
  d$is_outlier[d$z_abs > 3] <- TRUE
  return(d$is_outlier)
}

calc_IQR_outlier <- function(x){
  d <- data.frame(x = x)
  d$IQR <- quantile(d$x, 0.75, na.rm = TRUE) - quantile(d$x, 0.25, na.rm = TRUE)
  d$IQR_lower_thresh <- quantile(d$x, 0.25, na.rm = TRUE) - 1.5 * d$IQR
  d$IQR_upper_thresh <- quantile(d$x, 0.75, na.rm = TRUE) + 1.5 * d$IQR
  d$is_outlier <- FALSE
  d$is_outlier[d$x < d$IQR_lower_thresh | d$x > d$IQR_upper_thresh] <- TRUE
  return(d$is_outlier)
}

d_outlier_treat <- d %>%
  group_by(treatment)  %>%
  mutate(across(where(is.numeric), calc_z_outlier, .names = "z_{.col}")) %>%
  mutate(across(where(is.numeric), calc_IQR_outlier, .names = "iqr_{.col}")) %>%
  {.}

d_outlier_moats <- d %>%
  group_by(moats)  %>%
  mutate(across(where(is.numeric), calc_z_outlier, .names = "z_{.col}")) %>%
  mutate(across(where(is.numeric), calc_IQR_outlier, .names = "iqr_{.col}")) %>%
  {.}


d_outlier_treat %>% 
  pivot_longer(cols =  starts_with("z"), names_to = "metric", values_to = "is_outlier") %>%
  ggplot(aes(metric, sample_id)) +
           geom_tile(aes(fill = is_outlier), colour = "grey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("azure", "red")) +
  ggtitle("z-score outlier by treatment")
 

d_outlier_moats %>% 
  pivot_longer(cols =  starts_with("z"), names_to = "metric", values_to = "is_outlier") %>%
  ggplot(aes(metric, sample_id)) +
  geom_tile(aes(fill = is_outlier), colour = "grey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("azure", "red")) +
  ggtitle("z-score outlier by moats")

d_outlier_treat %>% 
  pivot_longer(cols =  starts_with("iqr"), names_to = "metric", values_to = "is_outlier") %>%
  ggplot(aes(metric, sample_id)) +
  geom_tile(aes(fill = is_outlier), colour = "grey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("azure", "red")) +
  ggtitle("IQR outlier by treatment")


d_outlier_moats %>% 
  pivot_longer(cols =  starts_with("iqr"), names_to = "metric", values_to = "is_outlier") %>%
  ggplot(aes(metric, sample_id)) +
  geom_tile(aes(fill = is_outlier), colour = "grey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("azure", "red")) +
  ggtitle("IQR outlier by moats")















