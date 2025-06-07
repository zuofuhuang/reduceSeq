library(tidyverse)
# setwd('/Users/marth/C3PO/')
source('clean_data_code/mbarnard_functions.R')

###THIS IS WHAT WAS USED TO CREATE THE FILE BELOW###
p_df <-read_csv("data/processed_ucal_items_without_surveys_6_23_2022.csv")
labels <- c("T", "T", "T",
            "W", "H", "O",
            "O", "T", "O",
            "O", "O", "T",
            "T", "T", "T",
            "W","T","X")
names(labels) <- c("OTHER","WALK", "CAR",
                   "WORK", "HOME", "SHOP",
                   "UNKNOWN_ACTIVITY", "IN_VEHICLE", "LEISURE_RECREATION", 
                   "EAT_OUT", "PERSONAL_BUSINESS","WAIT",
                   "BIKE", "BUS","UNKNOWN_TRAVEL_MODE",
                   "EDUCATION", "RAIL", "MISSING")
out <- data_to_sequence(p_df, 5, labels)
#write_csv(out, 'data/cleaned_cal_data.csv')
#####

out <- read_csv('data/cleaned_cal_data.csv')
out <- out %>%
  rowwise() %>%
  mutate(num_states = length(unique(strsplit(seq, '')[[1]]))) %>%
  mutate(num_missing = sum(str_detect('X', strsplit(seq, '')[[1]]))) %>%
  ungroup() %>%
  mutate(hour_missing = num_missing*5/60) %>%
  mutate(missing_ind = as.factor(ifelse(hour_missing > 0, 1, 0))) %>%
  mutate(weekday = weekdays(start_date)) %>%
  mutate(day_type = ifelse(weekday == 'Saturday' | weekday == 'Sunday', 'weekend', 'M-F'))

df_prep <- out %>%
  filter(missing_ind ==0) %>%
  group_by(user_id) %>%
  mutate(num_seq_ind = n()) %>%
  ungroup()
df1 <- df_prep %>%
  filter(num_seq_ind <= 20)
#randomly sample 20 rows from each individual with more than 20 rows
set.seed(5)
df2 <- df_prep %>%
  filter(num_seq_ind > 20) %>%
  group_by(user_id) %>%
  sample_n(20)
df <- bind_rows(df1, df2) %>%
  rename(seqs = seq)
write_csv(df,'real_data/daynamica/data/day_seqs.csv')



