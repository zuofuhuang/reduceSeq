library(tidyverse)
library(lubridate)

# start_days <- c(st, seq(floor_date((st + 60*60*24), unit = "days"), 
#             floor_date(en, unit = "days"),
#             by = "1 day"))
# end_days <- c(seq(floor_date(st + 60*60*24, unit = "days"), 
#                   floor_date(en, unit = "days"),
#                   by = "1 day") -1, en)

split_mult_days <- function(df){
  id_df <- df %>%
    mutate(mult_day = ifelse(date(start_posix) != date(end_posix),1,0))
  single_df <- id_df %>%
    filter(mult_day == 0) %>%
    dplyr::select(-mult_day)
  mult_df <- id_df %>%
    filter(mult_day == 1) %>%
    dplyr::select(-mult_day) %>%
    mutate(num_days = as.double(date(end_posix) -date(start_posix) + 1))
  mult_df_exp <- mult_df %>%
    mutate(rn = row_number()) %>%
    rowwise() %>%
    slice(rep(1, num_days)) %>%
    group_by(rn) %>%
    #the 60*60*24 adds a day, floor gets 00:00:00
    mutate(start_posix = c(start_posix[1], seq(floor_date((start_posix[1] + 60*60*24), unit = "days"), 
                                               floor_date(end_posix[1], unit = "days"),
                                               by = "1 day"))) %>%
    #subtract one to get the 23:59:59
    mutate(end_posix = c(seq(floor_date(start_posix[1] + 60*60*24, unit = "days"), 
                             floor_date(end_posix[1], unit = "days"),
                             by = "1 day") -1, end_posix[1])) %>%
    ungroup() %>%
    dplyr::select(-rn, -num_days)
  final_df <- bind_rows(single_df, mult_df_exp) %>% arrange(user_id, start_posix, end_posix, cal_item_id)
  return(final_df)
}


time_to_sequence <- function(row, segments, segment_midpoints){
  day_sequence <- rep(NA, segments)
  #day_sequence[which(segment_midpoints >= row$start_minute_total & segment_midpoints <= row$end_minute_total)] <- row$subtype_trunc
  
  day_sequence[which(segment_midpoints >= as.numeric(row['start_minute_total']) & segment_midpoints <= as.numeric(row['end_minute_total']))] <- row['subtype_trunc']
  new_seq <- day_sequence
  return(new_seq)
}

day_to_sequence <- function(day_data, segments, segment_midpoints){
  test2 <-apply(day_data, 1, time_to_sequence, segments = segments, segment_midpoints = segment_midpoints)
  vec_list <- lapply(seq_len(ncol(test2)), function(i) test2[,i])  
  final_vec <- coalesce(!!!vec_list)
  final_vec[is.na(final_vec)] <- "X"
  seq <- paste0(final_vec, collapse = '')
  return(seq)
}

data_to_sequence <- function(data, bandwidth, conversion_codes){
  segments = round(1440/bandwidth)
  segment_midpoints <- 1:segments*bandwidth - (bandwidth/2)
  data <- data %>% 
    mutate(start_minute_total = hour(start_posix)*60 + minute(start_posix),
           end_minute_total = hour(end_posix)*60 + minute(end_posix),
           subtype = as.character(subtype),
           subtype_trunc = conversion_codes[subtype],
           uniq_id = paste(user_id, '_',as.character(date(start_posix))))
  
  unique_id_dates <- data %>%
    mutate(start_date = date(start_posix)) %>%
    select(user_id, start_date, uniq_id) %>%
    unique()
  
  list_data <- split(data, f = data$uniq_id)
  final_seq <- lapply(list_data, day_to_sequence, segments = segments, segment_midpoints = segment_midpoints)
  final_df <- data.frame(Reduce(rbind, final_seq)) %>%
    rename(seq = `Reduce.rbind..final_seq.`) %>%
    mutate(uniq_id = names(final_seq)) %>%
    full_join(unique_id_dates, by = 'uniq_id')
  
  return(final_df)
}

#confirmed this was doing the same thing as Andy's
# split_mult_days <- function(df){
#   id_df <- df %>%
#     mutate(mult_day = ifelse(date(start_timestamp) != date(end_timestamp),1,0))
#   single_df <- id_df %>%
#     filter(mult_day == 0) %>%
#     dplyr::select(-mult_day)
#   mult_df <- id_df %>%
#     filter(mult_day == 1) %>%
#     dplyr::select(-mult_day) %>%
#     mutate(num_days = as.double(date(end_timestamp) -date(start_timestamp) + 1))
#   mult_df_exp <- mult_df %>%
#     mutate(rn = row_number()) %>%
#     rowwise() %>%
#     slice(rep(1, num_days)) %>%
#     group_by(rn) %>%
#     #the 60*60*24 adds a day, floor gets 00:00:00
#     mutate(start_timestamp = c(start_timestamp[1], seq(floor_date((start_timestamp[1] + 60*60*24), unit = "days"), 
#                                        floor_date(end_timestamp[1], unit = "days"),
#                                        by = "1 day"))) %>%
#     #subtract one to get the 23:59:59
#     mutate(end_timestamp = c(seq(floor_date(start_timestamp[1] + 60*60*24, unit = "days"), 
#                                  floor_date(end_timestamp[1], unit = "days"),
#                                  by = "1 day") -1, end_timestamp[1])) %>%
#     ungroup() %>%
#     dplyr::select(-rn, -num_days)
#   final_df <- bind_rows(single_df, mult_df_exp) %>% arrange(user_id, start_timestamp, end_timestamp, cal_item_id)
#   return(final_df)
# }
# 
# out <- split_mult_days(r_df)
# 
# data <- out %>%
#   filter(user_id == 'user_1') %>%
#   arrange(start_timestamp)
# 
# 
# 
# labels <- c("T", "T", "T",
#             "W", "H", "O",
#             "O", "T", "O",
#             "O", "O", "T",
#             "T", "T", "T",
#             "W","T","X")
# names(labels) <- c("OTHER","WALK", "CAR",
#                    "WORK", "HOME", "SHOP",
#                    "UNKNOWN_ACTIVITY", "IN_VEHICLE", "LEISURE_RECREATION", 
#                    "EAT_OUT", "PERSONAL_BUSINESS","WAIT",
#                    "BIKE", "BUS","UNKNOWN_TRAVEL_MODE",
#                    "EDUCATION", "RAIL", "MISSING")
# 
# 
# time_to_sequence <- function(row, segments, segment_midpoints){
#   day_sequence <- rep(NA, segments)
#   #day_sequence[which(segment_midpoints >= row$start_minute_total & segment_midpoints <= row$end_minute_total)] <- row$subtype_trunc
#  
#   day_sequence[which(segment_midpoints >= as.numeric(row['start_minute_total']) & segment_midpoints <= as.numeric(row['end_minute_total']))] <- row['subtype_trunc']
#   new_seq <- day_sequence
#   return(new_seq)
# }
# 
# day_to_sequence <- function(day_data, segments, segment_midpoints){
#   test2 <-apply(day_data, 1, time_to_sequence, segments = segments, segment_midpoints = segment_midpoints)
#   vec_list <- lapply(seq_len(ncol(test2)), function(i) test2[,i])  
#   final_vec <- coalesce(!!!vec_list)
#   final_vec[is.na(final_vec)] <- "X"
#   seq <- paste0(final_vec, collapse = '')
#   return(seq)
# }
# 
# data_to_sequence <- function(data, bandwidth, conversion_codes){
#   segments = round(1440/bandwidth)
#   segment_midpoints <- 1:segments*bandwidth - (bandwidth/2)
#   data <- data %>% 
#     mutate(start_minute_total = hour(start_timestamp)*60 + minute(start_timestamp),
#            end_minute_total = hour(end_timestamp)*60 + minute(end_timestamp),
#            subtype = as.character(subtype),
#            subtype_trunc = conversion_codes[subtype],
#            uniq_id = paste(user_id, '_',as.character(date(start_timestamp))))
#   
#   unique_id_dates <- data %>%
#     mutate(start_date = date(start_timestamp)) %>%
#     select(user_id, start_date, uniq_id) %>%
#     unique()
# 
#   list_data <- split(data, f = data$uniq_id)
#   final_seq <- lapply(list_data, day_to_sequence, segments = segments, segment_midpoints = segment_midpoints)
#   final_df <- data.frame(Reduce(rbind, final_seq)) %>%
#     rename(seq = `Reduce.rbind..final_seq.`) %>%
#     mutate(uniq_id = names(final_seq)) %>%
#     full_join(unique_id_dates, by = 'uniq_id')
# 
#   return(final_df)
# }
# 
# start_time <- Sys.time()
# out2 <- data_to_sequence(data, 5, labels)
# end_time <- Sys.time()
# end_time - start_time
# 
# start_time <- Sys.time()
# out2 <- data_to_sequence(out, 5, labels)
# end_time <- Sys.time()
# end_time - start_time

