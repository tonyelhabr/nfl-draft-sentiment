
library(tidyverse)
library(googledrive)
# library(reticulate)
options(
  gargle_oauth_cache = '.secrets',
  gargle_oauth_email = TRUE
)

drive_files <- googledrive::drive_find(pattern = 'comments', type = 'csv', n_max = 5) # download from your drive
comments_file <- drive_files %>% filter(name == 'comments.csv')
path_local <- file.path('data', 'comments.csv')
path_backup <- file.path('data', 'comments_backup.csv')

# run <- function() {
#   job::job({
#     Sys.setenv(RETICULATE_PYTHON = 'C:\\Users\\aelhabr\\anaconda3\\envs\\boometer\\python.exe') 
#     reticulate::source_python('boo_meter_scraper.py')
#   })
# }
# while(TRUE) {
#   # run()
#   scraper_is_running <- TRUE
#   while(scraper_is_running) {
#     file.copy(path_local, path_backup, overwrite = TRUE)
#     path_info <- file.info(path_local)
#     drive_update(file = as_id(comments_file$id), media = path_backup)
#     cat(glue::glue('{Sys.time()}: Updating...'), sep = '\n')
#     Sys.sleep(5)
#     sec_diff <- lubridate::as.duration(Sys.time() - path_info$mtime) %>% lubridate::seconds() %>% as.numeric()
#     if(sec_diff >= 60) {
#       scraper_is_running <- FALSE
#       cat(glue::glue('{Sys.time()}: Updating...'), sep = '\n')
#     }
#   }
#   cat(glue::glue('{Sys.time()}: Hanging...'), sep = '\n')
#   beepr::beep(3)
#   Sys.sleep(1)
# }

while(TRUE) {
  file.copy(path_local, path_backup, overwrite = TRUE)
  path_info <- file.info(path_local)
  drive_update(file = as_id(comments_file$id), media = path_backup)
  cat(glue::glue('{Sys.time()}: Updating...'), sep = '\n')
  Sys.sleep(5)
  sec_diff <- lubridate::as.duration(Sys.time() - path_info$mtime) %>% lubridate::seconds() %>% as.numeric()
  if(sec_diff >= 30) {
    scraper_is_running <- FALSE
    # beepr::beep(3)
    cat(glue::glue('{Sys.time()}: Updating...'), sep = '\n')
  }
}
