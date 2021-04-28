
library(tidyverse)
library(googledrive)

options(
  gargle_oauth_cache = '.secrets',
  gargle_oauth_email = TRUE
)

drive_files <- googledrive::drive_find(pattern = 'comments', type = 'csv', n_max = 2) # download from your drive
comments_file <- drive_files %>% filter(name == 'comments.csv')
path_local <- file.path('data', 'comments.csv')

drive_update(file = as_id(comments_file$id), media = path_local)
path_backup <- file.path('data', 'comments_backup.csv')
while(TRUE) {
  file.copy(path_local, path_backup, overwrite = TRUE)
  drive_update(file = as_id(comments_file$id), media = path_backup)
  cat(glue::glue('{Sys.time()}: Updating...'), sep = '\n')
  Sys.sleep(5)
}
