library('rvest')
library('RSelenium')
library('stringr')
library('dplyr')
library('lubridate')
# declare variables -------------------------------------------------------
result_path <- '.'
source_path <- ''

# source functions --------------------------------------------------------
source('./functions.R', encoding = 'UTF-8')

# code run ----------------------------------------------------------------
#remDr$server$stop()
#remDr$client$open()
# remDr$client$screenshot(display = TRUE)
remDr <- fn_start_driver(4445L)

# 빨간맛맛
MAIN_URL <- 'https://www.youtube.com/c/%EC%8A%B9%EC%9A%B0%EC%95%84%EB%B9%A0/videos'
PLAYLIST_URL <- 'https://www.youtube.com/c/%EC%8A%B9%EC%9A%B0%EC%95%84%EB%B9%A0/playlists'

init_page_to_crawl(remDr, MAIN_URL)
info_df <- get_video_infos(remDr)

reply_list <- list()
info_df$upload_date <- NA_character_
info_df$good <- NA_integer_
info_df$bad <- NA_integer_
info_df$n_replies <- NA_integer_

for ( v in 371:dim(info_df)[1] ){
  
  print( str_interp('[${now()}] ${v}/${dim(info_df)[1]}') )

  video_title <- info_df$title[v]
  video_url   <- info_df$video_url[v]

  init_page_to_crawl(remDr, video_url, 3)

  # open_all_details(remDr)
  
  page_src <- remDr$client$getPageSource()
  # stop('')
  info_df$upload_date[v] <- get_upload_date(page_src)
  
  good_bad <- get_good_bad(page_src)
  info_df$good[v] <- good_bad[1]
  info_df$bad[v] <- good_bad[2]
  
  info_df$n_replies[v] <- get_n_replies(page_src)

  reply_list[[video_title]] <- get_replies(page_src)
  
  rand_delay()
}
# 362,370 번 동영상 댓글사용중지 <- 예외처리 할 것

remDr$client$navigate(PLAYLIST_URL)
playlist_master <- get_playlist_infos(remDr)

playlist_videos <- data.frame()

for (  v in 1:dim(playlist_master)[1] ){
  
  playlist_url <- playlist_master$playlist_url[v]
  playlist_title <- playlist_master$playlist_title[v]
  
  init_page_to_crawl(remDr, playlist_url, 3)
  
  page_src <- remDr$client$getPageSource()
  video_title <- read_html(page_src[[1]]) %>% 
    html_nodes(xpath = '//span[@id="video-title"]') %>%
    html_text() %>% 
    gsub('\n', '', .) %>%
    trimws()
  
  playlist_videos <- rbind(playlist_videos,
                           data.frame(playlist_title = playlist_title,
                                      video_title = video_title,
                                      timestamp = now())
                           )

  rand_delay()
  }

playlist_videos
remDr$server$stop()

# save result -------------------------------------------------------------
file_name <- str_interp('${result_path}/sw_daddy_${today()}_v2.rds')

saveRDS(list(info_df, playlist_master, reply_list), file_name)
