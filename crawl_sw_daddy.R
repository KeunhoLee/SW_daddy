library('rvest')
library('RSelenium')
library('stringr')
library('dplyr')
library('lubridate')
# Selenium 연결

# base --------------------------------------------------------------------
fn_start_driver <- function(port = 4445L, browser = "chrome", chromever = "84.0.4147.30"){
  
  eCaps <- list(chromeOptions = list(
    args = c('--window-size=1280,800') #'--headless','--disable-gpu', '--window-size=1280,800'
  ))
  
  remDr <- rsDriver(port = port, browser = browser, chromever = chromever)#, extraCapabilities = eCaps)
  
  return(remDr)
}

init_page_to_crawl <- function(remDr, url, n = 10){
  
  remDr$client$navigate(url)
  Sys.sleep(1) # wait 1 sec to load page
  
  scroll_from <- 0 
  scroll_to <- 500
  
  for ( i in 1:n){
    remDr$client$executeScript(str_interp('window.scrollTo(${scroll_from}, ${scroll_to});')) # return to top
    Sys.sleep(1) # wait 1 sec to load page
    
    scroll_from <- scroll_to 
    scroll_to <- scroll_from + 500
  }
  remDr$client$executeScript('window.scrollTo(0, 0);') # return to top
  
}

get_video_infos <- function(remDr){
  
  #get title, views, uploader
  page_src <- remDr$client$getPageSource() # get page source
  items <- read_html(page_src[[1]]) %>% html_nodes(xpath = '//*[@class="style-scope ytd-grid-renderer"]') # get items from page source
  url_to_detail <- items %>% html_nodes(xpath = '//*[@id="video-title"]') %>% html_attrs()
  url_to_detail <- sapply(url_to_detail, function(x) x['aria-label'])
  
  #get video url
  sub_urls <- read_html(page_src[[1]]) %>%
    html_nodes(xpath = '//*[@class="yt-simple-endpoint inline-block style-scope ytd-thumbnail"]') %>%
    html_attrs()
  sub_urls <- sapply(sub_urls, function(x) x['href'])
  sub_urls <- na.omit(sub_urls)
  
  #get running time
  video_times <- read_html(page_src[[1]]) %>%
    html_nodes(xpath = '//*[@class="style-scope ytd-thumbnail-overlay-time-status-renderer"]') %>%
    html_text()
  
  video_times <- video_times[c(FALSE, TRUE)]
  video_times <- trimws( gsub('\n', '', video_times) )
  
  info_df <- data.frame( detail  = url_to_detail,
                         video_url = sub_urls,
                         running_time = video_times,
                         stringsAsFactors = FALSE)
  
  info_df <- info_df %>% mutate( title = str_sub(detail,
                                                 1,
                                                 str_locate(detail, '게시자')[,1] - 1),
                                 n_views = str_sub(detail,
                                                   str_locate(detail, '조회수')[,2] + 1,
                                                   nchar(detail) - 1) %>% str_replace_all(',', '') %>% as.numeric
  )
  
  info_df <- info_df %>% select( title, running_time, n_views, video_url, -detail)
  info_df$timestamp <- now()
  return(info_df)
}

get_playlist_infos <- function(remDr){
  
  page_src <- remDr$client$getPageSource()
  
  playlists <- read_html(page_src[[1]]) %>% html_nodes(xpath = '//*[@class="style-scope ytd-grid-renderer"]') # get items from page source
  
  playlist_titles <- playlists %>% html_nodes(xpath = '//*[@id="video-title"]') %>% html_text()
  
  playlist_url <- playlists %>%
    html_nodes(xpath = '//*[@class="yt-simple-endpoint style-scope yt-formatted-string"]') %>%
    html_attr('href')
 
  playlist_n <- playlists %>%
    html_nodes(xpath = '//*[@class="style-scope ytd-thumbnail-overlay-side-panel-renderer"]') %>%
    html_text() %>% .[c(TRUE, FALSE)] %>% as.numeric()
  
  playlist_master <- data.frame( playlist_title = playlist_titles,
                                 n_contents = playlist_n,
                                 playlist_url = playlist_url,
                                 stringsAsFactors = FALSE)
  
  playlist_master$timestamp <- now()
  
  return(playlist_master)
}
#remDr$server$stop()
remDr <- fn_start_driver(4448L)

MAIN_URL <- 'https://www.youtube.com/c/%EC%8A%B9%EC%9A%B0%EC%95%84%EB%B9%A0/videos'
PLAYLIST_URL <- 'https://www.youtube.com/c/%EC%8A%B9%EC%9A%B0%EC%95%84%EB%B9%A0/playlists'

init_page_to_crawl(remDr, MAIN_URL, 100)
info_df <- get_video_infos(remDr)

remDr$client$navigate(PLAYLIST_URL)
playlist_master <- get_playlist_infos(remDr)

playlist_master
# playlist ----------------------------------------------------------------


