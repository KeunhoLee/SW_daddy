
# base --------------------------------------------------------------------
rand_delay <- function(mean_delay=0.5){
  
  Sys.sleep(rexp(1,1/mean_delay))
  
}

fn_start_driver <- function(port = 4445L, browser = "chrome", chromever = "84.0.4147.30"){
  
  eCaps <- list(chromeOptions = list(
    args = c('--window-size=1280,800') #'--headless','--disable-gpu', '--window-size=1280,800'
  ))
  
  remDr <- rsDriver(port = port, browser = browser, chromever = chromever)#, extraCapabilities = eCaps)
  
  return(remDr)
}

init_page_to_crawl_old <- function(remDr, url, n = 10){
  
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

init_page_to_crawl <- function(remDr, url, sleep_time = 3){
  
  remDr$client$navigate(url)
  Sys.sleep(2) # wait 1 sec to load page
  
  last_scroll_height <- remDr$client$executeScript('return document.documentElement.scrollHeight')[[1]]
  
  while(TRUE){
    
    remDr$client$executeScript('window.scrollTo(0, document.documentElement.scrollHeight);')
    Sys.sleep(sleep_time)
    new_scroll_height <- remDr$client$executeScript('return document.documentElement.scrollHeight')[[1]]
    
    if(new_scroll_height == last_scroll_height){
      break
    } else {
      last_scroll_height <- new_scroll_height
    }
    
  }
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
  sub_urls <- paste0('https://www.youtube.com', sub_urls)
  
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
  
  info_df <- info_df %>% mutate(title = 
                                  trimws(title))
  
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

open_all_details <- function(remDr){
  
  # 자세히 보기를 누르지 않아도 내용을 가져 올 수 있다.
  # more_detail_buttons <- remDr$client$findElements(using = 'xpath', '//paper-button[@id="more"]')
  # for(i in 1:length(more_detail_buttons)){
  #   
  #   print(paste('More detail :', length(more_detail_buttons)))
  #   more_detail_buttons[[i]]$clickElement()
  #   print(paste('detail', i))
  #   rand_delay(1)
  # }
  
  more_reply_buttons <- remDr$client$findElements(using = 'xpath', '//ytd-button-renderer[@id="more-replies"]')
  for(i in 1:length(more_reply_buttons)){
    
    print(paste('More reply :', length(more_reply_buttons)))
    more_reply_buttons[[i]]$clickElement()
    print(paste('reply', i))
    rand_delay(1)
  }
}

get_upload_date <- function(page_src){
  
  upload_date <- read_html(page_src[[1]]) %>% 
    html_nodes(xpath = '//div[@id="date"]/yt-formatted-string[@class="style-scope ytd-video-primary-info-renderer"]') %>%
    html_text()
  
  paste(sapply(str_split(upload_date, '\\.')[[1]][-4],
               function(x) sprintf('%02d', as.numeric(x))),
        collapse = '-')
  
  return(upload_date)
  
}

get_good_bad <- function(page_src){
  
  good_bad <- read_html(page_src[[1]]) %>% 
    html_nodes(xpath = '//yt-formatted-string[@class="style-scope ytd-toggle-button-renderer style-text"]') %>%
    html_attr('aria-label')
  
  good_bad <- as.numeric(gsub('[^0-9]','',good_bad))
  
  names(good_bad) <- c('good', 'bad')
  
  return(good_bad)
  
}

get_n_replies <- function(page_src){
  
  n_replies <- read_html(page_src[[1]]) %>% 
    html_nodes(xpath = '//yt-formatted-string[@class="count-text style-scope ytd-comments-header-renderer"]') %>%
    html_text()
  
  n_replies <- as.numeric(gsub('[^0-9]','',n_replies))
  
  return(n_replies)
}

get_replies <- function(page_src){
  
  reply_html <- read_html(page_src[[1]]) %>%
    html_nodes(xpath = '//ytd-item-section-renderer/div[@id="contents"]') %>%
    html_nodes(xpath = 'ytd-comment-thread-renderer') #%>% length
  
  result <- data.frame()
  
  for( i in 1:length(reply_html)){
    
    # 댓글쓴이  
    reply_author <- reply_html[i] %>% 
      html_nodes(css="#author-text") %>%
      html_text %>%
      gsub('\n', '', .) %>%
      gsub(' ', '', .)
    
    # 댓글
    reply_comment <- reply_html[i] %>%
      html_nodes(css="#content") %>% 
      html_text %>%
      gsub('\n', ' ', .) %>%
      gsub(' +', ' ', .) %>%
      trimws
    
    
    # 댓글
    reply_like <- reply_html[i] %>%
      html_nodes(css="#vote-count-middle") %>% 
      html_text %>%
      gsub('[^0-9]', '', .) %>%
      as.numeric
    
    # 댓글 좋아요/ 나빠요
    
    result <- rbind(result, data.frame(reply_author = reply_author,
                                       reply_comment = reply_comment,
                                       reply_like = reply_like,
                                       reply_id = i,
                                       reply_sub_id = 1:length(reply_comment)))
    
  }
  
  result$timestamp <- now() 
  
  return(result)
}