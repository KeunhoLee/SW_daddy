library('dplyr')
library('KoNLP')
library('wordcloud2')
library('stringr')
library('KoSpacing')

useSejongDic()

sw_list <- readRDS('sw_daddy_2020-08-30_v2.rds')
df <- sw_list[[1]]
spam_replies <- sw_list[[3]][[168]]
df[which.max(df$n_views),]
which.max(df$n_views)

# ㅋ 제거
clean_reply <- spam_replies$reply_comment %>%
  str_replace_all('[^가-힣]', ' ') %>%
  str_replace_all('[:punct:]', ' ') %>%
  str_replace_all('[A-Za-z]', ' ') %>%
  str_replace_all('[:digit:]', ' ') %>%
  str_replace_all('[ ]+', ' ') %>% 
  str_replace_all('아조씨', '아저씨') %>%
  str_replace_all('아버님|아버지', '아빠') %>%
  str_replace_all('백종원씨|백종원님', '백종원') %>%
  str_replace_all('승우님|승우야|승우가|승우는', '승우') %>%
  str_replace_all('스펨', '스팸') %>%
  str_replace_all('후라이펜', '후라이팬') %>%
  str_replace_all('으로 ', ' ') %>%
  str_replace_all('에게 ', ' ') %>%
  str_replace_all('[이을에은게] ', ' ') %>%
  str_replace_all('재미나|재미있|재밋|재밌게|재밌|재밌고|재밌어|재밌어서|재밌으시', '재미') %>%
  spacing() %>%
  unlist

# 이 을 에 은 으로 에게 
# 백종원님 백종원씨
# 재미나 재미있 재밋 재밋 재밌게 재밌고 재밌는 재밌어 재밌어서 재밌으시
# 승우님 승우야

clean_word <- SimplePos09(clean_reply) %>% unlist
extracted_word <- str_match(clean_word, '([가-힣]+)/[NPM]')
keyword <- extracted_word[,2]
keyword <- keyword[!is.na(keyword)]
keyword <- keyword[nchar(keyword) > 1]

wordcount <- table(keyword)
wordcount <- wordcount[wordcount>1]

freqword <- wordcount[order(wordcount, decreasing = TRUE)]
names(freqword)[names(freqword) == '기르'] <- '기름'
wordcloud2(data = freqword[-c(1:10)],
           color = 'random-light',
           backgroundColor = 'grey')

letterCloud(data = freqword,
            word='S',
            size=1)

write.csv(keyword, 'keyword.csv')

head(freqword[-c(1:7)])

grep('만들', clean_word, value = TRUE)
