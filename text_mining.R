library('tm')
library('igraph')
library('tidytext')
library('KoNLP')
library('NLP4kec')
library('dplyr')
library('ggraph')
library('qgraph')
library('tidygraph')
library('stringr')
library('tidyr')

df <- reply_list[[20]]
dim(df)

doc <- as.character(df$reply_comment)

#####################
##   전처리 영역   ##
#####################

doc <- doc %>%
  str_replace_all(pattern="\r", replacement="") %>%
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>% #removePunctuation
  str_replace_all(pattern="[ㄱ-ㅎㅏ-ㅣ]+", replacement="") %>%
  str_replace_all(pattern="/", replacement=" ") %>%
  str_trim(side="both")

doc_parsed <- doc %>% 
  tibble(line=1:643, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09)

doc_count <- doc_parsed %>%
  mutate(noun=str_match(pos, '([가-힣]+)/n')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  count(noun, sort=TRUE) %>%
  head(15)

doc_parsed_2 <- doc_parsed %>%
  mutate(noun=str_match(pos, '([가-힣]+)/n')[,2]) %>%
  na.omit %>%
  select(noun, line)

doc_parsed_3 <- doc_parsed_2 %>%
  filter(noun %in% doc_count$noun)

doc_parsed_graph <- graph_from_data_frame(doc_parsed_3)

V(doc_parsed_graph)$type <- bipartite_mapping(doc_parsed_graph)$type

mm <- as_incidence_matrix(doc_parsed_graph) %*% t(as_incidence_matrix(doc_parsed_graph))
diag(mm) <- 0
mg <- graph_from_adjacency_matrix(mm)

plot(mg)

mg %>% as_tbl_graph() %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))


# bigram ------------------------------------------------------------------

doc_parsed_2 %>%
  select(noun) %>%
  mutate(lead = lead(noun))

doc_parsed_2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun),
         bigram = paste(noun, lead, sep=' ')) %>% 
  count(bigram, sort=TRUE)
  

bigram_df <- doc_parsed_2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun),
         bigram = paste(noun, lead, sep=' ')) %>% 
  count(bigram, sort=TRUE) %>%
  head(30) %>%
  separate(bigram, c('word1', 'word2'))
  

bigram_df %>%
  as_tbl_graph %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

# -------------------------------------------------------------------------

ko_words <- function(doc) {
  d <- as.character(doc)
  pos <- unlist(SimplePos22(d))
  
  extracted <- str_match(pos, '([가-힣]+)/[NP][A-Z]')
  
  keyword <- extracted[, 2]
  keyword[!is.na(keyword)]
}

texts <- texts[texts != ""]

pos <- Map(ko_words, texts)

corpus_2 <- Corpus(VectorSource(pos))
corpus_2 <- tm_map(corpus_2, removePunctuation)
corpus_2 <- tm_map(corpus_2, removeNumbers)

tdm <- TermDocumentMatrix(corpus_2, control=list(
  removePunctuation=TRUE,
  removeNumbers=TRUE, wordLengths=c(4, 10), weighting=weightBin))

tdm

tdm_matrix <- as.matrix(tdm)

word_count <- rowSums(tdm_matrix)
word_order <- order(word_count, decreasing = TRUE)
freq_words <- tdm_matrix[word_order[1:30], ]

co_matrix <- freq_words %*% t(freq_words)
co_matrix

qgraph(co_matrix, labels=rownames(co_matrix),
       diag=FALSE, layout='spring', threshold=3,
       vsize=log(diag(co_matrix)) * 2)
