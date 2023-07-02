# downloading all the necessary packages required for assessment number 2

library(vosonSML)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)

library(tuber)

library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


library(qdapRegex)
library(syuzhet)

library(spotifyr)
library(C50)
library(caret)
library(e1071)

library(qdapRegex)
library(tm)
library(topicmodels)
library(slam)
library(Rmpfr)
library(dplyr)
library(reshape2)

library(rtweet)
library(ggplot2)


options(httr_oauth_cache = TRUE)


# credentials
app_id <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
app_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
token <- "1"


# Authentication for Rspotify package:
keys <- spotifyOAuth(token, app_id, app_secret) 


#########################################################################
###########################  QUESTION NO 2.1  ###########################
#########################################################################

# Get Spotify data on 'The weeknd'
the_weeknd_spotify_data <- searchArtist("The Weeknd", token = keys)
View(the_weeknd_spotify_data)


# Retrieve information about artist
the_weeknd_data <- getArtist("1Xyo4u8uXC1ZmMpatF05PJ", token = keys)
View(the_weeknd_data)


# part b: How many albums and songs they have published
albums <- getAlbums("1Xyo4u8uXC1ZmMpatF05PJ", token = keys)
total_albums = n_distinct(albums$name)
total_albums

unique_albums = albums[!duplicated(albums$name), ]
View(unique_albums)

total_songs <- 0
for (i in unique_albums$id)
{
  
  #print(i)
  songs <- getAlbum(i, token = keys)
  total_songs <- total_songs + n_distinct(songs$name)
}
total_songs 
#my_names_new <- songs[1:length(songs)]   #debugging purposes


# Authentication for spotifyr package:
Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

# part c: the artist that have collaborated with the weeknd
related_artist_the_weeknd <- getRelated("The Weeknd", token = keys)
View(related_artist_the_weeknd)
related_artist_the_weeknd <- related_artist_the_weeknd[order(related_artist_the_weeknd$popularity),]
top_10_related_artist_the_weeknd <- top_n(related_artist_the_weeknd,10)

#View(top_10_related_artist_the_weeknd)

#making bar plot of the top 10 collaborators based on their popularity
barplot(top_10_related_artist_the_weeknd$popularity,
        names.arg=top_10_related_artist_the_weeknd$name,ylab="Popularity",
        col="blue",las=2,main="Collaborators of The Weeknd")


# Create a network of artists related to the Top 100 artists
topsongs <- getPlaylistSongs("spotify", "4hOKQuZbraPDIfaGbM3lKI", token = keys)

edges <- c()
for (artist in topsongs$artist){
  related <- getRelated(artist, token = keys)
  for (relatedartist in related$name){
    edges <- append(edges, artist)
    edges <- append(edges, relatedartist)
  }
}


# Convert network to graph and save as external file
related_artists_graph <- graph(edges)
write.graph(related_artists_graph, file = "RelatedArtists.graphml", format = "graphml")


# Part d: prevalent features the weeknd
audio_features <- get_artist_audio_features("The Weeknd")
View(audio_features)
audio_features <- audio_features[!duplicated(audio_features$track_name), ]

# Plot happiness (valence) scores for each album
ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges(fill = "lightblue") +
  theme_ridges() + theme(axis.title = element_text(size = 1.5)) + 
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Happiness in The Weeknd Albums",
          subtitle = "Based on valence from Spotify's Web API")

# Plot energy scores for each album
ggplot(audio_features, aes(x = energy, y = album_name)) +
  geom_density_ridges(fill = "lightgreen") +
  theme_ridges() + theme(axis.title = element_text(size = 1.5)) + 
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Energy in The Weeknd Albums")

# Plot danceability scores for each album
ggplot(audio_features, aes(x = danceability, y = album_name)) +
  geom_density_ridges(fill = "red") +
  theme_ridges() + theme(axis.title = element_text(size = 1.5)) + 
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Danceability in The Weeknd Albums")

# Plot speechiness scores for each album
ggplot(audio_features, aes(x = speechiness, y = album_name)) +
  geom_density_ridges(fill = "orange") +
  theme_ridges() + theme(axis.title = element_text(size = 1.5)) + 
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Speechiness in The Weeknd Albums")

#########################################################################
###########################  QUESTION NO 2.2  ###########################
#########################################################################

api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
client_id <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
client_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

yt_oauth(app_id = client_id, app_secret = client_secret, token = '')

video_search <- yt_search("The Weeknd")
View(video_search)

#looping through the first 10 videos to get the stats
the_weeknd_first_10_videos_search <- data.frame()

for (i in 1:10)
{
  the_weeknd_first_10_videos_search <- rbind(the_weeknd_first_10_videos_search,
                                             get_stats(video_id = video_search$video_id[i]))
}

View(the_weeknd_first_10_videos_search)

# video with the most like count
the_weeknd_first_10_videos_search[which.max(the_weeknd_first_10_videos_search$likeCount),]

# video with the most view count
the_weeknd_first_10_videos_search[which.max(the_weeknd_first_10_videos_search$viewCount),]

View(the_weeknd_first_10_videos_search)

# analyzing correlation between views and likes
the_weeknd_first_10_videos_search <- 
  transform(the_weeknd_first_10_videos_search, likeCount = as.numeric(likeCount), 
                                               viewCount = as.numeric(viewCount))

view_likes_df <- the_weeknd_first_10_videos_search[order(the_weeknd_first_10_videos_search$viewCount), ] 

#View(view_likes_df) #for debugging

ggplot(view_likes_df, aes(x=viewCount, y=likeCount)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)

#########################################################################
###########################  QUESTION NO 2.3  ###########################
#########################################################################

# Setting up my twitteer creds
my_app_name = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_api_key = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_api_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_access_token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_access_token_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"


# Authenticate to Twitter and collect data
the_weeknd_twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "The Weeknd",
          searchType = "recent",
          numTweets = 5000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress

View(the_weeknd_twitter_data$tweets)

# Clean the tweet text
clean_text <- the_weeknd_twitter_data$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()

# Convert clean_text vector into a document corpus (collection of documents)
text_corpus <- VCorpus(VectorSource(clean_text))

# Perform further pre-processing 
text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

#text_corpus[[1]]$content  #to view the preprocessed tweet text


# Transform corpus into a Document Term Matrix
the_weeknd_doc_term_matrix <- DocumentTermMatrix(text_corpus)

# Sort words by total frequency across all documents
the_weeknd_dtm_df <- as.data.frame(as.matrix(the_weeknd_doc_term_matrix))
View(the_weeknd_dtm_df)

freq <- sort(colSums(the_weeknd_dtm_df), decreasing = TRUE)
head(freq, n = 10)

# Plot word frequency
word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

# making bar chart of words having frequency greater than 200
ggplot(subset(word_frequ_df, freq > 200), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

# making wordcloud of 1000 words having frequency greater than 1
wordcloud(words=word_frequ_df$word, freq=word_frequ_df$freq, min.freq=1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#plotting top 10 frequency words

top_10_words <- top_n(word_frequ_df,10)

barplot(top_10_words$freq,names.arg=top_10_words$word,ylab="Frequency",
        col="purple",las=2,main="Top 10 words associated to The Weeknd")


#########################################################################
###########################  QUESTION NO 2.4  ###########################
#########################################################################

# Create twomode (bimodal) network
the_weeknd_twomode_network <- the_weeknd_twitter_data %>% Create("twomode", 
                                           removeTermsOrHashtags = c("#TheWeeknd"))
the_weeknd_twomode_graph <- the_weeknd_twomode_network %>% Graph()


# Write graph to file
write.graph(the_weeknd_twomode_graph, file = "TheWeekndTwitterTwomode.graphml", 
            format = "graphml")


# Find all maximum components that are weakly connected
the_weeknd_twomode_comps <- components(the_weeknd_twomode_graph, mode = c("weak"))

the_weeknd_twomode_comps$no   #how many components (island) our network has
the_weeknd_twomode_comps$csize #size of each island
#head(the_weeknd_twomode_comps$membership, n = 30) #top 30 nodes and which component they belong to


# Get sub-graph with most members
largest_comp <- which.max(the_weeknd_twomode_comps$csize) #selecting the component with most nodes
#separating the largest component from the main graph
twomode_subgraph <- the_weeknd_twomode_graph %>% 
  induced_subgraph(vids = which(the_weeknd_twomode_comps$membership == largest_comp))


# Display top 10 nodes from the sub-graph ordered by degree centrality with total mode
the_weeknd_top10_nodes_degree_centrality <- data.frame(sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:10])
colnames(the_weeknd_top10_nodes_degree_centrality) <- c('Degree Centrality')
View(the_weeknd_top10_nodes_degree_centrality)
barplot(the_weeknd_top10_nodes_degree_centrality$`Degree Centrality`,
        names.arg=row.names(the_weeknd_top10_nodes_degree_centrality),ylab="degree centrality",
        col="purple",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (The Weeknd)")


# Display top 10 nodes from the sub-graph ordered by closeness centrality with total mode
the_weeknd_top10_nodes_closeness_centrality <- data.frame(sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:10])
colnames(the_weeknd_top10_nodes_closeness_centrality) <- c('Closeness Centrality')
View(the_weeknd_top10_nodes_closeness_centrality)
barplot(the_weeknd_top10_nodes_closeness_centrality$`Closeness Centrality`,
        names.arg=row.names(the_weeknd_top10_nodes_closeness_centrality),ylab="Closeness Centrality",
        col="green",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (The Weeknd)")

# Display top 10 nodes from the sub-graph ordered by betweenness
the_weeknd_top10_nodes_betweenness <- data.frame(sort(betweenness(twomode_subgraph, directed = FALSE), 
                                                      decreasing = TRUE)[1:10])
colnames(the_weeknd_top10_nodes_betweenness) <- c('Betweenness Centrality')
View(the_weeknd_top10_nodes_betweenness)
barplot(the_weeknd_top10_nodes_betweenness$`Betweenness Centrality`,
        names.arg=row.names(the_weeknd_top10_nodes_betweenness),ylab="Betweenness Centrality",
        col="orange",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (The Weeknd)")


## 2.4: related artist: Ariana Grande (Close friend and Collaborator of the Weeknd)
ariana_grande_twitter_data <- Authenticate("twitter",
                                        appName = my_app_name,
                                        apiKey = my_api_key,
                                        apiSecret = my_api_secret,
                                        accessToken = my_access_token,
                                        accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "Ariana Grande",
          searchType = "recent",
          numTweets = 5000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress

View(ariana_grande_twitter_data)

# Create twomode (bimodal) network
ariana_grande_twomode_network <- ariana_grande_twitter_data %>% Create("twomode")
ariana_grande_twomode_graph <- ariana_grande_twomode_network %>% Graph()


# Write graph to file
write.graph(ariana_grande_twomode_graph, file = "ArianaGrandeTwitterTwomode.graphml", 
            format = "graphml")


# Find all maximum components that are weakly connected
ariana_grande_twomode_comps <- components(ariana_grande_twomode_graph, mode = c("weak"))

ariana_grande_twomode_comps$no   #how many components (island) our network has
ariana_grande_twomode_comps$csize #size of each island
#head(the_weeknd_twomode_comps$membership, n = 30) #top 30 nodes and which component they belong to


# Get sub-graph with most members
ariana_grande_largest_comp <- which.max(ariana_grande_twomode_comps$csize) #selecting the component with most nodes
#separating the largest component from the main graph
ariana_grande_twomode_subgraph <- ariana_grande_twomode_graph %>% 
  induced_subgraph(vids = which(ariana_grande_twomode_comps$membership == ariana_grande_largest_comp))


# Display top 10 nodes from the sub-graph ordered by degree centrality with total mode
ariana_grande_top10_nodes_degree_centrality <- data.frame(sort(
  degree(ariana_grande_twomode_subgraph, mode = "total"), decreasing = TRUE)[1:10])
colnames(ariana_grande_top10_nodes_degree_centrality) <- c('Degree Centrality')
View(ariana_grande_top10_nodes_degree_centrality)
barplot(ariana_grande_top10_nodes_degree_centrality$`Degree Centrality`,
        names.arg=row.names(ariana_grande_top10_nodes_degree_centrality),ylab="degree centrality",
        col="purple",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (Ariana Grande)")


# Display top 10 nodes from the sub-graph ordered by closeness centrality with total mode
ariana_grande_top10_nodes_closeness_centrality <- data.frame(sort(closeness(ariana_grande_twomode_subgraph, mode = "total"), decreasing = TRUE)[1:10])
colnames(ariana_grande_top10_nodes_closeness_centrality) <- c('Closeness Centrality')
View(ariana_grande_top10_nodes_closeness_centrality)
barplot(ariana_grande_top10_nodes_closeness_centrality$`Closeness Centrality`,
        names.arg=row.names(ariana_grande_top10_nodes_closeness_centrality),ylab="Closeness Centrality",
        col="green",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (Ariana Grande)")

# Display top 10 nodes from the sub-graph ordered by betweenness
ariana_grande_top10_nodes_betweenness <- data.frame(sort(betweenness(ariana_grande_twomode_subgraph, directed = FALSE), 
                                                      decreasing = TRUE)[1:10])
colnames(ariana_grande_top10_nodes_betweenness) <- c('Betweenness Centrality')
View(ariana_grande_top10_nodes_betweenness)
barplot(ariana_grande_top10_nodes_betweenness$`Betweenness Centrality`,
        names.arg=row.names(ariana_grande_top10_nodes_betweenness),ylab="Betweenness Centrality",
        col="orange",cex.names=0.5,las=2,main="Top 10 Nodes according to degree centrality (Ariana Grande)")


#########################################################################
###########################  QUESTION NO 2.5  ###########################
#########################################################################

# Search YouTube
yt_oauth(app_id = client_id, app_secret = client_secret, token = '')


the_weeknd_video_search <- yt_search("The Weeknd")
ariana_grande_video_search <- yt_search("Ariana Grande")


the_weeknd_video_ids <- as.vector(the_weeknd_video_search$video_id[1:3])
ariana_grande_video_ids <- as.vector(ariana_grande_video_search$video_id[1:3])


the_weeknd_yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = the_weeknd_video_ids,
          writeToFile = TRUE,
          maxComments = 250,
          verbose = TRUE)
ariana_grande_yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = ariana_grande_video_ids,
          writeToFile = TRUE,
          maxComments = 250,
          verbose = TRUE)


View(the_weeknd_yt_data)
View(ariana_grande_yt_data)


the_weeknd_yt_actor_network <- the_weeknd_yt_data %>% Create("actor")
the_weeknd_yt_actor_graph <- Graph(the_weeknd_yt_actor_network)
ariana_grande_yt_actor_network <- ariana_grande_yt_data %>% Create("actor")
ariana_grande_yt_actor_graph <- Graph(ariana_grande_yt_actor_network)



# Transform into an undirected graph

the_weeknd_undir_yt_actor_graph <- as.undirected(the_weeknd_yt_actor_graph, mode = "collapse")
ariana_grande_undir_yt_actor_graph <- as.undirected(ariana_grande_yt_actor_graph, mode = "collapse")

write.graph(the_weeknd_undir_yt_actor_graph, file = "2.5TheWeekndUndirectedGraph.graphml", 
            format = "graphml")
write.graph(ariana_grande_undir_yt_actor_graph, file = "2.5ArianaGrandeUndirectedGraph.graphml", 
            format = "graphml")


# Run Louvain algorithm
the_weeknd_louvain_yt_actor <- cluster_louvain(the_weeknd_undir_yt_actor_graph)
ariana_grande_louvain_yt_actor <- cluster_louvain(ariana_grande_undir_yt_actor_graph)


# See sizes of communities
sizes(the_weeknd_louvain_yt_actor)
sizes(ariana_grande_louvain_yt_actor)


# Run Girvan-Newman (edge-betweenness) algorithm
the_weeknd_eb_yt_actor <- cluster_edge_betweenness(the_weeknd_undir_yt_actor_graph)
ariana_grande_eb_yt_actor <- cluster_edge_betweenness(ariana_grande_undir_yt_actor_graph)


# See sizes of communities
sizes(the_weeknd_eb_yt_actor)
sizes(ariana_grande_eb_yt_actor)

#########################################################################
###########################  QUESTION NO 2.6  ###########################
#########################################################################

# Set up Twitter authentication variables

twitter_data_theWeeknd_2_6 <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#TheWeeknd",
          searchType = "recent",
          numTweets = 1500,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


# Clean the tweet text
clean_text <- twitter_data_theWeeknd_2_6$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Assign sentiment scores to tweets
the_weeknd_sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

the_weeknd_sentiment_df <- data.frame(text = clean_text, sentiment = the_weeknd_sentiment_scores)
View(the_weeknd_sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative
the_weeknd_sentiment_df$sentiment <- factor(the_weeknd_sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(the_weeknd_sentiment_df)


# Plot sentiment classification -> BAR CHART
ggplot(the_weeknd_sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")

#PIE CHART

The_weeknd_sents <- data.frame(table(the_weeknd_sentiment_df['sentiment']))

slices <- The_weeknd_sents$Freq
lbls <- The_weeknd_sents$sentiment
colors = c("green", "yellow","red") 
pie(slices, labels = lbls, main="The Weeknd Sentiment Proportion", col=colors)


# Assign emotion scores to tweets
the_weeknd_emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

the_weeknd_emo_scores_df <- data.frame(clean_text, the_weeknd_emo_scores)
View(the_weeknd_emo_scores_df)


# Calculate proportion of emotions across all tweets
the_weeknd_emo_sums <- the_weeknd_emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(the_weeknd_emo_scores_df) 

names(the_weeknd_emo_sums)[1] <- "Proportion" 
View(the_weeknd_emo_sums)


# Plot emotion classification
ggplot(the_weeknd_emo_sums, aes(x = reorder(rownames(the_weeknd_emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(the_weeknd_emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of The Weeknd Tweets")


#exporting df as csv to be used in the tableau
write.csv(the_weeknd_sentiment_df, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.csv", row.names=FALSE)
write.csv(the_weeknd_emo_sums, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.csv", row.names=FALSE)

#########################################################################
###########################  QUESTION NO 2.7  ###########################
#########################################################################
Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


the_weeknd_features <- get_artist_audio_features("The Weeknd")
View(the_weeknd_features)

data.frame(colnames(the_weeknd_features))

the_weeknd_features_subset <- the_weeknd_features[ , 9:20]
View(the_weeknd_features_subset)
write.csv(the_weeknd_features_subset, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.csv", row.names=FALSE)


# Get top 100 songs and their audio features
top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)
data.frame(colnames(top100_features))
top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)
top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isTheWeeknd' column (class variable) to each data frame
# to indicate which songs are by The Weeknd and which are not

top100_features_subset["isTheWeeknd"] <- 0
the_weeknd_features_subset["isTheWeeknd"] <- 1


# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top100_features_noTheWeeknd <- anti_join(top100_features_subset,
                                         the_weeknd_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_noTheWeeknd, the_weeknd_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isTheWeeknd' column into a factor
# and remove the 'track_id' column

comb_data$isTheWeeknd <- factor(comb_data$isTheWeeknd)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)
comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)
split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model
dt_model <- train(isTheWeeknd~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)
prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix
confusionMatrix(dt_model, reference = testing_set$isTheWeeknd)

#########################################################################
###########################  QUESTION NO 2.8  ###########################
#########################################################################


#NOTE: USING THE CLEAN TEXT THAT I EXTRACTED PREVIOUSLY

# Convert clean tweet vector into a document corpus (collection of documents)
text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content


# Remove stop words

text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix and remove 0 entries
the_weeknd_doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(the_weeknd_doc_term_matrix$i)
dtm = the_weeknd_doc_term_matrix[non_zero_entries,]

save(dtm, file = "doc_term_matrix.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix.RData")


# Create LDA model with 5 topics related to the weeknd
lda_model <- LDA(dtm, k = 5)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic
the_weeknd_tweet_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic
top_terms <- the_weeknd_tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



#########################################################################
###########################  QUESTION NO 2.9  ###########################
#########################################################################


# Authenticate to Twitter and collect data
# Refer to the instructions if your Twitter access has been suspended

the_weeknd_user_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#TheWeeknd",
          searchType = "recent",
          numTweets = 2000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress

# View(the_weeknd_user_data$users)


# # Write graph to file
# # Make sure to set your working directory to where you want to save the file
# # before you execute the next line

twitter_actor_network <- the_weeknd_user_data %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()

write.graph(twitter_actor_graph, file = "The_weeknd_TwitterActor.graphml", format = "graphml")


