install.packages("remotes")
library(remotes)

# install GitHub version of vosonSML 0.32.10
install_github("vosonlab/vosonSML")

# install GitHub version of rtweet 1.1.0.9001
install_github("ropensci/rtweet")


#importing the libraries
library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)


#delaring the variables
my_app_name <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_api_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
my_access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"


#########################################
############ QUESTION NO 1.2 ############
#########################################

"DATA COLLECTION OF THE ARTIST: The Weeknd"

#creating a list of hashtags to iterate and collect data on
my_hashtags = list("#Weeknd","#TheWeeknd","#StarBoy","#Kissland","#AfterHours",
                   "TheWeekndXO","BlindingLights","#AbelTesfaye","#XOTWOD","#FaveTourStyle")

#creating an empty list that will be populated with the retrieved data
twitter_data_list = list()

#this for loop will run from 1 to 10 and will loop through the my_hashtags list and retrieve
# the appropriate key word's data

for (i in 1:10) {
  print(my_hashtags[i])
  twitter_data_list[i] <- Authenticate("twitter",
                               appName = my_app_name,
                               apiKey = my_api_key,
                               apiSecret = my_api_secret,
                               accessToken = my_access_token,
                               accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm = my_hashtags[[i]],
            searchType = "mixed",
            numTweets = 1500,
            lang = "en",
            includeRetweets = TRUE,
            writeToFile = TRUE,
            verbose = TRUE) # use 'verbose' to show download progress

append(twitter_data_list,twitter_data)

  print(i)
}

# Once we have seen the results and now we know that TheWeeknd has most tweets we will select
# it as our main hashtag
the_weeknd_twitter_data <- Authenticate("twitter",
                                     appName = my_app_name,
                                     apiKey = my_api_key,
                                     apiSecret = my_api_secret,
                                     accessToken = my_access_token,
                                     accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#TheWeeknd",
          searchType = "mixed",
          numTweets = 15000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress

View(the_weeknd_twitter_data$tweets)


#########################################
############ QUESTION NO 1.3 ############
#########################################

"Top 5 most influential people"

#creating actor network graph
the_Weeknd_twitter_actor_network <- the_weeknd_twitter_data %>% Create("actor")
the_Weeknd_twitter_actor_graph <- the_Weeknd_twitter_actor_network %>% Graph()
V(the_Weeknd_twitter_actor_graph)$name <- V(the_Weeknd_twitter_actor_graph)$screen_name

write.graph(the_Weeknd_twitter_actor_graph, file = "TheWeekndTwitterActor.graphml", 
            format = "graphml")

#running the ranking algo to find the top 5 most influential people
rank_twitter_actor_the_weeknd <- sort(page_rank(the_Weeknd_twitter_actor_graph)$vector,
                                      decreasing = TRUE)

#making a barplot of top 5 people and their influence
top_5_inf_people = data.frame(head(rank_twitter_actor_the_weeknd, n = 5))
colnames(top_5_inf_people) <- c('Influence')
view(top_5_inf_people)
H <- top_5_inf_people[['Influence']]
M <- rownames(top_5_inf_people)
barplot(H,cex.names=0.8,names.arg=M,,xlab="User Name",ylab="Influence",
        col="lightblue",main="Top 5 Influential People")




#########################################
############ QUESTION NO 1.4 ############
#########################################

"Finding out the top 10 important terms that appear in tweets"

twitter_semantic_the_weeknd <- the_weeknd_twitter_data %>%
  Create("semantic",termFreq = 25,hashtagFreq = 75,
         removeTermsOrHashtags = c("#TheWeeknd","weeknd","Weeknd"))

twitter_semantic_graph_the_weeknd <- twitter_semantic_the_weeknd %>% Graph()
write.graph(twitter_semantic_graph_the_weeknd, file = "TwitterSemanticTheWeeknd.graphml",
            format = "graphml")

# Run Page Rank algorithm to find important terms/hashtags
rank_twitter_semantic_the_weeknd <- sort(page_rank(twitter_semantic_graph_the_weeknd)$vector, 
                                         decreasing = TRUE)
head(rank_twitter_semantic_the_weeknd, n = 10)

#########################################
############ QUESTION NO 1.5 ############
#########################################

"Finding out the all the unique users in my dataset"

total_users_in_data_set <- length(the_weeknd_twitter_data$tweets$user_id)
unique_users_in_data_set <- length(unique(the_weeknd_twitter_data$tweets$user_id))
print(unique_users_in_data_set)

unique_user_occurences <- data.frame(table(the_weeknd_twitter_data$tweets$user_id))
View(unique_user_occurences)

#plotting the unique users
x <- c(total_users_in_data_set,unique_users_in_data_set)
label <- c("Repeated Users","Unique Users")
piepercent <- round(100*x/sum(x),1)
pie(x, labels = piepercent, main = "Unique Users Proportion",radius=1,
    col = rainbow(length(x)))
legend("topright", label, cex = 0.8,fill = rainbow(length(x)))


