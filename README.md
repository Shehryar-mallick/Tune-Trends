# Tune-Trends
## Project objective
The objective of the project was to select an artist/band and perform social media analytics in-order to increase the popularity of the said artist/band. The requirement was to use the software tools, techniques and algorithms for analysis, predictions and produce written reports for the finding.

## Project stages
Data Collection
Data Preprocessing
Analytics
ML: Sentiment Analysis, Decision Tree, LDA

## Tech Stack
R Studio, Gephi, Tableau

## Description
The artist selected for the purpose of the project was “The Weeknd”

### Data Collection:
The data was collected from three different sources which included: Twitter, Spotify, and Youtube using the developer keys/account provided by the respective platforms.

### Data Preprocessing:
The text pre-processing steps include
* Removal of url
* Removal of hashtags
* Removal of tags
* Removal of emojis
* Removal of emoticons
* Converting text into lowercase
* Removing numbers
* Removing punctuations
* Removal of stopwords using (SMART strategy)
* Stemming of the words in text corpus

### Analytics and Findings:

The description of the analytics and findings can be found on the report using the links: 
https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Analytics-Report-1.pdf
https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Analytics-Report-2.pdf 

Moreover the assets folder (https://github.com/Shehryar-mallick/Tune-Trends/tree/main/Assets) contains all of the analytics images as well.

The techniques involved to find out the most used words include: page rank and term frequency matrix.
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/2.3.png)
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/word_TheWeekndSemanticGraph.png)


The techniques to find out the top influential nodes include: page rank, degree centrality, betweenness centrality and closeness centrality.
The algorithms involved in the analytics for the detection of communities include Girvan-Newman algorithm and Louvain Algorithm.
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/User_TheWeekndNwGraph.png)

Apart from that sentiment analysis was performed to find out the emotions of the general public. And a decision tree was trained using the spotify data with the unique features of songs associated with the artist. Finally LDA topic modeling was conducted to categorize the words into respective groups on the basis of their association.
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/2.6-1.png)
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/2.6-3.png)
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/2.6-2.png)
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/2.8_top_ten_LDA.png)

Furthermore, an analytics dashboard was also created on Tableau for the purpose of gaining additional insights regarding the data set.
![alt text](https://github.com/Shehryar-mallick/Tune-Trends/blob/main/Assets/Dashboard.png)

### Note: Further analytics can be found in the reports.
