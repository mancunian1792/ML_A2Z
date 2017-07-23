library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(NLP)
library(openNLP)
library(RWeka)


# Declare Twitter API Credentials
api_key <- "baQf9BnwiEkg2CgqLQElnXudB" # From dev.twitter.com
api_secret <- "Oszev27NDQBklKMShyfIyUjP81oevjM5gIdbgq6lumFjkUg608" # From dev.twitter.com
token <- "155447641-4cmJ9TgNOE1Zh9hGMDzDIuCidnhYnl8yuRtHfcK8" # From dev.twitter.com
token_secret <- "3TjGyKU8hstmHHOltIDBVJqhhxmi1pZfYTWNxMJPYAZDp" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).

tweets <- searchTwitter("#OviyaArmy OR Oviya OR BigBoss OR Julie OR Kamal ", n=1000, lang="en", resultType = "popular", since="2017-06-01")

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)

# convert into single text
tweetText <- as.String(paste0(tweets.df$text, collapse=" "))

#Create annotators that mark the word/sentence beginning and end 
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

#Apply annotators on our text
tweet_annotations <- annotate(tweetText, list(sent_ann, word_ann))

#Create a document of words
tweet_doc <- AnnotatedPlainTextDocument(tweetText, tweet_annotations)
sents(tweet_doc) %>% head(2)
words(tweet_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
tweet_annotations_pipeline <- annotate(tweetText, pipeline)
tweet_doc <- AnnotatedPlainTextDocument(tweetText, tweet_annotations_pipeline)


# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}
entities(tweet_doc, kind = "person")
entities(tweet_doc, kind="organization")
entities(tweet_doc, kind="location")