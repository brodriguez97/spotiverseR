#'@title create_wordcloud
#'
#'
#'@description
#'\code{create_wordcloud} produces a wordcloud with the lyrics of songs in a playlist that
#'are already stored within the geniusR package.
#'
#'@details
#'This function takes in a string of a playlist, a stop_vector that can remove certain common words
#'like "yeah" and "like" (as well as any other words the user find as irrelevent), and del_file which is
#'a boolean value that asserts whether the user wants to delete the output text file that is generated within
#'the function and that stores all the lyrics that are accumulated for the purposes of generating
#'the word cloud.
#'
#'@param playlist a character string
#'@param stop_vector a boolean
#'@param del_file a boolean
#'
#'@return a wordcloud generated from rquery
#'
#'@import geniusR
#'@import dplyr
#'@import tm
#'@import SnowballC
#'@import RColorBrewer
#'@import RCurl
#'@import XML
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'create_wordcloud("Christmas Jazz", c("yeah", "like"), del_file = T)

create_wordcloud <- function(playlist, stop_vector = F, del_file = T) {
  require(geniusR)
  require(dplyr)
  require(tm)
  require(SnowballC)
  require(RColorBrewer)
  require(RCurl)
  require(XML)
  playlist_name <- gsub(" ", "", playlist)
  file_name <- paste(playlist_name, "_lyrics.txt", sep = "")
  file.create(file_name, showWarnings = TRUE)
  playlist <- filter(data, playlist_name == playlist)
  for (i in 1:nrow(playlist)) {
    row <- playlist[i, ]
    song <- row$track_name
    artist <- row$artist_name
    
    tryCatch(
      geniusr_song <- genius_lyrics(artist = artist, song = song)
      ,
      message = function(m) {
        geniusr_song <- "err"
      },
      warning = function(w) {
        geniusr_song <- "err"
      },
      error = function(e) {
        geniusr_song <- "err"
      }
    )
    if (geniusr_song == "err") next
    
    write(geniusr_song$lyric, file = file_name, append = TRUE)
  }
  script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
  source(script)
  
  if (class(stop_vector) == "character") {
    stop <- stop_vector
    txt <- readLines(file_name)
    names(txt) <- 1:length(text)
    corpus <- Corpus(VectorSource(txt))
    corpus <- tm_map(corpus, removeWords, stop)
    corpus <- content(corpus)
    writeLines(corpus, con = file_name)
  }
  
  res <- rquery.wordcloud(file_name,
                          type = "file",
                          lang = "english"
  )
  
  if (del_file) {
    file.remove(file_name)
  }
  return(res)
}