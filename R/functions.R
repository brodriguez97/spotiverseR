library(dplyr)
library(spotifyr)
library(ggplot2)
library(chron)
library(knitr)
library(readxl)
library(geniusR)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)
library(wordcloud)
library(ggridges)
library(kableExtra)
library(rlang)
library(stringr)


# run the following to set up
Sys.setenv(SPOTIFY_CLIENT_ID = "5edbea8880314a4fa53863228faea20b")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "09bab3ff0dcc4163a4d5af6c1f1e8566")
access_token <- get_spotify_access_token()
uri <- 1272777153
data <- get_user_audio_features(uri)
View(data)





# filter data by a substring and variable
filter_by_ss <- function(col = playlist_name, substring) {
  col <- enquo(col)
  new_data <- filter(data, str_detect(!!col, substring))
  return(new_data)
}
christmas_playlists <- filter_by_ss(col = playlist_name, substring = "Christmas")
save(christmas_playlists, file="christmas_playlists.rda", compress="xz")
View(christmas_playlists)





# get track feature names
feat_names <- function() {
  dfx <- feat_attributes()
  return(dfx$KEY)
}
feat_names()





# get track feature description
feat_attributes <- function() {
  return(definitions)
}
df <- feat_attributes()
definitions <- as.data.frame(df)
save(definitions, file="definitions.rda", compress="xz")

# get definition for a track feature
define_feat <- function(feat) {
  require(dplyr)
  feat <- enquo(feat)
  dfx <- feat_attributes()
  dfx <- filter(dfx, KEY == quo_name(feat))
  return(dfx$`VALUE DESCRIPTION`)
}
define_feat(mode)






# add new column with track number per playlist
counter_col <- function(df) {
  df <- df %>%
    group_by(playlist_name) %>%
    mutate(counter = row_number())
  return(df)
}
c <- counter_col(data)
View(c)





# get histogram for a single mean variable, compare across playlists
create_mean_hist <- function(data = data, param, color = "pink", border = "darkred") {
  require(ggplot2)
  require(dplyr)
  param <- enquo(param)
  plotdata <- data %>%
    group_by(playlist_name) %>%
    summarize(mean_param = mean(!!param))

  # plot mean
  ggplot(
    plotdata,
    aes(
      x = playlist_name,
      y = mean_param
    )
  ) +
    geom_bar(
      stat = "identity",
      fill = color,
      color = border
    ) +
    xlab("playlist name") +
    ylab(param) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
create_mean_hist(d, energy, color = "red", border = "darkgreen")
create_mean_hist(d, liveness, color = "red", border = "darkgreen")






# get dataframe with a variable average per playlist
create_mean_df <- function(data = data, param, asc = T, kable = F, bg ="green", text = "red") {
  require(knitr)
  require(spotifyr)
  require(dplyr)
  param <- enquo(param)
  dataset <- group_by(data, playlist_name)
  df <- aggregate(dataset[, quo_name(param)], list(dataset$playlist_name), mean)
  if (asc) {
    df <- df %>% arrange(!!param)
  } else {
    df <- df %>% arrange(desc(!!param))
  }
  avg <- paste("mean (", quo_name(param), ")", sep = "")
  colnames(df) <- c("playlist", avg)
  if (!kable) {
    return(df)
  } else {
    df <- df %>% kable() %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(row = 1:nrow(df), background = bg, color = text)
    return(df)
  }
}
create_mean_df(d, valence, asc = F, kable=T, bg = "lightgreen", text = "red")
create_mean_df(d, valence, asc = T, kable=F)






# shows the distribution of a numeric value for several playlists
create_joyplot <- function(data = data, param, color = c("blue", "green")) {
  require(ggplot2)
  require(ggridges)
  param <- enquo(param)
  ggplot(
    data,
    aes(
      x = !!param,
      y = playlist_name,
      fill = playlist_name
    )
  ) +
    geom_density_ridges() +
    scale_fill_cyclical(values = color) +
    theme_ridges() +
    ylab("playlist name") +
    theme_ridges(font_size = 13, grid = TRUE) + theme(
      legend.position = "none",
      axis.title.y = element_blank()
    )
}
create_joyplot(d, danceability, c("red","green"))




# linear regression, facet_wrap is optional
create_lr <- function(data = data, param, param2, line = "white",
                      points = "green", theme = "dark",
                      multi = T) {
  require(spotifyr)
  require(dplyr)
  require(ggplot2)
  param <- enquo(param)
  param2 <- enquo(param2)
  if (multi == F) {
    create_lr_one(data = data, param = param, param2 = param2, line = line, points = points, theme = theme)
  } else {
    if (theme == "dark") {
      t <- theme_dark()
    } else {
      t <- theme_light()
    }

    ggplot(
      data = data,
      mapping = aes(
        x = !!param,
        y = !!param2
      )
    ) +
      geom_point(
        color = points,
        alpha = .7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        size = 1.5,
        color = line
      ) +
      facet_wrap(~playlist_name) +
      t
  }
}

create_lr(
  data = d,
  param = tempo,
  param2 = valence,
  points = "darkgreen",
  line = "red",
  theme = "light",
  multi = F
)

create_lr(
  data = d,
  param = tempo,
  param2 = valence,
  points = "darkgreen",
  line = "red",
  theme = "light",
  multi = T
)






#helper function for create_lr
create_lr_one <- function(data, param, param2, line = "white",
                          points = "green", theme = "dark") {
  if (theme == "dark") {
    t <- theme_dark()
  } else {
    t <- theme_light()
  }
  g <- ggplot(
    data = data,
    mapping = aes(
      x = !!param,
      y = !!param2
    )
  ) +
    geom_point(
      color = points,
      alpha = .7
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      size = 1.5,
      color = line
    ) +
    t
  return(g)
}






# may need to run this to do next function
install.packages(c("tm", "SnowballC", "RColorBrewer", "RCurl", "XML"))

# create word cloud
create_wordcloud <- function(playlist, stop_vector = F, del_file = T) {
  require(spotifyr)
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
create_wordcloud("Christmas Hits", c("yeah", "like"), del_file = T)





# get top songs for a given variable and playlist
playlist_top_songs <- function(playlist, param, top = 5, asc = T, kable = T, bg = "azure", text = "palevioletred") {
  require(dplyr)
  require(knitr)
  require(formattable)
  require(kableExtra)
  param <- enquo(param)
  df <- filter(data, playlist_name == playlist)
  if (asc) {
    df <- arrange(df, -!!param)
  } else {
    df <- arrange(df, !!param)
  }
  df <- select(df, track_name, !!param) %>%
    head(top)
  if (kable){
    df <- df %>% kable() %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(row = 1:nrow(df), background = bg, color = text)
    return(df)
  } else {
    return(df)
  }
}
playlist_top_songs("Christmas Jazz", valence, asc = T, top = 10, bg = "lightgreen", text = "red")
