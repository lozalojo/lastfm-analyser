
get.info<-function(i.artist=NA, i.album=NA, i.track=NA, i.method="track.getInfo", i.api_key){
  infotxt<-"http://ws.audioscrobbler.com/2.0/" %>%
    param_set("method", i.method) %>%
    param_set("api_key", i.api_key) %>%
    param_set("format", "json")
  if (!is.na(i.artist)) infotxt<-infotxt %>% param_set("artist", URLencode(i.artist, reserved = TRUE))
  if (!is.na(i.album)) infotxt<-infotxt %>% param_set("album", URLencode(i.album, reserved = TRUE))
  if (!is.na(i.track)) infotxt<-infotxt %>% param_set("track", URLencode(i.track, reserved = TRUE))
  # infotxt <- substr(infotxt, 1, 999)
  info <- try(fromJSON(infotxt))
  if(class(info) != "try-error") return(info)
}

# get.info<-function(i.artist=NA, i.album=NA, i.track=NA, i.method="track.getInfo", i.api_key){
#   infotxt<-"http://ws.audioscrobbler.com/2.0/" %>%
#     param_set("method", i.method) %>%
#     param_set("api_key", i.api_key) %>%
#     param_set("format", "json")
#   if (!is.na(i.artist)) infotxt<-infotxt %>% param_set("artist", URLencode(i.artist, reserved = TRUE))
#   if (!is.na(i.album)) infotxt<-infotxt %>% param_set("album", URLencode(i.album, reserved = TRUE))
#   if (!is.na(i.track)) infotxt<-infotxt %>% param_set("track", URLencode(i.track, reserved = TRUE))
#   info <- content(GET(infotxt, as="parsed"))
#   info
# }

# get.track.info<-function(i.track, i.artist, i.album, i.api_key){
#   if (length(i.track)==0) stop("incorrect number of dimensions")
#   dataunique <- data.frame(track=as.character(i.track), artist=as.character(i.artist), album=as.character(i.album), stringsAsFactors = F) %>%
#     distinct() %>%
#     arrange(artist, album, track)
#   cat(NROW(dataunique), " tracks to fetch\n")
#   res<-mapply(memoise(function(i.track, i.artist, i.album) {
#     cat("Fetching ", i.track, " from ", i.artist,"/", i.album, "\n")
#     info<-get.info(i.artist=i.artist, i.track=i.track, i.album=i.album, i.method="track.getInfo", i.api_key=i.api_key)
#     data.frame(track=ifelse(is.null(info$track$name),"", as.character(info$track$name)), 
#                mbid=ifelse(is.null(info$track$mbid),"",as.character(info$track$mbid)),
#                duration=ifelse(is.null(info$track$duration),"",as.character(info$track$duration)), 
#                artist=ifelse(is.null(info$track$artist$name),"",as.character(info$track$artist$name)), 
#                album=ifelse(is.null(info$track$album$title),"",as.character(info$track$album$title)), 
#                stringsAsFactors = F)
#   }), i.track = dataunique$track, i.artist = dataunique$artist, i.album = dataunique$album)
#   res=data.frame(matrix(unlist(res), ncol=5, byrow=T), stringsAsFactors = F)
#   names(res)<-c("ttrack","tmbid","tduration","tartist","talbum")
#   dataunique %>% 
#     bind_cols(res)
# }

get.track.info<-function(i.track, i.artist, i.album, i.api_key, i.slice=200, i.restart=T, i.temp=""){
  if (!file.exists("temp")) dir.create("temp")
  if (i.restart) do.call(file.remove, list(list.files("temp", pattern=paste0("^",i.temp,"-",i.slice,"-track_([0-9])+\\.rds$"), full.names = TRUE)))
  dataoriginal<- data.frame(track=as.character(i.track), artist=as.character(i.artist), album=as.character(i.album), stringsAsFactors = F) %>%
    distinct() %>%
    arrange(artist, album, track) %>%
    mutate(no=1:n(), slice=as.factor(1+floor((1:n()-1)/i.slice)))
  dataoriginal.split<-split(dataoriginal, dataoriginal$slice)
  cat(NROW(dataoriginal), " tracks to fetch\n")
  for (j in 1:length(dataoriginal.split)){
    cat("Processing part ", j, " of ", length(dataoriginal.split),"\n")
    temp.rds<-paste0("temp/",i.temp,"-",i.slice,"-track_",j,".rds")
    if (file.exists(temp.rds)){
      cat("Junk already processed\n")
    }else{
      dataunique<-dataoriginal.split[[j]]

  iteration<-1
  result<-data.frame(no=1, track.title="", stringsAsFactors = F) %>%
    filter(track.title!="")
  while(NROW(result)<NROW(dataunique) & iteration<6){
    dataunique.temp<- dataunique %>%
      left_join(result, by="no") %>%
      filter(track.title=="" | is.na(track.title))
    cat("Iteration ",iteration, "\t", NROW(dataunique.temp), " tracks to fetch\n")
    cl <- makeCluster(4)
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = NROW(dataunique.temp), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    result.it<-foreach(i = 1:NROW(dataunique.temp), .combine = bind_rows, .options.snow = opts,
                       .export = c("get.info", "%>%", "param_set", "fromJSON", "try")) %dopar% {
                         i.track<-dataunique.temp$track[i]
                         i.artist<-dataunique.temp$artist[i]
                         i.album<-dataunique.temp$album[i]
                         i.number<-dataunique.temp$no[i]
                         cat("Fetching ", i.track, " from ", i.artist,"/", i.album, "\n")
                         info<-get.info(i.artist=i.artist, i.track=i.track, i.album=i.album, i.method="track.getInfo", i.api_key=i.api_key)
                         data.frame(no=i.number,
                                    track.title=ifelse(is.null(info$track$name),"", as.character(info$track$name)), 
                                    track.mbid=ifelse(is.null(info$track$mbid),"",as.character(info$track$mbid)),
                                    track.duration=ifelse(is.null(info$track$duration),"",as.character(info$track$duration)), 
                                    track.artist=ifelse(is.null(info$track$artist$name),"",as.character(info$track$artist$name)), 
                                    track.album=ifelse(is.null(info$track$album$title),"",as.character(info$track$album$title)), 
                                    stringsAsFactors = F)
                       }
    close(pb)
    stopCluster(cl)
    result <- result %>%
      bind_rows(result.it) %>%
      filter(track.title!="" & !is.na(track.title))
    iteration<-iteration+1
  }
  saveRDS(result, temp.rds)
  Sys.sleep(5)
    }
  }
  
  result<-data.frame()
  for(j in 1:length(dataoriginal.split)) result <- result %>%
    bind_rows(readRDS(paste0("temp/",i.temp,"-",i.slice,"-track_",j,".rds")))
  
  dataoriginal %>% 
    left_join(result, by="no") %>%
    select(-no)
    
 
}

# get.artist.info<-function(i.artist){
#   dataunique<- data.frame(artist=as.character(i.artist), stringsAsFactors = F) %>%
#     distinct() %>%
#     filter(artist!="") %>%
#     arrange(artist)
#   cat(NROW(dataunique), " artists to fetch\n")
#   res<-mapply(memoise(function(i.artist) {
#     cat("Fetching ", i.artist, "\n")
#     info<-get.info(i.artist=i.artist, i.method="artist.getTopTags")
#     data.frame(genre1=ifelse(NROW(info$toptags$tag) < 1,"", as.character(info$toptags$tag[1,"name"])),
#                count1=ifelse(NROW(info$toptags$tag) < 1,"", as.character(info$toptags$tag[1,"count"])),
#                genre2=ifelse(NROW(info$toptags$tag) < 2,"", as.character(info$toptags$tag[2,"name"])),
#                count2=ifelse(NROW(info$toptags$tag) < 2,"", as.character(info$toptags$tag[2,"count"])),
#                genre3=ifelse(NROW(info$toptags$tag) < 3,"", as.character(info$toptags$tag[3,"name"])),
#                count3=ifelse(NROW(info$toptags$tag) < 3,"", as.character(info$toptags$tag[3,"count"])),
#                stringsAsFactors = F)
#   }), i.artist = dataunique$artist)
#   res=data.frame(matrix(unlist(res), ncol=6, byrow=T), stringsAsFactors = F)
#   names(res)<-c("artist.genre1","artist.count1","artist.genre2","artist.count2","artist.genre3","artist.count3")
#   dataunique %>% 
#     bind_cols(res)
# }

get.artist.info<-function(i.artist, i.api_key, i.slice=200, i.restart=T, i.temp=""){
  if (!file.exists("temp")) dir.create("temp")
  if (i.restart) do.call(file.remove, list(list.files("temp", pattern=paste0("^",i.temp,"-",i.slice,"-artist_([0-9])+\\.rds$"), full.names = TRUE)))
  dataoriginal<- data.frame(artist=as.character(i.artist), stringsAsFactors = F) %>%
    distinct() %>%
    filter(artist!="") %>%
    arrange(artist) %>%
    mutate(no=1:n(), slice=as.factor(1+floor((1:n()-1)/i.slice)))
  dataoriginal.split<-split(dataoriginal, dataoriginal$slice)
  cat(NROW(dataoriginal), " artists to fetch\n")
  for (j in 1:length(dataoriginal.split)){
    cat("Processing part ", j, " of ", length(dataoriginal.split),"\n")
    temp.rds<-paste0("temp/",i.temp,"-",i.slice,"-artist_",j,".rds")
    if (file.exists(temp.rds)){
      cat("Junk already processed\n")
    }else{
      dataunique<-dataoriginal.split[[j]]
      
  iteration<-1
  result<-data.frame(no=1, artist.genre1="", stringsAsFactors = F) %>%
    filter(artist.genre1!="")
  while(NROW(result)<NROW(dataunique) & iteration<6){
    dataunique.temp<- dataunique %>%
      left_join(result, by="no") %>%
      filter(artist.genre1=="" | is.na(artist.genre1))
    cat("Iteration ",iteration, "\t", NROW(dataunique.temp), " artists to fetch\n")
    cl <- makeCluster(4)
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = NROW(dataunique.temp), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    result.it<-foreach(i = 1:NROW(dataunique.temp), .combine = bind_rows, .options.snow = opts,
                       .export = c("get.info", "%>%", "param_set", "fromJSON", "try")) %dopar% {
                         i.artist<-dataunique.temp$artist[i]
                         i.number<-dataunique.temp$no[i]
                         cat("Fetching ", i.artist, "\n")
                         info<-get.info(i.artist=i.artist, i.method="artist.getTopTags", i.api_key=i.api_key)
                         data.frame(no=i.number,
                                    artist.genre1=ifelse(NROW(info$toptags$tag) < 1,"", as.character(info$toptags$tag[1,"name"])),
                                    artist.count1=ifelse(NROW(info$toptags$tag) < 1,"", as.character(info$toptags$tag[1,"count"])),
                                    artist.genre2=ifelse(NROW(info$toptags$tag) < 2,"", as.character(info$toptags$tag[2,"name"])),
                                    artist.count2=ifelse(NROW(info$toptags$tag) < 2,"", as.character(info$toptags$tag[2,"count"])),
                                    artist.genre3=ifelse(NROW(info$toptags$tag) < 3,"", as.character(info$toptags$tag[3,"name"])),
                                    artist.count3=ifelse(NROW(info$toptags$tag) < 3,"", as.character(info$toptags$tag[3,"count"])),
                                    stringsAsFactors = F)
                       }
    close(pb)
    stopCluster(cl)
    result <- result %>%
      bind_rows(result.it) %>%
      filter(artist.genre1!="" & !is.na(artist.genre1))
    iteration<-iteration+1
  }
  saveRDS(result, temp.rds)
  Sys.sleep(5)
    }
  }
  
  result<-data.frame()
  for(j in 1:length(dataoriginal.split)) result <- result %>%
    bind_rows(readRDS(paste0("temp/",i.temp,"-",i.slice,"-artist_",j,".rds")))
  
  dataoriginal %>% 
    left_join(result, by="no") %>%
    select(-no)
    

}


# get.album.info<-function(i.artist, i.album, i.api_key){
#   dataunique<- data.frame(artist=as.character(i.artist), album=as.character(i.album), stringsAsFactors = F) %>%
#     distinct() %>%
#     filter(artist!="" & album!="") %>%
#     arrange(artist, album)
#   cat(NROW(dataunique), " albums to fetch\n")
#   res<-mapply(memoise(function(i.artist, i.album) {
#     cat("Fetching ", i.album, "/", i.artist, "\n")
#     info1<-get.info(i.artist=i.artist, i.album=i.album, i.method="album.getInfo", i.api_key=i.api_key)
#     info2<-get.info(i.artist=i.artist, i.album=i.album, i.method="album.getTopTags", i.api_key=i.api_key)
#     data.frame(album=ifelse(is.null(info1$album$name),"", as.character(info1$album$name)), 
#                artist=ifelse(is.null(info1$album$artist),"",as.character(info1$album$artist)),
#                published=ifelse(is.null(info1$album$wiki$published),"",as.character(info1$album$wiki$published)),
#                genre1=ifelse(NROW(info2$toptags$tag) < 1,"", as.character(info2$toptags$tag[1,"name"])),
#                count1=ifelse(NROW(info2$toptags$tag) < 1,"", as.character(info2$toptags$tag[1,"count"])),
#                genre2=ifelse(NROW(info2$toptags$tag) < 2,"", as.character(info2$toptags$tag[2,"name"])),
#                count2=ifelse(NROW(info2$toptags$tag) < 2,"", as.character(info2$toptags$tag[2,"count"])),
#                genre3=ifelse(NROW(info2$toptags$tag) < 3,"", as.character(info2$toptags$tag[3,"name"])),
#                count3=ifelse(NROW(info2$toptags$tag) < 3,"", as.character(info2$toptags$tag[3,"count"])),
#                stringsAsFactors = F)
#   }), i.artist = dataunique$artist, i.album = dataunique$album)
#   res=data.frame(matrix(unlist(res), ncol=9, byrow=T), stringsAsFactors = F)
#   names(res)<-c("lalbum","lartist","lpublished","album.genre1","album.count1","album.genre2","album.count2","album.genre3","album.count3")
#   dataunique %>% 
#     bind_cols(res)
# }

get.album.info<-function(i.artist, i.album, i.api_key, i.slice=200, i.restart=T, i.temp=""){
  if (!file.exists("temp")) dir.create("temp")
  if (i.restart) do.call(file.remove, list(list.files("temp", pattern=paste0("^",i.temp,"-",i.slice,"-album_([0-9])+\\.rds$"), full.names = TRUE)))
  dataoriginal<- data.frame(artist=as.character(i.artist), album=as.character(i.album), stringsAsFactors = F) %>%
    distinct() %>%
    filter(artist!="" & album!="") %>%
    arrange(artist, album) %>%
    mutate(no=1:n(), slice=as.factor(1+floor((1:n()-1)/i.slice)))
  dataoriginal.split<-split(dataoriginal, dataoriginal$slice)
  cat(NROW(dataoriginal), " albums to fetch\n")
  for (j in 1:length(dataoriginal.split)){
    cat("Processing part ", j, " of ", length(dataoriginal.split),"\n")
    temp.rds<-paste0("temp/",i.temp,"-",i.slice,"-album_",j,".rds")
    if (file.exists(temp.rds)){
      cat("Junk already processed\n")
    }else{
      dataunique<-dataoriginal.split[[j]]
      iteration<-1
      result<-data.frame(no=1, album.title="", stringsAsFactors = F) %>%
        filter(album.title!="")
      while(NROW(result)<NROW(dataunique) & iteration<6){
        dataunique.temp<- dataunique %>%
          left_join(result, by="no") %>%
          filter(album.title=="" | is.na(album.title))
        cat("Iteration ",iteration, "\t", NROW(dataunique.temp), " albums to fetch\n")
        cl <- makeCluster(4)
        registerDoSNOW(cl)
        pb <- txtProgressBar(max = NROW(dataunique.temp), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress = progress)
        result.it<-foreach(i = 1:NROW(dataunique.temp), .combine = bind_rows, .options.snow = opts,
                           .export = c("get.info", "%>%", "param_set", "fromJSON", "try")) %dopar% {
                             i.artist<-dataunique.temp$artist[i]
                             i.album<-dataunique.temp$album[i]
                             i.number<-dataunique.temp$no[i]
                             cat("Fetching ", i.album, "/", i.artist, "\n")
                             info1<-get.info(i.artist=i.artist, i.album=i.album, i.method="album.getInfo", i.api_key=i.api_key)
                             info2<-get.info(i.artist=i.artist, i.album=i.album, i.method="album.getTopTags", i.api_key=i.api_key)
                             data.frame(no=i.number,
                                        album.title=ifelse(is.null(info1$album$name),"", as.character(info1$album$name)), 
                                        album.artist=ifelse(is.null(info1$album$artist),"",as.character(info1$album$artist)),
                                        album.published=ifelse(is.null(info1$album$wiki$published),"",as.character(info1$album$wiki$published)),
                                        album.genre1=ifelse(NROW(info2$toptags$tag) < 1,"", as.character(info2$toptags$tag[1,"name"])),
                                        album.count1=ifelse(NROW(info2$toptags$tag) < 1,"", as.character(info2$toptags$tag[1,"count"])),
                                        album.genre2=ifelse(NROW(info2$toptags$tag) < 2,"", as.character(info2$toptags$tag[2,"name"])),
                                        album.count2=ifelse(NROW(info2$toptags$tag) < 2,"", as.character(info2$toptags$tag[2,"count"])),
                                        album.genre3=ifelse(NROW(info2$toptags$tag) < 3,"", as.character(info2$toptags$tag[3,"name"])),
                                        album.count3=ifelse(NROW(info2$toptags$tag) < 3,"", as.character(info2$toptags$tag[3,"count"])),
                                        stringsAsFactors = F)
                           }
        close(pb)
        stopCluster(cl)
        result <- result %>%
          bind_rows(result.it) %>%
          filter(album.title!="" & !is.na(album.title))
        iteration<-iteration+1
      }
      saveRDS(result, temp.rds)
      Sys.sleep(5)
    }
  }
  
  result<-data.frame()
    for(j in 1:length(dataoriginal.split)) result <- result %>%
              bind_rows(readRDS(paste0("temp/",i.temp,"-",i.slice,"-album_",j,".rds")))
              
  dataoriginal %>% 
    left_join(result, by="no") %>%
    select(-no)
}

coalesceplus<-function (...) 
{
  if (missing(..1)) {
    abort("At least one argument must be supplied")
  }
  values <- rlang::dots_list(...)
  x <- values[[1]]
  values <- values[-1]
  for (i in seq_along(values)) {
    x <- dplyr:::replace_with(x, is.na(x) | x=="", values[[i]], glue("Argument {i + 1}"), 
                              glue("length of {fmt_args(~x)}"))
  }
  x
}

full_seq_yearmon <- function(x) {
  as.yearmon(full_seq(as.numeric(x), 1/12, 0.1))
}

lastfm_export_multi<-function(i.user){
  out.export<-lastfm_export(i.user[1]) %>%
    select(-time) %>%
    mutate(user=i.user[1])
  if (length(i.user)>1) for (i in 2:length(i.user)) out.export <- out.export %>%
      rbind(lastfm_export(i.user[i]) %>%
                  select(-time) %>%
                  mutate(user=i.user[i])) %>%
      distinct()
  out.export
}

import.lastfm.user <- function(i.user, i.restart=F){
  cat("import.profile> start\n")  
  cat("import.profile> importing user(s): ", paste(i.user, collapse = "+"), "\n")
  o.filename<-paste0("processed/",paste(i.user, collapse = "+"),"-lastfm.rds")
  if (i.restart & file.exists(o.filename)){
    cat("import.profile> removing existing files\n")
    file.remove(o.filename)
  } 
  if (file.exists(o.filename)){
    cat("import.profile> found existing file: ",o.filename,"\n")
    o.data<-readRDS(o.filename)
  }else{
    cat("import.profile> fetching scrobbling list from lastfm\n")
    o.data <- lastfm_export_multi(i.user) %>%
      mutate(artist=as.character(artist),
             track=as.character(track),
             album=as.character(album),
             weekday=as.character(weekday)) %>%
      filter(track!="")
    cat("import.profile> saving data\n")
    saveRDS(o.data, o.filename)
  }
  cat("import.profile> end\n")
  o.data
}

import.lastfm.data <- function(i.data, i.user, i.restart=F, i.api_key, i.slice=500){
  cat("import.data> start\n")
  o.filename<-paste0("processed/",paste(i.user, collapse = "+"),"-lastfm.info.rds")
  o.filename.tracks<-paste0("processed/",paste(i.user, collapse = "+"),"-tracks.info.rds")
  o.filename.albums<-paste0("processed/",paste(i.user, collapse = "+"),"-albums.info.rds")
  o.filename.artists<-paste0("processed/",paste(i.user, collapse = "+"),"-artists.info.rds")
  if (i.restart){
    cat("import.data> removing existing files\n")
    if (file.exists(o.filename)) file.remove(o.filename)
    if (file.exists(o.filename.tracks)) file.remove(o.filename.tracks)
    if (file.exists(o.filename.albums)) file.remove(o.filename.albums)
    if (file.exists(o.filename.artists)) file.remove(o.filename.artists)
  }
  # Tracks information
  cat("import.data> tracks information\n")  
  if (file.exists(o.filename.tracks)){
    cat("import.data> found existing file: ",o.filename.tracks,"\n")    
    o.data.tracks<-readRDS(o.filename.tracks)
  }else{
    cat("import.data> fetching tracks data from lastfm\n")    
    o.data.tracks<-get.track.info(i.data$track, i.data$artist, i.data$album, i.api_key=i.api_key, i.slice=i.slice, i.restart=i.restart, i.temp=paste(i.user, collapse = "+"))
    saveRDS(o.data.tracks, o.filename.tracks)
  }
  # Albums information
  cat("import.data> albums information\n")  
  if (file.exists(o.filename.albums)){
    cat("import.data> found existing file: ",o.filename.albums,"\n")    
    o.data.albums<-readRDS(o.filename.albums)
  }else{
    cat("import.data> fetching albums data from lastfm\n")    
    o.data.albums<-get.album.info(i.data$artist, i.data$album, i.api_key=i.api_key, i.slice=i.slice, i.restart=i.restart, i.temp=paste(i.user, collapse = "+"))
    saveRDS(o.data.albums, o.filename.albums)
  }
  # Artists information
  cat("import.data> artists information\n")  
  if (file.exists(o.filename.artists)){
    cat("import.data> found existing file: ",o.filename.artists,"\n")    
    o.data.artists<-readRDS(o.filename.artists)
  }else{
    cat("import.data> fetching artists data from lastfm\n")    
    o.data.artists<-get.artist.info(i.data$artist, i.api_key=i.api_key, i.slice=i.slice, i.restart=i.restart, i.temp=paste(i.user, collapse = "+"))
    saveRDS(o.data.artists, o.filename.artists)
  }
  cat("import.data> creating output file\n")    
  o.data <- i.data %>%
    left_join(o.data.tracks %>% 
                select(-slice), by=c("track", "artist", "album")) %>%
    left_join(o.data.artists %>% 
                select(-slice), by=c("artist")) %>%
    left_join(o.data.albums %>% 
                select(-slice) %>%
                mutate(
                  album.genre1=if_else(tolower(album.genre1)=="spanish","",album.genre1),
                  album.genre2=if_else(tolower(album.genre2)=="spanish","",album.genre2),
                  album.genre3=if_else(tolower(album.genre3)=="spanish","",album.genre3)                  
                ) %>%
                mutate(
                  album.genre1=if_else(tolower(album.genre1)=="albums i own","",album.genre1),
                  album.genre2=if_else(tolower(album.genre2)=="albums i own","",album.genre2),
                  album.genre3=if_else(tolower(album.genre3)=="albums i own","",album.genre3)                  
                ) %>%
                mutate(
                  album.genre1=if_else(grepl("^\\d{4}$",album.genre1),"",album.genre1),
                  album.genre2=if_else(grepl("^\\d{4}$",album.genre2),"",album.genre2),
                  album.genre3=if_else(grepl("^\\d{4}$",album.genre3),"",album.genre3)
                ), by=c("artist", "album")) %>%
    mutate(dummy=1) %>%
    left_join(o.data.tracks %>%
                mutate(track.duration=as.numeric(track.duration)) %>%
                filter(track.duration>0 & !is.na(track.duration)) %>%
                summarise(gduration=round(mean(track.duration),0)) %>%
                mutate(dummy=1), by="dummy") %>%
    mutate(
      published=album.published,
      track=coalesceplus(track, track.title),
      album=coalesceplus(album, track.album, album.title),
      artist=coalesceplus(artist, track.artist, album.artist),
      album.genre1=coalesceplus(album.genre1, artist.genre1),
      album.count1=coalesceplus(album.count1, artist.count1),
      album.genre2=coalesceplus(album.genre2, artist.genre2),
      album.count2=coalesceplus(album.count2, artist.count2),
      album.genre3=coalesceplus(album.genre3, artist.genre3),
      album.count3=coalesceplus(album.count3, artist.count3),
      # duration=if_else(track.duration=="0" | track.duration=="" | is.na(track.duration), gduration, as.numeric(track.duration))
      duration=as.numeric(if_else(track.duration=="0" | track.duration=="" | is.na(track.duration), "", track.duration))
    ) %>%
    select(-album.published, -track.title, -track.mbid, -track.artist, -track.album, -album.title, -album.artist, -track.duration, -gduration, -dummy) %>%
    arrange(fulldate) %>%
    mutate(artist=stri_trans_totitle(artist),
           track=stri_trans_totitle(track),
           album=stri_trans_totitle(album),
           artist.genre1=stri_trans_totitle(artist.genre1),
           artist.genre2=stri_trans_totitle(artist.genre2),
           artist.genre3=stri_trans_totitle(artist.genre3),
           album.genre1=stri_trans_totitle(album.genre1),
           album.genre2=stri_trans_totitle(album.genre2),
           album.genre3=stri_trans_totitle(album.genre3),
           yrmth = zoo::as.yearmon(fulldate), year = year(fulldate))
  saveRDS(o.data, o.filename)
  cat("import.data> end\n")  
  o.data
}

#' Find tickmarks for a given range of the y-axis that best fit an optimal number of tickmarks
#' you decide. f.i: what if i want to have a graph with 8 tickmarks in a range of 34 to 345

# i.min=50
# i.max=max(data.label$value)
# i.number.ticks=3 
# i.include.min=T
# i.include.max=T
# i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(1:3)), 1, FUN = function(x) {x[1] * x[2]})

optimal.tickmarks<-function(i.min,i.max,i.number.ticks=10,
                            i.valid.ticks=apply(expand.grid(c(1,2,2.5,5), 10^(-10:10)), 1, FUN = function(x) {x[1] * x[2]}),
                            i.include.min=F,i.include.max=F){
  # Y ahora calculo el tickmark que m?s se acerca a esos 10 tickmarks objetivo.
  # Option 1: free, I can put tickmarks outside c(i.min,i.max)
  if (i.include.min) dif0<-i.min else dif0<-0
  i.min=i.min-dif0
  i.max=i.max-dif0
  ticks.min<-floor(i.min/i.valid.ticks)
  ticks.max<-ceiling(i.max/i.valid.ticks)
  ticks.maxmin<-ticks.max-ticks.min+1
  n.valid.ticks<-length(i.valid.ticks)
  posicion.ticks<-(1:n.valid.ticks)[min(abs(ticks.maxmin-i.number.ticks))==abs(ticks.maxmin-i.number.ticks)][1]
  ini<-(ticks.min*i.valid.ticks)[posicion.ticks]+dif0
  fin<-(ticks.max*i.valid.ticks)[posicion.ticks]+dif0
  salto<-i.valid.ticks[posicion.ticks]
  # Tickmarks
  tickmarks<-seq(ini,fin,salto)
  # Number of ticks
  numero.ticks<-length(tickmarks)
  if (i.include.max) {
    fin<-i.max
    tickmarks[numero.ticks]<-i.max
  }
  # Rank
  range.y<-c(ini,fin)
  # Returning
  return(list(by=salto,number=numero.ticks,range=range.y,tickmarks=tickmarks))
}

# i<-1
# i.profile.name=profile.names.list[[i]]
# i.restart=restart.tasks
# i.api_key=api_key
# i.slice=500

lastfm.analiser <- function(i.profile.name, i.restart=F, i.api_key, i.slice=500){
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("processed")) dir.create("processed")
  if (!dir.exists("temp")) dir.create("temp")
  
  lastfm.data <- import.lastfm.user(i.user=i.profile.name$lastfm.users, i.restart=i.restart) %>%
    import.lastfm.data(i.user=i.profile.name$lastfm.users, i.restart=i.restart, i.api_key=i.api_key, i.slice=i.slice) %>%
    mutate(weekday=as.numeric(strftime(fulldate,'%u')))
  
  plot.index<-0
  do.call(file.remove, list(list.files("output", pattern=paste0("^",i.profile.name$name,"_p[0-9]{2}_.*$"), full.names = TRUE)))

  # Process data
  
  # Most listened albums and month it was most listened
  
  # Scrobbles per album
  temp1 <- lastfm.data %>%
    group_by(artist, album, album.genre1) %>%
    summarise(n=n(), startdate=min(fulldate), enddate=max(fulldate)) %>%
    mutate(fullalbum=paste0(artist, "\n", album)) %>%
    ungroup() %>%
    arrange(desc(n), fullalbum) %>%
    mutate(rank = row_number(-n))
  # Most heard month per album
  temp2 <- lastfm.data %>%
    group_by(artist, album, yrmth) %>%
    summarise(n=n()) %>%
    group_by(artist, album) %>%
    arrange(artist, album, desc(n), yrmth) %>%
    slice(1) %>%
    select(-n) %>%
    ungroup()
  albums.summary <- temp1 %>%
    inner_join(temp2, by=c("artist", "album")) %>%
    arrange(artist, album, yrmth) %>%
    rename(yrmthmostheared=yrmth) %>%
    arrange(artist, album)
  
  rm(temp1, temp2)
  
  # Most listened albums and month it was most listened year by year
  
  # Scrobbles per album
  temp1 <- lastfm.data %>%
    group_by(artist, album, year) %>%
    summarise(n=n()) %>%
    mutate(fullalbum=paste0(artist, "\n", album)) %>%
    group_by(year) %>%
    arrange(year, desc(n), fullalbum) %>%
    mutate(rank = row_number(-n))  %>%
    ungroup()
  # Most heard month per album
  temp2 <- lastfm.data %>%
    group_by(artist, album, year, yrmth) %>%
    summarise(n=n()) %>%
    group_by(artist, album, year) %>%
    arrange(artist, album, year, desc(n), yrmth) %>%
    slice(1) %>%
    select(-n) %>%
    ungroup()
  albums.summary.per.year <- temp1 %>%
    inner_join(temp2, by=c("artist", "album", "year")) %>%
    arrange(artist, album, year, yrmth) %>%
    rename(yrmthmosthearedinyear=yrmth) %>%
    arrange(year, artist, album)
  
  rm(temp1, temp2)  
  # General graphs - Scrobbles

  # Plot: Serie scrobbles per month
  
  n.top.albums.per.year<-5
  
  temp1 <- albums.summary.per.year %>%
    group_by(year) %>%
    arrange(year, desc(n), artist, album) %>%
    slice(1:n.top.albums.per.year) %>%
    arrange(year, desc(n)) %>%
    mutate(rank = row_number(-n)) %>%
    ungroup()
  
  temp2 <- lastfm.data %>%
    group_by(yrmth) %>%
    summarise(n=n()) %>%
    tidyr::complete(yrmth = full_seq_yearmon(yrmth))
  
  ggplot(temp2, aes(x=yrmth, y=n)) + 
    geom_bar(stat="identity", fill="steelblue") +
    zoo::scale_x_yearmon() +
    # geom_line() + 
    # geom_point() +
    geom_label_repel(data=temp1, aes(yrmthmosthearedinyear, n, label = fullalbum), size=2) +
    labs(x = "Month", y = "Plays", title = "Scrobbling frequency over time",
         subtitle = "Number of scrobbles per month and top scrobbled albums", 
         caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal()
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_scrobbles_serie.png"), width = 12, height = 9)
  
  rm("temp1", "temp2")
  
  # Plot: Scrobbles per month
  
  ggplot(lastfm.data, aes(x=factor(month(fulldate), levels=1:12, labels=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")))) +
    geom_bar(stat = "count", width=0.7, fill="steelblue") +
    labs(x = "Week day", y = "Listens", title = "Scrobbling per week day",
         subtitle = "Number of scrobbles per month", 
         caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal()
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_scrobbles_month.png"), width = 8, height = 6)
  
  # Plot: Scrobbles per weekday
  
  ggplot(lastfm.data, aes(x=factor(weekday, levels=1:7, labels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")))) +
    geom_bar(stat = "count", width=0.7, fill="steelblue") +
    labs(x = "Week day", y = "Listens", title = "Scrobbling per week day",
         subtitle = "Number of scrobbles per week day", 
         caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal()
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_scrobbles_weekday.png"), width = 8, height = 6)
  
  # Plot:  Listening clock
  
  temp1 <- lastfm.data %>%
    mutate(hour=hour(fulldate)) %>%
    group_by(hour) %>%
    summarise(value=n()) %>%
    ungroup() %>%
    full_join(data.frame(hour=0:23), by="hour") %>%
    arrange(hour) %>%
    mutate(id=1:n(),  hour=stringi::stri_pad(trimws(as.character(hour)), width = 2, side = "left", pad = "0"),
           value=ifelse(is.na(value), 0, value),
           group="scrobblings")
  temp2<-optimal.tickmarks(0, max(temp1$value), i.number.ticks=5, i.include.min=F, i.include.max=F)
  temp3 <- temp1 %>%
    bind_rows(temp1 %>% mutate(group="nonscronnlings", value=temp2$range[2]-value))
  temp4<-data.frame(xstart=1, xend=24, y=temp2$tickmarks[c(-1,-temp2$number)])
  
  ggplot() +       
    # Note that id is a factor. If x is numeric, there is some space between the first bar
    # bar plot
    geom_bar(data=temp3, aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=1) +  
    # bar colors, from brewer, in blues
    scale_fill_manual(values = c("#CDE1FF", "#0066FF")) +   
    # polar coordinates, to make it look like a clock
    # horizontal/circular lines that gives the scale of the y axis
    geom_segment(data=temp4, mapping=aes(x=xstart, y=y, xend=xend, yend=y), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    # i put again the bars to be on top of the circular lines
    geom_bar(data=temp3, aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=1) + 
    # the scale numbers, I place it at 24.5
    annotate("text", x = rep(24.5,temp2$number-2), y = temp4$y, label = temp4$y , color="grey", size=3 , angle=0, fontface="bold", hjust=0.5) +
    ylim(-temp2$range[2]*2/3,y=10/8*temp2$range[2]) +
    theme_minimal() +
    scale_x_discrete() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      #axis.title = element_blank(),
      panel.grid = element_blank()
      # , plot.margin = unit(rep(-1,4), "cm")
    ) +
    coord_polar() +
    labs(x = "Time", y = "Scrobblings", title = "Hours of day scrobbling",
         subtitle = "Scrobbling frequency per hour of day",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) + 
    #geom_text(data=temp1, aes(x=id-0.5, y=9/8*temp2$range[2], label=hour, hjust=0.5), color="black", fontface="bold",alpha=0.6, size=5, inherit.aes = FALSE )
    geom_text(data=temp1 %>% filter(hour %in% c('00','06','12','18')), aes(x=id-0.5, y=-1.5/8*temp2$range[2], label=hour, hjust=0.5), color="black", fontface="bold",alpha=0.6, size=5.5, inherit.aes = FALSE )
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_scrobbles_clock.png"), width = 12, height = 9)

  rm("temp1", "temp2", "temp3", "temp4")
  
  # Artists
  
  # Plot: top artists
  
  n.top.artists<-25
  
  top.artists <- lastfm.data %>%
    group_by(artist, artist.genre1) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    slice(1:n.top.artists) %>%
    ungroup()
  
  ggplot(top.artists, aes(reorder(artist, n), n, color = artist.genre1)) +
    geom_segment(aes(xend = reorder(artist, n)), yend = 0, color = 'grey50') +
    geom_point(size = 3) +
    coord_flip() +
    labs(y = "Plays", x = "", title = "Favourite Artists", subtitle = "Top artist by scrobbles", caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal() +
    scale_color_viridis(discrete = T, name = "Genre") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey60', linetype = 'dashed'))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_artists_top.png"), width = 8, height = 6)
  
  # Plot: Preferencences over time
  
  n.top.artists <- 25
  
  minmaxrank.artists <- lastfm.data  %>%
    select(year, artist) %>%
    distinct() %>%
    group_by(year) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    summarise(maxrank=max(n), minrank=min(n), percentage.over.top=sum(n>n.top.artists)/n())

  if (minmaxrank.artists$percentage.over.top<0.80) n.top.artists <- minmaxrank.artists$minrank
  
  artist.summary.per.year <- albums.summary.per.year %>%
    group_by(year, artist) %>%
    summarise(n=sum(n)) %>%
    ungroup() %>%
    group_by(year) %>%
    arrange(year, desc(n), artist) %>%
    slice(1:n.top.artists) %>%
    mutate(rank = row_number(-n)) %>%
    ungroup()
  
  enough.artist.information<-artist.summary.per.year %>%
    group_by(year) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    filter(n==n.top.artists) %>%
    summarise(minyear=min(year), maxyear=max(year))
  
  artist.summary.per.year <- artist.summary.per.year %>%
    filter(year>=enough.artist.information$minyear & year<=enough.artist.information$maxyear)
  
  artist_end_tags <- artist.summary.per.year %>%
    ungroup() %>%
    filter(year == enough.artist.information$maxyear) %>%
    mutate(year = enough.artist.information$maxyear+(enough.artist.information$maxyear-enough.artist.information$minyear)/6)
  
  artist_start_tags <- artist.summary.per.year %>%
    ungroup() %>%
    filter(year == enough.artist.information$minyear) %>%
    mutate(year = enough.artist.information$minyear-(enough.artist.information$maxyear-enough.artist.information$minyear)/6)
  
  colors.n<-n.top.artists
  colors <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,colors.n)),"Accent"))(colors.n)
  names(colors)<-lastfm.data %>%
    group_by(artist) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    slice(1:colors.n) %>%
    pull(artist)
  
  othertags <- artist.summary.per.year %>% distinct(artist) %>% filter(!artist %in% names(colors)) %>% .$artist
  
  colors <- c(colors, setNames(rep("gray", length(othertags)), othertags))
  
  highlights <- filter(artist.summary.per.year, artist %in% names(colors)[colors != "gray"])
  
  ggplot(data = artist.summary.per.year, aes(year, rank, color = artist, group = artist, label = artist)) +
    geom_line(size = 1.7, alpha = 0.25) +
    geom_line(size = 2.5, data = highlights) +
    geom_point(size = 4, alpha = 0.25) +
    geom_point(size = 4, data = highlights) +
    geom_point(size = 1.75, color = "white") +
    geom_text(data = artist_start_tags, x = enough.artist.information$minyear-(enough.artist.information$maxyear-enough.artist.information$minyear)/6, size = 4.5) +
    geom_text(data = artist_end_tags, x = enough.artist.information$maxyear+(enough.artist.information$maxyear-enough.artist.information$minyear)/6, size = 4.5) +
    scale_y_reverse(breaks = 1:n.top.artists) +
    scale_x_continuous(
      breaks = seq(min(artist.summary.per.year$year), max(artist.summary.per.year$year)),
      limits = c(min(artist.summary.per.year$year) - 1.5, max(artist.summary.per.year$year) + 1.6)) +
    scale_color_manual(values = colors) +
    theme_minimal() + 
    theme(
      legend.position = "",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
    labs(x = "Year", y = "Rank", title = "Artist preference over time",
         subtitle = "Top artists per year by scrobbles",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_artists_preferences.png"), width = 12, height = 9)
  
  # Plot 5:  distribution over time
  
  n.top.artists<-10
  
  top.artists <- lastfm.data %>%
    group_by(artist) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(desc(n), artist) %>%
    mutate(rank=row_number(-n)) %>%
    ungroup() %>%
    filter(rank<=n.top.artists)
  
  data <- lastfm.data %>%
    inner_join(
      top.artists %>%
        select(artist), by="artist") %>%
    mutate(genre=as.factor(artist), year=as.factor(year)) %>%
    group_by(genre, year) %>%
    summarise(value=10*log1p(n())) %>%
    arrange(year, genre, value) %>%
    ungroup()
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$year), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$year=rep(levels(data$year), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(year)
  data$id=seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>% 
    group_by(year) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  # Make the plot
  ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.5) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      #axis.title = element_blank(),
      panel.grid = element_blank()
      # , plot.margin = unit(rep(-1,4), "cm")
    ) +
    labs(x = "Year", y = "Rank", title = "Artists preferences over time",
         subtitle = "Scrobbling frequency of artists by year",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=value+10, label=genre, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=year), 
              hjust=0.5,
              # hjust=c(1,1,0,0),
              colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_artists_distribution.png"), width = 12, height = 9)

  # Artist discovery
  
  artist.first.time <- lastfm.data %>%
    group_by(artist) %>%
    summarise(yrfirst=min(year), n=n()) %>%
    group_by(yrfirst) %>%
    arrange(yrfirst, desc(n)) %>%
    mutate(rank = row_number(-n))
  
  artist.first.time.data <- artist.first.time %>%
    group_by(yrfirst) %>%
    summarise(n=n())
  
  artist.first.time.labels <- artist.first.time %>%
    group_by(yrfirst) %>%
    summarise(n=n()) %>%
    inner_join(artist.first.time %>% filter(rank<=3), by="yrfirst") %>%
    group_by(yrfirst) %>%
    arrange(n.y) %>%
    mutate(freq = n.y / sum(n.y, na.rm=T), freqcum=cumsum(freq), position=n.x*if_else(rank==3,freq/2,lag(freqcum)+freq/2))
  
  ggplot(artist.first.time.data, aes(x=as.factor(yrfirst), y=n)) +
    geom_bar(stat = "identity", width=0.7, fill="steelblue") +
    geom_label(data=artist.first.time.labels, aes(as.factor(yrfirst), position, label = artist), size=3) +
    labs(x = "Year first listened", y = "Number of artists", title = "Artist discovery",
         subtitle = "New artists discovered per year", 
         caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal()
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_artists_discovered.png"), width = 8, height = 6)
  
  # Albums
  
  # Plot: top albums
  
  n.top.albums<-25
  
  top.albums <- albums.summary %>%
    filter(rank<=n.top.albums)
    # group_by(fullalbum, album.genre1) %>%
    # summarise(n=n()) %>%
    # ungroup() %>%
    # arrange(desc(n)) %>%
    # slice(1:n.top.albums) %>%
    # ungroup()
  
  ggplot(top.albums, aes(reorder(fullalbum, n), n, color = album.genre1)) +
    geom_segment(aes(xend = reorder(fullalbum, n)), yend = 0, color = 'grey50') +
    geom_point(size = 3) +
    coord_flip() +
    labs(y = "Plays", x = "", title = "Favourite Albums", subtitle = "Top albums by scrobbles", caption = paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    theme_minimal() +
    scale_color_viridis(discrete = T, name = "Genre") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey60', linetype = 'dashed'))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_albums_top.png"), width = 8, height = 6)
  
  # Plot: distribution
  
  n.top.albums<-10
  
  ds <- lastfm.data %>%
    inner_join(albums.summary %>%
                 filter(rank<=n.top.albums) %>%
                 select(artist, album, fullalbum), by=c("artist", "album")) %>%
    mutate(date=as.Date(format(fulldate,"%Y%m%d"), format="%Y%m%d")) %>%
    select(fullalbum, date)
  ggplot(ds, aes(date, fill=fullalbum)) + 
    geom_density(alpha=0.55) +
    scale_x_date() +
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
    scale_fill_brewer(name="Albums", palette="Spectral") +
    labs(x = "Year", y = "Rank", title = "Albums scrobbled distribution",
         subtitle = "Distribution of the top albums scrobbling frequency",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_albums_distribution.png"), width = 12, height = 9)

  # Plot: time listening
  
  n.top.albums<-25
  
  data <- albums.summary %>%
    filter(rank<=n.top.albums) %>%
    mutate(x=as.factor(fullalbum), value1=as.Date(format(startdate, "%Y%m%d"), format="%Y%m%d"), value2=as.Date(format(enddate, "%Y%m%d"), format="%Y%m%d")) %>%
    select(x, value1, value2, rank)
  
  # Reorder data using average?
  data = data %>% rowwise() %>% mutate( mymean = mean.Date(c(value1,value2) )) %>% arrange(mymean) %>% mutate(x=factor(x, x))
  
  # With a bit more style
  ggplot(data) +
    geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color=ifelse(data$rank<=10, "black", "grey"), size=ifelse(data$rank<=10, 1.3, 0.7)) +
    geom_point( aes(x=x, y=value1), color=ifelse(data$rank<=10, rgb(0.2,0.7,0.1,1), rgb(0.2,0.7,0.1,0.5)), size=ifelse(data$rank<=10, 4, 2) ) +
    geom_point( aes(x=x, y=value2), color=ifelse(data$rank<=10, rgb(0.7,0.2,0.1,1), rgb(0.7,0.2,0.1,0.5)), size=ifelse(data$rank<=10, 4, 2) ) +
    coord_flip()+
    theme_light() +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    # xlab("") +
    # ylab("Value of Y") + 
    annotate("text", x = data$x[data$rank<=10], y=data$mymean[data$rank<=10], label = data$x[data$rank<=10], color="black", size=3 , angle=0, fontface="bold", hjust=0.5) +
    # ggtitle("Top albums persistence across time") +
    labs(x = "", y = "Time", title = "Top albums persistence",
         subtitle = "Time of the first and last scroble of each album",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_albums_persistence.png"), width = 12, height = 9)
  
  # Songs
    
  # Plot 3: TOP 10 SONGS
  
  n.top.tracks <- 25
  
  top.tracks <- lastfm.data %>%
    group_by(track, artist) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    slice(1:n.top.tracks) %>%
    ungroup()%>%
    mutate(fulltrack=paste0(track, " (", artist,")"))
  
  ggplot(top.tracks, aes(reorder(fulltrack, n), n)) +
    geom_segment(aes(xend = reorder(fulltrack, n)), yend = 0, color = 'grey50') +
    geom_point(size = 3, color = viridis(1)) +
    scale_color_viridis() + coord_flip() + theme_minimal() + theme(legend.position = "") +
    theme_minimal() +
    labs(x = "", y = "Plays", title = "Favourite songs", subtitle = "Top songs by scrobbles",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_songs_top.png"), width = 8, height = 6)
  
  # Genres
  
  temp1 <- lastfm.data %>%
     mutate(n=1:n())
  
  temp2 <- temp1 %>%
    select(n, album.genre1, album.genre2, album.genre3) %>%
    gather(genretype, genre, -n) %>%
    extract(genretype, into = "position", regex = "^.*(\\d{1})$", remove = T) %>%
    arrange(n, position)
  
  temp3 <- temp1 %>%
    select(n, album.count1, album.count2, album.count3) %>%
    gather(counttype, count, -n) %>%
    extract(counttype, into = "position", regex = "^.*(\\d{1})$", remove = T) %>%
    mutate(count=as.numeric(count)) %>%
    group_by(n) %>%
    mutate(freq1=if_else(position==1, 1, 0), freq3 = count / sum(count, na.rm=T)) %>%
    arrange(n, position) %>%
    ungroup()
  
  lastfm.data.3.genres <- temp1 %>%
    left_join(temp2 %>%
    inner_join(temp3, by=c("n", "position")), by="n") %>%
    arrange(n, position)  
  
  # Plot: top genres
  
  n.top.genres <- 25
  
  top.genres <- lastfm.data.3.genres %>%
    group_by(genre) %>%
    summarise(n=sum(freq3, na.rm=T)) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    slice(1:n.top.genres) %>%
    ungroup()
  
  ggplot(top.genres, aes(reorder(genre, n), n)) +
    geom_segment(aes(xend = reorder(genre, n)), yend = 0, color = 'grey50') +
    geom_point(size = 3, color = viridis(1)) +
    scale_color_viridis() + coord_flip() + theme_minimal() + theme(legend.position = "") +
    theme_minimal() +
    labs(x = "", y = "Plays", title = "Favourite genres", subtitle = "Top genres by scrobbles",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_genres_top.png"), width = 8, height = 6)
  
  
  # Plot: Subway-style Genre Ranking
  
  n.top.genre.per.year <- 25
  
  minmaxrank.genres <- lastfm.data.3.genres  %>%
    filter(!is.na(genre) &  genre!= "") %>%  
    select(year, genre, freq3) %>%
    distinct() %>%
    group_by(year) %>%
    summarise(n=sum(freq3, na.rm=T)) %>%
    ungroup() %>%
    summarise(maxrank=max(n), minrank=min(n), percentage.over.top=sum(n>n.top.genre.per.year, na.rm=T)/n())
  
  if (minmaxrank.genres$percentage.over.top<0.80) n.top.genre.per.year <- minmaxrank.genres$minrank
  
  genre.summary.per.year <- lastfm.data.3.genres  %>%
    filter(!is.na(genre) &  genre!= "") %>%
    group_by(year, genre) %>%
    summarise(n=sum(freq3, na.rm=T)) %>%
    ungroup() %>%
    group_by(year) %>%
    arrange(year, desc(n), genre) %>%
    slice(1:n.top.genre.per.year) %>%
    mutate(rank = row_number(-n)) %>%
    ungroup()
  
  enough.genre.information<-genre.summary.per.year %>%
    group_by(year) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    filter(n==n.top.genre.per.year) %>%
    summarise(minyear=min(year), maxyear=max(year))
  
  genre.summary.per.year <- genre.summary.per.year %>%
    filter(year>=enough.genre.information$minyear & year<=enough.genre.information$maxyear)
  
  genre_end_tags <- genre.summary.per.year %>%
    ungroup() %>%
    filter(year == enough.genre.information$maxyear) %>%
    mutate(year = enough.genre.information$maxyear+(enough.genre.information$maxyear-enough.genre.information$minyear)/6)
  
  genre_start_tags <- genre.summary.per.year %>%
    ungroup() %>%
    filter(year == enough.genre.information$minyear) %>%
    mutate(year = enough.genre.information$minyear-(enough.genre.information$maxyear-enough.genre.information$minyear)/6)
  
  colors.n<-n.top.genre.per.year
  colors <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,colors.n)),"Accent"))(colors.n)
  names(colors)<-lastfm.data.3.genres %>%
    group_by(genre) %>%
    summarise(n=sum(freq3, na.rm=T)) %>%
    arrange(desc(n)) %>%
    slice(1:colors.n) %>%
    pull(genre)
  
  othertags <- genre.summary.per.year %>% distinct(genre) %>% filter(!genre %in% names(colors)) %>% .$genre
  
  colors <- c(colors, setNames(rep("gray", length(othertags)), othertags))
  
  highlights <- filter(genre.summary.per.year, genre %in% names(colors)[colors != "gray"])
  
  ggplot(data = genre.summary.per.year, aes(year, rank, color = genre, group = genre, label = genre)) +
    geom_line(size = 1.7, alpha = 0.25) +
    geom_line(size = 2.5, data = highlights) +
    geom_point(size = 4, alpha = 0.25) +
    geom_point(size = 4, data = highlights) +
    geom_point(size = 1.75, color = "white") +
    geom_text(data = genre_start_tags, x = enough.genre.information$minyear-(enough.genre.information$maxyear-enough.genre.information$minyear)/6, size = 4.5) +
    geom_text(data = genre_end_tags, x = enough.genre.information$maxyear+(enough.genre.information$maxyear-enough.genre.information$minyear)/6, size = 4.5) +
    scale_y_reverse(breaks = 1:n.top.genre.per.year) +
    scale_x_continuous(
      breaks = seq(min(genre.summary.per.year$year), max(genre.summary.per.year$year)),
      limits = c(min(genre.summary.per.year$year) - 1.5, max(genre.summary.per.year$year) + 1.6)) +
    scale_color_manual(values = colors) +
    theme_minimal() + 
    theme(
      legend.position = "",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
    labs(x = "Year", y = "Rank", title = "Genres preference over time",
         subtitle = "Top genres per year by scrobbles",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name))
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_genres_evolution.png"), width = 12, height = 9)
  
  # Plot test 7:  distribution of genres
  
  n.top.genres<-10
  
  top.genres <- lastfm.data.3.genres %>%
    group_by(genre) %>%
    summarise(n=sum(freq3, na.rm=T)) %>%
    ungroup() %>%
    arrange(desc(n), genre) %>%
    mutate(rank=row_number(-n)) %>%
    ungroup() %>%
    filter(rank<=n.top.genres)
  
  data <- lastfm.data.3.genres %>%
    inner_join(
      top.genres %>%
        select(genre), by="genre") %>%
    mutate(genre=as.factor(genre), year=as.factor(year)) %>%
    group_by(genre, year) %>%
    summarise(value=10*log1p(sum(freq3, na.rm=T))) %>%
    arrange(year, genre, value) %>%
    ungroup()
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$year), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$year=rep(levels(data$year), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(year)
  data$id=seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>% 
    group_by(year) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  # Make the plot
  ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.5) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      #axis.title = element_blank(),
      panel.grid = element_blank()
      # , plot.margin = unit(rep(-1,4), "cm")
    ) +
    labs(x = "Year", y = "Rank", title = "Genre preferences over time",
         subtitle = "Scrobbling frequency of genres by year",
         caption=paste0("source: last.fm scrobbles\nUser: ", i.profile.name$name)) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=value+10, label=genre, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=year), 
              hjust=0.5,
              # hjust=c(1,1,0,0),
              colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  plot.index <- plot.index + 1
  plot.index.text <- stringi::stri_pad(trimws(as.character(plot.index)), width = 2, side = "left", pad = "0")
  ggsave(paste0("output/",i.profile.name$name,"_p",plot.index.text,"_genres_circle.png"), width = 12, height = 9)
  
 
  return(lastfm.data)
}
