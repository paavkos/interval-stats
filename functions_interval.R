# Funktiot intervallianalyysiin

# tiedoston luku
parse_gpx <- function(filename) {
  st_read(filename, layer = "track_points")
}

# # lukee tiedoston
# data <- parse_gpx("~/Downloads/Evening_Run.gpx")

# puhdistaa datan
clean_data <- function(data){
  # hae ajat ja metrierotukset
  times <- diff(data$time) %>% as.numeric()
  meters <- sapply(2:nrow(data),function(i){distm(data[i-1,],data[i,])}) %>% as.numeric()
  # liitä yhteen
  df <- as.data.frame(cbind(times,meters)) %>% 
    mutate(m_per_s = meters/times) %>% 
    # smoothataan GPS-piikit 
    mutate(speed_smooth = rollmean(m_per_s, k = 15, fill = NA, align = "center")) %>% 
    mutate(speed_kmh = speed_smooth * 3.6) %>% 
    mutate(min_per_km = 60/speed_kmh)
  df
}

# compute the intervals
intervals <- function(data){
  data <- data %>% 
    mutate(class = ifelse(speed_smooth > 1.2*mean(speed_smooth,na.rm=TRUE), "sprint", "slow")) %>% 
    tidyr::fill(class, .direction = "up") %>% 
    tidyr::fill(speed_smooth, .direction = "up") %>% 
    mutate(x = ifelse(class != dplyr::lag(class),1,0)) %>%
    tidyr::fill(x, .direction = "up") %>% 
    mutate(segment = cumsum(x)+1) 
}

# segmenttien laskemiseen
segments <- function(data){
  # segmenttien pituudet
  metrit <- data %>% 
    group_by(segment,class) %>% 
    summarise(length = sum(meters)) %>% 
    ungroup()
  
  # segmenttien ajat sekunteina
  sekunnit <- data %>% 
    group_by(segment,class) %>% 
    summarise(seconds = sum(times)) %>% 
    ungroup()
  
  # vielä tämä
  segmentit <- left_join(metrit, sekunnit, by = c('segment','class'))
  segmentit <- segmentit %>% mutate(speed = length/seconds) %>% 
    mutate(interval_pace = 1000/(speed*60))
}

# geomit kuntoon
to_linestrings <- function(data, intervals){
  cbind(data, 
        rbind(NA,intervallit)) %>% 
  dplyr::group_by(segment) %>%
  arrange(time) %>% 
  dplyr::summarize() %>%
  filter(st_geometry_type(.) == "MULTIPOINT") %>%
  sf::st_cast("LINESTRING")
}


# SÄLÄÄ JA ESIMERKIT

a <- clean_data(data)
a

intervallit <- intervals(a)
intervallit

seg <- segments(intervallit)
seg

to_linestrings(data, intervallit)
