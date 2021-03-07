########################################
####### LOAD IN PACKAGES/FONTS  ########
########################################


require(pacman)

p_load(lubridate,stringr,ggplot2,tidyverse,sf,tidycensus,
       ggthemes,mapproj,patchwork,remotes,here)

# ggtext is not on CRAN and is still being developed,
# but it provides fantastic 
# you'll have to make a selection when you run these lines

############################################
###### READ IN FILES/ CLEAN THE DATA #######
############################################

# read in the most up-to-date Covid data from NYT
nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# shape files for counties are from US Census
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

data(state_laea) # data set from tidycensus
centroids <- st_centroid(state_laea)

# add leading 0s to fips codes if necessary, so it will join with the map/data from the census bureau
nyt$fips <- ifelse(str_length(nyt$fips)==1,paste0("0",nyt$fips),nyt$fips) %>% as.character()
nyt$date <- ymd(nyt$date)


nyt_data <- expand.grid(date=unique(nyt$date),fips=unique(centroids$GEOID))
nyt_data$fips <- nyt_data$fips %>% as.character()
nyt_data <- left_join(nyt_data,nyt)
nyt_data$cases <- ifelse(is.na(nyt_data$cases),0,nyt_data$cases)
nyt_data$deaths <- ifelse(is.na(nyt_data$deaths),0,nyt_data$deaths)

dat <- left_join(centroids,nyt_data,by=c("GEOID"="fips"))

dat <- dat %>%
  dplyr::arrange(desc(GEOID)) %>% 
  dplyr::group_by(GEOID) %>% 
  mutate(casesnew = c(NA,diff(cases))) %>% 
  mutate(casesma7 = zoo::rollmean(casesnew, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

datagg <- dat %>% group_by(date) %>% summarise(cases7total = sum(casesma7,na.rm=T))
datagg$cases7total <- datagg$cases7total/1000

addzero <- function (x) {
  ifelse(str_length(x)<3,paste0("0",x),x)
}

makename <- function (x) {
  paste0("~/quick-map/frames/",addzero(addzero(x)),".png")
}

start <- ymd("2020-02-15")
end <- today() - ddays(5)

for (i in 76:362) {
  
  plotdate <- today() - ddays(365) + ddays(i-1)
  
  mapplot <-  ggplot() +
    geom_sf(data=state_laea,fill="lightblue",alpha=1,color="white",size=.1) +
    geom_sf(data=dat[which(dat$date==plotdate),],aes(size=casesma7),color="red",shape=16,alpha=.5,show.legend=F) +
    scale_size_continuous(limits=c(1,max(dat$casesma7,na.rm=T)),range=c(.5,25)) +
    labs(title=format(plotdate, format="%B %d, %Y")) +
    theme(plot.title = element_text(hjust=.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())
  
  lineplot <- ggplot(datagg[which(datagg$date <= plotdate),]) +
    geom_line(aes(x=date,y=cases7total),color="red",alpha=.5) + 
    xlim(start,end) +
    ylim(0,max(datagg$cases7total)) +
    labs(y="New cases (1000s)",x="") +
    theme_minimal()
  
  mapplot / lineplot + plot_layout(widths = c(4, 4), heights = c(2, 0.5))
  
  ggsave(makename(i),last_plot(),width=6,height=5,units="in")
  
}
ggplot(dat[which(dat$GEOID=="01001"),]) +
  geom_point(aes(x=date,y=casesma7))
