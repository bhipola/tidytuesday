library(dplyr)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(tidyr)
# Get the Data

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

#filters
beer_states_2019_onpremise<- beer_states %>%   filter(year==2019 & type=="On Premises") %>% select(-year)
beer_states_2019_bottlescans<- beer_states %>%   filter(year==2019 & type=="Bottles and Cans") %>% select(-year)
beer_states_2019_kegsbarrels<- beer_states %>%   filter(year==2019 & type=="Kegs and Barrels") %>% select(-year)


#display.brewer.all()
beer_states_2019_onpremise<-beer_states_2019_onpremise[-52,] 
beer_states_2019_bottlescans<-beer_states_2019_bottlescans[-52,]
beer_states_2019_kegsbarrels<-beer_states_2019_kegsbarrels[-52,]

beer_states_2019_onpremise$state_long<-c('Alaska', 'Alabama', 'Arkansas', 'Arizona', 'California', 'Colorado', 'Connecticut', 'District of Columbia', 'Delaware', 'Florida', 'Georgia','Hawaii', 'Iowa','Idaho','Illinois','Indiana', 'Kansas', 'Kentucky', 'Louisiana', 'Massachusetts', 'Maryland', 'Maine', 'Michigan', 'Minnesota', 'Missouri', 'Mississippi', 'Montana', 'North Carolina', 'North Dakota', 'Nebraska', 'New Hampshire', 'New Jersey', 'New Mexico', 'Nevada', 'New York', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Virginia', 'Vermont', 'Washington', 'Wisconsin', 'West Virginia', 'Wyoming')


beer_states_2019_onpremise<-beer_states_2019_onpremise[,-3] %>% rename(On_premises=barrels)%>% mutate(On_premises=round(On_premises/1000,2))
beer_states_2019_bottlescans<-beer_states_2019_bottlescans[,-3]%>% rename(bottlescans=barrels)%>% mutate(bottlescans=round(bottlescans/1000,2))
beer_states_2019_kegsbarrels<-beer_states_2019_kegsbarrels[,-3]%>% rename(kegsbarrels=barrels)%>% mutate(kegsbarrels=round(kegsbarrels/1000,2))
beer_states_2019_groups<-cbind(beer_states_2019_onpremise,beer_states_2019_bottlescans,beer_states_2019_kegsbarrels)[,-c(4,6)]


#MAP
states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

states@data<-left_join(states@data,beer_states_2019_groups, by=c("name"="state_long"))

states@data$total<-states@data$On_premises+states@data$bottlescans+states@data$kegsbarrels

pal_range=colorQuantile(palette="YlOrBr", domain=c(states@data$total,0), n=7)


m <- 
  leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("CartoDB") %>% 
  addPolygons( fillOpacity = 1,smoothFactor = 0.2,fillColor= ~pal_range(On_premises), popup=~paste0("<b>", name, "</b>",'<br/>', "Number of barrels (thousands): ",On_premises), group="On premises", color="grey")%>%
  addPolygons( fillOpacity = 1,smoothFactor = 0.2, fillColor= ~pal_range(bottlescans), popup=~paste0("<b>", name, "</b>",'<br/>', "Number of barrels (thousands): ",bottlescans), group="Bottles/cans", color="grey")%>%
  addPolygons( fillOpacity = 1, smoothFactor = 0.2, fillColor= ~pal_range(kegsbarrels), popup=~paste0("<b>", name, "</b>",'<br/>', "Number of barrels (thousands): ",kegsbarrels), group="Kegs/barrels", color="grey")%>%
  addPolygons( fillOpacity = 1, smoothFactor = 0.2, fillColor= ~pal_range(total), popup=~paste0("<b>", name, "</b>",'<br/>', "Number of barrels (thousands): ",total), group="Total (all types)", color="grey")%>%
  addLayersControl(overlayGroups = c("On premises", "Bottles/cans", "Kegs/barrels", "Total (all types)"))
m 
m$width <-1000
m$height <- 700

saveWidget(m, "c:/Users/belen/OneDrive/Escritorio/mapbeer.html", selfcontained = FALSE, title="Beer Production in the US (2019)")