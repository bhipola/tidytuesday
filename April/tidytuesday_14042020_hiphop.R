

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')
str(rankings)

top_ever<-rankings%>% group_by(year) %>% summarise(n=n(), suma=sum(points))%>% arrange(year)

blue<-brewer.pal(9,'Blues')

sum_evolution<-ggplot(top_ever,aes(x=year, y=suma))+
  geom_col(color=blue[9],fill=blue[4])+
  theme_classic()+
  labs(x='Release year', y='Sum of points',
       title="The 90s were hip-hop's golden age",
       subtitle = 'Sum of points of all songs according to release year')+
  theme(legend.title=element_text(family='serif'),
        axis.title=element_text(family='serif'),
        title=element_text(family='serif'))

top1<-rankings%>% group_by(year)%>% top_n(1,points) %>% arrange(year)
toptop1_text<-as_tibble(top1) %>% top_n(2, points)

evolution_plot<-ggplot(top1, aes(x=year, y=points))+
  geom_line(size=1.1, color=blue[4])+
  scale_y_continuous(limits=range(0,160))+
  geom_label(data=toptop1_text,
            aes(y=points+10,label=paste0('Title: ', title, '\n Artist: ',artist,'\n Year: ',year,'\n Points: ',points)), 
            size=3,
            family='serif',
            fontface='bold',
            alpha=0.3)+
  theme_classic()+
  labs(x='Release year', y='Points',
       title='The best of each year',
       subtitle = "Points given to each year's top ranked song")+
  theme(legend.title=element_text(family='serif'),
        axis.title=element_text(family='serif'),
        title=element_text(family='serif'))


polls_gender<-polls %>% group_by(rank,gender)%>% summarise(n=100*n()/107)

polls_gender_1519<-polls %>% filter(year %in% 2015:2019)%>%count(rank, gender) %>%group_by(rank)%>% mutate(pc=100*n/sum(n)) %>%
                            select(rank,gender, n=pc) 
 
gender_plot<-ggplot(polls_gender, aes(x=rank, y=n, group=gender, fill=gender))+
  geom_col(position='stack')+
  scale_fill_brewer(palette='Blues',name='Gender')+
  geom_text(aes(label=paste0(round(n,2),'%')),
            position = position_stack(vjust = 0.7),
            fontface='bold',
            family='serif',
            size=3)+
  labs(x='Rank',y='Percentage of artists',
       title='Hip-hop glass ceiling?',
       subtitle='Ranking of hip-hop songs according to artist gender')+
  theme_minimal()+
  theme(legend.title=element_text(family='serif'),
        axis.title=element_text(family='serif'),
        legend.text=element_text(family='serif'),
        title=element_text(family='serif'),
        panel.grid=element_blank())

multiplot(evolution_plot,sum_evolution,gender_plot, cols=3)

