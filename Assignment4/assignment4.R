#############################
## reading data and change column name "mr" to "mortality rate" and "if to "infection fatality rate"
#############################
counts<-read.csv("Desktop/HMS/HMS520-Autumn2022/Assignment4/counts.csv")

counts <- counts %>% rename("mortality_rate" = "mr","infection_fatality_rate" = "ifr")

#getwd()
#setwd('Desktop/HMS/HMS520-Autumn2022/Assignment4')
###########################################################
## Create scatter plot w/ x-axis as date and y-axis as mortality rate
## use facet wrap to create panel for each state
## specify number of rows to 10
## use the scale_y_log10 to plot the y axis in log scale
## add title "Mortality Rate"
## save figure as "mr_by_state_date.png"
###########################################################

png(filename = 'mr_by_state_date.png', width=10, height=20, units='in', res=75)

ggplot() +
  facet_wrap(~state, nrow=10)+
  geom_point(
    data= counts,
    aes(x=counts$date, y=counts$mortality_rate))+
  scale_y_log10() +
  labs(title="Mortality Rate", x="Date", y="Mortality Rate")+
  theme(plot.title = element_text(hjust=0.5))

dev.off()

###########################################################
## Create boxplot w/ x-axis as diff states and y axis as mortality rate
## order x axis so that mr is plotting in descending order
## change y axis to log scale
## add title Mortality Rate
## change x axis label to "state" and y axis label to "mortality rate"
## save figure as "mr_by_state_date.png"
###########################################################

png(filename="mr_by_state.png", width=8, height=15, units='in', res=75)

ggplot(counts, 
       aes(x=reorder(counts$state, -counts$mortality_rate), 
           y=counts$mortality_rate))+
           geom_boxplot()+
            scale_y_log10()+
            stat_summary(fun=mean, geom='point',color='blue')+
            theme_classic2()+
            ggtitle("Mortality Rate")+
            xlab("State")+
            ylab("Mortality Rate")
            
dev.off()


###########################################################
## Create histogram for the mean mr and ifr across states
## aggregate mr and ifr by state and assign the resulting data frame to a variable named mean_counts
## reshape mean_counts longer so each row is one state and one rate type 
## assign to variable named mean_counts_long
## plot mean_counts_long using geom_histogram
## 2 panels for mr and other for ifr
## scale x axis to log scale
## save figure as "mr_and_ifr.png" in 8 by 8
###########################################################

mean_counts<-counts %>%
  group_by(state) %>%
  summarize(mean_mortality_rate= mean(mortality_rate), mean_infection_fatality_rate=mean(infection_fatality_rate))

mean_counts_long <- pivot_longer(mean_counts, cols= !state, names_to="means", values_to="values")


png(filename='mr_and_ifr.png', width=8, height=8, units='in', res=72)

ggplot(mean_counts_long)+
  facet_wrap(~means)+
  geom_histogram(aes(x=values), binwidth=0.1, color='blue', fill='lightblue')+
  scale_x_log10()+
  theme(plot.title=element_text(hjust=0.5))+
  ggtitle("Mean Mortality and Infection Fatality Rates by State") +
  xlab("Mean Rate") +
  ylab("State")

dev.off()
