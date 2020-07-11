###### plotting all of the grouped prediction line graphs
source('~/Desktop/crime_reports/grouped_rates_plots.R')


nice_crime_names = c('Damage & Arson','Other Theft','ASB','Burglary','Public Order', 'Vehicle Crime', 'Violence & Sex Offences','Other Crime', 'Shoplifting','Bike Theft', 'Drugs','Theft Person','Robbery','Weapon Possession')

###results is output from automated_2_months function
results_df1 = bind_rows(results)

write.csv(results_df1, file = 'nat_patterns_plot_data.csv')
y = c()
for(i in nice_crime_names){
  x = rep(i,26)
  
  y = append(y,x)
  
}

results_df1$nice_crime_names  = y

#### set up the groupings
acq1 = c('Bike Theft','Theft Person','Robbery')
acq2 = c('Other Theft','Burglary', 'Vehicle Crime', 'Shoplifting')
non_acq1 = c('ASB','Violence & Sex Offences')
non_acq2 = c( 'Other Crime',  'Drugs','Weapon Possession')
non_acq3 = c('Damage & Arson','Public Order')

#### set up the colours
x = palette(brewer.pal(n = 7, name = "Dark2"))
set1 = colorRampPalette(x)(15)

cols = c('Damage & Arson' = set1[1],'Other Theft' = set1[2],'ASB' = set1[3],'Burglary'= set1[4],'Public Order'= set1[13], 'Vehicle Crime'= set1[6], 'Violence & Sex Offences'= set1[7],
         'Other Crime'= set1[8], 'Shoplifting'= set1[11],'Bike Theft'= set1[10], 'Drugs'= set1[9],'Theft Person'= set1[15],'Robbery'= set1[5],'Weapon Possession'= set1[14], 'All' = set1[12])

g = grouped_rates_graph(results_df1, acq1,cols)
ggsave(filename = 'acq1.png', width = 9, height = 7)
grouped_rates_graph(results_df1, acq2,cols)
ggsave(filename = 'acq2.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq1,cols)
ggsave(filename = 'non_acq1.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq2,cols)
ggsave(filename = 'non_acq2.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq3,cols)
ggsave(filename = 'non_acq3.png', width = 9, height = 7)




####### Alll Crime 

crime.list_no_ASB = crime.list[c(1:2,4:14)]

mar_count = nrow( march_data_all%>% filter(Crime.type %in% crime.list_no_ASB))

apr_count = nrow(april_data_all%>% filter(Crime.type%in% crime.list_no_ASB))

g_df = automated_2_months(historic_data, crime.list_no_ASB, march = mar_count, april = apr_count, All = TRUE, graph = FALSE)

g_df$nice_crime_names = 'All'
grouped_rates_graph(g_df, 'All',cols)

ggsave(filename = 'all_crime.png', width = 9, height = 7)


######### bar plots


results_df = bind_rows(results) %>% filter(complete.cases(.))%>%
  mutate(percent_change = 100*(actual-Forecast)/Forecast) %>%
  mutate(upper_perc = 100*(actual - upper_CI)/upper_CI) %>%
  mutate(lower_perc = 100*(actual - lower_CI)/lower_CI)



data_to_plot = results_df %>% filter(Month == 'Mar 20') %>% mutate(percent_change = round(percent_change,1))
data_to_plot$nice_crime = nice_crime_names
data_to_plot = data_to_plot %>% arrange(desc(percent_change))
crime_levels = data_to_plot$nice_crime
data_to_plot$nice_crime = factor(data_to_plot$nice_crime, levels = crime_levels)


ggplot(data =data_to_plot , aes(x = nice_crime, y = percent_change, fill = nice_crime))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle('')+
  xlab('')+
  ylab('% Difference from Expected Rate')+
  theme_gray()+
  ylim(c(-100,200))+
  geom_errorbar(aes(ymin=lower_perc, ymax=upper_perc),
                width=.2, colour = 'blue', alpha = 0.5)+ 
  #geom_text(aes(label=percent_change, hjust = ifelse(percent_change > 0, -0.2,1)),
            #vjust = -0.5,
            #size = 3)+
  theme(legend.position = 'none', legend.title = element_blank())+
  scale_fill_manual(values = cols)+ theme(axis.text.y  = element_text( size = 15))+
  theme(axis.text.x  = element_text( size = 13))+
  theme(axis.title.x = element_text(size = 15))

ggsave(filename = 'march_bar_plot.png')

data_to_plot = results_df %>% filter(Month == 'Apr 20')%>% mutate(percent_change = round(percent_change,1))
data_to_plot$nice_crime = nice_crime_names
data_to_plot = data_to_plot %>% arrange(desc(percent_change))
crime_levels = data_to_plot$nice_crime
data_to_plot$nice_crime = factor(data_to_plot$nice_crime, levels = crime_levels)


ggplot(data =data_to_plot , aes(x = nice_crime, y = percent_change, fill = nice_crime))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle('')+
  xlab('')+
  ylab('% Difference from Expected Rate')+
  theme_gray()+
  ylim(c(-100,200))+
  geom_errorbar(aes(ymin=lower_perc, ymax=upper_perc),
                width=.2, colour = 'blue', alpha = 0.5)+ 
  #geom_text(aes(label=percent_change, hjust = ifelse(percent_change > 0, -0.2,1)),
            #vjust = -0.5,
            #size = 3)+
  theme(legend.position = 'none', legend.title = element_blank())+
  scale_fill_manual(values = cols)+ 
  theme(axis.text.y  = element_text( size = 15))+
  theme(axis.text.x  = element_text( size = 13))+
  theme(axis.title.x = element_text(size = 15))

ggsave(filename = 'april_bar_plot.png')
