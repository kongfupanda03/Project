source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
g<-ggplot(Values, aes(x = endtime, y = test_value)) +
  geom_point(alpha=0.5)+
  geom_boxplot(outlier.size=0)+
  labs(title="UMP test value",x="Endtime",y="Test Value")+
  theme(plot.title = element_text(hjust = 0.5))
####################################################

############# Create and save widget ###############
p = ggplotly(g);
internalSaveWidget(p, 'out.html');
####################################################