#Data visualizations

library(TraMineR) #need for seqformat & seqdef function 


#TODO

#look into seq def see other ways to define the colors...




# Change in states over time (Germany non absorbing) ----------------------------------------------

#Filter synced data. 
#syria_data
#The sync set still has the full time that people remain in Germany
syria_sequence_sync <- syria_sequence_sync %>% 
  group_by(pid) %>% 
  filter(!any(country %in% small_sample)) %>%
  ungroup() 

seqformat_syria <- TraMineR::seqformat(data = syria_sequence_sync, 
                                       from="SPELL", to="STS", 
                                       begin="start", end="end",
                                       id="pid",
                                       status = "country",
                                       process = FALSE,
                                       tmin = '2010-01-01')


sf <- seqdef(seqformat_syria, var=1:ncol(seqformat_syria))

seqplot(sf, type = "d", border = NA, with.legend = "right")
