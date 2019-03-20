############getting data ############################
#: setting directory of data files
DATA_DIR <- "G://#Ethoscope_R_analysis"
list.files(DATA_DIR)
setwd(DATA_DIR)

#2: Loading libraries 
library(data.table)
library(behavr)
library(scopr)
library(sleepr)
library(ggplot2)
library(ggetho)
library(dplyr) 
library(cowplot) # to show different ggetho plots side by side with plot_grid()



#3: loading and reading metadata file
metadata <- fread("metadata.csv")
metadata

#4: Linking, link metadata with ethoscope
metadata <- link_ethoscope_metadata(metadata, 
                                    result_dir = "ethoscope_results")
print(metadata)

#5: Loading raw data without dead or escaped animals [e.g status == "Ok","Dead", "Escaped"]
## this can be used to select animals with specific criteria
metadata_subset <- metadata[loaded == "Y"]
dt <- load_ethoscope(metadata_subset,
                     FUN = sleepr::sleep_annotation, 
                     verbose=FALSE)
summary(dt)
print(dt)

#6: setting Zeitgeber (ZT0). here light starts at 06:00 AM; ZT0 is the reference hour
## caching option: when you load data, it stores a snapshot in ethoscope folder; next time you load will be faster because from snapshot

system.time(
  dt <- load_ethoscope(metadata_subset,
                       reference_hour= 07.0, 
                       FUN = sleepr::sleep_annotation,
                       cache = "ethoscope_cache",
                       verbose=FALSE)
)

#####################################################################

#dt <- dt[t %between% c(days(0), days(2.1))]
#dt[, phase := ifelse(t %% hours(24) < hours(15), "L", "D")]

#######################Plotting#####################

library(ggetho)
plot1 <- ggetho(dt, aes(y=interactions, colour=motor)) +
  stat_pop_etho() +
  stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
  facet_grid(fly_no ~ .)

plot1



plot2 <- ggetho(dt, aes(y=asleep, colour=motor)) +
  stat_pop_etho() +
  stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
  facet_grid(fly_no ~ .)

plot2

plot_grid(plot1, plot2, ncol=2,nrow=1)

#####select flies from different replication######
selected_fly = "05"
id_nchar <- nchar(as.character(dt$id[1]))
dt <- dt[,fly_id2 := substr(dt$id, id_nchar-1, id_nchar)]

plot3 <- ggetho(data = dt[fly_id2 == selected_fly],
       mapping = aes(y=asleep), colour=factor(1)) +
  stat_pop_etho() +
  stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
  facet_grid(~ replicate) +
  ggtitle(label = paste0('Fly: ', selected_fly))

plot4 <- ggetho(data = dt[fly_id2 == selected_fly],
                mapping = aes(y=interactions), colour=factor(1)) +
  stat_pop_etho() +
  stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
  facet_grid(~ replicate) +
  ggtitle(label = paste0('Fly: ', selected_fly))

plot_grid(plot3, plot4, ncol=1,nrow=2)

########plotting side by side########
plot_grid(plot1, plot2, ncol=1,nrow=2)
plot_grid(plot1, plot2, ncol=2,nrow=1)

