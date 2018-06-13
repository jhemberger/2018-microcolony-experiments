# ==========================================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# May 29, 2018

# Model microcolony response to landscape-simulated food treatments
# Experiment 1 dates: March 15, 2018 - May 24, 2018
# Experiment 2 dates: 
# ==========================================================================

# Import relevant packages 
library(tidyverse)
library(lubridate)
library(lme4)

##### Import/clean data #####
# Experiment 1
mc1.df <- read_csv("D2018_MicroCol_Round1.csv", skip = 2, 
                   col_names = c("id", "date", "time", "initials", "n_new_drones", 
                                 "n_drones", "n_worker_deaths", "age_class",
                                 "workers_replaced", "drones_removed",
                                 "activity", "dom_worker", "mc_mass", "p_mass_rm", 
                                 "n_mass_rm", "p_mass_fd", "n_mass_fd", "n_culled", 
                                 "frozen?", "temp", "humidity"), 
                   na = c("-", "", " ")) 
mc1.df$date <- paste(mc1.df$date, "2018", sep = "/")
mc1.df$date <- mdy(mc1.df$date)
mc1.df$date <- parse_date(mc1.df$date)


# Remove unused columns from data frame
mc1.df$initials = NULL
mc1.df$time = NULL
mc1.df$n_culled = NULL
mc1.df$dom_worker = NULL
mc1.df$`frozen?` = NULL
# Bind final observations not on MC data sheets
mc1end.df <- read_csv("D2018_MicroCol_Breakdown.csv", skip = 1,
                    col_names = c("id", "end_mc_mass", "end_mass_comb", "mass_box", "date"))
mc1end.df$treatment <- ifelse(mc1end.df$id < 2, paste("zone.1"),
                              ifelse(mc1end.df$id < 3 & mc1end.df$id > 1, paste("zone.2"),
                                     ifelse(mc1end.df$id < 4 & mc1end.df$id > 3, paste("zone.3"), paste("zone.4"))
                              )
)
mass.box.df <- data_frame(id = mc1end.df$id, mass_box = mc1end.df$mass.box)
mc1end.df$mass_box <- NULL
mc1.df <- bind_rows(mc1.df, mc1end.df)
mc1.df$date <- parse_date(mc1.df$date)
rm(mc1end.df)

# Add mass of microcolony box to all obsvs
mc1.df <- inner_join(mc1.df, mass.box.df, by = "id")

# Create treatment variable
mc1.df$treatment <- ifelse(mc1.df$id < 2, paste("zone.1"),
                           ifelse(mc1.df$id < 3 & mc1.df$id > 1, paste("zone.2"),
                                  ifelse(mc1.df$id < 4 & mc1.df$id > 3, paste("zone.3"), paste("zone.4"))
                           )
)

# Correct worker replace/drone removed to be amount actually replaced/removed
# e.g. 3/5 means 3 of 5 total were replaced/removed 
mc1.df <- separate(mc1.df, drones_removed,
                   into = c("drones_removed", "delete"),
                   sep = "[[:punct:]]", 
                   remove = TRUE)
mc1.df$delete = NULL 
# Values to be over-written <- values copied - only if drones_removed = 'yes'
mc1.df[which(mc1.df$drones_removed == "yes"), 8] <- mc1.df[which(mc1.df$drones_removed == "yes"), 4] 

##### Summarize/calculate colony growth and resource consumption #####
mc1.df <- mc1.df %>%
  group_by(id) %>%
  mutate(true_mc_mass = mc_mass - mass_box - lag(p_mass_fd))



##### Basic summary plots/tables #####
# Final comb mass
mc1.df %>%
  filter(!is.na(end_mass_comb)) %>%
  group_by(treatment) %>%
  summarise(mean.comb.mass = mean(end_mass_comb), se = sd(end_mass_comb) / sqrt(n())) %>%
  ggplot() + 
    #geom_col(mapping = aes(x = treatment, y = mean.comb.mass, width = 0.5)) + 
    geom_pointrange(mapping = aes(x = treatment, y = mean.comb.mass,
                                  ymax = mean.comb.mass + se, 
                                  ymin = mean.comb.mass - se)) +
    scale_y_continuous(limits = c(0, 10)) + 
    theme_minimal()

ggplot() + 
  geom_line(data = mc1.df, mapping = aes(x = date, 
                                         y = mc_mass, 
                                         group = id, 
                                         color = treatment), na.rm = TRUE)

ggplot(data = mc1.df) + 
  geom_smooth(span = 0.75, mapping = aes(x = date, 
                            y = mc_mass - mass_box - lag(p_mass_fd), 
                            #group = id, 
                            color = treatment), na.rm = TRUE) + 
  theme_minimal()
