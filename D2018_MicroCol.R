# METADATA===================================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# May 29, 2018

# Model microcolony response to landscape-simulated food treatments
# Experiment 1 dates: March 15, 2018 - May 24, 2018
# Experiment 2 dates: 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

##### Import relevant packages #####
library(tidyverse)
library(lubridate)
library(lme4)
library(viridis)

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

# Export cleaned .csv ready for summary and analysis
write_csv(mc1.df, "./D2018_MicroCol_Round1_Clean.csv")
mc1.df <- read_csv("./D2018_MicroCol_Round1_Clean.csv")

##### Summarize/calculate colony growth and resource consumption #####
mc1.df <- mc1.df %>%
  group_by(id) %>%
  mutate(true_mc_mass = mc_mass - mass_box - lag(p_mass_fd))

test <- data_frame(id = mc1.df$id, 
                   date = mc1.df$date,
                   p_mass_rm = mc1.df$p_mass_rm, 
                   p_mass_fd = mc1.df$p_mass_fd)

test <- test[-c(426:449), ]
p_mass_fd <- test$p_mass_fd

a <- which(!is.na(test$p_mass_rm))
a.diff <- 3
a.diff <- append(a.diff, diff(a))
start <- a - a.diff # start index position for adding pollen
stop <- a - 1 # stop index position for adding pollen

# This works (but not with NAs - need to figure out how to group so that sums
# don't add across microcolony IDs

p_mass_fd[is.na(p_mass_fd)] <- 0
sum<-c(rep(0,length(start)))
for (i in 1:length(start)) {
  for (j in start[i]:stop[i]) {
    sum[i] <- sum[i] + p_mass_fd[j]
  }
}



lapply(list(p_mass_fd), food.calc, na.rm = TRUE)
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

ggplot(na.rm = TRUE) + 
  geom_line(data = mc1.df, mapping = aes(x = date, 
                                         y = true_mc_mass, 
                                         group = id, 
                                         color = treatment,
                                         na.rm = FALSE))

# Total Males Produced
mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  summarise(mean_males = mean(total_males), se = sd(total_males) / sqrt(n())) %>%
  ggplot() + 
    geom_pointrange(mapping = aes(x = treatment, y = mean_males, 
                                  ymax = mean_males + se,
                                  ymin = mean_males - se)) + 
    theme_minimal()
mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  ggplot(mapping = aes(x = treatment, y = total_males)) + 
    geom_boxplot() + 
    theme_minimal()

# Male production normalized by microcolony - diverging dot plot
mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(drone_z = round((total_males - mean(total_males, na.rm = TRUE)) / sd(total_males, na.rm = TRUE), digits = 2)) %>%
  mutate(drone_score = ifelse(drone_z < 0, "below", "above")) %>%
  arrange(desc(drone_z)) %>%
  mutate(z_order = factor(`id`, levels = `id`)) %>%
  #mutate(y_text_col = ifelse(id < 2, "yellow", 
                             #ifelse(id < 3, "green", 
                                    #ifelse(id < 4, "orange", "red")))) %>%
  ggplot(aes(x = z_order, y = drone_z, label = drone_z)) + 
    #geom_tile(mapping = aes(fill = treatment, width = 1, height = Inf), alpha = 0.2) + 
    #scale_fill_manual(values = "green") + 
    #scale_fill_manual(values = c("yellow", "green", "orange", "darkred")) + 
    geom_point(stat = "identity", mapping = aes(col = drone_score), size = 8) + 
    scale_color_manual(name = "Drone Production", 
                       labels = c("Above Average", "Below Average"),
                       values = c("above" = "#00ba38", "below" = "#f8766d")) + 
    geom_text(color = "white", size = 2) + 
    labs(y = "Drone Production Z-Score", x = "Microcolony ID") + 
    coord_flip() + 
    theme_bw() 

# Mass gain over experiment (smoothed)
ggplot(data = mc1.df) + 
  geom_point(mapping = aes(x = date, y = mc_mass - mass_box - lag(p_mass_fd),
                           color = treatment)) + 
  geom_smooth(span = 0.75, mapping = aes(x = date, 
                            y = mc_mass - mass_box - lag(p_mass_fd), 
                            #group = id, 
                            color = treatment)) + 
  scale_color_viridis(discrete = TRUE, option = "plasma") + 
  theme_minimal()



