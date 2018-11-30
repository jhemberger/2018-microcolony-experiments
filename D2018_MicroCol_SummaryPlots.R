# PROJECT | METADATA ======================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# September 25, 2018

# Summary plots for experiment round 1, 2, 3

library(tidyverse)
library(viridis)
library(lubridate)


# Round 1 Summary Plots ---------------------------------------------------
# **Summary plots ---------------------------------------------------------

# Final comb mass
mc1end.df %>%
  filter(!is.na(end_mass_comb)) %>%
  group_by(treatment) %>%
  summarise(mean.comb.mass = mean(end_mass_comb), se = sd(end_mass_comb) / sqrt(n())) %>%
  ggplot() + 
  #geom_col(mapping = aes(x = treatment, y = mean.comb.mass, width = 0.5)) + 
  geom_pointrange(mapping = aes(x = treatment, y = mean.comb.mass,
                                ymax = mean.comb.mass + se, 
                                ymin = mean.comb.mass - se)) +
  scale_y_continuous(limits = c(0, 10)) + 
  theme_bw()

ggplot(na.rm = TRUE) + 
  geom_line(data = mc1.df, mapping = aes(x = date, 
                                         y = true_mc_mass, 
                                         group = id, 
                                         color = treatment,
                                         na.rm = FALSE)) + 
  theme_bw()

# Total Males Produced as point range plot
mc1.df %>%
  group_by(id, treatment) %>%
  filter(id != 2.4) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  summarise(mean_males = mean(total_males), se = sd(total_males) / sqrt(n())) %>%
  ggplot() + 
  geom_pointrange(mapping = aes(x = treatment, y = mean_males, 
                                ymax = mean_males + se,
                                ymin = mean_males - se)) + 
  theme_bw()

# Total males produced as boxplot
mc1.df %>%
  group_by(id, treatment) %>%
  filter(id != 2.4) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  ggplot(mapping = aes(x = treatment, y = total_males)) + 
  geom_boxplot() + 
  theme_minimal()

# Male production normalized across microcolonies - diverging dot plot
mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(drone_z = round((total_males - mean(total_males, na.rm = TRUE)) / sd(total_males, na.rm = TRUE), digits = 2)) %>%
  mutate(drone_score = ifelse(drone_z < 0, "below", "above")) %>%
  arrange(desc(drone_z)) %>%
  mutate(z_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = z_order, y = drone_z, label = drone_z)) + 
  geom_point(stat = "identity", mapping = aes(col = drone_score), size = 8) + 
  scale_color_manual(name = "Drone Production", 
                     labels = c("Above Average", "Below Average"),
                     values = c("above" = "green2", "below" = "red2")) + 
  geom_text(color = "black", size = 2) + 
  labs(y = "Drone Production Z-Score", x = "Microcolony ID") + 
  coord_flip() + 
  theme_bw() 

# Mass gain over experiment (smoothed)
ggplot(data = mc1.df) + 
  # geom_point(mapping = aes(x = date, y = mc_mass, #- mass_box - lag(p_mass_fd),
  #                          color = treatment)) + 
  geom_smooth(span = 0.75, 
              mapping = aes(x = date, 
                            y = mc_mass, #- mass_box - lag(p_mass_fd), 
                            color = treatment,
                            fill = treatment)) + 
  theme_bw()

# Nectar consumptiion grouped by treatment
mc1.feed.df %>%
  group_by(treatment, date) %>%
  summarise(nectar_cons = mean(n_mass_cons),
            nectar_se = (sd(n_mass_cons) / (sqrt(n()))), 
            pollen_cons = mean(p_mass_cons_avg), 
            pollen_se = (sd(p_mass_cons_avg) / sqrt(n())))

mc1.feed.df %>%
  group_by(id) %>%
  mutate(cum_pollen = cumsum(p_mass_cons_avg), 
         cum_nectar = cumsum(n_mass_cons)) %>%
  group_by(date, treatment) %>%
  summarise(nectar_cons = mean(cum_nectar), 
            nectar_se = (sd(cum_nectar) / (sqrt(n()))), 
            pollen_cons = mean(cum_pollen, na.rm = TRUE), 
            pollen_se = (sd(cum_pollen, na.rm = TRUE) / (sqrt(n())))) %>%
  # ggplot() +
  # geom_pointrange(mapping = aes(x = date,
  #                     y = nectar_cons,
  #                     ymin = nectar_cons - nectar_se,
  #                     ymax = nectar_cons + nectar_se,
  #                     color = treatment)) +
  # geom_smooth(mapping = aes(x = date,
  #                           y = nectar_cons,
  #                           color = treatment), se = FALSE) + 
  # theme_minimal() #%>% 
  ggplot() +
  geom_pointrange(mapping = aes(x = date,
                                y = pollen_cons,
                                ymin = pollen_cons - pollen_se,
                                ymax = pollen_cons + pollen_se,
                                color = treatment)) +
  geom_smooth(mapping = aes(x = date,
                            y = pollen_cons,
                            color = treatment), se = FALSE) +
  theme_bw()

# Average interval mass gain across entire experiment - diverging dot plot
mc.massgain <- mc1.df %>%
  select(id, date, mc_mass, p_mass_rm, p_mass_fd, treatment, mass_box) %>%
  mutate(p_mass_cons = mc1.feed.df$p_mass_cons_avg) %>%
  replace_na(list(mc_mass = 0)) %>%
  mutate(mc_mass_diff = c(0, diff(mc1.df$mc_mass))) %>%
  #filter(id == "3.3") %>%
  mutate(mc_mass_gain = ifelse(lag(!is.na(p_mass_rm == TRUE)), 
                               mc_mass_diff + lag(p_mass_rm) - lag(p_mass_fd) + p_mass_cons, 
                               mc_mass_diff - lag(p_mass_fd) + p_mass_cons)) # this needs to be checked...

mc.massgain$mc_mass_gain[which(diff(mc.massgain$id) != 0) + 1] <- NA

# plot using z-scores
mc.massgain %>% 
  group_by(id, treatment) %>%
  summarise(mean_intv_massgain = mean(mc_mass_gain, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mass_gain_z = round((mean_intv_massgain - mean(mean_intv_massgain, na.rm = TRUE)) / sd(mean_intv_massgain, na.rm = TRUE), digits = 2)) %>%
  mutate(mass_score = ifelse(mass_gain_z < 0, "below", "above")) %>%
  arrange(desc(mass_gain_z)) %>%
  mutate(z_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = z_order, y = mass_gain_z, label = mass_gain_z)) + 
  
  geom_point(stat = "identity", mapping = aes(col = mass_score), size = 8) + 
  scale_color_manual(name = "Mass gain between feeding intervals", 
                     labels = c("Above Average", "Below Average"),
                     values = c("above" = "green2", "below" = "red2")) + 
  geom_text(color = "black", size = 2) + 
  labs(y = "Mass Gain Z-Score", x = "Microcolony ID") + 
  coord_flip() + 
  theme_bw()

# plot using raw average mass gains (still above/below 0)
mc.massgain %>% 
  group_by(id, treatment) %>%
  summarise(mean_intv_massgain = round(mean(mc_mass_gain, na.rm = TRUE), digits = 2)) %>%
  ungroup() %>%
  mutate(mass_score = ifelse(mean_intv_massgain < 0, "below", "above")) %>%
  arrange(desc(mean_intv_massgain)) %>%
  mutate(mass_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = mass_order, y = mean_intv_massgain, label = mean_intv_massgain)) + 
  geom_point(stat = "identity", mapping = aes(col = mass_score), size = 8) + 
  scale_color_manual(name = "Average change in mass between feeding intervals", 
                     labels = c("Gained Mass", "Lost Mass"),
                     values = c("above" = "slateblue", "below" = "powderblue")) + 
  geom_text(color = "white", size = 2) + 
  labs(y = "Mean feeding interval mass gain", x = "Microcolony ID") + 
  coord_flip() + 
  theme_bw()


