mc.end.all.df <- mc.end.all.df %>%
  mutate(unique = paste(round, id, sep = "_"))


mc.all.masscomp.df <- mc.feed.all.fix.df
mc.all.masscomp.df$rep <- as.character(mc.all.masscomp.df$rep)
mc.all.masscomp.df$id <- as.character(mc.all.masscomp.df$id)
mc.all.masscomp.df <- mc.all.masscomp.df %>%
  mutate(unique = temp) %>%
  group_by(unique) %>%
  filter(fd_day == 16) %>%
  left_join(mc.end.all.df, by = "unique")
plot(mc.all.masscomp.df$end_mass_comb ~ mc.all.masscomp.df$mc_mass_true)
abline(fit <- lm(mc.all.masscomp.df$end_mass_comb ~ mc.all.masscomp.df$mc_mass_true), col = "red")
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)))

+anova(lme(end_mass_comb ~ mc_mass_true,
    random = ~ 1 | id.x,
    data = mc.all.masscomp.df))


Anova(lmerTest::lmer(end_mass_comb ~ trt_pulse * trt_total * rep + (1 | id),
               data = mc.end.all.df,
               na.action = na.exclude))

endmass.lmm <- lme(end_mass_comb ~ trt_pulse * trt_total * rep,
                   random = ~ 1 | id,
                   data = mc.end.all.df,
                   # contrasts = list(trt_pulse = contr.sum,
                   #                  trt_total = contr.sum,
                   #                  round = contr.sum),
                   na.action = na.exclude)
plotNormalHistogram(residuals(endmass.lmm))
plot(residuals(endmass.lmm),
     fitted(endmass.lmm))
plotNormalHistogram(endmass.lmm$end_mass_comb)
plot(endmass.lmm, which = 1 :4)

summary(endmass.lmm)
Anova(endmass.lmm)


example.feed.calc.df <- mc.feed.all.df %>%
  filter(rep == 2) %>%
  filter(id == "2.2")
write_csv(example.feed.calc.df, "./example_feed.csv")  


mc1.feed.fix.df <- mc.feed.all.df %>%
  filter(rep == 1)

mc1.feed.fix.df <- mc1.feed.fix.df %>%
  mutate(n_fd_days = ifelse(p_mass_cons != 0,
                            fd.days.r1,
                            NA)) %>%
  mutate(p_mass_cons_avg = p_mass_cons / n_fd_days) %>%
  mutate_at(c("p_mass_cons_avg"), list(lead), n = 1)

mc1.feed.fix.df$p_mass_cons_avg[mc1.feed.fix.df$fd_day == 17] <- 0
mc1.feed.fix.df <- fill(mc1.feed.fix.df, 
                        p_mass_cons_avg, 
                        .direction = "up")
mc1.feed.fix.df <- mc1.feed.fix.df %>%
  ungroup() %>%
  mutate(mc_mass_true = ifelse(
    fd_day_count == 1,
    ((mc_mass - mc_mass_init) + p_mass_cons_avg),
    ifelse(
      fd_day_count == 2,
      (((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) + p_mass_cons_avg),
      ifelse(
        fd_day_count == 3,
        ((((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) - lag(p_mass_fd, n = 2)) + p_mass_cons_avg),
        ifelse(fd_day_count == 0,
               ((((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) - lag(p_mass_fd, n = 2)) - lag(p_mass_fd, n = 3)) + p_mass_cons_avg, 0)
      )
    )
  ))
mc1.feed.fix.df <- mc1.feed.fix.df %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(mc_mass_true_intrp = na.approx(mc_mass_true, 
                                        maxgap = 4,
                                        rule = 2)) %>%
  mutate(mc_mass_true = mc_mass_true_intrp)
mc1.feed.fix.df$id <- as.factor(mc1.feed.fix.df$id)
mc.feed.all.fix.df <- mc1.feed.fix.df %>%
  select(id, date, fd_day, treatment, p_mass_cons, p_mass_cons_avg, n_mass_cons, mc_mass_true, pulse_cat) %>%
  filter(id != 2.4) %>%
  bind_rows(mc2.feed.df %>%
              select(id, date, fd_day, treatment, p_mass_cons, p_mass_cons_avg, n_mass_cons, mc_mass_true, pulse_cat) %>%
              filter(id != 1.4),
            .id = "rep") %>%
  bind_rows(mc3.feed.df %>%
              select(id, date, fd_day, treatment, p_mass_cons, p_mass_cons_avg, n_mass_cons, mc_mass_true, pulse_cat) %>%
              filter(id != 2.7)) %>%
  mutate(trt_pulse = ifelse(id >= 3, 
                            "pulse",
                            "constant"),
         trt_total = ifelse(id < 2 | id > 4,
                            "60",
                            "100")) %>%
  ungroup() %>%
  group_by(rep, id) %>%
  mutate(mc_mass_true = na.approx(mc_mass_true,
                                  maxgap = 2,
                                  rule = 3))

test <- mc.end.all.df %>%
  mutate(unique = paste(rep, id, 
                        sep = ".")) %>%
  filter(unique != "1.2.4") %>%
  filter(unique != "2.1.4") %>%
  filter(unique != "3.2.7")


mc.pollen.cum.df <- mc.feed.all.cum.df %>%
  ungroup() %>%
  group_by(rep, id) %>%
  mutate(cum_pollen_step = cumsum(p_mass_cons))
pcons.step.gwm.df <- groupwiseMean(cum_pollen_step ~ trt_total * trt_pulse + fd_day,
                                   data = mc.pollen.cum.df,
                                   conf = 0.95,
                                   digits = 3)
pcons.step.gwm.df %>%
  mutate(treatment = ifelse(trt_total == 100 & trt_pulse == "constant",
                            "zone.2",
                            ifelse(trt_total == 100 & trt_pulse == "pulse",
                                   "zone.3",
                                   ifelse(trt_total == 60 & trt_pulse == "constant",
                                          "zone.1",
                                          "zone.4")))) %>%
  ggplot(mapping = aes(x = fd_day,
                       y = Mean,
                       group = treatment)) + 
  annotate("rect",
           xmin = 3.5,
           xmax = 6.5,
           ymin = -Inf,
           ymax = Inf,
           fill = "black",
           alpha = 1) +
  annotate("rect",
           xmin = 10.5,
           xmax = 13.5,
           ymin = -Inf,
           ymax = Inf,
           fill = "black",
           alpha = 1) +
  geom_errorbar(
    mapping = aes(ymin = Trad.lower,
                  ymax = Trad.upper),
    width = 0,
    size = 0.7,
    #position = position_dodge(0.2),
    color = "black") + 
  geom_point(mapping = aes(color = treatment),
             size = 4,
             #position = position_dodge(0.2),
             alpha = 1) + 
  geom_line(mapping = aes(color = treatment),
            alpha = 1,
            size = 1.5) +
  # geom_smooth(mapping = aes(color = treatment),
  #             alpha = 0.5,
  #             size = 2.0,
  #             se = FALSE,
  #             span = 0.6) + 
  theme_microcol() + 
  scale_color_manual(values = c("#9FC3EA", 
                                "#172A3A", 
                                "#E6AF2E",
                                "#F3E37C"),
                     labels = c("50% - Constant",
                                "100% - Constant",
                                "100% - Variable",
                                "50% - Variable"),
                     name = "Treatment") + 
  theme(legend.justification = c(0, 1), 
        legend.position = c(0, 1)) + 
  facet_grid(rows = vars(trt_pulse)) + 
  ylab("Least square mean cumulative pollen consumption") +
  xlab("Interval Feeding Day") + 
  scale_x_continuous(breaks = unique(pcons.step.gwm.df$fd_day))
