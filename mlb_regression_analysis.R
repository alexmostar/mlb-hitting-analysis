#Author: Alex Mostar
#Title: MLB Breakout Hitter Analysis
#Description: Uses Statcast and batted ball metrics to identify potential breakout candidates


library(readxl)
savant_breakout = read_excel("D:/savant_breakout.xlsx")
View(savant_breakout)

library(dplyr)

savant_breakout = savant_breakout %>%
  rename(player = `last_name, first_name`)
library(readxl)
breakout_age = read_excel("D:/breakout_age.xlsx")
View(breakout_age)
library(dplyr)

savant_breakout =left_join(savant_breakout, breakout_age, by = "player")


savant_breakout = savant_breakout %>%
  mutate(barrel_rate = (barrel / pa) * 100)


#MODELCODE

model_xwoba <- lm(xwoba ~ hard_hit_percent + barrel_rate, data = savant_breakout)

# Calculate predicted xwOBA and difference
savant_breakout = savant_breakout %>%
  mutate(
    predicted_xwoba = predict(model_xwoba, newdata = savant_breakout),
    xwoba_diff = predicted_xwoba - xwoba
  )



summary(model_xwoba)








library(dplyr)
library(janitor)
library(gt)

# TABLE ONE
clean_barrel_table = savant_breakout %>%
  clean_names() %>%
  mutate(barrels_per_pa_percent = round((barrel / pa) * 100, 1)) %>%
  filter(pa >= 100, !is.na(barrels_per_pa_percent)) %>%
  arrange(desc(barrels_per_pa_percent)) %>%
  slice_head(n = 10) %>%
  select(player, pa, barrels_per_pa_percent, home_run) %>%
  gt() %>%
  cols_label(
    player = "Player",
    pa = "Plate Appearances",
    barrels_per_pa_percent = "Barrels/PA (%)",
    home_run = "Home Runs"
  ) %>%
  tab_header(
    title = md("**Top 10 Hitters by Barrels/PA % (2024, Min. 100 PA)**")
  )

clean_barrel_table





#TOP 15 BREAKOUT PLAYERS TABLE FILTERING FOR AGE AND PA
library(gt)

savant_breakout %>%
  arrange(desc(predicted_xwoba)) %>%
  filter(age < 29, pa < 250) %>%
  select(player, age, pa, hard_hit_percent, barrel_rate, xwoba, predicted_xwoba) %>%
  head(15) %>%
  gt() %>%
  fmt_number(
    columns = c(hard_hit_percent),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(barrel_rate),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(predicted_xwoba),
    decimals = 3
  ) %>%
  cols_label(
    player = "Player",
    age = "Age",
    pa = "PA",
    hard_hit_percent = "Hard Hit %",
    barrel_rate = "Barrels/PA %",
    xwoba = "xwOBA",
    predicted_xwoba = "Predicted xwOBA"
  ) %>%
  tab_header(
    title = md("**Top 15 Players in Predicted xwOBA**")
  )






# ARANDA COMPARISON TABLE
aranda_stats = savant_breakout %>%
  filter(player == "Aranda, Jonathan") %>%
  transmute(
    Player = "Jonathan Aranda",
    launch_angle = round(launch_angle_avg, 1),
    exit_velocity = round(exit_velocity_avg, 1),
    hard_hit = round(hard_hit_percent, 1),
    barrel_rate = round(barrel / pa * 100, 1),  # Use actual Barrels/PA %
    barrels = barrel,
    k_percent = round(k_percent, 1),
    bb_percent = round(bb_percent, 1),
    woba = round(woba, 3),
    xwoba = round(xwoba, 3)
  )

# MLB Averages (min 100 PA)
mlb_avg_stats = savant_breakout %>%
  filter(pa >= 100) %>%
  summarise(
    Player = "MLB Average",
    launch_angle = round(mean(launch_angle_avg, na.rm = TRUE), 1),
    exit_velocity = round(mean(exit_velocity_avg, na.rm = TRUE), 1),
    hard_hit = round(mean(hard_hit_percent, na.rm = TRUE), 1),
    barrel_rate = round(mean(barrel / pa, na.rm = TRUE) * 100, 1),
    barrels = round(mean(barrel, na.rm = TRUE), 1),
    k_percent = round(mean(k_percent, na.rm = TRUE), 1),
    bb_percent = round(mean(bb_percent, na.rm = TRUE), 1),
    woba = round(mean(woba, na.rm = TRUE), 3),
    xwoba = round(mean(xwoba, na.rm = TRUE), 3)
  )

# Combine both rows into a final table
comparison_table <- bind_rows(aranda_stats, mlb_avg_stats) %>%
  gt() %>%
  cols_label(
    launch_angle = "Launch Angle (°)",
    exit_velocity = "Exit Velocity (mph)",
    hard_hit = "Hard Hit %",
    barrel_rate = "Barrels/PA (%)",
    barrels = "Barrels",
    k_percent = "K %",
    bb_percent = "BB %",
    woba = "wOBA",
    xwoba = "xwOBA"
  ) %>%
  tab_header(
    title = md("**Jonathan Aranda vs. MLB Average (2024, Min. 100 PA)**")
  )

print(comparison_table)

















#ARANDA 2024 VS 2025 TABLE

aranda_growth =data.frame(
  Metric = c("Plate Appearances", "Launch Angle (°)", "Exit Velocity (mph)", "Hard Hit %", "Barrels/PA (%)",
             "Barrels", "K %", "BB %", "wOBA", "xwOBA"),
  Aranda_2024 = c(143, 11.0, 91.9, 48.5, 11.2, 16, 22.4, 8.4, 0.320, 0.362),
  Aranda_2025_as_of_616 = c(257, 13.4, 92.5, 54.4, 6.7, 17, 23.7, 11.1, 0.395, 0.400)
)

aranda_growth %>%
  gt() %>%
  fmt_number(
    columns = c("Aranda_2024", "Aranda_2025_as_of_616"),
    decimals = 0,
    rows = Metric == "Plate Appearances"
  ) %>%
  fmt_number(
    columns = c("Aranda_2024", "Aranda_2025_as_of_616"),
    decimals = 1,
    rows = Metric %in% c("Launch Angle (°)", "Exit Velocity (mph)", "Hard Hit %", "Barrels/PA (%)", "Barrels", "K %", "BB %")
  ) %>%
  fmt_number(
    columns = c("Aranda_2024", "Aranda_2025_as_of_616"),
    decimals = 3,
    rows = Metric %in% c("wOBA", "xwOBA")
  ) %>%
  cols_label(
    Aranda_2024 = "2024",
    Aranda_2025_as_of_616 = "2025 (as of 6/16)"
  ) %>%
  tab_header(
    title = md("**Jonathan Aranda: 2024 vs 2025 Performance**")
  )










#JERAR ENCARNACION 


encarnacion_stats = savant_breakout %>%
  filter(player == "Encarnacion, Jerar") %>%
  transmute(
    Player = "Jerar Encarnacion",
    pa = pa,
    launch_angle = round(launch_angle_avg, 1),
    exit_velocity = round(exit_velocity_avg, 1),
    hard_hit = round(hard_hit_percent, 1),
    barrel_rate = round(barrel_rate, 1),
    barrels = barrel,
    k_percent = round(k_percent, 1),
    bb_percent = round(bb_percent, 1),
    woba = round(woba, 3),
    xwoba = round(xwoba, 3)
  )

# Combine both into a gt table
encarnacion_table <- bind_rows(encarnacion_stats, mlb_avg_stats) %>%
  gt() %>%
  cols_label(
    pa = "PA",
    launch_angle = "Launch Angle (°)",
    exit_velocity = "Exit Velocity (mph)",
    hard_hit = "Hard Hit %",
    barrel_rate = "Barrels/PA (%)",
    barrels = "Barrels",
    k_percent = "K %",
    bb_percent = "BB %",
    woba = "wOBA",
    xwoba = "xwOBA"
  ) %>%
  tab_header(
    title = md("**Jerar Encarnacion vs. MLB Average (2024, Min. 100 PA)**")
  )
encarnacion_table



# DOMINIC CANZONE
canzone_stats = savant_breakout%>%
  filter(player == "Canzone, Dominic") %>%
  transmute(
    Player = "Dominic Canzone",
    pa = pa,
    launch_angle = round(launch_angle_avg, 1),
    exit_velocity = round(exit_velocity_avg, 1),
    hard_hit = round(hard_hit_percent, 1),
    barrel_rate = round(barrel_rate, 1),
    barrels = barrel,
    k_percent = round(k_percent, 1),
    bb_percent = round(bb_percent, 1),
    woba = round(woba, 3),
    xwoba = round(xwoba, 3)
  )


# Combine both into a gt table
canzone_comparison_table =bind_rows(canzone_stats, mlb_avg_stats) %>%
  gt() %>%
  cols_label(
    pa = "PA",
    launch_angle = "Launch Angle (°)",
    exit_velocity = "Exit Velocity (mph)",
    hard_hit = "Hard Hit %",
    barrel_rate = "Barrels/PA (%)",
    barrels = "Barrels",
    k_percent = "K %",
    bb_percent = "BB %",
    woba = "wOBA",
    xwoba = "xwOBA"
  ) %>%
  tab_header(
    title = md("**Dominic Canzone vs. MLB Average (2024, Min. 100 PA)**")
  )
canzone_comparison_table



#SCATTERPLOT


library(ggplot2)

ggplot(savant_breakout, aes(x = barrel_rate, y = xwoba)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(
    data = subset(savant_breakout, player %in% c("Aranda, Jonathan", "Encarnacion, Jerar", "Canzone, Dominic")),
    aes(color = player),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Aranda, Jonathan" = "#08306B",      # Dark blue
      "Encarnacion, Jerar" = "#E87722",    # Orange
      "Canzone, Dominic" = "#30D5C8"       # Turquoise
    )
  ) +
  labs(
    title = "Barrel Percentage vs. xwOBA",
    x = "Barrels/PA (%)",
    y = "xwOBA",
    color = "Player"
  ) +
  theme_bw()

