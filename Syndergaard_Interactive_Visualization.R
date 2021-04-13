# Import necessary packages and set working directory
library(tidyverse)
library(baseballr)
library(htmlwidgets)
library(plotly)
library(crosstalk)
library(DT)

# Create vector of pitcher names and look up IDs
playerid_lookup('Syndergaard')
  
syndergaard_id <- playerid_lookup('Syndergaard')$mlbam_id

# Syndergaard
# Scrape Savant to get Pitch Movement for Break Chart
syndergaard_savant_scrape2015 <- scrape_statcast_savant(start_date = "2015-03-01",
                                                        playerid = syndergaard_id, player_type='pitcher')
syndergaard_savant_scrape2016 <- scrape_statcast_savant(start_date = "2016-03-01",
                                                        playerid = syndergaard_id, player_type='pitcher')
syndergaard_savant_scrape2017 <- scrape_statcast_savant(start_date = "2017-03-01",
                                                        playerid = syndergaard_id, player_type='pitcher')
syndergaard_savant_scrape2018 <- scrape_statcast_savant(start_date = "2018-03-01",
                                                        playerid = syndergaard_id, player_type='pitcher')
syndergaard_savant_scrape2019 <- scrape_statcast_savant(start_date = "2019-03-01",
                                                        playerid = syndergaard_id, player_type='pitcher')

# Combine
syndergaard_savant_scrape <- rbind(syndergaard_savant_scrape2015,
                                   syndergaard_savant_scrape2016, 
                                   syndergaard_savant_scrape2017,
                                   syndergaard_savant_scrape2018, 
                                   syndergaard_savant_scrape2019)
syndergaard_savant_scrape$game_year <- year(as.Date(as.character(syndergaard_savant_scrape$game_year), format = '%Y'))
syndergaard_savant_scrape$pitch_type <- as.factor(syndergaard_savant_scrape$pitch_type)
syndergaard_savant_scrape$pitch_name <- as.factor(syndergaard_savant_scrape$pitch_name)
syndergaard_savant_scrape$pitch_type <- syndergaard_savant_scrape$pitch_type %>% plyr::revalue(c('FT' = 'SI'))
syndergaard_savant_scrape$pitch_name <- syndergaard_savant_scrape$pitch_name %>% plyr::revalue(c('2-Seam Fastball' = 'Sinker'))

# Put movement in inches
syndergaard_savant_scrape <- syndergaard_savant_scrape %>% 
  mutate(pfx_x = round(pfx_x * 12, 1), pfx_z = round(pfx_z * 12, 1))

# Remove extraneous pitch types
syndergaard_savant_scrape <- syndergaard_savant_scrape %>%
  filter(pitch_type != 'PO' & pitch_type != 'IN' & pitch_type != 'null') %>%
  droplevels()

# Identify extraneous columns
colnames(syndergaard_savant_scrape)

# Select only necessary columns
syndergaard_savant_scrape <- syndergaard_savant_scrape %>% 
  select(1:10, 15, 24:26, 27:31, 54:58, 71:73, 76:79)

# Create Syndergaard pfx DataFrame
syndergaard_pfx <- syndergaard_savant_scrape %>% 
  group_by(game_year, player_name, pitcher, pitch_type, pitch_name) %>%
  summarize(pitches = n(),
            pfx_x = round(mean(pfx_x, na.rm = TRUE), 1), 
            pfx_z = round(mean(pfx_z, na.rm = TRUE), 1), 
            speed = round(mean(release_speed, na.rm = TRUE), 1),
            spin_rate = round(mean(release_spin_rate, na.rm = TRUE)),
            bauer_units = round(spin_rate/speed, 1))

syndergaard_pfx <- syndergaard_pfx %>% group_by(game_year) %>% mutate(total_pitches = sum(pitches))
syndergaard_pfx <- syndergaard_pfx %>% mutate(percent_usage = round(pitches / total_pitches * 100, 1))
syndergaard_pfx <- syndergaard_pfx[, c(1:5, 10, 9, 13, 6:8, 11:12)]
syndergaard_pfx

# Charts
# Create Grid to make Breaks clearer
topGrid <- 30
botGrid <- -30
centerGrid <- 0
rightGrid <- 30
leftGrid <- -30
Grid <- data.frame(
  x=c(centerGrid, centerGrid, centerGrid, leftGrid, rightGrid),
  y=c(topGrid, botGrid, centerGrid, centerGrid, centerGrid)
)

# Syndergaard Average Break Chart
syndergaard_break_chart_avg <- syndergaard_pfx %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_name)) + geom_point(shape = 20, size = 7) + 
  geom_path(aes(x, y), data=Grid, lwd=2, col="black") + 
  ylim(-30, 30) + xlim(-30, 30) + xlab('Horizontal Movement (in.)') + ylab('Vertical Movement (in.)') + 
  ggtitle('Syndergaard Break Charts by Year') +
  facet_wrap(~ game_year)

syndergaard_break_chart_avg

# Save as preliminary widget
noah_widget <- plotly::ggplotly(syndergaard_break_chart_avg)
htmlwidgets::saveWidget(as_widget(noah_widget), "noah_widget.html")

# Animate movement by year
syndergaard_break_chart_animated <- syndergaard_pfx %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_name)) + geom_point(shape = 20, size = 7) + 
  geom_path(aes(x, y), data=Grid, lwd = 2, col = "black") + 
  ylim(-30, 30) + xlim(-30, 30) + xlab('Horizontal Movement (in.)') + ylab('Vertical Movement (in.)') + 
  ggtitle('Syndergaard Break Charts by Year') +
  scale_color_discrete(name = "Pitch") +
  transition_time(as.integer(game_year))

animate(syndergaard_break_chart_animated, renderer = gifski_renderer())
anim_save("noah_pitch_movement.gif")

# Improve upon preliminary animation by showing the year as it passes
syndergaard_break_chart_animated2 <- syndergaard_pfx %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_name)) + geom_point(shape = 20, size = 7) + 
  geom_path(aes(x, y), data = Grid, lwd = 2, col="black") + 
  ylim(-30, 30) + xlim(-30, 30) +
  ggtitle('Syndergaard Break Charts by Year') +
  scale_color_discrete(name = "Pitch") +
  labs(title = 'Year: {frame_time}', x = 'Horizontal Movement (in.)', y = 'Vertical Movement (in.)') +
  transition_time(as.integer(game_year)) +
  ease_aes('linear')

animate(syndergaard_break_chart_animated2, duration = 10, fps = 10, width = 500, height = 500, renderer = gifski_renderer())
anim_save("noah2.gif")

# Make interactive
syndergaard_break_chart_interactive <- syndergaard_pfx %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_name, frame = game_year)) + geom_point(shape = 20, size = 7) +
  geom_path(aes(x, y), data = Grid, lwd = 2, col = "black", inherit.aes = FALSE) + 
  ylim(-30, 30) + xlim(-30, 30) +
  scale_color_discrete(name = "Pitch") +
  labs(title = "Noah Syndergaard's Pitch Evolution", x = 'Horizontal Movement (in.)', y = 'Vertical Movement (in.)')

syndergaard_break_chart_interactive <- ggplotly(syndergaard_break_chart_interactive) %>% highlight("plotly_hover")

noah_widget_revised <- plotly::ggplotly(syndergaard_break_chart_interactive)
htmlwidgets::saveWidget(as_widget(noah_widget_revised), "noah_widget_revised.html")
