library(readr, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(tidyr, quietly=TRUE)

# To install gganimate and ggsoccer:
# > install.packages("devtools")
# > devtools::install_github("thomasp85/gganimate")
# > devtools::install_github("torvaney/ggsoccer")
library(gganimate)
library(ggsoccer)

frames.file <- "../data/csv/frames.csv"
events.file <- "../data/csv/events.csv"
sync.file <- "../data/csv/sync.csv"

data <- read_csv(frames.file) %>%
  mutate(is.ball=(object == 0)) %>%
  filter(team %in% c(0,1)) %>%   # Drop officials
  left_join(read_csv(sync.file)) %>%
  mutate( # Keep events matched to the ball rows only
    event=ifelse(is.ball, event, NA),
    is.event=!is.na(event)
  ) %>%
  left_join(read_csv(events.file), by="event", suffix=c(".f", ".e")) %>%
  mutate(
    team.f=factor(ifelse(is.ball, -1, team.f)),  # Give the ball its own "team"
    desc.f=sprintf("Implied Tracab clock: %.2fs", clock),
    desc.e=ifelse(is.event, sprintf("%02d:%02d %s", minute, second, event_type), "")
  ) %>%
  # Repeat the frames that are aligned to an event 20 times to simulate a pause
  mutate(weight=ifelse(is.event, 20, 1)) %>%
  uncount(weight) %>%
  arrange(clock, object) %>%
  mutate(animation.clock=cumsum(as.numeric(is.ball)))

# Take only an initial segment of data for animation development
data <- data %>% filter(clock > 60*8 & clock < 60*9)

animation <- ggplot(data, aes(x=x.f, y=y.f)) +
  geom_text(data=filter(data, is.ball), aes(x=-3500, y=3550, label=desc.f)) +
  geom_text(aes(x=3500, y=3550, label=desc.e), colour='red') +
  annotate_pitch(x_scale=105.0, y_scale=68.0, x_shift=-10500/2, y_shift=-6800/2) +
  annotate("text", x=4200, y=-3200, label="sync.soccer", fontface="italic") +
  geom_point(data=filter(data, is.ball), size=1.5) +
  geom_point(data=filter(data, !is.ball), aes(color=team.f), size=3, alpha=0.75) +
  geom_point(aes(x=x.e, y=y.e), colour="red", shape=4, size=4) +
  scale_color_manual(values=c("#034694", "#6cabdd")) +
  coord_cartesian(xlim=c(-5500, 5500), ylim=c(-3500, 3500)) +
  transition_time(animation.clock) +
  theme_pitch() +
  theme(legend.position="none")

animate(animation, fps=5, nframes=nrow(filter(data, is.ball)), renderer=ffmpeg_renderer())
