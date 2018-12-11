library(readr, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(gganimate)

frames.file <- "../data/csv/frames.csv"
events.file <- "../data/csv/events.csv"
sync.file <- "../data/csv/sync.csv"

data <- read_csv(frames.file) %>%
  filter(team %in% c(0,1)) %>%   # Render players only
  left_join(read_csv(sync.file)) %>%
  mutate(event=ifelse(object==0, event, NA)) %>%
  left_join(read_csv(events.file), by="event", suffix=c(".f", ".e")) %>%
  mutate(team.f=factor(ifelse(object == 0, -1, team.f)))  # Give the ball its own "team"

# Scatter Tracab and Opta y coordinates to check for agreement
ggplot(data, aes(x=y.e, y=y.f)) + geom_point()

# Take only an intial segment of data for animation development
data <- data %>% head(10000)

animation <- ggplot(data, aes(x=x.f, y=y.f)) +
  geom_point(data=filter(data, team.f==-1), size=2) +
  geom_point(data=filter(data, team.f!=-1), aes(color=team.f), size=3) +
  geom_label(aes(x=x.e, y=y.e, label=type)) +
  scale_color_manual(values=c("#6cabdd", "#034694")) +
  transition_time(clock) +
  theme(legend.position="none")

animate(animation, fps=20, duration=nrow(filter(data, object==0))/20)

