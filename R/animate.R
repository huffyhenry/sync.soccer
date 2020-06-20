library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

# Install my fork of ggsoccer for Tracab support.
# Can be replaced with the official CRAN version of the package in the future.
library(devtools)
install_github("https://github.com/huffyhenry/ggsoccer")
library(ggsoccer)

# Source files
frames.file <- "../data/csv/frames.csv"
events.file <- "../data/csv/events.csv"
sync.file <- "../data/csv/sync.csv"

# The segment of data to animate (in seconds, based on Tracab's implied clock)
start.clock <- 60
end.clock <- start.clock + 30

# Pitch dimensions in metres, from Tracab metadata.
pitch.length <- 105.0
pitch.width <- 68.0

# Presentation
team1.colour <- "#034694"
team2.colour <- "#6cabdd"
tracab.colour <- "black"
opta.colour <- "red"


# Load both data streams and merge them according to the sync file.
# Each row is the position of a single player or of the ball.
# Positions in frames aligned to an event are annotated with that event data.
data <- read_csv(frames.file) %>%
  # Drop officials
  filter(team != 3) %>%
  # Add matched event IDs
  left_join(read_csv(sync.file)) %>%
  # Add event information
  left_join(read_csv(events.file), by="event", suffix=c(".f", ".e")) %>%
  # Create descriptions to print (running clock / event type and time)
  mutate(
    desc.f=sprintf(
      "Implied Tracab clock - %02d:%02d.%03d",
      floor(clock) %/% 60, floor(clock) %% 60, floor(1000*(clock - floor(clock)))
    ),
    desc.e=ifelse(
      is.na(event),
      "",
      sprintf("Opta event - %02d:%02d %s", minute, second, event_type)
    )
  ) %>%
  # Repeat the ball positions aligned to an event 25 times to simulate a pause
  uncount(ifelse(object == 0 & !is.na(event), 25, 1)) %>%
  # Create a frame counter (cumulative count of ball positions)
  arrange(clock, object) %>%
  mutate(animation.clock=cumsum(as.numeric(object == 0))) %>%
  # Take only an interval of data as defined by constants above
  filter(clock > start.clock & clock < end.clock)

# Split the dataset to simplify and speed up plotting logic.
ball <- filter(data, object == 0)
players <- filter(data, object != 0)

# Animate
animation <- ggplot(ball, aes(x=x.f, y=y.f)) +
  annotate_pitch(dimensions=make_pitch_tracab(pitch.length, pitch.width)) +
  geom_text(aes(x=-25*pitch.length, y=47*pitch.width, label=desc.f), colour=tracab.colour) +
  geom_text(aes(x=25*pitch.length, y=47*pitch.width, label=desc.e), colour=opta.colour) +
  geom_point(size=1.5) +
  geom_point(data=players, aes(color=as.factor(team.f)), size=3, alpha=0.75) +
  geom_point(aes(x=x.e, y=y.e), colour=opta.colour, shape=4, size=4) +
  scale_color_manual(values=c(team1.colour, team2.colour)) +
  transition_time(animation.clock) +
  theme_pitch(aspect_ratio=pitch.width/pitch.length) +
  theme(legend.position="none")

animate(animation, nframes=nrow(ball), duration=end.clock-start.clock)

