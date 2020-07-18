library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(ggsoccer)


# Source files
frames.file <- "../tmp/frames.csv"
events.file <- "../tmp/events.csv"
sync.file <- "../tmp/sync.csv"

# File to save animation to
anim.file <- "../clip.gif"

# The segment of data to animate (in seconds, based on Tracab's implied clock)
start.clock <- 60*67 + 10
end.clock <- start.clock + 30

# Pitch dimensions in metres, from Tracab metadata.
pitch.length <- 105.0
pitch.width <- 68.0

# Presentation
team1.colour <- "#e6b800"
team2.colour <- "#6cabdd"
tracab.colour <- "black"
opta.colour <- "red"
annotation <- "sync.soccer (full algo)"
font <- "Helvetica-Narrow"


# Load both data streams and merge them according to the sync file.
# Each row is the position of a single player or of the ball.
# Positions in frames aligned to an event are annotated with that event data.
data <- read_csv(frames.file, col_types=cols()) %>%
  # Drop officials
  filter(team != 3) %>%
  # Add matched event IDs
  left_join(read_csv(sync.file, col_types=cols()), by="frame") %>%
  # Add event information
  left_join(read_csv(events.file, col_types=cols()), by="event", suffix=c(".f", ".e")) %>%
  # Create descriptions to print (running clock / event type and time)
  mutate(
    desc.f=sprintf(
      "Implied frame clock - %02d:%02d.%03d",
      floor(clock) %/% 60, floor(clock) %% 60, floor(1000*(clock - floor(clock)))
    ),
    desc.e=ifelse(
      is.na(event),
      "",
      sprintf("Event - %02d:%02d %s", minute, second, event_type)
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
  annotate("text", x=40*pitch.length, y=-47*pitch.width, label=annotation, size=2.5) +
  transition_time(animation.clock) +
  theme_pitch(aspect_ratio=pitch.width/pitch.length) +
  theme(legend.position="none", text=element_text(family=font))

animate(animation, nframes=nrow(ball), duration=end.clock-start.clock)

anim_save(anim.file)
