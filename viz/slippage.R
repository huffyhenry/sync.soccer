library(readr)
library(dplyr)
library(ggplot2)

# Source files
frames.file <- "../tmp/frames.csv"
events.file <- "../tmp/events.csv"
sync.file <- "../tmp/sync.csv"

# File to save the plot to
out.file <- "../slippage.png"

# Build the plot
read_csv(frames.file, col_types=cols(), progress=FALSE) %>%
filter(object == 0) %>%
left_join(read_csv(sync.file, col_types=cols()), by="frame") %>%
inner_join(read_csv(events.file, col_types=cols()), by="event") %>%
transmute(
  half.desc=ifelse(half == 1, "First half", "Second half"),
  frame.clock=clock - 45*60*(half - 1),
  event.clock=60*minute + second + 0.5 - 45*60*(half - 1),
) %>%
ggplot(aes(x=event.clock/60, y=event.clock-frame.clock)) +
  geom_col(position="dodge") +
  scale_x_continuous(breaks=c(0, 15, 30, 45), expand=expansion(add=1)) +
  scale_y_continuous(breaks=seq(-3, 3, 1)) +
  coord_cartesian(xlim=c(0, 50), ylim=c(-3.5, 3.5)) +
  theme_bw() +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    text=element_text(family="Helvetica-Narrow")
  ) +
  labs(
    title="Clock difference between aligned events and frames (seconds)",
    x=NULL,
    y=NULL
  ) +
  facet_wrap(~half.desc, nrow=2, ncol=1, strip.position="right")

# Save the plot to file
ggsave(out.file)
