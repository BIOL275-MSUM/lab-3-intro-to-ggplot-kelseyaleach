

firefly_data <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q19FireflySpermatophoreMass.csv")

distinct(firefly_data, spermatophoreMass)
count(firefly_data, spermatophoreMass)

ggplot(data = firefly_data) + 
  geom_histogram(mapping = aes(x = spermatophoreMass), binwidth = 0.01, 
                 boundary = 0, closed = "left",
                 fill = "#C5351B", color = "black") +
  labs(x = "Spermatophore", y = "Frequency (number of individuals)") +
  scale_y_continuous(breaks = seq(0, 12, 2), limits = c(0, 12),
                     expand = expansion(mult=0)) +
  scale_x_continuous(breaks = seq(0, 0.2, 0.02)) +
  theme_classic() + 
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1))
  )
