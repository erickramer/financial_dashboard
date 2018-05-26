ggplot(investments, aes(month, total), group = simulation_id) +
  geom_line()
