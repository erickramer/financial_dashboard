portfolio_figure = function(simulations){
  stats = simulations %>%
    group_by(month) %>%
    summarize(median = median(total_value),
              max = quantile(total_value, probs = 0.95),
              min = quantile(total_value, probs = 0.05)) %>%
    mutate(year = month / 12) %>%
    select(-month) 
  
  polydata = data.frame(year = c(stats$year, rev(stats$year)),
                        value = c(stats$max, rev(stats$min)))
  
  ggplot() +
    geom_polygon(aes(year, value), data = polydata, fill = "grey") +
    geom_line(aes(year, median), lwd = 1, data = stats) +
    xlab("Year") +
    ylab("Portfolio Value") +
    scale_y_continuous(labels = scales::dollar)
}

portfolio_figure_holding = function(extremes, medians){
  ggplot() +
    geom_polygon(aes(year, value), fill = "grey", data = extremes) +
    geom_line(aes(year, value, col=holding), lwd=1, data = medians) +
    facet_grid(cols = vars(instrument)) +
    scale_y_continuous(labels = scales::dollar) +
    xlab("Year") +
    ylab("Value") +
    scale_colour_brewer(palette = "Dark2")
}

portfolio_figure_instrument = function(extremes, medians){
  ggplot() +
    geom_polygon(aes(year, value), fill = "grey", data = extremes) +
    geom_line(aes(year, value, col=instrument), lwd=1, data = medians) +
    facet_grid(cols = vars(holding)) +
    scale_y_continuous(labels = scales::dollar) +
    xlab("Year") +
    ylab("Value") +
    scale_colour_brewer(palette = "Dark2")
}