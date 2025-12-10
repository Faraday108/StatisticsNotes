library(ggplot2)
gg_norm_region <- function(lower = -1, upper = 1, mean = mean, sd = sd) {
    lb <- mean - 3*sd
    ub <- mean + 3*sd
    ggplot2::ggplot() + 
        theme_minimal(base_size = 14) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black")) + 
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "#00674E", xlim = c(lower,upper), 
                  args = list(mean = mean, sd = sd)) +
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "lightgrey", xlim = c(lb,lower),
                  args = list(mean = mean, sd = sd)) +
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "lightgrey", xlim = c(ub,upper),
                  args = list(mean = mean, sd = sd)) + 
        stat_function(fun = dnorm, geom = "function",
                      args = list(mean = mean, sd = sd)) + 
        geom_vline(xintercept = mean, color = "black", linetype = "dashed") + 
        scale_x_continuous(breaks = c(lower,mean,upper), 
                           limits = c(lb,ub)) + 
        scale_y_continuous(labels = NULL) + 
        labs(y = "", x = "")}

gg_norm_region(87,92,90,3)
