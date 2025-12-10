#spacer <- function(space = 2) {
#    sprintf('::: {pdf}\n\\vspace{%dcm}\n:::\n\n:::: {html}\n::: {style="height: #%dpx;"}\n:::\n::::', space, space*25)
#}

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

# Plotting function that shows a normal distribution with labeled alpha/2 regions
# and Confidence Level as CL=1-alpha
gg_norm_CI <- function(lower = -2, upper = 2, mean = mean, sd = sd) {
    lb <- mean - 3*sd
    ub <- mean + 3*sd
    ggplot2::ggplot() + 
        theme_minimal(base_size = 14) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"), 
              axis.ticks.x = element_line(linewidth = 1)) + 
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
        scale_x_continuous(breaks = c(lower,mean,upper), 
                           limits = c(lb,ub), 
                           labels = c(expression(bar(x)-EBM), 
                                      expression(bar(x)), 
                                      expression(bar(x)+EBM))) + 
        scale_y_continuous(labels = NULL) + 
        annotate("text", 
                 x = c(-2.5,2.5,2), 
                 y = c(.1,.1,.25), 
                 label = c("alpha/2", "alpha/2","CL*'='*1-alpha"), parse = TRUE) + 
        geom_segment(aes(x = c(-2.5, 2.5,1.5), 
                         y = c(.08, .08,.25), 
                         xend = c(-2.25, 2.25,.75), 
                         yend = c(.020, 0.020,.25)), 
                     arrow = arrow(length = unit(.25, "cm"))) + 
        labs(y = "", x = "")}

# Plotting function for a normal distribution that shows a highlighted region
# for the confidence interval, and area above and below. 
gg_norm_CI_CL <- function(CL = .9, mean = 0, sd = 1, mean_label = FALSE) {
    lb <- mean - 3*sd # plot lower bound
    ub <- mean + 3*sd # plot upper bound
    alpha <- (1-CL)
    CL_b <- qnorm(1-alpha/2) # Confidence Interval bound
    x_label1 <- (2*mean + sd*(CL_b+3))/2 # interval label x-value
    x_label2 <- (2*mean - sd*(CL_b+3))/2 # interval label x-value
    y_max <- 1/(sd*sqrt(2*pi))
    ggplot2::ggplot() + 
        theme_minimal(base_size = 14) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black")) + 
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "#00674E", xlim = c(mean-CL_b*sd,mean + CL_b*sd), 
                  args = list(mean = mean, sd = sd)) +
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "lightgrey", xlim = c(lb,mean-CL_b*sd),
                  args = list(mean = mean, sd = sd)) +
        geom_area(aes(c(lb,ub)), 
                  stat = "function", fun = dnorm, 
                  fill = "lightgrey", xlim = c(ub,mean+CL_b*sd),
                  args = list(mean = mean, sd = sd)) + 
        stat_function(fun = dnorm, geom = "function",
                      args = list(mean = mean, sd = sd)) +
        scale_y_continuous(labels = NULL) + 
        geom_segment(aes(x = c(mean-2.5*sd,mean+2.5*sd,mean+2*sd), 
                         y = c(y_max*.3, y_max*.3, y_max*.8), 
                         xend = c(x_label2, x_label1,mean+.75*sd), 
                         yend = c(y_max*.1, y_max*.1, y_max*.7)), 
                     arrow = arrow(length = unit(.25, "cm"))) + 
        annotate("label", 
                 label.size = 0,
                 x = c(mean-2.5*sd,mean+2.5*sd,mean+2*sd), 
                 y = c(y_max*.3,y_max*.3,y_max*.8), 
                 label = c(alpha/2, alpha/2,paste("CL =",CL)), 
                 fill = "white") + 
        labs(y = "", x = "") + 
        scale_x_continuous(breaks = if (mean_label) c(mean) else NULL) + 
        if (mean_label) geom_vline(aes(xintercept = mean), 
                                   color = "black", linetype = "dashed") else NULL
}


gg_t <- function(df = c(5)) {
    lb <- -4.5
    ub <- 4.5
    
    ggplot() +
        theme_minimal(base_size = 14) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black")) +
        scale_x_continuous(breaks = 2 * (-2:2), limits = c(lb, ub)) +
        scale_y_continuous(labels = NULL) +
        labs(y = "", x = "") +
        purrr::map(df, ~ stat_function(fun = dt, geom = "function",
                                       args = list(df = .x),
                                       aes(color = factor(.x)))) +
        scale_color_discrete(name = "df", 
                             labels = lapply(df, function(d) bquote(nu == .(d))))
}

# Example usage
#gg_t(c(2, 5, 10, 30))

# --- Setup a custom theme for notes ---
theme_notes <- function(show_axis_titles = FALSE) {
    t <- theme_minimal(base_size = 14, base_family = "serif") +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(size = 12),
            panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black", linewidth = 0.5),
            panel.border = element_blank()
        )
    
    if (!show_axis_titles) {
        t <- t + theme(axis.title = element_blank())
    }
    
    return(t)
}
