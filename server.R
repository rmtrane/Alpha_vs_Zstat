library(shiny)
library(tidyverse)
library(grid)
library(data.table)

theme_set(theme_bw())

scale_color_continuous <- scale_colour_continuous <- function(...) scale_color_viridis_c(...)
scale_color_discrete <- scale_colour_discrete <- function(...) scale_color_viridis_d(...)

scale_fill_continuous <- function(...) scale_fill_viridis_c(...)
scale_fill_discrete <- function(...) scale_fill_viridis_d(...)


# Set theme
theme_set(theme_bw())

fontsize <- 18

# Tibble with x-values
xs <- data.table(x = seq(-4, 4, by = 0.005))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$main_plot <- renderPlot({
        if(input$control == "z_obs"){
            z_stat <- input$z_stat
            
            if(input$HA == "less")
                p_value <- pnorm(z_stat)
            
            if(input$HA == "greater")
                p_value <- pnorm(z_stat, lower.tail = F)
            
            if(input$HA == "two-sided")
                p_value <- 2*pnorm(abs(z_stat), lower.tail = F)
            
            p_value <- round(p_value, digits = 2)
            z_kind <- "obs"
            p_value_text <- paste("p-value =", p_value)
        } else {
            p_value <- input$p_value
            
            if(input$HA == "less")
                z_stat <- qnorm(input$p_value)
            
            if(input$HA == "greater")
                z_stat <- qnorm(input$p_value, lower.tail = F)
            
            if(input$HA == "two-sided")
                z_stat <- qnorm(input$p_value/2, lower.tail = F)
            
            z_stat <- round(z_stat, digits = 2)
            z_kind <- "critical"
            p_value_text <- bquote(alpha == .(p_value))
        }
        
        base_plot <- ggplot(xs, aes(x = x)) +
            stat_function(fun = dnorm) +
            geom_vline(data = data.frame(),
                       aes(xintercept = z_stat, color = 'z_obs'),
                       linetype = 'dashed') +
            scale_x_continuous(breaks = c(-4:-1, 1:4, 0),
                               labels = c(-4:-1, 1:4, bquote(atop(0, (mu[0])))),
                               sec.axis = sec_axis(trans = function(x) x,
                                                   breaks = z_stat,
                                                   labels = c(bquote(bold(z[obs] == .(z_stat)))))) +
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
            scale_color_manual(values = 'red') +
            labs(x = '', y = '', color = '', fill = '') +
            theme(legend.position = 'none',
                  text = element_text(size = fontsize),
                  axis.text.x.top = element_text(face = 'bold'),
                  axis.ticks.x.top = element_blank())
        
        if(input$HA == 'less'){
            tmp_plot <- base_plot +
                geom_area(data = xs[x < z_stat,],
                          aes(x = x, y = dnorm(x)),
                          fill = 'red', alpha = 0.5)
        }
        
        if(input$HA == 'greater'){
            p_value <- round(pnorm(z_stat, lower.tail = F), 5)
            
            tmp_plot <- base_plot +
                geom_area(data = xs[x > z_stat,], # %>% filter(x > z_stat),
                          aes(x = x, y = dnorm(x)),
                          fill = 'red', alpha = 0.5)
        }
        
        if(input$HA == 'two-sided'){
            tmp_plot <- base_plot +
                geom_vline(data = data.frame(),
                           aes(xintercept = -z_stat, color = 'z_obs'),
                           linetype = 'dashed') +
                geom_area(data = xs[x > abs(z_stat),],#%>% filter(x > abs(z_stat)),
                          aes(x = x, y = dnorm(x)),
                          fill = 'red', alpha = 0.5) +
                geom_area(data = xs[x < -abs(z_stat),],# %>% filter(x < -abs(z_stat)),
                          aes(x = x, y = dnorm(x)),
                          fill = 'red', alpha = 0.5) +
                scale_x_continuous(breaks = c(-4:-1, 1:4, 0),
                                   labels = c(-4:-1, 1:4, bquote(0 == mu[0])),
                                   sec.axis = sec_axis(trans = function(x) x,
                                                       breaks = c(z_stat, -z_stat),
                                                       labels = c(bquote(bold(z[.(z_kind)] == .(z_stat))), bquote(bold(-z[.(z_kind)] == .(-z_stat))))))
        }
        
        # p_value_text <- ifelse(input$control == "z_obs", paste("p-value =", p_value),
        #                        bquote(alpha == .(p_value)
        # lhs_pval <- if_else(input$control == "z_obs", "p-value", "alpha")
        
        
        p_value_label <- grobTree(textGrob(p_value_text, x = 0.975, y = 0.95,
                                           hjust = 1, gp = gpar(fontsize = fontsize, fontface = 'bold', col = 'red')))
        
        out_plot <- tmp_plot +
            #annotate(geom = 'text', x = 0.975, y = 0.95, hjust = 1, label = p_value_text)
            annotation_custom(p_value_label)
        
        return(out_plot)
    })
}
