radioButtonsHTML <- function(inputId, label, choices, choicesLabels = NULL, selected = NULL, width = NULL, inline = FALSE){
    if(is.null(choicesLabels)){
        return(radioButtons(inputId = inputId, label = label, choices = choices, selected = selected, inline = inline, width = width))
    }else{
        if(length(choicesLabels) != length(choices)){
            stop("")
        }else{
            rb <- radioButtons(inputId = inputId, label = label, choices = choices, selected = selected, inline = inline, width = width)
            for(i in seq_along(choices)){
                if(inline){
                    rb$children[[2]]$children[[1]][[i]]$children[[2]]$children[[1]] <- HTML(choicesLabels[i])
                } else {
                    rb$children[[2]]$children[[1]][[i]]$children[[1]]$children[[2]]$children[[1]] <- HTML(choicesLabels[i])
                }
            }
            return(rb)
        }
    }
}


# Define UI for application that draws a histogram
fluidPage(
    
    # Application title
    titlePanel("Test statistic and p-values"),
    
    # Sidebar with a slider input for number of bins
    fluidRow(
        # First row
        column(
            4,
            radioButtonsHTML(inputId = "HA",
                             label = HTML("Alternative hypothesis (H<sub>A</sub>)"),
                             choicesLabels = c("Not equal (H<sub>A</sub>: &mu; &ne; &mu;<sub>0</sub>)",
                                               "Greater than (H<sub>A</sub>: &mu; > &mu;<sub>0</sub>)",
                                               "Less than (H<sub>A</sub>: &mu; < &mu;<sub>0</sub>)"),
                             choices = c("two-sided", "greater", "less"),
                             inline = TRUE)
        ),
        column(
            8,
            radioButtonsHTML(inputId = 'control',
                             label = HTML('Control z<sub>obs</sub> or &alpha;?'),
                             choices = c('z_obs', 'p_value'),
                             choicesLabels = c('z<sub>obs</obs>', '&alpha;'),
                             inline = TRUE)
        ),
        # Second row
        column(
            12,
            # conditionalPanel('input.HA == "less"',
            #                  plotOutput("main_plot1")),
            # conditionalPanel('input.HA == "greater"',
            #                  plotOutput("main_plot2")),
            # conditionalPanel('input.HA == "two-sided"',
            #                  plotOutput("main_plot3"))
            plotOutput("main_plot")
        ),
        # Third row
        column(
            12,
            conditionalPanel('input.control == "p_value"',
                             sliderInput("p_value",
                                         HTML("&alpha;"),
                                         min = 0,
                                         max = 1,
                                         value = 0.25,
                                         step = 0.01)
            ),
            conditionalPanel('input.control == "z_obs"',
                             sliderInput("z_stat",
                                         HTML("Observed value of test statistic (z<sub>obs</sub>)"),
                                         min = -4,
                                         max = 4,
                                         value = 0.5,
                                         step = 0.001)
            )
        )
    )
)