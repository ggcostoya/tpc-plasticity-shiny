
# define user interface
user_interface <- fluidPage(

  # set application title
  titlePanel("Exploring the effects of thermal plasticity on adaptation to climate change"),

  # add description
    # add description
  fluidRow(
    column(12,
      wellPanel(
        p("This application allows you to explore how can phenotypic plasticity in
          thermal performance curves (TPC) aids adaptation to changing thermal environments.
          Adjust the thermal optima (Topt) and critical thermal limits (CTmin and CTmax) to
          shape the TPC. Set an initial temperature distribution (initial mean and standard deviation)
          and explore how changes in mean and thermal variability (change in SD) impact performance.
          You can also control the TPC's acclimation response ratio (ARR) which will determine
          how much do all thermal physiology traits change as a function of changes in mean temperature
          (e.g., ARR = 0.5 will mean that all thermal physiology traits will change by 50% the amount by
          which the mean environmental temperature changes)")
      )
    )
  ),

  # define sidebar layout
  sidebarLayout(

    # add a sidebar panel with input options
    sidebarPanel(

      # add a numeric input for Topt
      numericInput("Topt", "Thermal optima (Topt, °C)",
                   30, min = 25, max = 35),

      # add a numeric input for CTmin
      numericInput("CTmin", "Critical thermal minimum (CTmin, °C):",
                   15, min = 10, max = 20),

      # add a numeric input for CTmax
      numericInput("CTmax", "Critical thermal maximum (CTmax, °C):",
                   40, min = 36, max = 45),

      # add a numeric input for Mean temperature
      numericInput("Mean_Init", "Initial Mean temperature (°C):",
                  min = 0, max = 50, value = 25),

      # add a numeric input for Standard deviation
      numericInput("SD_Init", "Initial Standard deviation of temperature (°C):",
                  min = 1, max = 10, value = 5),

      # add a slider input for change in Mean temperature
      sliderInput("Mean_delta", "Change in Mean temperature (°C):",
                  min = -10, max = 10, value = 0),

      # add a slider input for change in Standard deviation
      sliderInput("SD_delta", "Change in Standard deviation of temperature (°C):",
                  min = 0, max = 5, value = 0),

      # add a numeric input for ARR
      sliderInput("ARR", "Acclimation Response Ratio (ARR):",
                  0, min = 0, max = 1),

    ),

    # add a main panel for displaying the plot
    mainPanel(

      #set width (over 12)
      width = 8,

      # add a plot output
      plotOutput("plot", height = "600px"),

    )

  )

)

# define server logic
server_logic <- function(input, output){

  library(tidyverse)

  output$plot <- renderPlot({

    # define TPC function
    tpc <- function(temps, topt, ctmin, ctmax){

      # define performance object
      perf <- rep(NA, length(temps))

      # define psig
      psig <- -((ctmin - topt)/4)

      for(i in 1:length(temps)){

        # if temperature is below topt
        if(temps[i] < topt){

          perf[i] <- exp(-((temps[i]-topt)/(2*psig))^2)

          # if temperature is above topt
        }else{

          perf[i] <- 1 - ((temps[i] - topt)/(topt - ctmax))^2

        }

      }

      # correct for negative values
      perf <- ifelse(perf < 0, 0, perf)

      return(perf)
    }

    # generate a sequence of temperatures
    temp <- seq(0, 55, by = 0.1)

    # determine TPC without plasticity
    perf_without <- tpc(temp, input$Topt, input$CTmin, input$CTmax)

    # determine amount of plasticity by ARR
    change_ARR <- input$ARR * input$Mean_delta

    # determine with plasticity
    perf_with <- tpc(temp, input$Topt + change_ARR, input$CTmin + change_ARR,
                      input$CTmax + change_ARR)

    # determine initial temperature frequency
    temp_freq_init <- dnorm(temp, mean = input$Mean_Init, sd = input$SD_Init)

    # determine temperature frequency post change
    temp_freq_post <- dnorm(temp, mean = input$Mean_Init + input$Mean_delta,
                            sd = input$SD_Init + input$SD_delta)

    # determine predicted performance without plasticity
    pred_perf_without <- tpc(rnorm(10000, input$Mean_Init + input$Mean_delta,
                                   input$SD_Init + input$SD_delta),
                             input$Topt, input$CTmin, input$CTmax)

    # determine predicted performance with plasticity
    pred_perf_with <- tpc(rnorm(10000, input$Mean_Init + input$Mean_delta,
                                input$SD_Init + input$SD_delta),
                          input$Topt + change_ARR, input$CTmin + change_ARR,
                          input$CTmax + change_ARR)

    # build TPC data
    TPC_data <- tibble(temp = temp, perf_with = perf_with, perf_without = perf_without) %>%
      pivot_longer(cols = !"temp", names_to = "Plasticity", values_to = "perf") %>%
      mutate(Plasticity = ifelse(Plasticity == "perf_with", "Present", "Absent"))

    # build data for original and after change temperature distribution
    Temp_data <- tibble(temp = temp, freq_init = temp_freq_init, freq_post = temp_freq_post) %>%
      pivot_longer(cols = !temp, names_to = "stage", values_to = "density") %>%
      mutate(distribution = ifelse(stage == "freq_init", "Initial", "After change"))

    # build data for original and after change performance
    Perf_data <- tibble(perf_with = pred_perf_with, perf_without = pred_perf_without) %>%
      pivot_longer(cols = c("perf_with", "perf_without"), names_to = "plasticity",
                   values_to = "perf") %>%
      mutate(plasticity = ifelse(plasticity == "perf_with", "Present", "Absent"))

    # plot
    ggplot() +
      geom_line(data = TPC_data, aes(x = temp, y = perf, col = Plasticity, group = Plasticity),
                linewidth = 2, alpha = 0.75) +
      scale_color_manual(values = c("black", "orange")) +
      geom_area(data = Temp_data, aes(x = temp, y = 2*density, fill = distribution),
                position = "identity", alpha = 0.25, show.legend = FALSE) +
      scale_fill_manual(values = c("darkgray", "darkred", "black", "orange")) +
      geom_density(data = Perf_data, aes(y = perf, fill = plasticity),
                   col = NA, alpha = 0.25, show.legend = FALSE) +
      geom_vline(xintercept = input$Mean_Init + input$Mean_delta,
                 linetype = "dashed", col = "darkred", linewidth = 1.5, alpha = 0.75) +
      geom_segment(aes(x = 0, xend = input$Mean_Init + input$Mean_delta,
                   y = mean(pred_perf_without), yend = mean(pred_perf_without)),
                   col = "black", linewidth = 1.5, alpha = 0.75) +
      geom_segment(aes(x = 0, xend = input$Mean_Init + input$Mean_delta,
                       y = mean(pred_perf_with), yend = mean(pred_perf_with)),
                   col = "orange", linewidth = 1.5, alpha = 0.75) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(limits = c(0,1.01), expand = c(0,0)) +
      geom_point(aes(x = 45, y = 0.9), size = 10, col = "darkgray", shape = 15,
                 alpha = 0.5, fill = "darkgray") +
      geom_point(aes(x = 45, y = 0.85), size = 10, col = "darkred", shape = 15,
                 alpha = 0.5, fill = "darkred") +
      geom_text(aes(x = 47, y = 0.9, label = "Initial"),
                size = 5, col = "black", hjust = 0) +
      geom_text(aes(x = 47, y = 0.85, label = "After change"),
                size = 5, col = "black", hjust = 0) +
      geom_text(aes(x = 44, y = 0.95, label = "Temperature distribution"),
                size = 5, col = "black",hjust = 0) +
      geom_point(aes(x = 45, y = 0.75), size = 10, col = "black", shape = 22,
                 alpha = 0.5, fill = "black") +
      geom_point(aes(x = 45, y = 0.7), size = 10, col = "orange", shape = 22,
                 alpha = 0.5, fill = "orange") +
      geom_text(aes(x = 47, y = 0.75, label = "Absent"),
                size = 5, col = "black", hjust = 0) +
      geom_text(aes(x = 47, y = 0.7, label = "Present"),
                size = 5, col = "black", hjust = 0) +
      geom_text(aes(x = 44, y = 0.8, label = "Plasticity"),
                size = 5, col = "black", hjust = 0) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.line = element_line(),
        axis.ticks = element_line(),
        legend.position = "none",
        plot.title = element_text(size = 20)
      ) +
      xlab("Temperature (°C)") +
      ylab("Performance") +
      ggtitle(paste("Change in performance via plasticity: ",
        round(mean(pred_perf_with) - mean(pred_perf_without), digits = 2),
        sep = ""))

  })

}

shinyApp(ui = user_interface, server = server_logic)
