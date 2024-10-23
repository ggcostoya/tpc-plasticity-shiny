
library(tidyverse)

# define user interface
user_interface <- fluidPage(

  # set application title
  titlePanel("Exploring the effects of plasticity"),

  # define sidebar layout
  sidebarLayout(

    # add a sidebar panel with input options
    sidebarPanel(

      # add a numeric input for Topt
      numericInput("Topt", "Thermal optima (Topt):",
                   30, min = 25, max = 35),

      # add a numeric input for CTmin
      numericInput("CTmin", "Critical thermal minimum (CTmin):",
                   15, min = 10, max = 20),

      # add a numeric input for CTmax
      numericInput("CTmax", "Critical thermal maximum (CTmax):",
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

      # add a plot output
      plotOutput("plot")

    )

  )

)

# define server logic
server_logic <- function(input, output){

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
    temp <- seq(0, 50, by = 0.1)

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
      geom_area(data = Temp_data, aes(x = temp, y = 2*density, fill = distribution),
                position = "identity", alpha = 0.25) +
      geom_density(data = Perf_data, aes(y = perf, fill = plasticity), alpha = 0.5) +
      scale_color_manual(values = c("orange", "black"))

  })

}

shinyApp(ui = user_interface, server = server_logic)
