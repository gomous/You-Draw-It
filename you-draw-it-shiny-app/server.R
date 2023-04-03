# Load libraries
library(shiny)
library(shinyjs)
library(r2d3)
library(tidyverse)
library(purrr)
library(gridSVG)
library(lubridate)
library(readxl)

# Turn a list of data into a json file
data_to_json <- function(data) {
  jsonlite::toJSON(data, 
                   dataframe = "rows", 
                   auto_unbox = FALSE, 
                   rownames = TRUE)
} 

# Redefine drawr function
drawr <- function(data, 
                  linear            = "true", 
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0.01, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  
  line_data  <- data$line_data
  point_data <- data$point_data
  
  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)
  
  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }
  
  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }
  
  r2d3::r2d3(data   = data_to_json(data), 
             script = "www/you-draw-it.js",
             d3_version = "5",
             dependencies = c("d3-jetpack"),
             options = list(draw_start        = draw_start, 
                            points_end        = points_end,
                            linear            = as.character(linear),
                            free_draw         = free_draw, 
                            points            = points,
                            aspect_ratio      = aspect_ratio,
                            pin_start         = T, 
                            x_range           = x_range,
                            x_by              = x_by,
                            y_range           = y_range,
                            line_style        = NULL,
                            data_tab1_color   = data_tab1_color, 
                            drawn_line_color  = drawn_line_color,
                            show_finished     = show_finished,
                            shiny_message_loc = shiny_message_loc,
                            title             = title)
  )
  
}

# Linear Data Simulation
linearDataGen <- 
  function(y_xbar, 
           slope, 
           sigma, 
           points_choice = "full", 
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
    
    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)
    
    yintercept = y_xbar - slope*mean(xVals)
    
    errorVals <- rnorm(N, 0, sigma)

    
    # Simulate point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yintercept + slope*x + errorVals) %>%
      arrange(x)
    
    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()
    
    # Simulate best fit line data
    line_data <- tibble(data = "line_data", 
                        x = seq(x_min, x_max, x_by), 
                        y = yintercepthat + slopehat*x)
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  linear_data <- reactive({
    
    input$reset
    
    tibble(
      dataset = c("S"),
      y_xbar = c(3.9),
      slope  = c(0.8),
      sigma  = c(2.8),
      x_min   = c(0),
      x_max   = c(20),
      N       = 40,
      x_by    = 0.25) %>%
      mutate(data = purrr::pmap(list(y_xbar = y_xbar, 
                                     slope  = slope, 
                                     sigma  = sigma, 
                                     x_min  = x_min, 
                                     x_max  = x_max, 
                                     x_by   = x_by, 
                                     N      = N), 
                                linearDataGen)) %>%
      unnest(data) %>%
      unnest(data)
  })
  
  linear_y_range <- reactive({
    linear_data <- linear_data()
    range(linear_data$y) * c(1.5, 1.5)
  })
  
  linear_x_range <- reactive({
    linear_data <- linear_data()
    c(min(linear_data$x), max(linear_data$x))
  })
  
  message_loc <- session$ns("drawr_message")
  output$shinydrawr <- r2d3::renderD3({
    
    line_data <- linear_data() %>%
      filter(data == "line_data")
    
    point_data <- linear_data() %>%
      filter(data == "point_data")
    
    data <- list(line_data = line_data, point_data = point_data)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 1,
          points_end        = 20,
          show_finished     = T,
          shiny_message_loc = message_loc,
          x_range           = linear_x_range(),
          y_range           = linear_y_range()
    )
    
  })
}
