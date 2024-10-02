wait_time_long <- function() {
  wait_start <- proc.time()[3]
  wait_time <- 1.0 # sec
  while (proc.time()[3] - wait_start < wait_time) {}
}

# Function to print a message with a timestamp
print_with_timestamp <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, msg))
}

#' Interactive object detection
#'
#' This function uses the \code{\link[biopixR]{objectDetection}} function to
#' visualize the detected objects at varying input parameters.
#' @param img image (import by \code{\link[biopixR]{importImage}})
#' @param resolution resolution of slider
#' @param return_param if TRUE the final parameter values for alpha and
#' sigma are printed to the console (TRUE | FALSE)
#' @returns Values of alpha, sigma and the applied method.
#' @details
#' The function provides a graphical user interface (GUI) that allows users to
#' interactively adjust the parameters for object detection:
#' \itemize{
#'   \item \strong{Alpha:} Controls the threshold adjustment factor for edge detection.
#'   \item \strong{Sigma:} Determines the amount of smoothing applied to the image.
#'   \item \strong{Scale:} Adjusts the scale of the displayed image.
#' }
#' The GUI also includes a button to switch between two detection methods:
#' \itemize{
#'   \item \strong{Edge Detection:} Utilizes the \code{\link[biopixR]{edgeDetection}} function. The alpha parameter acts as a threshold adjustment factor, and sigma controls the smoothing.
#'   \item \strong{Threshold Detection:} Applies a thresholding method, utilizing \code{\link[imagerExtra]{SPE}} for background reduction and the \code{\link[imager]{threshold}} function. (No dependency on alpha or sigma!)
#' }
#' @import magick
#' @import imager
#' @import data.table
#' @references https://CRAN.R-project.org/package=magickGUI
#' @examples
#' \donttest{
#' if (interactive()) {
#'   interactive_objectDetection(beads)
#'   }
#' }
#' @export
interactive_objectDetection <-
  function(img,
           resolution = 0.1,
           return_param = FALSE) {

    # Check for required namespace
    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop("The 'tcltk' package is required but not installed. Please install it using install.packages('tcltk').")
    }

    # Convert the image to the desired format
    image_original <- cimg2magick(img)
    object_img <- img

    # Initialize detection parameters
    alpha <- 1
    sigma <- 2
    scale <- 1
    method <- "edge"

    # Perform initial object detection
    initial_cimg <-
      objectDetection(object_img,
                      alpha = alpha,
                      sigma = sigma,
                      method = method)
    initial_cimg <- mirror(initial_cimg$marked_objects, axis = "x")
    initial_cimg <- imresize(initial_cimg, scale = scale)
    initial <- cimg2magick(initial_cimg)

    # Define parameter ranges for sliders
    range_alpha <- c(0, 3)
    range_sigma <- c(0, 3)
    range_scale <- c(1, 3)
    length_slider <- max(as.integer(nrow(object_img) * 0.6), 200)

    # Set up labels for sliders
    text_label_alpha <- "Alpha/Threshold:"
    text_label_sigma <- "Sigma/Smoothing:"
    text_label_scale <- "Scale:"

    quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
    temp <- tempfile(fileext = ".jpg")
    on.exit(unlink(temp), add = TRUE)
    image_write(initial, temp)
    image_tcl <- tcltk::tkimage.create("photo", "image_tcl", file = temp)

    # Format label digits based on resolution
    label_digits <- -as.integer(log(resolution, 10))
    label_digits <- ifelse(label_digits > 0, label_digits, 0)
    label_template <- sprintf("%%.%df", label_digits)

    # Create GUI window and frames
    win1 <- tcltk::tktoplevel()
    on.exit(tcltk::tkdestroy(win1), add = TRUE)
    win1.frame1 <- tcltk::tkframe(win1)
    win1.frame2 <- tcltk::tkframe(win1)
    win1.frame3 <- tcltk::tkframe(win1)
    win1.frame4 <- tcltk::tkframe(win1)
    win1.im <- tcltk::tklabel(win1, image = image_tcl)

    # Create and configure labels
    win1.frame1.label <-
      tcltk::tklabel(win1.frame1, text = sprintf("%s%s", text_label_alpha, sprintf(label_template, alpha)))
    win1.frame2.label <-
      tcltk::tklabel(win1.frame2, text = sprintf("%s%s", text_label_sigma, sprintf(label_template, sigma)))
    win1.frame3.label <-
      tcltk::tklabel(win1.frame3, text = sprintf("%s%s", text_label_scale, sprintf(label_template, scale)))
    win1.frame4.label <-
      tcltk::tklabel(win1.frame4, text = paste("Method: ", method))
    win1.method_status <-
      tcltk::tklabel(win1, text = paste("Current method in use: ", method))

    # Initialize slider values
    slider_value_alpha <- tcltk::tclVar(alpha)
    slider_value_sigma <- tcltk::tclVar(sigma)
    slider_value_scale <- tcltk::tclVar(scale)

    # Functions to update slider values
    command_slider_alpha <- function(...) {
      assign("slider_value_alpha", slider_value_alpha, inherits = TRUE)
    }
    command_slider_sigma <- function(...) {
      assign("slider_value_sigma", slider_value_sigma, inherits = TRUE)
    }
    command_slider_scale <- function(...) {
      assign("slider_value_scale", slider_value_scale, inherits = TRUE)
    }

    # Create sliders for parameters
    win1.frame1.slider <-
      tcltk::tkscale(
        win1.frame1,
        from = range_alpha[1],
        to = range_alpha[2],
        variable = slider_value_alpha,
        orient = "horizontal",
        length = length_slider,
        command = command_slider_alpha,
        resolution = resolution,
        showvalue = 0
      )
    win1.frame2.slider <-
      tcltk::tkscale(
        win1.frame2,
        from = range_sigma[1],
        to = range_sigma[2],
        variable = slider_value_sigma,
        orient = "horizontal",
        length = length_slider,
        command = command_slider_sigma,
        resolution = resolution,
        showvalue = 0
      )
    win1.frame3.slider <-
      tcltk::tkscale(
        win1.frame3,
        from = range_scale[1],
        to = range_scale[2],
        variable = slider_value_scale,
        orient = "horizontal",
        length = length_slider,
        command = command_slider_scale,
        resolution = resolution,
        showvalue = 0
      )

    # Function to update image based on slider values
    temp_val <- c(alpha, sigma, scale)
    update_image <- function() {
      temp_image <-
        objectDetection(object_img,
                        alpha = temp_val[1],
                        sigma = temp_val[2],
                        method = method)
      temp_image <- mirror(temp_image$marked_objects, axis = "x")
      temp_image <- imresize(temp_image, temp_val[3])
      temp_image <- cimg2magick(temp_image)
      image_write(temp_image, temp)
      image_tcl <- tcltk::tkimage.create("photo", "image_tcl", file = temp)
      tcltk::tkconfigure(win1.im, image = image_tcl)
    }

    # Function to handle OK button click
    command_button <- function(...) {
      assign("quit_waiting", TRUE, inherits = TRUE)
    }

    # Function to handle method switch button click
    command_switch_method <- function(...) {
      method <<- ifelse(method == "edge", "threshold", "edge")
      tcltk::tkconfigure(win1.frame4.label, text = paste("Method: ", method))
      tcltk::tkconfigure(win1.method_status, text = paste("Current method in use: ", method))
      update_image()
    }

    # Add GUI elements to the window
    win1.button <-
      tcltk::tkbutton(win1, text = "OK", command = command_button)
    win1.switch_button <-
      tcltk::tkbutton(win1, text = "Switch Method", command = command_switch_method)
    tcltk::tkpack(win1.im, side = "left")
    tcltk::tkpack(win1.frame1.label, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame1.slider, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame1, side = "top", anchor = "c")
    tcltk::tkpack(win1.frame2.label, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame2.slider, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame2, side = "top", anchor = "c")
    tcltk::tkpack(win1.frame3.label, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame3.slider, side = "left", anchor = "c")
    tcltk::tkpack(win1.frame3, side = "top", anchor = "c")
    tcltk::tkpack(win1.frame4.label, side = "left", anchor = "c")
    tcltk::tkpack(
      win1.switch_button,
      side = "top",
      anchor = "c",
      pady = 10
    )
    tcltk::tkpack(
      win1.method_status,
      side = "top",
      anchor = "c",
      pady = 5
    )
    tcltk::tkpack(win1.button,
           side = "top",
           anchor = "c",
           pady = 20)

    pre_slider_values <- c(as.numeric(tcltk::tclvalue(slider_value_alpha)),
                           as.numeric(tcltk::tclvalue(slider_value_sigma)),
                           as.numeric(tcltk::tclvalue(slider_value_scale)))

    # Handle GUI state and update logic
    if (quit_waiting) {
      wait_test <- TRUE
      while (wait_test) {
        wait_test <- FALSE
        tryCatch({
          tcltk::tkwm.state(win1)
        }, error = function(e) {
          assign("wait_test", TRUE, inherits = TRUE)
        })
      }
      wait_time_long()
      tcltk::tkdestroy(win1.button)
    }
    tcltk::tkwm.state(win1, "normal")

    # If values change - update image
    while (TRUE) {
      tryCatch({
        tcltk::tkwm.state(win1)
      }, error = function(e) {
        assign("quit_waiting", TRUE, inherits = TRUE)
      })
      if (quit_waiting) {
        break
      }

      temp_val <- c(as.numeric(tcltk::tclvalue(slider_value_alpha)),
                    as.numeric(tcltk::tclvalue(slider_value_sigma)),
                    as.numeric(tcltk::tclvalue(slider_value_scale)))

      # Validate the new parameter values
      if (class(try(edgeDetection(object_img, alpha = temp_val[1], sigma = temp_val[2])))[1] == 'try-error') {
        temp_val <-
          c(pre_slider_values[1],
            pre_slider_values[2],
            pre_slider_values[3])
        tcltk::tkset(win1.frame1.slider, temp_val[1])
        tcltk::tkset(win1.frame2.slider, temp_val[2])
      }

      # Update image if parameter values have changed
      if (any(temp_val != pre_slider_values)) {
        temp_label_alpha <-
          sprintf("%s%s",
                  text_label_alpha,
                  sprintf(label_template, temp_val[1]))
        temp_label_sigma <-
          sprintf("%s%s",
                  text_label_sigma,
                  sprintf(label_template, temp_val[2]))
        temp_label_scale <-
          sprintf("%s%s",
                  text_label_scale,
                  sprintf(label_template, temp_val[3]))

        tcltk::tkconfigure(win1.frame1.label, text = temp_label_alpha)
        tcltk::tkconfigure(win1.frame2.label, text = temp_label_sigma)
        tcltk::tkconfigure(win1.frame3.label, text = temp_label_scale)

        print_with_timestamp("Creating new image...")
        update_image()
        print_with_timestamp("Finished processing new variables")
        pre_slider_values <- temp_val
      }
    }

    val_res <- c(pre_slider_values, method = method)
    names(val_res) <- c("alpha", "sigma", "scale", "method")
    if (return_param == TRUE) {
      return(val_res)
    }
    out <- val_res
  }
