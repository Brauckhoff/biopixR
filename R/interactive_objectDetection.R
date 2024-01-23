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
#' This function uses the objectDetection function to visualize the detected
#' objects at varying threshold an smoothing parameters.
#' @param img image (preferred import: \code{\link[imager]{load.image}})
#' @param resolution resolution of slider
#' @param return_param used to define the final parameter values for alpha and
#' sigma printed in the console (TRUE or FALSE).
#' @returns values of alpha and sigma
#' @import magick
#' @import imager
#' @import data.table
#' @import tcltk
#' @references https://CRAN.R-project.org/package=magickGUI
#' @examples
#' \donttest{
#' if (interactive()) {
#'   interactive_objectDetection(beads)
#' }
#' }
#' @export
interactive_objectDetection <-
  function(img,
           resolution = 0.1,
           return_param = FALSE) {
    # assign imports
    image_original <- cimg2magick(img)
    beads <- img

    # make initial input
    alpha <- 1
    sigma <- 2
    scale <- 1
    initial_cimg <- objectDetection(beads, alpha, sigma)
    initial_cimg <- mirror(initial_cimg$marked_beads, axis = "x")
    initial_cimg <- imresize(initial_cimg, scale = scale)
    initial <- cimg2magick(initial_cimg)

    # set variable range
    range_alpha <- c(0, 3)
    range_sigma <- c(0, 3)
    range_scale <- c(1, 3)
    length_slider <- as.integer(nrow(beads) * 0.6)
    if (length_slider < 200) {
      length_slider <- 200
    }

    # name sliders and create initial temporary image
    text_label_alpha <- "Alpha/Threshold:"
    text_label_sigma <- "Sigma/Smoothing:"
    text_label_scale <- "Scale:"
    quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
    temp <- tempfile(fileext = ".jpg")
    on.exit(unlink(temp), add = TRUE)
    image_write(initial, temp)
    image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
    label_digits <- -as.integer(log(resolution, 10))
    label_digits <- ifelse(label_digits > 0, label_digits, 0)
    label_template <- sprintf("%%.%df", label_digits)

    # configure widgets
    win1 <- tktoplevel()
    on.exit(tkdestroy(win1), add = TRUE)
    win1.frame1 <- tkframe(win1)
    win1.frame2 <- tkframe(win1)
    win1.frame3 <- tkframe(win1)
    win1.im <- tklabel(win1, image = image_tcl)
    win1.frame1.label <-
      tklabel(win1.frame1, text = sprintf("%s%s", text_label_alpha, sprintf(label_template, alpha)))
    win1.frame2.label <-
      tklabel(win1.frame2, text = sprintf("%s%s", text_label_sigma, sprintf(label_template, sigma)))
    win1.frame3.label <-
      tklabel(win1.frame3, text = sprintf("%s%s", text_label_scale, sprintf(label_template, scale)))
    slider_value_alpha <- tclVar(alpha)
    slider_value_sigma <- tclVar(sigma)
    slider_value_scale <- tclVar(scale)
    command_slider_alpha <- function(...) {
      assign("slider_value_radius", slider_value_alpha, inherits = TRUE)
    }
    command_slider_sigma <- function(...) {
      assign("slider_value_sigma", slider_value_sigma, inherits = TRUE)
    }
    command_slider_scale <- function(...) {
      assign("slider_value_scale", slider_value_scale, inherits = TRUE)
    }

    # configure slider
    win1.frame1.slider <-
      tkscale(
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
      tkscale(
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
      tkscale(
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

    # update function when sliders are moved
    temp_val <- c(alpha, sigma, scale)
    update_image <- function() {
      temp_image <- objectDetection(beads, temp_val[1], temp_val[2])
      temp_image <- mirror(temp_image$marked_beads, axis = "x")
      temp_image <- imresize(temp_image, temp_val[3])
      temp_image <- cimg2magick(temp_image)
      image_write(temp_image, temp)
      image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
      tkconfigure(win1.im, image = image_tcl)
    }
    command_button <- function(...) {
      assign("quit_waiting", TRUE, inherits = TRUE)
    }

    # add image and slider to the GUI
    win1.button <-
      tkbutton(win1, text = "OK", command = command_button)
    tkpack(win1.im, side = "left")
    tkpack(win1.frame1.label, side = "left", anchor = "c")
    tkpack(win1.frame1.slider, side = "left", anchor = "c")
    tkpack(win1.frame1, side = "top", anchor = "c")
    tkpack(win1.frame2.label, side = "left", anchor = "c")
    tkpack(win1.frame2.slider, side = "left", anchor = "c")
    tkpack(win1.frame2, side = "top", anchor = "c")
    tkpack(win1.frame3.label, side = "left", anchor = "c")
    tkpack(win1.frame3.slider, side = "left", anchor = "c")
    tkpack(win1.frame3, side = "top", anchor = "c")

    tkpack(win1.button,
      side = "top",
      anchor = "c",
      pady = 20
    )
    pre_slider_values <-
      c(
        as.numeric(tclvalue(slider_value_alpha)),
        as.numeric(tclvalue(slider_value_sigma)),
        as.numeric(tclvalue(slider_value_scale))
      )
    if (quit_waiting) {
      wait_test <- TRUE
      while (wait_test) {
        wait_test <- FALSE
        tryCatch(
          {
            tkwm.state(win1)
          },
          error = function(e) {
            assign("wait_test", TRUE, inherits = TRUE)
          }
        )
      }
      wait_time_long()
      tkdestroy(win1.button)
    }
    tkwm.state(win1, "normal")

    # check if values changed, if values changed update image
    while (TRUE) {
      tryCatch(
        {
          tkwm.state(win1)
        },
        error = function(e) {
          assign("quit_waiting", TRUE, inherits = TRUE)
        }
      )
      if (quit_waiting) {
        break
      }

      temp_val <-
        c(as.numeric(tclvalue(slider_value_alpha)),
          as.numeric(tclvalue(slider_value_sigma)),
          as.numeric(tclvalue(slider_value_scale)))

      # warn if parameter would cause an error and reset values
      if (class(try(edgeDetection(beads, alpha = temp_val[1], sigma = temp_val[2]))
      )[1] == 'try-error') {

        temp_val <-
          c(
            pre_slider_values[1],
            pre_slider_values[2],
            pre_slider_values[3]
          )
        tkset(win1.frame1.slider, temp_val[1])
        tkset(win1.frame2.slider, temp_val[2])
      }

      if (any(temp_val != pre_slider_values)) {
        temp_label_alpha <-
          sprintf(
            "%s%s",
            text_label_alpha,
            sprintf(label_template, temp_val[1])
          )
        temp_label_sigma <-
          sprintf(
            "%s%s",
            text_label_sigma,
            sprintf(label_template, temp_val[2])
          )
        temp_label_scale <-
          sprintf(
            "%s%s",
            text_label_scale,
            sprintf(label_template, temp_val[3])
          )

        tkconfigure(win1.frame1.label, text = temp_label_alpha)
        tkconfigure(win1.frame2.label, text = temp_label_sigma)
        tkconfigure(win1.frame3.label, text = temp_label_scale)
        print_with_timestamp("Creating new image...")
        update_image()
        print_with_timestamp("Finished processing new variables")
        pre_slider_values <- temp_val
      }
    }
    val_res <- pre_slider_values
    names(val_res) <- c("alpha", "sigma", "scale")
    if (return_param) {
      return(val_res)
    }
    out <- val_res
    out
  }
