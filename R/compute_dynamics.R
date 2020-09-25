#' Compute emotion dynamics for two-dimensional data
#'
#' compute_dynamics() takes a data frame of time-ordered (or user-specified time-ordered) data. In the UED processing pipeline this function
#' is called after text pre-processing. This means that this function can be called on any kind of numeric data.
#'
#' @param data A data frame or tibble containing the data.
#' @param x A numeric variable contained in the data object
#' @param y A second numeric variable contained in the data object
#' @param id If there are multiple groups (e.g., data from many participants) this will perform the operation by group
#' @param time A variable in data identifying temporal ordering. If NULL data is assumed to be ordered.
#' @param level alpha level at which to calculate confidence ellipse for home base (defaults to .68 or +- 1 SD).
#' @param disp_length_min Minimal number of time points that a displacement can have. Must be between 1 and length of data,
#' but we recommend at least 3.
#'
#' @return tibble
#' @export
#'
#' @examples
compute_dynamics <- function(data, x, y, id = NULL, time = NULL, level = .68,
                             disp_length_min = 3) {

  ## -- PREPARATION -- ##

  enq_x <- enquo(x)
  enq_y <- enquo(y)
  enq_time <- enquo(time)

  if(!is.data.frame(data)) {
    stop("'data' must be in the form of a data frame or tibble")
  }

  if(!is.numeric(rlang::eval_tidy(enq_x, data))) {
    stop("Variable ", quo_name(enq_x), " is not numeric. Please enter a numeric variable.")
  }

  if(!is.numeric(rlang::eval_tidy(enq_y, data))) {
    stop("Variable ", quo_name(enq_y), " is not numeric. Please enter a numeric variable.")
  }

  is_time_null <- is.null(rlang::eval_tidy(enq_time, data))

  if(disp_length_min <= 0) {
    stop("disp_length_min must be greater than 0.")
  } else if(disp_length_min >= nrow(data)) {
    stop("disp_length_min cannot be equal to or greater than the number of rows in the data.")
  } else if(disp_length_min %in% 1:2) {
    warning("We recommend against setting disp_length_min < 3. Displacements should be able to have a rise, peak, and recovery period.",
            call. = FALSE)
  }

  ## -- FUNCTIONS -- ##
  # Home base detection

  ellipse_distance <- function(a, b, x, y) {
    px <- abs(x)
    py <- abs(y)
    t <- pi/4
    for(i in 1:4) {
      x <- a*cos(t)
      y <- b*sin(t)
      ex <- (a*a - b*b)*cos(t)^3/a
      ey <- (b*b - a*a)*sin(t)^3/b
      rx <- x-ex
      ry <- y-ey
      qx <- px-ex
      qy <- py-ey
      r <- sqrt(ry^2 + rx^2)
      q <- sqrt(qy^2 + qx^2)
      delta_c <- r*asin((rx*qy - ry*qx)/(r*q))
      delta_t <- delta_c/sqrt(a^2 + b^2 - x^2 - y^2)
      t <- t + delta_t
      t <- min(pi/2, max(0, t))
    }
    x <- abs(x) * sign(px)
    y <- abs(y) * sign(py)
    dist <- sqrt((py-y)^2 + (px-x)^2)
    return(dist)
  }

  in_ellipse <- function(x, y, level) {
    # Compute statistics for ellipse
    mat <- matrix(c(x, y), ncol = 2)
    mus <- mat %>%
      colMeans()
    sigma <- mat %>%
      cov()
    eigens <- eigen(sigma)
    eg_val <- eigens$values
    eg_vec <- eigens$vectors
    theta <- qchisq(level, df = 2)
    a <- sqrt(theta * eg_val[1])
    b <- sqrt(theta * eg_val[2])
    angle <- atan(eg_vec[2,1]/eg_vec[1,1])
    cos_theta <- cos(angle)
    sin_theta <- sin(angle)

    # Check if points are in the ellipse
    in_bases <- vector("logical", length(x))
    distances <- vector("numeric", length(x))
    for(i in seq_along(x)) {
      dist <- ((cos_theta*(x[[i]]-mus[[1]])+sin_theta*(y[[i]]-mus[[2]]))^2)/a^2 +
        ((sin_theta*(x[[i]]-mus[[1]])-cos_theta*(y[[i]]-mus[[2]]))^2)/b^2
      if(dist <= 1) {
        in_bases[[i]] <- TRUE
        distances[[i]] <- 0
      } else {
        in_bases[[i]] <- FALSE
        distances[[i]] <- ellipse_distance(a * cos_theta, b * sin_theta, x[[i]] - mus[[1]], y[[i]] - mus[[2]])
      }
    }
    results <- data.frame(in_home_base = in_bases, dist_home_base = distances)
    return(results)
  }

  # Number displacements
  displacement_number <- function(x) {
    disp_count <- 1
    disp_nums <- vector("integer", length(x))
    for(i in 1:length(x)) {
      if(x[[i]]) {
        disp_nums[[i]] <- NA
      } else {
        if(i == length(x)) {
          disp_nums[[i]] <- disp_count
        } else {
          disp_nums[[i]] <- disp_count
          if(x[[i]] != x[[i + 1]]) {
            disp_count <- disp_count + 1
          }
        }
      }
    }
    return(disp_nums)
  }

  # Displacement distance
  displacement_distance <- function(x, indices) {
    disp_count <- 1
    disp_dists <- vector("integer", length(x))
    indices <- which(indices)
    for(i in 1:length(x)) {
      if(x[[i]]) {
        disp_dists[[i]] <- NA
      } else {
        disp_dists[[i]] <- i - indices[[disp_count]]
      }
      if(i == length(x)) {
        disp_count <- disp_count
      } else if(!x[[i]] & x[[i + 1]]) {
        disp_count <- disp_count + 1
      }
    }
    return(disp_dists)
  }

  ## -- COMPUTATION -- ##
  # Compute and return results
  if(!is_time_null) {
    data <- data %>%
      group_by({{id}}) %>%
      arrange({{time}})
  } else {
    data <- data %>%
      group_by({{id}})
  }

  data <- data %>%
    mutate(across(c({{x}}, {{y}}), sd, .names = "{col}_emo_var"),
           across(c({{x}}, {{y}}), psych::rmssd, .names = "{col}_temp_var"),
           avg_emo_var = mean(c_across(ends_with("_emo_var"))),
           avg_temp_var = mean(c_across(ends_with("_temp_var")))) %>%
    select(-glue::glue("{quo_name(enq_x)}_emo_var"),
           -glue::glue("{quo_name(enq_y)}_emo_var"),
           -glue::glue("{quo_name(enq_x)}_temp_var"),
           -glue::glue("{quo_name(enq_y)}_temp_var")) %>%
    group_modify(~ {
      mutate(.x, in_ellipse({{x}}, {{y}}, level))
    }) %>%
    mutate(disp_num = displacement_number(in_home_base),
           disp_count = n_distinct(disp_num)) %>%
    group_by({{id}}, disp_num) %>%
    mutate(peak_dist = max(dist_home_base),
           is_peak = ifelse(!in_home_base, ifelse(dist_home_base == peak_dist, TRUE, FALSE), NA),
           disp_dist = displacement_distance(in_home_base, is_peak),
           disp_length = ifelse(in_home_base, NA, n()),
           rise_recovery = ifelse(disp_length %in% c(NA, 1:(disp_length_min - 1)), NA,
                                  case_when(disp_dist < 0 ~ "rise_rate",
                                            disp_dist > 0 ~ "recovery_rate"))) %>%
    group_by({{id}}, disp_num, rise_recovery) %>%
    mutate(disp_length = ifelse(disp_length >= disp_length_min, disp_length, NA),
           state_rate = ifelse(rise_recovery %in% c("rise_rate", "recovery_rate"), peak_dist/n(), NA),
           rise_rate = ifelse(rise_recovery == "rise_rate", state_rate, NA),
           recovery_rate = ifelse(rise_recovery == "recovery_rate", state_rate, NA)) %>%
    group_by({{id}}) %>%
    ungroup() %>%
    select(-rise_recovery, -state_rate, -disp_dist)

  return(data)
}
