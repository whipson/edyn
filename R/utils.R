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
