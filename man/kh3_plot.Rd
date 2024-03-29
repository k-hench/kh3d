% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kh3d_basic.R
\name{kh3_plot}
\alias{kh3_plot}
\title{Plot xyz tibble in rgl}
\usage{
kh3_plot(data, color = "#084082ff", style = c("surface", "points",
  "lines"), line_overlay = TRUE, show_ticks = TRUE, aspect = c(8, 2,
  1), xlab = "", ylab = "", zlab = "", plot_bg = rgb(0.3, 0.3,
  0.3), shininess = 3, theta = 0, phi = -80, zoom = 1.03,
  fov = 60)
}
\arguments{
\item{data}{tibble with 3 columns (x, y, z), the data to be plotted.}

\item{color}{color value, plotting color.}

\item{style}{string ('surface', 'points', 'lines'), plotting style.}

\item{line_overlay}{logical scalar, should lines be drawn ontop the 3d surface?}

\item{aspect}{numeric vector of length 3, defining the plot aspect ratio.}

\item{xlab}{sting scalar, the x axis label.}

\item{ylab}{sting scalar, the y axis label.}

\item{zlab}{sting scalar, the z axis label.}

\item{shininess}{numeric scalar, shininess of the plotting surface.}
}
\description{
\code{kh3_plot} is the main kh3d plotting function.
}
\details{
Main plotting function of kh3d.
}
\examples{

n_steps <- 31
data <- tibble(x = 2*pi*(1:n_steps)/n_steps,
               y = x) \%>\%
  purrr::cross_df() \%>\%
  mutate( z = sin(x)*sin(y))

data \%>\% kh3_plot(., color = kh_clr[2],
                 xlab = 'x test',
                 ylab = 'y test',
                 zlab = 'z test',
                 aspect = c(2,3,1.3),
                 style = 'surface',
                 shininess = 4)

}
