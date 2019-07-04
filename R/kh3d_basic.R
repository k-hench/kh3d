.onAttach <- function(libname, pkgname) {
  cli::rule( center = str_c("Welcome to", crayon::green("kh3d")))
}
#' Initialize 3d plot
#'
#' \code{rgl_init} creates the 3d device.
#'
#' stolen from \link{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization}
#'
#'
#' @examples
#'
#' rgl_init()
#'
#' @export
rgl_init <- function(new.device = FALSE, bg = "white", width = 640,
                     aspect = c(1,1,1)) {
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 0, phi = -80, zoom = 1.03)
  aspect3d(aspect[1], aspect[2], aspect[3])
}

#' Add 3d axes
#'
#' \code{rgl_add_axes} add 3d axes device.
#'
#' stolen from \link{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization}
#'
#'
#' @examples
#'
#' rgl_init(bg = rgb(.3,.3,.3), aspect = aspect)
#' rgl_add_axes(data$x, data$y, data$z,
#'             show.bbox = FALSE,
#'             show.plane = FALSE)
#'
#' @export
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="",
                         show.plane = TRUE,
                         show.ticks = TRUE,
                         show.bbox = FALSE,
                         bbox.col = c(lpha("#333377",.4),"black"))
{

  #lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  lim <- function(x){c(min(x), max(x)) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)

  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0),
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3.5)

  # Add axis labels
  rgl.texts(axes,
            text = c(xlab, ylab, zlab),
            color = axis.col,
            adj = c(0.5, -0.8), size = 2)

  if(show.ticks){
    xbrks <- kh3_breaks(xlim,n = 3)$breaks[2:4]
    ybrks <- kh3_breaks(ylim,n = 3)$breaks[2:4]
    zbrks <- kh3_breaks(zlim,n = 3)$breaks[2:4]

    brks <- cbind(c(xbrks, rep(0,length(ybrks)), rep(0,length(zbrks))),
                  c(rep(0,length(xbrks)), ybrks,  rep(0,length(zbrks))),
                  c(rep(0,length(xbrks)),  rep(0,length(ybrks)), zbrks))

    rgl.points(brks, color = axis.col, size = 2.5)


    # Add tick labels
    rgl.texts(cbind(xbrks,rep(0,length(xbrks)),rep(0,length(xbrks))),
              text = xbrks,color = axis.col,
              adj = c(.5, 2), cex = .8)
    rgl.texts(cbind(rep(0,length(ybrks)),ybrks,rep(0,length(ybrks))),
              text = ybrks,color = axis.col,
              adj = c(3, 0), cex = .8)
    rgl.texts(rep(0,length(zbrks)),rep(0,length(zbrks)),cbind(zbrks),
              text = zbrks,color = axis.col,
              adj = c(2, 0), cex = .8)
  }

  # Add plane
  if(show.plane){
    xlim <- xlim/1.1; ylim <- ylim /1.1
    rgl.quads( x = rep(xlim, each = 2), z = c(0, 0, 0, 0),
               y = c(ylim[1], ylim[2], ylim[2], ylim[1]),
               shininess=0.4, alpha = 0.05)}

  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
             emission=bbox.col[1], specular=bbox.col[1], shininess=5,
             xlen = 3, ylen = 3, zlen = 3)
  }
}

#' Convert tibble to rgl matrix
#'
#' \code{tib_xyz_to_mat} converts a tible to a matrix.
#'
#' Data preparation to convert a data tibble
#' cith three columns (x,y,z) into a matrix for
#' use with rgl.
#'
#' @examples
#'
#' tib_mat <- data %>%
#'   tib_xyz_to_mat()
#'
#' @export
tib_xyz_to_mat <- function(tib){
  prep_mat <- tib %>%
    tidyr::spread(key = 'y', value = 'z')

  export_mat <- prep_mat %>%
    dplyr::select(-x) %>%
    as.matrix()

  rownames(export_mat) <- prep_mat$x
  export_mat
}


#' Plot xyz tibble in rgl
#'
#' \code{kh3_plot} is the main kh3d plotting function.
#'
#' Main plotting function of kh3d.
#'
#' @param data tibble with 3 columns (x, y, z), the data to be plotted.
#' @param aspect numeric vector of length 3, defining the plot aspect ratio.
#' @param style string ('surface', 'points', 'lines'), plotting style.
#' @param color color value, plotting color.
#' @param xlab sting scalar, the x axis label.
#' @param ylab sting scalar, the y axis label.
#' @param zlab sting scalar, the z axis label.
#' @param shininess numeric scalar, shininess of the plotting surface.
#'
#' @examples
#'
#' n_steps <- 31
#' data <- tibble(x = 2*pi*(1:n_steps)/n_steps,
#'                y = x) %>%
#'   purrr::cross_df() %>%
#'   mutate( z = sin(x)*sin(y))
#'
#' data %>% kh3_plot(., color = kh_clr[2],
#'                  xlab = 'x test',
#'                  ylab = 'y test',
#'                  zlab = 'z test',
#'                  aspect = c(2,3,1.3),
#'                  style = 'surface',
#'                  shininess = 4)
#'
#' @export
kh3_plot <- function(data, color = '#084082ff',
                    xlab = '', ylab = '', zlab = '',
                    aspect = c(8, 2, 1),
                    style = c('surface', 'points', 'lines'),
                    shininess = 3){

  rgl_init(bg = rgb(.3,.3,.3), aspect = aspect)
  rgl_add_axes(data$x, data$y, data$z,
               show.bbox = FALSE,
               xlab = xlab,
               ylab = ylab,
               zlab = zlab,
               show.plane = FALSE)

  tib_mat<- data %>%
    tib_xyz_to_mat()

  x <- as.numeric(rownames(tib_mat))
  y <- as.numeric(colnames(tib_mat))

  if (style == 'surface') {
    surface3d(x, y, tib_mat,
              col = color,
              axes = FALSE,
              smooth = FALSE,
              shininess = shininess,
              alpha = 0.9)

    surface3d(x, y, tib_mat,
              col = 'black',
              front = "lines",
              shininess = shininess,
              alpha = 1)

  } else if (style %in% c('points', 'lines')) {

    surface3d(x, y, tib_mat,
              col = color,
              front = style,
              shininess = shininess,
              alpha = 1)
  }
}

#' Break helper function
#'
#' \code{kh3_breaks} is a helper for breaks in kh3d plots.
#'
#' Helper function to create axis breaks within kh3d.
#'
#' @param lim numeric vector of length 2, range of the axis.
#' @param aspect numeric vector of length 3, defining the plot aspect ratio.
#'
#' @export
kh3_breaks <- function(lim, n = 3){
  scales::cbreaks(lim, scales::pretty_breaks(n))
}

#' kh color vector
#'
#' \code{kh_clr} is a combination of colors I like.
#'
#' A vector containg blue and orange.
#'
#' @export
kh_clr <- c('#084082ff','#f0a830ff')
