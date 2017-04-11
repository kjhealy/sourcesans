#' A [ggplot2] theme using Source Sans Pro, adapted from [hrbrthemes]'s roboto condensed.
#'
#' You should [import_source_sans]() first and also install the fonts on your
#' system before trying to use this theme.
#'
#' @title theme_sourcesans
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_sourcesans()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults(family=font_source_sans_light)
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_sourcesans(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
theme_sourcesans <- function(base_family="Source Sans Pro", base_size = 12,
                         plot_title_family=base_family, plot_title_size = 18,
                         plot_title_face="bold", plot_title_margin = 10,
                         subtitle_family="Source Sans Pro", subtitle_size = 12,
                         subtitle_face = "plain", subtitle_margin = 15,
                         strip_text_family = base_family, strip_text_size = 12,
                         strip_text_face = "plain",
                         caption_family = "Source Sans Pro", caption_size = 9,
                         caption_face = "plain", caption_margin = 10,
                         axis_title_family = base_family, axis_title_size = 9,
                         axis_title_face = "plain", axis_title_just = "rt",
                         plot_margin = margin(1, 1, 1, 1),
                         panel_spacing = unit(0.5, "lines"),
                         grid = TRUE, axis = FALSE, ticks = FALSE) {

  require(sysfonts)
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_line(color="gray90", size=0.10))
    ret <- ret + theme(panel.grid.major=element_line(color="gray90", size=0.1))
    ret <- ret + theme(panel.grid.minor=element_line(color="gray90", size=0.1))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="gray90", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color="gray90", size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color="gray90", size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color="gray90", size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color="gray90", size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing.x=grid::unit(2, "lines"))
  ret <- ret + theme(panel.spacing.y=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret <-  ret + theme(panel.spacing=panel_spacing)

  ret

}

#' Import Source Sans Pro font for use in charts
#'
#'
#' @note This will take care of ensuring PDF/PostScript usage. The location of the
#'   font directory is displayed after the base import is complete. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
import_source_sans <- function() {
    source_sans_font_dir <- system.file("fonts", "sourcesans-pro", package="sourcesans")
    sysfonts::font.add("Source Sans Pro",
                       regular = paste0(source_sans_font_dir, "/", "SourceSansPro-Regular.otf"),
                       bold = paste0(source_sans_font_dir, "/", "SourceSansPro-Semibold.otf"),
                       italic = paste0(source_sans_font_dir, "/", "SourceSansPro-It.otf"),
                       bolditalic = paste0(source_sans_font_dir, "/", "SourceSansPro-SemiboldIt.otf")
                       )


  message(sprintf("You will likely need to install these fonts on your system as well. You can find them in [%s]", source_sans_font_dir))

}


#' @rdname SourceSansPro
#' @md
#' @title Source Sans Pro font name R variable aliases
#' @description `font_sourcesans` == "`Source Sans Pro`"
#' @format length 1 character vector
#' @export
font_mc <- "Source Sans Pro"

#' @rdname SourceSansProLight
#' @md
#' @title Source Sans Pro Light font name R variable aliases
#' @description `font_fc_light` == "`Source Sans Pro Light`"
#' @export
font_source_sans_light <- "Source Sans Pro Light"
