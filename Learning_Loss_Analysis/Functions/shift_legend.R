###   Load required package
require(lemon)

##    From https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2/54438496

shift_legend <- function(p) { # shift_legend2 from link above.
  # check if p is a valid object
  if(!(inherits(p, "gtable"))){
    if(inherits(p, "ggplot")){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }

  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]),
                               USE.NAMES = F)
  empty.facet.panels <- facet.panels[empty.facet.panels]

  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }

  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name

  # return repositioned legend
  reposition_legend(p, 'center', panel=names)
}
