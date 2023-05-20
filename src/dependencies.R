# from https://stackoverflow.com/questions/38928326/is-there-something-like-requirements-txt-for-r

pkgLoad <- function( packages = "needed" ) {
  
  if( length( packages ) == 1L && packages == "needed" ) {
    packages <- c('shiny', 'shinyWidgets', 'shinydashboard', 'stringr',
                  'ggplot2', 'lubridate', 'dplyr', 'shinycssloaders'
    )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "http://cran.csiro.au"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
}

pkgLoad()
