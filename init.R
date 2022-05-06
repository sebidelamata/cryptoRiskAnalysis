# setting up R repo and packages
# this script is executed in the chroot context; check paths!

r <- getOption('repos')


r['CRAN'] <- 'http://cloud.r-project.org'

options(repos=r)

# ============================================================== #

# packages go here
install.packages('remotes')
install.packages('quantmod', dependencies = TRUE)
helpers.installPackages("zoo")
helpers.installPackages("tidyr")
helpers.installPackages("dplyr")
helpers.installPackages("xts")
helpers.installPackages("moments")
helpers.installPackages("MASS")
install.packages("QRM", dependencies = TRUE)
helpers.installPackages("plotly")
helpers.installPackages("dash")
helpers.installPackages("dashCoreComponents")
helpers.installPackages("dashHtmlComponents")
helpers.installPackages("dashBootstrapComponents")

remotes::install_github(
			'plotly/dashR',
			upgrade = TRUE
			)
