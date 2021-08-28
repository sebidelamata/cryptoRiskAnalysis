# setting up R repo and packages
# this script is executed in the chroot context; check paths!

r <- getOption('repos')

r['CRAN'] <- 'http://cloud.r-project.org'

options(repos=r)

# ============================================================== #

# packages go here
install.packages('remote')

remotes::install_github(
			'plotly/dashR',
			upgrade = TRUE
			)
