# setting up R repo and packages
# this script is executed in the chroot context; check paths!

r <- getOption('repos')

r['CRAN'] <- 'http://cloud.r-project.org'

options(repos=r)

# ============================================================== #

# packages go here
install.packages('remotes', dependencies = TRUE)
install.packages("quantmod", dependencies = TRUE)
install.packages("zoo", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("xts", dependencies = TRUE)
install.packages("moments", dependencies = TRUE)
install.packages("MASS", dependencies = TRUE)
install.packages("QRM", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)
install.packages("dash", dependencies = TRUE)
install.packages("dashCoreComponents", dependencies = TRUE)
install.packages("dashHtmlComponents", dependencies = TRUE)
install.packages("dashBootstrapComponents", dependencies = TRUE)

remotes::install_github(
			'plotly/dashR',
			upgrade = TRUE
			)
