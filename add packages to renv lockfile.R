# enter packages
v_pkg <- c("tidyverse")

# install packages
install.packages(v_pkg)

# sync packages with renv lockfile
renv::snapshot(packages = v_pkg, update = TRUE)