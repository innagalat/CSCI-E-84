save.to.globalenv <- function(name, data) {
    # Saving objects to the global environment, by attaching them to a list
    if (is.null(get0("tab", e = globalenv()))) {
        tab <<- list()
    }
    tab[[name]] <<- data
}

ipak <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# usage packages <- c('ggplot2', 'plyr', 'reshape2', 'RColorBrewer', 'scales',
# 'grid') ipak(packages)
