
source('prepare_training_data.R')
source('sparse_models.R')
source('boosting.R')

library(knitr)
rmarkdown::render('post.Rmd', output_file = 'index.html')
