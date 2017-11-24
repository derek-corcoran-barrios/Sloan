if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}


library(RWordPress)
options(WordpressLogin = c(spatialball = '135792468++'),
        WordpressURL = 'https://spatialball.wordpress.com/xmlrpc.php')
library(knitr)
knit2wp('Projection.Rmd', title = 'Projections',  action = c("editPost"), postid = 77)

knit2wp('Rating.Rmd', title = 'Rankings',  action = c("newPost"))
