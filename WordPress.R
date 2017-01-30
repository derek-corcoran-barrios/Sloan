if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(RWordPress)
options(WordpressLogin = c(user = 'password'),
        WordpressURL = 'https://user.wordpress.com/xmlrpc.php')
library(knitr)
knit2wp('Projection.Rmd', title = 'Your post title')