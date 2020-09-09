#install package that enables downloading package from github
install.packages("remotes")
library(remotes)

#install "livecode" package
#remotes::install_github("rundel/livecode")
remotes::install_github("rundel/livecode@unicode_iface") #solve unicode(incl. maybe Kor) problem

#get ip address of my live share
s <- livecode::serve_file()