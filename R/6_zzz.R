# .onAttach <- function(libname, pkgname) {
#   needed <- core[!is_attached(core)]
#   if (length(needed) == 0)
#     return()
#   
#   crayon::num_colors(TRUE)
#   microbiomeplot_attach()
#   
#   # if (!"package:conflicted" %in% search()) {
#   #   x <-microbiomeplot_conflicts()
#   #   msg(microbiomeplot_conflict_message(x), startup = TRUE)
#   # }
# }
# 
# is_attached <- function(x) {
#   paste0("package:", x) %in% search()
# }
