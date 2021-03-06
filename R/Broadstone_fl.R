#' 'Forbidden' links to accompany the \code{Broadstone} data set
#'
#' Networks often contain 'forbidden links', where interactions are not possible
#'   between two taxa due to factors such as large differences in body size,
#'   spatio-temporal mismatches or specialised flower morphology in flower visitation
#'   networks.  \code{Broadstone.fl} summarises the forbidden links in the
#'   \code{\link{Broadstone}} food web, estimated from the interactions observed
#'   from a complete year's sampling (Table 3 in Woodward \emph{et al}. 2005). This
#'   table is illustrative, and should not be considered a definitive list.
#' @format A data frame with 20 columns and seven rows, one row for each of the
#'   seven predator taxa.  The first column (\code{Predator}) lists the seven taxa,
#'   whilst the remaining 19 columns are the invertebrate taxa in the food web:
#'   each element indicates whether the predator can consume that taxon (1) or
#'   whether the link is 'forbidden' (0).
#'
#' @source Woodward, G., Speirs, D.C. & Hildrew, A.G. (2005) Quantification and
#'   resolution of a complex, size-structured food web. \emph{Advances in
#'   Ecological Research}, \strong{36}, 84--135.
"Broadstone.fl"
