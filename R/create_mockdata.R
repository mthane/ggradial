##### MOCK DATA #####
#' @import tidyverse
#' @import ggiraph
create_mockdata <- function(N = 100,
                            Nf = 20,
                            ncluster = 5,
                            ngroups = 5,
                            n_treatments=2
) {
  dfs <- data.frame(
    .id = sort(rep(seq(1, N),n_treatments)),
    .phase = rep(paste0("T", round(runif(N, 1, n_treatments))),n_treatments),
    .cluster = rep(paste0("C", round(runif(N, 0, ncluster))),n_treatments)
  )
  dfs_feature <- replicate(Nf, sample(rnorm(100), N*n_treatments, rep = TRUE))
  colnames(dfs_feature) <- paste0("feature_name_", seq(-1, Nf - 2))
  dfs <- cbind(dfs, dfs_feature)

  grs <- data.frame(
    .gr_id = seq(-1, Nf - 2),
    .gr_name = paste0("group_name_", sample(1:ngroups, Nf, replace = T)),
    feature = paste0("feature_name_", seq(-1, Nf - 2))
  )
  list(dfs=dfs, grs=grs)
}
