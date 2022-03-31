##### MOCK DATA #####
#' @import tidyverse
#' @import ggiraph
#' @export
create_mockdata <- function(N = 100,
                            Nf = 20,
                            ncluster = 5,
                            ngroups = 5,
                            n_treatments=2
) {
  dfs <- data.frame(
    .id = sort(rep(seq(1, N),n_treatments)),
    .phase = rep(paste0("T", sample(1:n_treatments, replace = T, size = N)), n_treatments),
    .cluster = rep(paste0("C", round(runif(N, 0, ncluster))),n_treatments)
  ) %>% complete(.id,.phase)
  dfs_feature <- replicate(Nf, sample(rnorm(100), nrow(dfs)*n_treatments, rep = TRUE))
  colnames(dfs_feature) <- paste0("f_", seq(-1, Nf - 2))
  dfs <- cbind(dfs, dfs_feature)

  grs <- data.frame(
    .gr_id = seq(-1, Nf - 2),
    .gr_name = paste0("group_", sample(1:ngroups, Nf, replace = T)),
    feature = paste0("f_", seq(-1, Nf - 2))
  )
  list(dfs=dfs, grs=grs)
}
