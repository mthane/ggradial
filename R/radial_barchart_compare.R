## Removed: cluster_idx, .cluster, cluster_assignment, color_clusters
## Other changes: added cluster_id to cluster_abbrev, legend

## Notes:
## Make sure that there is no cluster id. The function will require only the data of one cluster, so no cluster id - more intuitive and simple
## The function should do only one thing but well

## Not working:
## arrows have colors (maybe no problem)
## more than one post treatment - will probably require a good amount of work


#' Creates a radial barchart which shows a cluster's average feature values.
#'
#' @param df The dataset as data frame. Should be scaled. See ?scale.
#' @param group_names Character vector with group names of features.
#' The lengths of df and group_names must be equal. Group
#' names are displayed in the inner circle.
#' @param cluster_name Name of the cluster (optional)
#' TODO: change text @param cluster_abbrev Prefix of cluster_idx, e.g. "PT1", "PT2" etc.
#' @param color_inner_circle Color of the inner circle (optional).
#' @param scale_rng Min and max values to be shown.
#' @param interactive Boolean to indicate whether the plot should be interactive or not.
#' @param tooltip_bars String to choose which tooltip texts will be displayed for the bars (when interactive).
#' Options to select from: average, standard deviation, ci (confidence interval), difference, percentage difference, all. Default is average (optional).
#' @param tooltip_labels Data frame to provide the tooltip texts for the labels (when interactive), needs to have a "label" and a "description" column. The description provides the tooltip text for the assigned label (optional).
#' @param show_group_names ...
#' @import tidyverse
#' @import ggiraph
#' @return the radial barchart as ggplot
#' @export
radial_barchart_post_treatment <- function(df,
                                           group_names,
                                           id = ".id",
                                           phase = ".phase",
                                           feature_names = NULL,
                                           inner_label = "",
                                           color_inner_circle = NULL,
                                           scale_rng = c(-1,1)*1.5,
                                           interactive = FALSE,
                                           tooltip_bars = "",
                                           tooltip_labels = NULL,
                                           delta_threshold = 0.25,
                                           show_group_names = TRUE) {

  ## ------------------------------------------------------- Assertions
  # df
  if(!is.data.frame(df)) stop(ERROR_DF_WRONG_TYPE)



  # group_names
  if(!is.character(group_names) && !is.factor(group_names))
    stop(ERROR_GN_WRONG_TYPE)

  # color_inner_circle
  an.error.occured <- FALSE
  tryCatch( { result <- col2rgb(color_inner_circle) }
            , error = function(e) {an.error.occured <<- TRUE})
  if(an.error.occured) stop(ERROR_CIC_UNKNOWN_COLOR)

  # scale_rng
  if(!is.vector(scale_rng, mode = "numeric")) stop(ERROR_SR_NOT_NUMERIC)
  if(scale_rng[1] > scale_rng[2]) stop(ERROR_SR_WRONG_ORDER)
  if(scale_rng[1] == scale_rng[2]) stop(ERROR_SR_SAME_VALUE)

  # interactive
  if(!is.logical(interactive)) stop(ERROR_INTACT_WRONG_TYPE)

  # tooltip_bars
  if(is.null(tooltip_bars)) stop(ERROR_TTB_NULL_TYPE)

  # tooltip_labels

  # show_group_names

  # legend_label
  ## ------------------------------------------------------- Assertions

  ## probably could be removed an checked with an assert statement
  #vars_dummy <- setdiff(c(".id", ".phase"), names(df))
  #if(length(vars_dummy) > 0) for(d in vars_dummy) df <- mutate(df, !!d := NA_character_)
  #df <- select(df, .id, .phase, everything())

  if(!phase%in%colnames(df)){
    stop("The passed data does not contain the columns id and phase!")
  }

  df <- df%>%
    mutate(.id = df[[id]])%>%
    mutate(.phase = df[[phase]])

  if(is.null(feature_names)){
    feature_names <- setdiff(colnames(df),c(".id",".phase"))
  }


  # Set fill color of inner circle.
  ## TODO: Set default color here
  if(is.null(color_inner_circle)) {
    color_inner_circle <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(1)
  }

  # Convert group_names to factor if it is not already
  if(!is.factor(group_names)) group_names <- as.factor(group_names)

  # If post_treatment, filter patients that have 2 measurements
  data_cluster <- df %>%
    add_count(.id) %>%
    filter(n == 2) %>%
    select(-n)

  # Calculate cluster average for each feature
  data <- data_cluster %>%
    pivot_longer(names_to = "f", values_to = "v", cols = -c(.id, .phase)) %>%
    #inner_join(janitor::tabyl(data_cluster, .cluster) %>% select(.cluster, n), by = ".cluster") %>%
    mutate(n = nrow(df)) %>%
    group_by(.phase, f) %>%
    summarize(avg = mean(v), sd = sd(v), n = n[1]) %>%
    ungroup() %>%
    mutate(error = qnorm(0.975)*sd/sqrt(n)) %>%
    # winsorize cluster averages
    mutate(avg = ifelse(avg > scale_rng[2], scale_rng[2], avg)) %>%
    mutate(avg = ifelse(avg < scale_rng[1], scale_rng[1], avg)) %>%
    # mutate(sd = if_else(avg < 0, sd, -sd)) %>%
    inner_join(tibble(f = names(df %>% select(-starts_with("."))), group = group_names), by = "f") %>%
    arrange(group, f)

  # q_id is the group rank
  # f_id is the feature id after inserting a space of 2 units between each q_id
  df_plot <- data %>%
    mutate(id = as.double(row_number())) %>%
    mutate(q_id = dense_rank(group)) %>%
    mutate(f_id = if_else(q_id == 1, id, id + 2*(q_id-1))) %>%
    mutate(f_id_adj = case_when(.phase == "A" ~ f_id + 0.15,
                                .phase == "E" ~ f_id - 0.15,
                                TRUE ~ f_id)) %>%
    complete(f_id = -1:(max(f_id)+2), fill = list(feature = "", avg = NA_real_)) %>%
    ungroup()

  # browser()

  # base_data ----
  # position of group lines and labels
  base_data <- df_plot %>%
    drop_na(-.phase) %>%
    group_by(group) %>%
    summarize(
      start = min(f_id_adj),
      end = max(f_id_adj),
      title = start + 0.5*(end-start)
    ) %>%
    ungroup() %>%
    mutate(hjust = case_when(between(title/max(end), 0.075, 0.425) ~ 1,
                             between(title/max(end), 0.575, 0.925) ~ 0,
                             TRUE ~ 0.5)) %>%
    mutate(hjust = if_else(title/max(end) > 0.9, 0, hjust)) %>%
    # mutate(hjust = case_when(title/max(end) < 0.5 ~ 1,
    #                          TRUE ~ 0)) %>%
    mutate(vjust = case_when(title/max(end) < 0.1 | title/max(end) > 0.9 ~ 1,
                             between(title/max(end), 0.4, 0.6) ~ 0,
                             TRUE ~ 0.5))
  if(!show_group_names) base_data <- base_data %>%
    mutate(group = factor(group, labels = seq_along(levels(group))))

  # grid_data ----
  # position of grid lines between groups
  grid_data <- df_plot %>%
    # filter(.cluster == .cluster[1]) %>%
    filter(is.na(id)) %>%
    select(f_id) %>%
    mutate(diff = f_id - lag(f_id, default = f_id[1])) %>%
    mutate(g = cumsum(diff > 1) + 1) %>%
    select(-diff) %>%
    group_by(g) %>%
    summarize(start = f_id[1], end = f_id[2]) %>%
    ungroup() %>%
    mutate(y = list(seq(scale_rng[1], scale_rng[2], 0.5))) %>%
    unnest(y)

  # label_data ----
  # position of feature names
  label_data <- df_plot %>%
    mutate(f_id_min = min(f_id) - 2, f_id_max = max(f_id) + 2) %>%
    filter(!is.na(f)) %>%
    mutate(y = avg) %>%
    group_by(f, f_id_min, f_id_max) %>%
    summarize(f_id = mean(f_id), avg = max(avg), y = max(y)) %>%
    ungroup() %>%
    mutate(rel_pos = f_id / diff(c(f_id_min[1], f_id_max[1]))) %>%
    # select(f_id_adj, f, avg) %>%
    # mutate(feature_suffix = str_trunc(feature_suffix, 12)) %>%
    mutate(y = if_else(y > 0, y, 0)) %>%
    mutate(angle = 90 - 360 * (rel_pos + 0.035)) %>%
    # mutate(angle = 90 - 360 * ((row_number() + 2 - 0.5) / (n() + 4))) %>%
    mutate(hjust = if_else(angle < -90, 1, 0)) %>%
    mutate(angle = if_else(angle < -90, angle + 180, angle)) %>%
    drop_na() #%>%
  # mutate(size = if_else(avg >= 0 & (avg * 5 + nchar(f) > 20), 7/.pt, 8/.pt))

  if(!is.null(tooltip_labels))
  {
    label_data <- label_data %>%
      left_join(tooltip_labels %>% filter(!is.na(label)) %>% select(f = label, f_desc = description), by = "f")
  } else {
    label_data <- label_data %>% mutate(f_desc = NA_character_)
  }


  x_lim <- c(-2,2)+range(df_plot$f_id)
  y_lim <- c(scale_rng[1] - (scale_rng[2] - scale_rng[1]) * c(0.5, 1)[show_group_names+1],
             scale_rng[2] + 1.05)

  treatments <- unique(df_plot$.phase)
  treatments <- treatments[!is.na(treatments)]
  segments_dfs <- list()
  for (i in seq(1,length(treatments)-1)){
    tr1 <- treatments[i]
    tr2 <- treatments[i+1]
    teffect_segments <- df_plot %>%
      filter(.phase==tr1|.phase==tr2)%>%
      select(.phase, f, avg, f_id_adj) %>%
      filter(!is.na(avg)) %>%
      pivot_wider(id_cols = f, names_from = .phase, values_from = c(avg, f_id_adj))%>%
      mutate(effect = .[[paste("avg",tr1,sep="_")]] - .[[paste("avg",tr2,sep="_")]]) %>%
      mutate(effect_category = case_when(effect > delta_threshold ~ 1,
                                         effect < -delta_threshold ~ 3,
                                         TRUE ~ 2)) %>%
      mutate(effect_category = factor(effect_category,
                                      levels = 1:3,
                                      labels = c("decreased",
                                                 paste0("unchanged ($\\Delta\\leq\\pm$", delta_threshold, " SD)"),
                                                 "increased")))

    segments_dfs[[i]]<- teffect_segments


  }
  teffect_segments <- bind_rows(segments_dfs)

  label_data <- label_data %>% left_join(teffect_segments %>% select(f, effect_category), by = "f")

  if(interactive) {

    df_plot <- df_plot %>%
      left_join(teffect_segments) %>% select(f, avg_T0, avg_T1) %>%
      mutate(tooltip = case_when(
        tooltip_bars == "sd" ~ paste0("&sigma;=", format(round(sd, 3), nsmall = 3)),
        tooltip_bars == "ci" ~ paste0("ci=[", format(round(avg-error, 3), nsmall = 3), ",", format(round(avg+error, 3), nsmall = 3), " ]"),
        tooltip_bars == "difference" ~ paste0("difference=", format(round(avg_T1 - avg_T0, 3), nsmall = 3)),
        tooltip_bars == "percentage difference" ~ paste0("% difference=", format(round(abs((avg_T1 - avg_T0) / avg_T0) * 100, 1), nsmall = 1)),
        tooltip_bars == "all" ~ paste0("&mu;=", format(round(avg, 3), nsmall = 3), "\n&sigma;=", format(round(sd, 3), nsmall = 3), "\nci=[", format(round(avg-error, 3), nsmall = 3), ",", format(round(avg+error, 3), nsmall = 3), " ]", "\ndifference=", format(round(avg_T1 - avg_T0, 3), nsmall = 3), "\n% difference=", format(round(abs((avg_T1 - avg_T0) / avg_T0)  * 100, 1), nsmall = 1)),
        TRUE ~ paste0("&mu;=", format(round(avg, 3), nsmall = 3)))
      ) %>%
      select(-c(avg_T0, avg_T1))
  }
  # browser()
  # define font sizes ----
  font_sizes <- list(
    inner_label = ifelse(interactive, 4/.pt, 1/.pt),
    y_axis = ifelse(interactive, 9/.pt, 6/.pt),
    group_names = ifelse(interactive, 7/.pt, 5/.pt),
    feature_names = ifelse(interactive, 11/.pt, 8/.pt),
    legend_title = ifelse(interactive, 9, 7),
    legend_key = ifelse(interactive, 2, 0.8), # in cm
    legend_text = ifelse(interactive, 9, 6)
  )

  # create ggplot2 object ----
  p <- ggplot(df_plot)
  p <- p + coord_polar()
  p <- p + scale_x_continuous(limits = x_lim, expand = c(0,0),
                              breaks = df_plot$f_id, labels = df_plot$f)
  p <- p + scale_y_continuous(limits = y_lim)
  # p <- p + guides(fill = "none")
  p <- p + theme_minimal()
  # p <- p + theme(legend.position = "top")
  p <- p + theme(axis.text = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(panel.grid = element_blank())
  # p <- p + theme(legend.title = element_text(size = font_sizes$legend_title, face = "bold"))
  # p <- p + theme(legend.text = element_text(size = font_sizes$legend_text))
  # p <- p + theme(legend.key.size = unit(font_sizes$legend_key, "cm"))
  # draw inner circle ----
  p <- p + annotate("rect", xmin = x_lim[1], xmax = x_lim[2],
                    ymin = y_lim[1], ymax = scale_rng[1]-0.2,
                    color = "transparent", fill = color_inner_circle, alpha = 0.2)
  # central inner label: cluster id and number of instances ----
  p <- p +
    annotate("text", x = x_lim[1], y = y_lim[1], # + 0.5,
             label = inner_label,
             size = font_sizes$inner_label, fontface = "bold", lineheight = 1, vjust = 0.5)
  # the actual bars ----
  current_opts <- list(
    data = filter(df_plot, !is.na(avg)),
    mapping = aes(x = f_id_adj, y = avg, fill = avg),
    width = 0.7
  )
  if(interactive) {
    current_opts$mapping <- ggplot2:::rename_aes(modifyList(current_opts$mapping,
                                                            aes(tooltip = tooltip, data_id = f)))
    p <- p + do.call(geom_col_interactive, current_opts)
  } else {
    p <- p + do.call(geom_col, current_opts)
    p <- p + guides(fill = guide_colorbar(title = NULL, ticks.colour = "grey", nbin = 300,
                                          ticks.linewidth = 1.5))
    p <- p + theme(legend.position = c(0.975,0.975))
    p <- p + theme(legend.justification = c(1,1))
    p <- p + theme(legend.direction = "vertical")
    p <- p + theme(legend.key.width = unit(0.5, "lines"))
    p <- p + theme(legend.key.height = unit(1, "lines"))
  }


  max_x = unique(label_data$f_id_max)[1]
  x_interval = max_x / length(unique(label_data$f))

  offset_x = x_interval/10
  p <- p + scale_fill_distiller(palette = "RdYlBu",
                                limits = c(scale_rng[1], scale_rng[2]),
                                breaks = c(scale_rng[1], 0, scale_rng[2]),
                                labels = c("-1.5 SD", "Patient\naverage", "+1.5 SD"))

  # add lines between features
  p <- p+geom_segment(data=label_data,aes(x=f_id-x_interval/2+offset_x  ,y=-1.5,
                                          xend=f_id-x_interval/2+offset_x  ,yend=1.5),
                      linetype="dashed",alpha=0.5)

  # add some lines between feature groups ----
  p <- p + geom_segment(data = grid_data, aes(x = start, xend = end, y = y, yend = y),
                        color = "grey", size = 0.3, linetype = 1)
  # y axis text ----
  p <- p + annotate("text", x = rep(x_lim[2], 7),
                    y = seq(scale_rng[1], scale_rng[2], length.out = 7),
                    label = str_replace(sprintf("%+.1f", seq(scale_rng[1], scale_rng[2], length.out = 7)), "\\+0.0", "0"),
                    color = "black", size = font_sizes$y_axis, hjust = 0.5, angle = 0)
  # add 0 line ----
  p <- p + geom_segment(data = base_data, aes(x = start - 0.5, y = 0,
                                              xend = end + 0.5, yend = 0),
                        color = "black", size = 0.6)
  # add baseline ----
  p <- p + geom_segment(data = base_data, aes(x = start - 0.5, y = -1.7,
                                              xend = end + 0.5, yend = -1.7),
                        color = "black", size = 0.6)
  # if(show_group_names) {
  # add group ticks ----
  p <- p + geom_segment(data = base_data, aes(x = title, xend = title,
                                              y = -1.8, yend = -1.7), color = "black")
  # add group names ----
  p <- p + geom_text(data = base_data, aes(x = title, y = scale_rng[1] - 0.15 * (scale_rng[2] - scale_rng[1]),
                                           label = group, hjust = hjust, vjust = vjust),
                     lineheight = 0.85,
                     colour = "black", alpha = 0.8, size = font_sizes$group_names)
  # }

  # add labels on top of each bar ----
  current_opts <- list(
    data = label_data,
    mapping = aes(x = f_id, y = y + 0.1,
                  label = f, hjust = hjust,
                  angle = angle), size = font_sizes$feature_names*c(1,0.8)[2],
    alpha = 0.6, show.legend = FALSE
  )
  current_opts$mapping <- ggplot2:::rename_aes(modifyList(current_opts$mapping,
                                                          aes(color = effect_category)))

  if(interactive) {
    current_opts$mapping <- ggplot2:::rename_aes(modifyList(current_opts$mapping,
                                                            aes(data_id = f, tooltip = f_desc)))

    p <- p + do.call(geom_text_interactive, current_opts)
  } else {
    p <- p + do.call(geom_text, current_opts)
  }
  p <- p + scale_size_identity()

  # add treatment effect arrows ----
  for (i in seq(1,length(treatments)-1)){
    tr1 <- treatments[i]
    tr2 <- treatments[i+1]

    p <- p +
      geom_segment(data = teffect_segments, aes_string(x = paste0("f_id_adj_",tr1), xend = paste0("f_id_adj_",tr2),
                                                       y = paste0("avg_",tr1), yend =paste0("avg_",tr2), color = "effect_category"),
                   arrow = arrow(length = unit(c(0.3,0.2)[1+interactive], "lines"), type = "closed"),
                   #           key_glyph = "segment_custom", size = 0.45) +
      )
  }
  # geom_segment(data = teffect_segments, aes(x = f_id_adj_A - 0.35, xend = f_id_adj_E + 0.35,
  #                                           color = effect_category),
  #              y = scale_rng[1], yend = scale_rng[1],
  #              size = 1) +
  guides(color = guide_legend(title = "Treatment effect")) +
    scale_color_manual(values = c("darkgreen", "black", "red"), drop = FALSE,
                       labels = unname(latex2exp::TeX(levels(teffect_segments$effect_category))))

  if(interactive) {
    girafe(ggobj = p)
  } else {
    p
  }
}

