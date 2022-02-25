
ERROR_DF_WRONG_TYPE <- "df needs to be a data frame"
ERROR_GN_WRONG_TYPE <- "group_names needs to be a character vector or factor"
ERROR_IL_WRONG_TYPE <- "inner_label needs to be of type character"
ERROR_CIC_UNKNOWN_COLOR <- "color_inner_circle has unknown color"
ERROR_SR_NOT_NUMERIC <- "scale_range is not numeric"
ERROR_SR_WRONG_ORDER <- "scale_range needs to start with the lower value"
ERROR_SR_SAME_VALUE <- "scale_range needs to have to different values"
ERROR_INTACT_WRONG_TYPE <- "interactive needs to be of type boolean"
ERROR_TTB_NULL_TYPE <- "tooltip_bars cannot be of NULL"
ERROR_TTL_WRONG_TYPE <- "tooltip_labels needs to be a data frame"
ERROR_TTL_MISSING_COLUMN <- "tooltip_labels does not contain the columns label and description"
ERROR_SGN_WRONG_TYPE <- "show_group_names needs to be of type boolean"

#' Creates a radial barchart which shows a cluster's average feature values.
#'
#' @param df The dataset as data frame. Should be scaled. See ?scale.
#' Should only contain the category values in a wide table form.
#' @param group_names Character vector with group names of features.
#' The number of columns in df and the length of group_names must be equal. Group
#' names are displayed in the inner circle.
#' @param inner_label Label inside the inner circle, e.g. name of the chart (optional).
#' @param color_inner_circle Color of the inner circle (optional).
#' @param scale_rng Min and max values to be shown (optional).
#' @param interactive Boolean to indicate whether the plot should be interactive or not (optional).
#' @param tooltip_bars String to choose which tooltip texts will be displayed for the bars (when interactive).
#' Options to select from: average, sd (standard deviation), ci (confidence interval), all. Default is average (optional).
#' @param tooltip_labels Data frame to provide the tooltip texts for the labels (when interactive), needs to have a "label" and a "description" column. The description provides the tooltip text for the assigned label (optional).
#' @param show_group_names Boolean to indicate whether the group names should be shown inside the inner circle. Default is true (optional).
#'
#' @return the radial barchart as ggplot

radial_barchart_static <- function(df,
                                   group_names,
                                   inner_label = "",
                                   color_inner_circle = "grey60",
                                   scale_rng = c(-1,1)*1.5,
                                   interactive = FALSE,
                                   tooltip_bars = "",
                                   tooltip_labels = NULL,
                                   show_group_names = TRUE
) {

  ## ------------------------------------------------------- Assertions
  # df
  if(!is.data.frame(df)) stop(ERROR_DF_WRONG_TYPE)

  # group_names
  if(!is.array(group_names) && !is.factor(group_names))
    stop(ERROR_GN_WRONG_TYPE)

  # inner_label
  if(!is.character(inner_label)) stop(ERROR_IL_WRONG_TYPE)

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
  if(!is.null(tooltip_labels)) {
    if (!is.data.frame(tooltip_labels)) stop(ERROR_TTL_WRONG_TYPE)
    if (!all(c("label", "description")%in%colnames(tooltip_labels))) stop(ERROR_TTL_MISSING_COLUMN)
  }

  # show_group_names
  if(!is.logical(show_group_names)) stop(ERROR_SGN_WRONG_TYPE)
  ## ------------------------------------------------------- Assertions

  # Convert group_names to factor if it is not already
  if(!is.factor(group_names)) group_names <- as.factor(group_names)

  # Calculate cluster average for each feature
  data <- df %>%
    pivot_longer(names_to = "f", values_to = "v", cols = everything()) %>%
    #inner_join(janitor::tabyl(data_cluster, .cluster) %>% select(.cluster, n), by = ".cluster") %>%
    mutate(n = nrow(df)) %>%
    group_by(f) %>%
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
    mutate(f_id_adj = f_id) %>%
    complete(f_id = -1:(max(f_id)+2), fill = list(feature = "", avg = NA_real_)) %>%
    ungroup()

  # browser()
  # base_data ----
  # position of group lines and labels
  base_data <- df_plot %>%
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
                             TRUE ~ 0.5)) %>%
    filter(!is.na(title))

  ## Sets numbers instead of names
  if(!show_group_names) base_data <- base_data %>%
    mutate(group = factor(group, labels = seq_along(levels(factor(group)))))

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
    mutate(y = avg + error) %>%
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

  if(interactive) {
    df_plot <- df_plot %>%
      mutate(tooltip = case_when(
        tooltip_bars == "sd" ~ paste0("&sigma;=", format(round(sd, 3), nsmall = 3)),
        tooltip_bars == "ci" ~ paste0("ci=[", format(round(avg-error, 3), nsmall = 3), ",", format(round(avg+error, 3), nsmall = 3), " ]"),
        tooltip_bars == "all" ~ paste0("&mu;=", format(round(avg, 3), nsmall = 3), "\n&sigma;=", format(round(sd, 3), nsmall = 3), "\nci=[", format(round(avg-error, 3), nsmall = 3), ",", format(round(avg+error, 3), nsmall = 3), " ]")))
  }

  # browser()
  # define font sizes ----
  font_sizes <- list(
    inner_label = ifelse(interactive, 14/.pt, 10/.pt),
    y_axis = ifelse(interactive, 9/.pt, 6/.pt),
    group_names = ifelse(interactive, 10/.pt, 8/.pt),
    feature_names = ifelse(interactive, 11/.pt, 8/.pt),
    legend_title = ifelse(interactive, 14, 10),
    legend_key = ifelse(interactive, 2, 0.8), # in cm
    legend_text = ifelse(interactive, 14, 8)
  )

  # create ggplot2 object ----
  p <- ggplot(df_plot)
  p <- p + coord_polar()
  p <- p + scale_x_continuous(limits = x_lim, expand = c(0,0),
                              breaks = df_plot$f_id, labels = df_plot$f)
  p <- p + scale_y_continuous(limits = y_lim)
  #p <- p + guides(fill = "none")
  p <- p + theme_minimal()
  #p <- p + theme(legend.position = "top")
  p <- p + theme(axis.text = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(panel.grid = element_blank())
  #p <- p + theme(legend.title = element_text(size = font_sizes$legend_title, face = "bold"))
  #p <- p + theme(legend.text = element_text(size = font_sizes$legend_text))
  #p <- p + theme(legend.key.size = unit(font_sizes$legend_key, "cm"))
  # draw inner circle ----
  p <- p + annotate("rect", xmin = x_lim[1], xmax = x_lim[2],
                    ymin = y_lim[1], ymax = scale_rng[1]-0.2,
                    color = "transparent",
                    fill = col2rgb(color_inner_circle), alpha = 0.2)
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

  # p <- p + scale_fill_distiller(palette = "RdYlBu",
  #                               limits = c(scale_rng[1], scale_rng[2]),
  #                               breaks = seq(scale_rng[1], scale_rng[2], length.out = 7),
  #                               labels = c("-1.5 SD", "-1.0 SD", "-0.5 SD", "Patient\naverage",
  #                                          "+0.5 SD", "+1.0 SD", "+1.5 SD"))
  p <- p + scale_fill_distiller(palette = "RdYlBu",
                                limits = c(scale_rng[1], scale_rng[2]),
                                breaks = c(scale_rng[1], 0, scale_rng[2]),
                                labels = c(scale_rng[1], "Patient\naverage", scale_rng[2]))

  # add some lines between feature groups ----
  ## the small grey lines
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
  if(show_group_names) {
    # add group ticks ----
    p <- p + geom_segment(data = base_data, aes(x = title, xend = title,
                                                y = -1.8, yend = -1.7), color = "black")
    # add group names ----
    p <- p + geom_text(data = base_data, aes(x = title, y = scale_rng[1] - 0.15 * (scale_rng[2] - scale_rng[1]),
                                             label = group, hjust = hjust, vjust = vjust),
                       lineheight = 0.85,
                       colour = "black", alpha = 0.8, size = font_sizes$group_names)
  }
  # add standard error as error bar ----
  p <- p + geom_errorbar(aes(x = f_id_adj, ymin = avg-error, ymax = avg+error),
                         color = "grey60", size = 0.4, width = 0.5, #alpha = 0.5,
                         na.rm = TRUE)
  # # add small horizontal line on top of error bar
  # p <- p + geom_segment(aes(x = f_id_adj-0.2, y = -sd, xend = f_id_adj+0.2, yend = -sd),
  #                       color = "grey60", size = 0.4, alpha = 0.5, na.rm = TRUE)


  # add labels on top of each bar ----
  current_opts <- list(
    data = label_data,
    mapping = aes(x = f_id, y = y + 0.1,
                  label = f, hjust = hjust,
                  angle = angle), size = font_sizes$feature_names*c(1,0.8)[1],
    alpha = 0.6, show.legend = FALSE
  )

  if(interactive) {
    current_opts$mapping <- ggplot2:::rename_aes(modifyList(current_opts$mapping,
                                                            aes(data_id = f, tooltip = f_desc)))

    p <- p + do.call(geom_text_interactive, current_opts)
  } else {
    p <- p + do.call(geom_text, current_opts)
  }
  p <- p + scale_size_identity()

  ## Added for interactive plot
  if(interactive) {
    girafe(ggobj = p)
  } else {
    p
  }
}

