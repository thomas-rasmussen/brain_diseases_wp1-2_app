# Version suffix used in filenames
version_suffix <- "v9_2024-11-05"

# Find label associated with brain disease definition key value
var_def_label <- function(x) {
  case_when(
    x == "def01" ~ "Sensitivity analysis 1",
    x == "def02" ~ "Sensitivity analysis 2",
    x == "def03" ~ "Main analysis",
    x == "def04" ~ "Sensitivity analysis 3"
  )
}

# Find label associated with population key value
population_label <- function(x) {
  case_when(
    x == "inc_2011_2015" ~ "Incident cohort 2011-2015",
    x == "inc_2016_2021" ~ "Incident cohort 2016-2021",
    x == "prev_2015" ~ "Prevalent cohort 2015",
    x == "prev_2016" ~ "Prevalent cohort 2016",
    x == "prev_2017" ~ "Prevalent cohort 2017",
    x == "prev_2018" ~ "Prevalent cohort 2018",
    x == "prev_2019" ~ "Prevalent cohort 2019",
    x == "prev_2020" ~ "Prevalent cohort 2020",
    x == "prev_2021" ~ "Prevalent cohort 2021",
  )
}

# Function for making stacked barplot
make_barplot <- function(
    x,
    title = NULL,
    subtitle = NULL,
    y_order = NULL,
    x_axis_label = c("eur", "bil_eur", "lost_prod"),
    include_yaxis_text = TRUE,
    flip_plot = FALSE
    ) {
  
  # Generate x axis label
  x_axis_label <- case_match(x_axis_label,
    "eur" ~ "Cost in EUR",
    "bil_eur" ~ "Cost in billion EUR",
    "lost_prod" ~ "Lost productivity in billion EUR"
  )
  
  if (isTRUE(flip_plot)) {
    tmp <- x %>%
      ggplot(aes(
        x = -cost_var,
        y = var_name_label,
        fill = cost_component_label
      ))
  }
  
  if (isFALSE(flip_plot)) {
    tmp <- x %>%
      ggplot(aes(
        x = cost_var,
        y = var_name_label,
        fill = cost_component_label
      ))
  }
  
  tmp <- tmp +
      geom_bar(position = "stack", stat = "identity") +    
      theme_bw()
  
  if (x_axis_label %in% c("Cost in EUR")) {
    tmp <- tmp +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) formatC(abs(x), format = "f", big.mark = ",", digits = 0)
      )
  }
  else if (x_axis_label %in% c("Cost in billion EUR", "Lost productivity in billion EUR")) {
    tmp <- tmp +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) formatC(abs(x), format = "f", big.mark = ",", digits = 1)
      )
  }
  
  tmp <- tmp +
      scale_y_discrete(expand = c(0, 0)) +
      # cvd-friendly qualitative color palette from "Fundamentals of Data
      # Visualization" figure 19.10
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                   "#0072B2", "#D55E00")) +
      labs(
        title = title,
        subtitle = subtitle,
        x = x_axis_label
      ) +
      theme(legend.position = "bottom",
            panel.grid.major.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 12, colour = "black"),
            axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
            axis.ticks.y = element_blank(),
            axis.line.x.bottom = element_line(colour = "black"),
            panel.border = element_blank(),
            legend.title = element_blank(),
            legend.direction = "vertical",
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 18)
      ) +
      guides(fill = guide_legend(nrow = 2, reverse = TRUE, byrow = TRUE))
  
  if (isFALSE(include_yaxis_text)) {
    tmp <- tmp +
      theme(
        axis.text.y = element_blank()
      )
  }
  
  tmp
}

# Utility function to make var_label variable in a data.frame into a factor,
# where level ordering is based on the total sum of the specified cost
# variable (minus lost productivity cost component)
update_var_label <- function(x, cost_var) {
  x[["cost_var"]] <- x[[cost_var]]
  
  level_order <- x %>%
    filter(cost_component != "lost_production_sickness") %>%
    group_by(var_name_label) %>%
    summarize(sum = sum(cost_var)) %>%
    arrange(sum) %>%
    select(var_name_label) %>%
    pull()
  
  x %>%
    mutate(
      var_name_label = factor(var_name_label, levels = level_order)
    ) %>%
    select(-cost_var)
}

# Create sidebars for costs analysis tabs
costs_sidebar <- function(id_prefix) {
  sidebar(
    selectInput(
      inputId = paste0(id_prefix, "_var_def_id"),
      label = "Brain disease definition",
      choices = list(
        "Main analysis"          = "def03",
        "Sensitivity analysis 1" = "def01",
        "Sensitivity analysis 2" = "def02",
        "Sensitivity analysis 3" = "def04"
      )
    ),
    selectInput(
      inputId = paste0(id_prefix, "_population_id"),
      label = "Population",
      choices = list(
        "Incident cohort 2011-2015" = "inc_2011_2015",
        "Incident cohort 2016-2021" = "inc_2016_2021",
        "Prevalent cohort 2015" = "prev_2015",
        "Prevalent cohort 2016" = "prev_2016",
        "Prevalent cohort 2017" = "prev_2017",
        "Prevalent cohort 2018" = "prev_2018",
        "Prevalent cohort 2019" = "prev_2019",
        "Prevalent cohort 2020" = "prev_2020",
        "Prevalent cohort 2021" = "prev_2021"
      )
    )
  )
}

costs_sidebar_no_inc <- function(id_prefix) {
  sidebar(
    selectInput(
      inputId = paste0(id_prefix, "_var_def_id"),
      label = "Brain disease definition",
      choices = list(
        "Main analysis"          = "def03",
        "Sensitivity analysis 1" = "def01",
        "Sensitivity analysis 2" = "def02",
        "Sensitivity analysis 3" = "def04"
      )
    ),
    selectInput(
      inputId = paste0(id_prefix, "_population_id"),
      label = "Population",
      choices = list(
        "Prevalent cohort 2015" = "prev_2015",
        "Prevalent cohort 2016" = "prev_2016",
        "Prevalent cohort 2017" = "prev_2017",
        "Prevalent cohort 2018" = "prev_2018",
        "Prevalent cohort 2019" = "prev_2019",
        "Prevalent cohort 2020" = "prev_2020",
        "Prevalent cohort 2021" = "prev_2021"
      )
    )
  )
}

costs_sidebar_no_pop<- function(id_prefix) {
  sidebar(
    selectInput(
      inputId = paste0(id_prefix, "_var_def_id"),
      label = "Brain disease definition",
      choices = list(
        "Main analysis"          = "def03",
        "Sensitivity analysis 1" = "def01",
        "Sensitivity analysis 2" = "def02",
        "Sensitivity analysis 3" = "def04"
      )
    )
  )
}

mortality_sidebar <- function(id_prefix) {
  sidebar(
    selectInput(
      inputId = paste0(id_prefix, "_var_def_id"),
      label = "Brain disease definition",
      choices = list(
        "Main analysis"          = "def03",
        "Sensitivity analysis 1" = "def01",
        "Sensitivity analysis 2" = "def02",
        "Sensitivity analysis 3" = "def04"
      )
    ),
    selectInput(
      inputId = paste0(id_prefix, "_population_id"),
      label = "Population",
      choices = list(
        "Incident cohort 2011-2015" = "inc_2011_2015",
        "Incident cohort 2016-2021" = "inc_2016_2021"
      )
    )
  )
}

mortality_sidebar_no_pop <- function(id_prefix) {
  sidebar(
    selectInput(
      inputId = paste0(id_prefix, "_var_def_id"),
      label = "Brain disease definition",
      choices = list(
        "Main analysis"          = "def03",
        "Sensitivity analysis 1" = "def01",
        "Sensitivity analysis 2" = "def02",
        "Sensitivity analysis 3" = "def04"
      )
    )
  )
}


# Make cost regression forestplot. Assumes input data.frame `x`
# is a suitable subset of the cost_reg_est data.frame.
cost_regression_forestplot <- function(x) {

  # Sort data for tables and plot
  x <- x %>%
    arrange(-estimate) %>%
    mutate(var_name_label = factor(var_name_label, levels = .$var_name_label))
      
  # Make data for striped rows in plot
  stripe_col_val <- rep(c("white", "lightblue"), nrow(x)/2)
  if (nrow(x) %% 2 == 1) {
    stripe_col_val <- c(stripe_col_val, "white")
  }
  
  stripes_data <- tibble(
    ymin_val = 1:nrow(x) - 0.5,
    ymax_val = 1:nrow(x) + 0.5,
    xmin_val = min(x$lcl) - 0.05,
    xmax_val = max(x$ucl) + 0.05,
    stripe_col = stripe_col_val
  )
  
  ### Make plots with parameter estimates ###
  
  estimates_plot <- x %>%
    ggplot(aes(x = estimate, y = rev(var_name_label), xmin = lcl, xmax = ucl)) +
    geom_rect(
      data = stripes_data,
      mapping = aes(
        xmin = xmin_val,
        xmax = xmax_val,
        ymin = ymin_val,
        ymax = ymax_val,
        fill = stripe_col,
        x = NULL,
        y = NULL
      )
    ) +
    geom_linerange() +
    geom_point() +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      panel.border = element_rect(colour = "grey")
    ) +
    labs(x = NULL, y = NULL) +
    scale_fill_identity() +
    scale_y_discrete(expand = expansion(0, 0)) +
    scale_x_continuous(
      expand = expansion(0, 0),
      limits = c(min(x$lcl) - 0.05, max(x$ucl) + 0.05)
    )
  
    ### Make tables ###
    tbl_left <- x %>%
      select(var_name_label) %>%
      gt() %>%
      cols_align(align = "left", columns = var_name_label) %>%
      cols_label(var_name_label = "Brain disorder") %>%
      tab_options(
        table.border.top.style = "hidden",
        column_labels.font.weight = "bold",
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightblue",
        table_body.hlines.color = "lightblue"
      )
    
    tbl_right <- x %>%
      select(est_ci) %>%
      gt() %>%
      cols_label(est_ci = "Estimate (95% CI)") %>%
      tab_options(
        table.border.top.style = "hidden",
        column_labels.font.weight = "bold",
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightblue",
        table_body.hlines.color = "lightblue"
      )
    
    wrap_table(tbl_left, space = "fixed") + estimates_plot + wrap_table(tbl_right, space = "fixed")
  
}


# Make mortality forestplot. Assumes input data.frame `x`
# is a suitable subset of the risk_cox data.frame.
mortality_forestplot <- function(x) {

  ### Restructure data ###
  df01 <- x %>%
    mutate(
      death_pct = paste0(
        formatC(deaths_n, digits = 0, format = "f", big.mark = ","),
        " (",
        formatC(deaths_prop * 100, digits = 1, format = "f", big.mark = ","),
        ")"
      ),
      hr_ci = paste0(
        formatC(hr, digits = 1, format = "f"),
        " (",
        formatC(hr_lcl, digits = 1, format = "f"),
        "-",
        formatC(hr_ucl, digits = 1, format = "f"),
        ")"
      )
    ) %>%
    select(
      var_name_label, var_name, case, death_pct, hr, hr_lcl, hr_ucl, hr_ci
    ) %>%
    pivot_wider(
      names_from = case,
      values_from = c(death_pct, hr, hr_lcl, hr_ucl, hr_ci)
    ) %>%
    select(-c(hr_0, hr_lcl_0, hr_ucl_0, hr_ci_0)) %>%
    rename(
      hr = hr_1,
      hr_lcl = hr_lcl_1,
      hr_ucl = hr_ucl_1,
      hr_ci = hr_ci_1
    ) %>%
    mutate(sort_var = case_when(
      word(var_name, 2, sep = fixed("_")) == "00" ~ hr + 100,
      TRUE ~ hr
    )) %>%
    arrange(-sort_var) %>%
    select(-c(var_name, sort_var)) %>%
    mutate(var_name_label = factor(var_name_label, levels = .$var_name_label))
  
  
  ### Make HR plot ###
  
  # Make data for striped lines in plot
  stripe_col_val <- rep(c("lightblue", "white"), nrow(df01)/2)
  if (nrow(df01) %% 2 == 1) {
    stripe_col_val <- c(stripe_col_val, "lightblue")
  }
    
  stripes_df <- tibble(
    ymin_val = 1:nrow(df01) - 0.5,
    ymax_val = 1:nrow(df01) + 0.5,
    xmin_val = min(df01$hr_lcl)- 0.05,
    xmax_val = max(df01$hr_ucl) + 0.05,
    stripe_col = stripe_col_val
  )
  
  hr_plot <- df01 %>% 
    ggplot(aes(x = hr, y = rev(var_name_label), xmin = hr_lcl, xmax = hr_ucl)) +
    geom_rect(
      data = stripes_df,
      mapping = aes(
        xmin = xmin_val,
        xmax = xmax_val,
        ymin = ymin_val,
        ymax = ymax_val,
        fill = stripe_col,
        x = NULL,
        y = NULL
      )
    ) +
    geom_linerange() +
    geom_point() +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      panel.border = element_rect(colour = "grey")
    ) +
    labs(
      x = "HR",
      y = NULL
    ) +
    scale_fill_identity() +
    scale_y_discrete(expand = expansion(0, 0)) +
    scale_x_log10(
      expand = expansion(0, 0),
      breaks = c(1, 2, 5, 10),
      limits = c(min(df01$hr_lcl)- 0.05, max(df01$hr_ucl + 0.05))
    )
    
  tbl_left <- df01 %>%
    select(var_name_label, death_pct_0, death_pct_1) %>%
    gt() %>%
    cols_align(align = "left", columns = var_name_label) %>%
    cols_label(
      var_name_label = "Brain disorder",
      death_pct_0 = md("Patient group<br>Deaths, N (%)"),
      death_pct_1 = md("Comparison group<br>Deaths, N (%)")
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      column_labels.font.weight = "bold",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "lightblue",
      table_body.hlines.color = "lightblue"
    )
  
  tbl_right <- df01 %>%
    select(hr_ci) %>%
    gt() %>%
    cols_label(hr_ci = md("Adjusted<br>hazard ratio (95% CI)")) %>%
    tab_options(
      table.border.top.style = "hidden",
      column_labels.font.weight = "bold",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "lightblue",
      table_body.hlines.color = "lightblue"
    )
  
  wrap_table(tbl_left, space = "fixed") + hr_plot + wrap_table(tbl_right, space = "fixed")
     
}

# Make mortality forestplot. Assumes input data.frame `x`
# is a suitable subset of the risk_cox data.frame.
mortality_changes_over_time_forestplot <- function(x) {
  
  ### Process input data ###

  df01 <-x %>%
    mutate(
      death_n_pct = paste0(
        formatC(deaths_n, digits = 0, format = "f", big.mark = ","),
        " (",
        formatC(deaths_prop * 100, digits = 1, format = "f", big.mark = ","),
        ")"
      ),
      hr_ci = paste0(
        formatC(hr, digits = 1, format = "f"),
        " (",
        formatC(hr_lcl, digits = 1, format = "f"),
        "-",
        formatC(hr_ucl, digits = 1, format = "f"),
        ")"
      ),
      hr_ci = ifelse(is.na(hr_lcl), "1.0 (ref)", hr_ci),
      population_label = factor(
        population,
        levels = c("inc_2011_2015", "inc_2016_2021"),
        labels = c("2011-2015", "2016-2021")
      ),
      case_label = factor(
        case,
        levels = c(0, 1),
        labels = c("Controls", "Patients")
      ),
      sort_var = case_when(
        word(var_name, 2, sep = fixed("_")) == "00" ~ hr + 100,
        TRUE ~ hr
      )
    ) %>%
    select(
      population_label, population, var_name_label, case, case_label, hr,
      hr_lcl, hr_ucl, hr_ci, death_n_pct, sort_var
    ) 

  var_name_label_order <- df01 %>%
    filter(case == 1 & population == "inc_2016_2021") %>%
    arrange(sort_var) %>%
    pull(var_name_label)
  
  df01 <- df01 %>%
    mutate(var_name_label = factor(var_name_label, levels = var_name_label_order))
  
  
  ### Make plot with effect estimates ###
  
  plot_dat <- df01 %>%
    filter(case == 1)
  
  n_var_name <- plot_dat %>%
    distinct(var_name_label) %>%
    nrow()
  
  stripe_col_val <- rep(c("lightblue", "white"), n_var_name / 2)
  if (n_var_name %% 2 == 1) {
    stripe_col_val <- c(stripe_col_val, "lightblue")
  }
  
  stripe_dat <- tibble(
    ymin_val = 1:n_var_name - 0.5,
    ymax_val = 1:n_var_name + 0.5,
    xmin_val = min(plot_dat$hr_lcl),
    xmax_val = max(plot_dat$hr_ucl),
    stripe_col =  stripe_col_val
  )

  hr_plot <- plot_dat %>%
    ggplot(aes(
      x = hr,
      y = var_name_label,
      xmin = hr_lcl,
      xmax = hr_ucl,
      group = population_label,
      colour = population_label
    )) +
    geom_rect(
      data = stripe_dat,
      mapping = aes(
        xmin = xmin_val,
        xmax = xmax_val,
        ymin = ymin_val,
        ymax = ymax_val,
        fill = stripe_col,
        colour = NULL,
        x = NULL,
        y = NULL,
        group = NULL
      )
    ) +
    geom_linerange(position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      panel.border = element_rect(colour = "grey")
    ) +
    labs(
      x = "HR",
      y = NULL
    ) +
    scale_fill_identity() +
    scale_colour_manual(guide = "legend", values = c("black", "orange")) +
    scale_x_log10(expand = expansion(0, 0), breaks = c(1, 2, 5, 10)) +
    scale_y_discrete(expand = expansion(0, 0)) +
    guides(colour = guide_legend(title = "Incidence cohort", override.aes = list(fill = NA)))
  
  
  ### Table outcomes and effect estimates ###

  tbl_dat <- df01 %>%
    select(var_name_label, population, case, hr_ci, death_n_pct, sort_var) %>%
    pivot_wider(
      id_cols = c(var_name_label, population),
      names_from = case,
      values_from = c(death_n_pct, hr_ci, sort_var)
    ) %>%
    select(-c(hr_ci_0, sort_var_0)) %>%
    pivot_wider(
      id_cols = var_name_label,
      names_from = population,
      values_from = c(death_n_pct_0, death_n_pct_1, hr_ci_1, sort_var_1)
    ) %>%
    arrange(-sort_var_1_inc_2016_2021) %>%
    select(-c(sort_var_1_inc_2011_2015, sort_var_1_inc_2016_2021))
    
  tbl_dat_left <- tbl_dat %>%
    select(
      var_name_label,
      death_n_pct_0_inc_2011_2015, death_n_pct_1_inc_2011_2015,
      death_n_pct_0_inc_2016_2021, death_n_pct_1_inc_2016_2021) %>%
    gt() %>%
    cols_label(
      var_name_label = "Brain disorder",
      death_n_pct_0_inc_2011_2015 = md("Comparison group<br>Deaths, N (%)"),
      death_n_pct_1_inc_2011_2015 = md("Patient group<br>Deaths, N (%)"),
      death_n_pct_0_inc_2016_2021 = md("Comparison group<br>Deaths, N (%)"),
      death_n_pct_1_inc_2016_2021 = md("Patient group<br>Deaths, N (%)"),
    ) %>%
    tab_spanner(
      label = "Incident cohort 2011-2015",
      columns = c(death_n_pct_0_inc_2011_2015, death_n_pct_1_inc_2011_2015)
    ) %>%
    tab_spanner(
      label = "Incident cohort 2016-2021",
      columns = c(death_n_pct_0_inc_2016_2021, death_n_pct_1_inc_2016_2021 )
    ) %>%
    cols_align(align = "left", columns = var_name_label) %>%
    tab_options(
      table.border.top.style = "hidden",
      column_labels.font.weight = "bold",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "lightblue",
      table_body.hlines.color = "lightblue"
    )

  tbl_dat_right <- tbl_dat %>%
    select(starts_with("hr_")) %>%
    gt() %>%
    cols_label(
      hr_ci_1_inc_2011_2015 = md("Incident cohort 2011-2015<br>HR (95% CI)"),
      hr_ci_1_inc_2016_2021 = md("Incident cohort 2016-2021<br>HR (95% CI)")
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      column_labels.font.weight = "bold",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "lightblue",
      table_body.hlines.color = "lightblue"
    )

  ### Combine elements of forestplot ###
  
  wrap_table(tbl_dat_left, space = "fixed") +
    hr_plot + 
    wrap_table(tbl_dat_right, space = "fixed") 
}
