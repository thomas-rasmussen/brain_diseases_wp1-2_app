# Version suffix used in filenames
version_suffix <- "v8_2024-09-12"

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

