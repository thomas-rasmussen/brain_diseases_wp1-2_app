library(shiny)
library(bslib)
library(DT)
library(shinycssloaders)

options(spinner.type = 6)

# Load helper functions
source("helpers.R")

page_navbar(
  title = "Brain diseases app (version v1.0.1)",
  theme = bs_theme(bootswatch = "lumen"),
  #### Brain disease definitions ####
  nav_panel(title = "Brain disease definitions",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "brain_disease_def_var_def_id",
          label = "Brain disease definition",
          choices = list(
            "Main analysis"          = "def03",
            "Sensitivity analysis 1" = "def01",
            "Sensitivity analysis 2" = "def02",
            "Sensitivity analysis 3" = "def04"
          )
        )
      ),
      htmlOutput("brain_disease_def_details"),
      withSpinner(tableOutput("brain_disease_def_table"))
    )
  ),
  #### Concurrent diseases ####
  nav_panel(title = "Concurrent diseases",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "concurrent_diseases_var_def_id",
          label = "Brain disease definition",
          choices = list(
            "Main analysis"          = "def03",
            "Sensitivity analysis 1" = "def01",
            "Sensitivity analysis 2" = "def02"
          )
        ),
        selectInput(
          inputId = "concurrent_diseases_population_id",
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
      ),
      navset_tab(
        nav_panel(title = "Heatmap - plot",
          withSpinner(plotOutput("concurrent_diseases_heatmap", height = "1000px", width = "800px"))
        ),
        nav_panel(title = "Heatmap - data",
          withSpinner(DTOutput("concurrent_diseases_table"))
        )
      )
    )
  ),
  #### Costs ####
  
  nav_panel(title = "Costs",
    navset_tab(
      nav_panel(title = "Actual costs - Total",
        layout_sidebar(
          sidebar = costs_sidebar_no_inc("costs_act_plot"),
          withSpinner(plotOutput("costs_act_plot", height = "600px", width = "1000px"))
        )
      ),
      nav_panel(title = "Actual costs - Per person",
        layout_sidebar(
          sidebar = costs_sidebar_no_inc("costs_act_py_plot"),
          withSpinner(plotOutput("costs_act_py_plot", height = "600px", width = "1000px"))
        )
      ),
      nav_panel(title = "Attributable costs - Total",
        layout_sidebar(
          sidebar = costs_sidebar_no_inc("costs_att_plot"),
          withSpinner(plotOutput("costs_att_plot", height = "600px", width = "1000px"))
        )
      ),
      nav_panel(title = "Attributable costs - Per person",
        layout_sidebar(
          sidebar = costs_sidebar("costs_att_py_plot"),
          withSpinner(plotOutput("costs_att_py_plot", height = "600px", width = "1000px"))
        )
      ),
      nav_panel(title = "Total costs over time",
        layout_sidebar(
          sidebar = costs_sidebar_no_pop("costs_over_time_plot"),
          withSpinner(plotOutput("costs_over_time_plot", height = "600px", width = "600px"))
        )
      ),
      nav_panel(title = "Plot data",
        layout_sidebar(
          sidebar = costs_sidebar("costs_table"),
          withSpinner(DTOutput("costs_table"))
        )
      )
    )
  ),
  #### Cost regression ####
  nav_panel(title = "Cost regression",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "cost_regression_var_def_id",
          label = "Brain disease definition",
          choices = list(
            "Main analysis"          = "def03",
            "Sensitivity analysis 1" = "def01",
            "Sensitivity analysis 2" = "def02",
            "Sensitivity analysis 3" = "def04"
          )
        ),
        selectInput(
          inputId = "cost_regression_population_id",
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
      ),
      navset_tab(
        nav_panel(title = "Forest plots of parameter estimates",
          htmlOutput("cost_reg_title"),
          layout_column_wrap(
            width = 1/2,
            withSpinner(plotOutput("cost_reg_no_cci", height = "800px", width = "800px")),
            withSpinner(plotOutput("cost_reg_cci", height = "800px", width = "800px"))
          )
        ),
        nav_panel(title = "Plot data",
          withSpinner(DTOutput("cost_reg_table"))
        )
      )
    )
  ),
  #### Mortality ####
  nav_panel(title = "Mortality",
    navset_tab(
      nav_panel(title = "One-year mortality",
          layout_sidebar(
          sidebar = mortality_sidebar("mortality_forest"),
          htmlOutput("mortality_title"),
          withSpinner(plotOutput("mortality_forest_plot", height = "800px", width = "1200px"))
        )
      ),
      nav_panel(title = "Changes in one-year mortality over time",
        layout_sidebar(
          sidebar = mortality_sidebar_no_pop("mortality_over_time"),
          withSpinner(plotOutput("mortality_changes_over_time", height = "800px", width = "1200px"))
        )
      ),
      nav_panel(title = "Plot data",
        layout_sidebar(
          sidebar = mortality_sidebar("mortality_table"),
          withSpinner(tableOutput("mortality_table"))
        )
      ),
      nav_panel(title = "Assessment of proportional hazard assumption",
        layout_sidebar(
          sidebar = mortality_sidebar("mortality_assess_ph"),
          htmlOutput("mortality_assess_ph_details"),
          withSpinner(withSpinner(plotOutput("mortality_assess_ph_plot", height = "800px", width = "1600px")))
        )
      )
    )
  ),
  #### Patient characteristics ####
  nav_panel(title = "Patient characteristics",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "patient_characteristics_var_def_id",
          label = "Brain disease definition",
          choices = list(
            "Main analysis"          = "def03",
            "Sensitivity analysis 1" = "def01",
            "Sensitivity analysis 2" = "def02",
            "Sensitivity analysis 3" = "def04"
          )
        ),
        selectInput(
          inputId = "patient_characteristics_population_id",
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
      ),
      navset_tab(
        nav_panel(title = "Plot",
          withSpinner(imageOutput("patient_characteristics_plot"))
        ),
        nav_panel(title = "Table",
          withSpinner(tableOutput("patient_characteristics_table"))
        )
      )
    )
  ),
  #### Prevalence/Incidence ####
  nav_panel(title = "Prevalence/Incidence",
      navset_tab(
        nav_panel(title = "Change in prevalence/incidence over time",
          layout_sidebar(
            sidebar = sidebar(
              selectInput(
                inputId = "prev_inc_plot_var_def_id",
                label = "Brain disease definition",
                choices = list(
                  "Main analysis"          = "def03",
                  "Sensitivity analysis 1" = "def01",
                  "Sensitivity analysis 2" = "def02",
                  "Sensitivity analysis 3" = "def04"
                )
              ),
              selectInput(
                inputId = "prev_inc_plot_pop_type",
                label = "Populations",
                choices = list(
                  "Incident cohorts" = "inc",
                  "Prevalent cohorts" = "prev"
                )
              )
            ),
            withSpinner(plotOutput("prev_inc_change_over_time", height = "700px", width = "800px"))
          )
        ),
        nav_panel(title = "Table",
          layout_sidebar(
            sidebar = sidebar(
              selectInput(
                inputId = "prev_inc_table_var_def_id",
                label = "Brain disease definition",
                choices = list(
                  "Main analysis"          = "def03",
                  "Sensitivity analysis 1" = "def01",
                  "Sensitivity analysis 2" = "def02",
                  "Sensitivity analysis 3" = "def04"
                )
              ),
              selectInput(
                inputId = "prev_inc_table_population_id",
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
            ),
            withSpinner(tableOutput("prev_inc_table"))
        )
      )
    )
  ),
  nav_spacer(),
  #### Download data ####
  nav_panel(title = "Download data",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "download_dataset_select",
          "Choose a dataset:",
          choices = list(
            "Concurrent diseases" = "concurrent_diseases",
            "Costs" = "all_costs",
            "Cost regression" = "cost_reg_est",
            "Mortality" = "risk_cox",
            "Patient characteristics" = "pat_char_tbl_dat",
            "Prevalence/incidence" = "prev_inc"
          )
        ),
        downloadButton("download_dataset_button", "Download")
      ),
      layout_columns(
        col_widths = c(4, 8),
        htmlOutput("download_details"),
        withSpinner(DTOutput("download_table"))
      )
    )
  ),
  #### Links ####
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a(shiny::icon("github"), "Source code", href = "https://github.com/thomas-rasmussen/brain_diseases_wp1-2_app", target = "_blank"))
  )
)
