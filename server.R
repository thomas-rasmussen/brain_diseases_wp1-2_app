library(shiny)
library(bslib)
library(here)
library(tidyverse)
library(DT)
library(patchwork)
library(gt)
library(shinycssloaders)

# Load helper functions
source("helpers.R")

# Load data
all_costs <- readRDS(here("data", "all_costs.rds"))
assess_ph_dat <- readRDS(here("data", "assess_ph_dat.rds"))
codelist <- readRDS(here("data", "codelist.rds"))
concurrent_diseases <- readRDS(here("data", "concurrent_diseases.rds"))
cost_reg_est <- readRDS(here("data", "cost_reg_est.rds"))
pat_char_tbl_dat <- readRDS(here("data", "pat_char_tbl_dat.rds"))
prev_inc <- readRDS(here("data", "prev_inc.rds"))
risk_cox <- readRDS(here("data", "risk_cox.rds"))

# Find DKK to EUR exchange rate used in codelist
eur_dkk_rate <- codelist %>%
  filter(str_starts(var_name, "eur_to_dkk")) %>%
  select(code_include) %>%
  pull() %>%
  as.numeric()

function(input, output, session) {
    
    #### output$brain_disease_def_details ####
    output$brain_disease_def_details <- renderUI({
      if (input$brain_disease_def_var_def_id == "def01") {
        div(HTML("
          <p>
          <h2>Sensitivity analysis 1</h2>
          Brain diseases are defined using ICD-10 diagnosis codes.
          </p>
        
          <p>
          Note that all ICD-10 subcodes are included/excluded.
          </p>
        "))
      } else if (input$brain_disease_def_var_def_id == "def02") {
        div(HTML("
          <p>
          <h2>Sensitivity analysis 2</h2>
          Brain diseases are defined using ICD-10 diagnosis codes and ATC prescription codes.
          </p>
        
          <p>
          Note that all ICD-10 and ATC subcodes are included/excluded, and that if indication
          codes are given, then a prescription must have both an included ATC code and indication code
          to qualify.
          </p>
        "))
      } else if (input$brain_disease_def_var_def_id == "def03") {
        div(HTML("
          <p>
          <h2>Main analysis</h2>
          Brain diseases are defined using ICD-10 diagnosis codes and ATC prescription codes.
          </p>
        
          <p>
          Note that all ICD-10 and ATC subcodes are included/excluded, and that if indication
          codes are given, then a prescription must have both an included ATC code and indication code
          to qualify.
          </p>
        "))
      } else if (input$brain_disease_def_var_def_id == "def04") {
        div(HTML("
          <p>
          <h2>Sensitiviy analysis 3</h2>
          Brain diseases are defined using ICD-10 diagnosis codes, ATC prescription codes, and SSSY codes.
          </p>
        
          <p>
          Note that all ICD-10, ATC and SSSY subcodes are included/excluded, and that if indication
          codes are given, then a prescription must have both an included ATC code and indication code
          to qualify.
          </p>
        "))
      } else {
        "No information on selected definition"
      }
    })
    
    #### output$brain_disease_def_table ####
    output$brain_disease_def_table <- renderTable(striped = TRUE, {
      
      codelist %>%
        filter(
          group == paste0("var_", input$brain_disease_def_var_def_id)
          & !(var_name %in% c("def01_00", "def02_00", "def03_00"))
        ) %>%
        select(var_label, code_type, code_include, code_exclude) %>%
        mutate(
          code_type = case_match(
            code_type,
            "icd10" ~ "ICD-10",
            "atc" ~ "ATC",
            "indo" ~ "Indication",
            "sssy" ~ "SSSY"
          )
        ) %>%
        rename(
          "Brain disorder" = var_label,
          "Code type" = code_type,
          "Codes included" = code_include,
          "Codes excluded" = code_exclude
        )
    })
    
    #### output$concurrent_diseases_heatmap ####
    output$concurrent_diseases_heatmap <- renderPlot({
      concurrent_diseases %>%
        filter(
          var_def == input$concurrent_diseases_var_def_id
          & population == input$concurrent_diseases_population_id
        ) %>%
        ggplot(aes(x = var_name_label, y = reorder(concurrent_disease_label, desc(conc_dis_order)))) +
        geom_tile(aes(fill = conc_dis_percent)) +
        geom_text(aes(label = conc_dis_pct_fmt), nudge_y = 0, size = 3, na.rm = TRUE) +
        scale_x_discrete(position = "top") +
        coord_equal() +
        scale_fill_viridis_c(begin = 0.6, end = 1, option = "inferno", na.value = "white",
            direction = -1) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1),
          axis.text = element_text(size = 14, colour = "black"),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 18)
        ) +
        labs(
          title = var_def_label(input$concurrent_diseases_var_def_id),
          subtitle = population_label(input$concurrent_diseases_population_id),
          fill = "Percent",
          x = "",
          y = ""
        )
    })
    
    #### output$concurrent_diseases_table ####
    output$concurrent_diseases_table <- renderDT(rownames = FALSE, {
      concurrent_diseases %>%
        filter(
          var_def == input$concurrent_diseases_var_def_id
          & population == input$concurrent_diseases_population_id
        ) %>%
        arrange(var_name, conc_dis_order) %>%
        select(
          var_def_label, population_label, var_name_label, concurrent_disease_label, conc_dis_pct_fmt
        ) %>%
        rename(
          "Brain disease definition" = var_def_label,
          "Population" = population_label,
          "Brain disease" = var_name_label,
          "Concurrent disease" = concurrent_disease_label,
          "Concurrency, %" = conc_dis_pct_fmt
        )
    })

    #### output$costs_act_plot ####
    output$costs_act_plot <- renderPlot({
      tmp <- all_costs %>%
        filter(
          var_def == input$costs_act_plot_var_def_id
          & population == input$costs_act_plot_population_id) %>%
        mutate(act_cost_eur_bil = act_cost / (eur_dkk_rate * 10**9))

      # If brain disease definition has more than one variable, assume
      # the "defxx_00" is a total group definition to be removed from
      # the plot
      if (length(unique(tmp$var_name)) > 1L) {
        tmp <- tmp %>%
          filter(var_name != paste0(var_def, "_00"))
      }
      
      tmp %>%
        update_var_label(cost_var = "act_cost_eur_bil") %>%
        rename(cost_var = act_cost_eur_bil) %>%
        filter(cost_component != "lost_production_sickness") %>%
        make_barplot(
          title = var_def_label(input$costs_act_plot_var_def_id),
          subtitle = population_label(input$costs_act_plot_population_id),
          x_axis_label = "bil_eur"
        )
    })
    
    #### output$costs_act_py_plot ####
    output$costs_act_py_plot <- renderPlot({
      pop_type <- word(input$costs_act_py_plot_population_id, 1, sep = "_")
      
      if (pop_type == "inc") return(ggplot() + theme_void() + labs(title = "Plot not available"))
      
      tmp <- all_costs %>%
        filter(
          var_def == input$costs_act_py_plot_var_def_id
          & population == input$costs_act_py_plot_population_id) %>%
        mutate(act_cost_py_eur = act_cost_py / eur_dkk_rate)

      # If brain disease definition has more than one variable, assume
      # the "defxx_00" is a total group definition to be removed from
      # the plot
      if (length(unique(tmp$var_name)) > 1L) {
        tmp <- tmp %>%
          filter(var_name != paste0(var_def, "_00"))
      }
      
      tmp %>%
        update_var_label(cost_var = "act_cost_py_eur") %>%
        rename(cost_var = act_cost_py_eur) %>%
        filter(cost_component != "lost_production_sickness") %>%
        make_barplot(
          title = var_def_label(input$costs_act_plot_var_def_id),
          subtitle = population_label(input$costs_act_plot_population_id),
          x_axis_label = "eur"
        )
    })
      
    #### output$costs_att_plot ####
    output$costs_att_plot <- renderPlot({
      pop_type <- word(input$costs_att_plot_population_id, 1, sep = "_")
      
      if (pop_type == "inc") return(ggplot() + theme_void() + labs(title = "Plot not available"))
      
      tmp <- all_costs %>%
        filter(
          var_def == input$costs_att_plot_var_def_id
          & population == input$costs_att_plot_population_id
        ) %>%
        mutate(att_cost_eur_bil = att_cost / (eur_dkk_rate * 10**9))

      # If brain disease definition has more than one variable, assume
      # the "defxx_00" is a total group definition to be removed from
      # the plot
      if (length(unique(tmp$var_name)) > 1L) {
        tmp <- tmp %>%
          filter(var_name != paste0(var_def, "_00"))
      }
      
      tmp_direct_plot <- tmp %>%
        update_var_label(cost_var = "att_cost_eur_bil") %>%
        rename(cost_var = att_cost_eur_bil) %>%
        filter(cost_component != "lost_production_sickness") %>%
        make_barplot(
          title = var_def_label(input$costs_act_plot_var_def_id),
          subtitle = population_label(input$costs_act_plot_population_id),
          x_axis_label = "bil_eur",
          flip_plot = TRUE,
          include_yaxis_text = FALSE
        )

      tmp_indirect_plot <- tmp %>%
        update_var_label(cost_var = "att_cost_eur_bil") %>%
        rename(cost_var = att_cost_eur_bil) %>%
        filter(cost_component == "lost_production_sickness") %>%
        make_barplot(x_axis_label = "bil_eur")
      
      tmp_direct_plot + tmp_indirect_plot
    })
    
    #### output$costs_att_py_plot ####
    output$costs_att_py_plot <- renderPlot({
      tmp <- all_costs %>%
        filter(
          var_def == input$costs_att_py_plot_var_def_id
          & population == input$costs_att_py_plot_population_id
        ) %>%
        mutate(att_cost_py_eur = att_cost / eur_dkk_rate)

      # If brain disease definition has more than one variable, assume
      # the "defxx_00" is a total group definition to be removed from
      # the plot
      if (length(unique(tmp$var_name)) > 1L) {
        tmp <- tmp %>%
          filter(var_name != paste0(var_def, "_00"))
      }
      
      tmp_direct_plot <- tmp %>%
        update_var_label(cost_var = "att_cost_py_eur") %>%
        rename(cost_var = att_cost_py_eur) %>%
        filter(cost_component != "lost_production_sickness") %>%
        make_barplot(
          title = var_def_label(input$costs_act_plot_var_def_id),
          subtitle = population_label(input$costs_act_plot_population_id),
          x_axis_label = "eur",
          flip_plot = TRUE,
          include_yaxis_text = FALSE
        )
      
      tmp_indirect_plot <- tmp %>%
        update_var_label(cost_var = "att_cost_py_eur") %>%
        rename(cost_var = att_cost_py_eur) %>%
        filter(cost_component == "lost_production_sickness") %>%
        make_barplot(x_axis_label = "eur")
          
      tmp_direct_plot + tmp_indirect_plot
      
    })
  
    
    #### output$costs_over_time_plot ####
    output$costs_over_time_plot <- renderPlot({
      
      # Restrict and restructure data
      tmp <- all_costs %>%
        filter(
          var_def == input$costs_over_time_plot_var_def_id
          & word(population, 1, sep = "_") == "prev"
          & var_name %in% c("def01_00", "def02_00", "def03_00", "def04_00")) %>%
        select(
          population, population_label, cost_component, cost_component_label, att_cost, att_cost_py
        ) %>%
        pivot_longer(
          cols = all_of(c("att_cost", "att_cost_py")),
          names_to = "cost_type",
          values_to = "cost"
        )
      
      # Summarize direct cost components
      tmp <- tmp %>%
        mutate(
          cost_component = case_match(
            cost_component,
            c("secondary_sector", "home_care", "filled_prescriptions",
              "nursing_home", "primary_sector") ~ "all_direct_costs",
            .default = cost_component
          ),
          cost_component_label = case_when(
            cost_component == "all_direct_costs" ~ "All direct costs",
            .default = cost_component_label
          )
        ) %>%
        group_by(population, population_label, cost_component, cost_component_label, cost_type) %>%
        summarize(cost = sum(cost), .groups = "keep") %>%
        ungroup()
        
        # Change costs tot (billions of) EUR
        tmp <- tmp %>%
          mutate(
           eur_dkk_rate = as.numeric(eur_dkk_rate),
            year = as.numeric(word(population, 2., sep = "_")),
           cost_type = case_match(
              cost_type,
              "act_cost" ~ "act_cost_bil_eur",
              "att_cost" ~ "att_cost_bil_eur",
              "act_cost_py" ~ "act_cost_py_eur",
              "att_cost_py" ~ "att_cost_py_eur"
            ),
            cost = case_when(
              cost_type == "act_cost_bil_eur" ~ cost / (eur_dkk_rate * 10**9),
              cost_type == "att_cost_bil_eur" ~ cost / (eur_dkk_rate * 10**9),
              cost_type == "act_cost_py_eur" ~ cost / eur_dkk_rate,
              cost_type == "att_cost_py_eur" ~ cost / eur_dkk_rate,
              .default = cost
            ),
            cost_type_label = case_match(
              cost_type,
              "act_cost_bil_eur" ~ "Total actual costs (billion EUR)",
              "att_cost_bil_eur" ~ "Total attributable costs (billion EUR)",
              "act_cost_py_eur" ~ "Actual costs per person (EUR)",
              "att_cost_py_eur" ~ "Attributable costs per person (EUR)"
            )
          )
      
        tmp %>%
          ggplot(aes(x = year, y = cost)) +
          geom_line() +
          facet_grid(
            rows = vars(cost_type_label),
            cols = vars(cost_component_label),
            scales = "free_y"
          ) +
          theme_bw() +
          theme(plot.title = element_text(size = 20)) +
          labs(
            title = var_def_label(input$costs_over_time_plot_var_def_id),
            x = "Prevalence year",
            y = NULL
          )      
    })
    
    #### output$costs_table ####
      output$costs_table <- renderDT({
        all_costs %>%
          filter(
            var_def == input$costs_table_var_def_id
            & population == input$costs_table_population_id
          ) %>%
          arrange(var_def_label, population_label, cost_component_label, var_name_label) %>%
          select(
            var_def_label, population_label, cost_component_label, var_name_label,
            act_cost, act_py, att_cost, att_py, act_cost_py, att_cost_py
          ) %>%
          rename(
            "Brain disease definition" = var_def_label,
            "Population" = population_label,
            "Cost component" = cost_component_label,
            "Brain disease" = var_name_label,
            "Actual costs - Total costs (DKK)" = act_cost,
            "Actual costs - Person-years" = act_py,
            "Attributable costs - Total costs (DKK)" = att_cost,
            "Attributable costs - Person-years" = att_py,
            "Actual costs per person (DKK)" = act_cost_py,
            "Attributable costs per person (DKK)" = att_cost_py
          )
      })

    #### output$cost_reg_title ####
    output$cost_reg_title <- renderUI({
      div(HTML(paste0(
        "<h3>", var_def_label(input$cost_regression_var_def_id), "</h3>",
        "<h4>", population_label(input$cost_regression_population_id), "</h4>",
        "Parameter estimates from model without (left) and with (right) CCI variables included"
      )))
    })
    
    #### output$cost_reg_table ####
    output$cost_reg_table <- renderDT(rownames = FALSE, {
      cost_reg_est %>%
        filter(
          var_def == input$cost_regression_var_def_id
          & population == input$cost_regression_population_id) %>%
        arrange(var_def_label, population_label, model_label) %>%
        select(var_def_label, population_label, model_label, var_name_label, est_ci) %>%
        rename(
          "Brain disease definition" = var_def_label,
          "Population" = population_label,
          "Model" = model_label,
          "Variable name" = var_name_label,
          "Estimate (95% CI)" = est_ci
        )
    })
    
    #### output$cost_reg_no_cci ####
    output$cost_reg_no_cci <- renderPlot({
      tmp <- cost_reg_est %>%
        filter(
          var_def == input$cost_regression_var_def_id
          & population == input$cost_regression_population_id
          & model == "no_cci"
          & substr(var_name, 1, 3) == "def"
        )
      
      cost_regression_forestplot(tmp)
        
    })
    
    #### output$cost_reg_cci ####
    output$cost_reg_cci <- renderPlot({
      tmp <- cost_reg_est %>%
        filter(
          var_def == input$cost_regression_var_def_id
          & population == input$cost_regression_population_id
          & model == "cci"
          & substr(var_name, 1, 3) == "def"
        )
          
      cost_regression_forestplot(tmp)
        
    })
    
    #### output$mortality_title ####
    output$mortality_title <- renderUI({
      div(HTML(paste0(
        "<h3>", var_def_label(input$mortality_forest_var_def_id), "</h3>",
        "<h4>", population_label(input$mortality_forest_population_id), "</h4>"
      )))
    })
    
    #### output$mortality_forest_plot ####
    output$mortality_forest_plot <- renderPlot({
      tmp <- risk_cox %>%
        filter(
          var_def == input$mortality_forest_var_def_id
          & population == input$mortality_forest_population_id
        ) 
      
      mortality_forestplot(tmp)
    
    })
    
    #### output$mortality_changes_over_time ####
    output$mortality_changes_over_time <- renderPlot({
      tmp <- risk_cox %>%
        filter(var_def == input$mortality_over_time_var_def_id) 
      
      mortality_changes_over_time_forestplot(tmp)
    })
    
    #### output$mortality_table ####
    output$mortality_table <- renderTable({
      risk_cox %>%
        filter(
          var_def == input$mortality_table_var_def_id
          & population == input$mortality_table_population_id
        ) %>%
        arrange(var_def_label, population_label, var_name_label, desc(case)) %>%
        select(
          var_def_label, population_label, var_name_label, case, n_pt, deaths_n,
          deaths_prop, hr, hr_lcl, hr_ucl
        ) %>%
        rename(
          "Brain disease definition" = var_def_label,
          "Population" = population_label,
          "Brain disease" = var_name_label,
          "Cases (1) / controls (0)" = case,
          "Number of persons" = n_pt,
          "Number of deaths" = deaths_n,
          "Proportion of deaths" = deaths_prop,
          "HR" = hr,
          "Lower 95% confidence interval limit" = hr_lcl,
          "Upper 95% confidence interval limit" = hr_ucl
        )
    })
    
    #### output$mortality_assess_ph_details ####
    output$mortality_assess_ph_details <- renderUI({
      div(HTML(paste0(
        "<h3>", var_def_label(input$mortality_assess_ph_var_def_id), "</h3>",
        "<h4>", population_label(input$mortality_assess_ph_population_id), "</h4>",
        "<p>",
        "Visual assessment of the proportional hazard assumption in Cox regression models.<br>",
        "Survival vs. time curves for exposure groups (left) should be proportional.<br>",
        "Log(-log(Survival)) vs. log(time) curves (right) should be parallel.",
        "</p"
      )))
    })
    
    #### output$mortality_assess_ph_plot ####
    output$mortality_assess_ph_plot <- renderPlot({
      tmp <- assess_ph_dat %>%
        filter(
          var_def == input$mortality_assess_ph_var_def_id
          & population == input$mortality_assess_ph_population_id) %>%
        select(var_def_label, population_label, var_name_label, case, fu_time, surv) %>%
        mutate(
          log_fu_time = log((fu_time)),
          lml_surv = log(-log(surv)),
          case = factor(
            case,
            levels = c("0", "1"),
            labels = c("Controls", "Cases")
          )
        )
        
      plot_surv <- tmp %>% 
        ggplot(aes(x = fu_time, y = surv, colour = case)) +
        geom_line() +
        facet_wrap(vars(var_name_label), ncol = 4) +
        labs(
          title = "Survival vs. time",
          x = "Time in days",
          y = "Survival proportion",
          colour = "Group"
        )
      
      plot_lml_surv <- tmp %>%
        ggplot(aes(x = log_fu_time, y = lml_surv, colour = case)) + 
        geom_line() +
        facet_wrap(vars(var_name_label), ncol = 4) +
        labs(
          title = "Log(-log(Survival)) vs. log(Time in days)",
          x = "Log(Time in days)",
          y = "Log(-log(Survival proportion))",
          colour = "Group"
        )
      
      plot_surv + plot_lml_surv  &
        # patchwork version v1.3.0 has unfortunately introduced a bug
        # making guides = "collect" return an error.
        # plot_layout(guides = "collect") &
        theme_bw() &
        theme(
          plot.title = element_text(size = 14),
          legend.position = "bottom"
        )
      
    })

    #### output$patient_characteristics_table ####
    output$patient_characteristics_table <- renderTable({
      pat_char_tbl_dat %>%
        filter(
          var_def == input$patient_characteristics_var_def_id
          & population == input$patient_characteristics_population_id
        ) %>%
        select(var_def_label, population_label, var_name_label, label, stat_char) %>%
        rename(
          "Brain disease definition" = var_def_label,
          "Popuation" = population_label,
          "Brain disease" = var_name_label,
          "Patient characteristic" = label,
          "Value" = stat_char
        )
    })
    
    #### output$patient_characteristics_plot ####
    output$patient_characteristics_plot <- renderImage({
      list(
        src = here(
          "data",
          paste0(
            "pat_char_graph_var_",
            input$patient_characteristics_var_def_id, "_",
            input$patient_characteristics_population_id, "_",
            version_suffix,
            ".svg"
          )
        )
      )
    }, deleteFile = FALSE)
    
    #### output$prev_inc_change_over_time ####
    output$prev_inc_change_over_time <- renderPlot({
      
      if (input$prev_inc_plot_pop_type == "inc") {
        tmp <- prev_inc %>%
          filter(
            var_def == input$prev_inc_plot_var_def_id
            & population %in% c("inc_2011_2015", "inc_2016_2021")
            & !var_name %in% c("def01_00", "def02_00", "def03_00", "def04_00")
          ) %>%
          mutate(
            prev_inc = n_diag / risk_time * 10**5,
            population = factor(
              population,
              levels = c("inc_2011_2015", "inc_2016_2021"),
              labels = c("2011-2015", "2016-2021")
            )
          )
      } else if (input$prev_inc_plot_pop_type == "prev") {
        tmp <- prev_inc %>%
          filter(
            var_def == input$prev_inc_plot_var_def_id
            & population %in% c("prev_2015", "prev_2021")
            & !var_name %in% c("def01_00", "def02_00", "def03_00", "def04_00")
          ) %>%
          mutate(
            prev_inc = n_diag/ risk_time *100,
            population = factor(
              population,
              levels = c("prev_2015", "prev_2021"),
              labels = c("2015", "2021")
            )
          )
      }
      
      # Order by highest incidence/prevalence
      if (input$prev_inc_plot_pop_type == "inc") {
        tmp_order <- tmp %>%
          filter(population %in% c("2016-2021")) %>%
          arrange(prev_inc)
      } else if (input$prev_inc_plot_pop_type == "prev") {
        tmp_order <- tmp %>%
          filter(population %in% c("2021")) %>%
          arrange(prev_inc)
      }
      
      tmp <- tmp %>%
        mutate(var_name_label = factor(var_name_label, tmp_order$var_name_label))
      
      if (input$prev_inc_plot_pop_type == "inc") {
        lab_x <- "Incidence rate per 100,000 person-years"
        lab_fill_colour = "Incidence population"
      } else if (input$prev_inc_plot_pop_type == "prev") {
        lab_x <- "Prevalence, %"
        lab_fill_colour = "Prevalence population"
      }
      
      if (input$prev_inc_plot_pop_type == "inc") {
        subtitle_label <- "Incidence"
      } else if (input$prev_inc_plot_pop_type == "prev") {
        subtitle_label <- "Prevalence"
      }
      
      tmp %>%
        ggplot(aes(x = prev_inc, y = var_name_label, colour = population, fill = population)) +
        geom_col(position = "dodge") +
        scale_x_continuous(expand = expansion(add = c(0, 1))) +
        theme_bw() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size =10),
          axis.line = element_line(colour = "black"),
          legend.position = "right"
        ) +
        labs(
          title = var_def_label(input$prev_inc_plot_var_def_id),
          subtitle = subtitle_label,
          x = lab_x,
          y = NULL,
          fill = lab_fill_colour,
          colour = lab_fill_colour
        )
    })
    
    #### output$prev_inc_table ####
    output$prev_inc_table <- renderTable({
      tmp <- prev_inc %>%
        filter(
          var_def == input$prev_inc_table_var_def_id
          & population == input$prev_inc_table_population_id
        ) %>%
        mutate(
          prev_inc = n_diag / risk_time,
          risk_time = formatC(risk_time, digits = 0, format = "f", big.mark = ","),
          n_diag = formatC(n_diag, digits = 0, format = "f", big.mark = ",")
        ) %>%
        select(
          var_def_label, population_label, var_name_label, n_diag,
          risk_time, prev_inc
        ) %>%
        rename(
          "Brain disease defintion" = var_def_label,
          "Population" = population_label,
          "Brain disease" = var_name_label,
          "Diagnoses, N" = n_diag
        )
      
      if (word(input$prev_inc_table_population_id, sep = fixed("_")) == "inc") {
        tmp <- tmp %>%
          mutate(
            prev_inc = prev_inc * 10**5) %>%
          rename(
            "Person-years" = risk_time,
            "Incidence per 100,000 person-years" = prev_inc
          )
          
      } else if (word(input$prev_inc_table_population_id, sep = fixed("_")) == "prev") {
        tmp <- tmp %>%
          mutate(prev_inc = prev_inc *100) %>%
          rename(
            "Persons, N" = risk_time,
            "Prevalence (%)" = prev_inc
          )
      }
      
      tmp
    })
    
    
    #### output$download_details ####
    output$download_details <- renderUI({
      ### concurrent_diseases ###
      if (input$download_dataset_select == "concurrent_diseases") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          Concurrent diseases among persons with brain disorders.
          </p>
          
          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>var_name_label:</b> Brain disease<br>
          -<b>concurrent_disease_label:</b> Concurrent disease<br>
          -<b>conc_dis_percent:</b>
            Percentage of persons with the brain disease that also has the concurrent disease<br>
          </p>
        "))
      ### all_costs ###
      } else if (input$download_dataset_select == "all_costs") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          Aggregated costs from actual and attributable cost analyses.
          </p>
          
          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>var_name_label:</b> Brain disease<br>
          -<b>cost_component_label:</b> Cost component<br>
          -<b>act_cost:</b> Total actual cost in DKK<br>
          -<b>act_py:</b> Number of person-years contributed by persons contributing to <b>act_cost</b><br>
          -<b>att_cost:</b> Total attributable cost in DKK<br>
          -<b>att_py:</b> Number of preson-years contributed by perosns contributing to <b>att_cost</b><br>
          -<b>act_cost_py:</b> Actual cost per person. Equal to <b>act_cost/act_py</b><br>
          -<b>att_cost_py:</b> Attributate cost per person. Equal to <b>att_cost/att_py</b>
          </p>
        "))
      ### cost_reg_est ###
      } else if(input$download_dataset_select == "cost_reg_est") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          Estimates from cost regression analyses.
          </p>
          
          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>model_label:</b> Model<br>
          -<b>var_name_label:</b> Variable included in mmodel<br>
          -<b>estimate:</b> Parameter estimate<br>
          -<b>lcl:</b> Lower 95% confidence interval limit<br>
          -<b>ucl:</b> Upper 95% confidence interval limit<br>
        "))
      } else if (input$download_dataset_select == "risk_cox") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          HR estimates from mortality Cox regression analyses together with
          descriptive statistics on the cases and controls.
          </p>

          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>var_name_label:</b> Brain disease<br>
          -<b>case:</b>
            Exposure strata - Cases = 1 / Controls = 0<br>
          -<b>n_pt:</b>
            Number of persons in exposure strata. Note that this descriptive
            stastistic only includes persons who died or where followed for
            the entire year of followup, eg censored persons are not included. <br>
          -<b>deaths_n:</b>
            Number of deaths in exposure strata<br>
          -<b>hr:</b>
            HR of death in cases versus controls<br>
          -<b>hr_lcl:</b>
            Lower 95% confidence interval limit<br>
          -<b>hr_ucl:</b>
            Upper 95% confidence interval limit<br>
          </p>
        "))
      } else if(input$download_dataset_select == "pat_char_tbl_dat") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          Patient characteristics on brain disorder populations.
          Note that the patient characteristics plots shown in the app can't be reproduced
          using the available aggregated data.
          </p>

          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>var_name_label:</b> Brain disease<br>
          -<b>var:</b> Patient characteristic variable<br>
          -<b>label:</b> Patient characteristic variable level<br>
          -<b>stat_char:</b>
            Patient characteristic value. 'N (%)' for dichotomous/categorical
            variables, and 'median (Q1;Q3) for continuous variables.<br>
          </p>
        "))
      } else if (input$download_dataset_select == "prev_inc") {
        div(HTML("
          <p>
          <h2>Dataset info</h2>
          Prevalence and incidence of brain disorders.
          </p>

          <p>
          <h2>Variable info</h2>
          -<b>var_def_label:</b> Set of defintions used for brain diseases<br>
          -<b>population_label:</b> Population<br>
          -<b>var_name_label:</b> Brain disease<br>
          -<b>n_diag:</b>
            Number of diagnoses in incidence/prevalence population<br>
          -<b>risk_time:</b>
            Number of person-years.
        "))
      } else {
        "No information on dataset is available"
      }
    })
    
    #### output$download_table ####
      # Reactive value for selected dataset ----
      datasetInput <- reactive({
        switch(
          input$download_dataset_select,
          "all_costs" = all_costs %>%
            select(
              var_def_label, population_label, var_name_label, cost_component_label,
              act_cost, act_py, att_cost, att_py, act_cost_py, att_cost_py
            ),
          "concurrent_diseases"  = concurrent_diseases %>%
            select(
              var_def_label, population_label, var_name_label,
              concurrent_disease_label, conc_dis_percent
            ),
          "cost_reg_est" = cost_reg_est %>%
            select(
              var_def_label, population_label, model_label, var_name_label,
              estimate, lcl, ucl
            ),
          "pat_char_tbl_dat" = pat_char_tbl_dat %>%
            select(
              var_def_label, population_label, var_name_label, var, label, stat_char
            ),
          "prev_inc" = prev_inc %>%
            select(
              var_def_label, population_label, var_name_label, n_diag, risk_time
            ),
          "risk_cox" = risk_cox %>%
            select(
              var_def_label, population_label, var_name_label, case, n_pt,
              deaths_n, hr, hr_lcl, hr_ucl
            )
        )
      })
      
      # Table of selected dataset ----
      output$download_table <- renderDT(rownames = FALSE, {
        datasetInput()
      })
      
      # Downloadable csv of selected dataset ----
      output$download_dataset_button <- downloadHandler(
        filename = function() {
          paste0(input$download_dataset_select, ".csv")
        },
        content = function(file) {
          write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
}
