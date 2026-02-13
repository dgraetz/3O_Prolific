
normWS <- function(d ,s, dv, between = NULL) {
  eval(substitute(d %>%
                    dplyr::group_by(s) %>%
                    dplyr::mutate(sAV = mean(dv, na.rm = TRUE)) %>%
                    dplyr::group_by_at(between) %>% #this is grouping by between subjects variables
                    dplyr::mutate(gAV = mean(dv, na.rm = TRUE)) %>%
                    dplyr::mutate(DV_n = dv - sAV + gAV), list(s = as.name(s), dv = as.name(dv))))
}


get_ci <- function(var, mean){
  sd <- sd(var, na.rm = TRUE)
  n <- length(var)
  se <- sd/sqrt(n)
  ci <- abs(qt(0.025, n-1, lower.tail = FALSE))*se
  return(c(mean + ci, mean - ci, sd, se, n))
}

convert_extremes <- function(value){
  
  if(value == 1){
    
    value = value - 1e-5
    
  } else if (value == 0 ){
    
    value = value + 1e-5
    
  }
  
  return(value)
  
}


get_ci2 <- function(var){
  mean <- mean(var)
  sd <- sd(var, na.rm = TRUE)
  n <- length(var)
  se <- sd/sqrt(n)
  ci <- abs(qt(0.025, n-1, lower.tail = FALSE))*se
  df <- data.frame(M = mean,
                   CI_lower = mean - ci,
                   CI_upper = mean + ci,
                   SD = sd, 
                   SE = se, 
                   N = n)
  
  var_name <- deparse(substitute(var))
  colnames(df) <- paste0(var_name, "_", colnames(df))
  return(df)
}



get_model_warnings <- function(model) {
  conv <- model@optinfo$conv
  singular <- isSingular(model, tol = 1e-4)
  
  conv_msg <- if (!is.null(conv$lme4$messages)) {
    paste("Convergence:", paste(conv$lme4$messages, collapse = "; "))
  } else {
    "Convergence: OK"
  }
  
  singular_msg <- if (singular) "Singular fit: yes" else "Singular fit: no"
  
  paste(conv_msg, singular_msg, sep = " | ")
}


make_apa_table <- function(df) {
  # Identify numeric and non-numeric columns
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  text_cols <- setdiff(names(df), numeric_cols)
  
  flextable::flextable(df) %>%
    flextable::colformat_double(j = numeric_cols, digits = 2) %>%
    flextable::set_table_properties(width = 1, layout = "autofit") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::padding(padding = 4, part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "header", border = officer::fp_border(color = "black", width = 1)) %>%
    flextable::hline_bottom(part = "header", border = officer::fp_border(color = "black", width = 1)) %>%
    flextable::hline_bottom(part = "body", border = officer::fp_border(color = "black", width = 1)) %>%
    flextable::align(j = text_cols, align = "left", part = "all") %>%
    flextable::align(j = numeric_cols, align = "right", part = "all")
}


make_model_table <- function(model) {
  # Extract coefficients
  df <- summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    mutate(term = gsub("relative_cue_pos", "rcp", term)) %>%
    mutate(term = gsub("age_group", "age", term)) %>%
    mutate(term = gsub("Dotdiff", "Dotd", term)) %>%
    mutate(term = gsub("BlockType", "BlockT", term))
  
  # Detect p-value column
  p_col_name <- names(df)[grepl("^Pr\\(>", names(df))]
  
  # Format and style with APA table
  ft <- make_apa_table(df)
  
  # Format p-values to 4 decimals
  ft <- ft %>%
    flextable::colformat_double(j = p_col_name, digits = 4)
  
  # Bold rows with p < 0.05
  ft <- ft %>%
    flextable::bold(i = df[[p_col_name]] < 0.05, part = "body")
  
  # Full model call (lm(), glmer(), etc.)
  model_call <- paste("Call:", paste(deparse(getCall(model)), collapse = " "))
  
  # Convergence warning if applicable
  model_warnings <- if (inherits(model, "merMod")) get_model_warnings(model) else NULL
  
  # Combine into single header string
  header_text <- if (!is.null(model_warnings)) {
    paste(model_call, "\n", model_warnings)
  } else {
    model_call
  }
  
  # Add header to table
  ft <- ft %>%
    flextable::add_header_lines(values = header_text)
  
  return(ft)
}

model_comp <- function(m1, m2, m1_label = "m1", m2_label = "m2") {
  
  # Get model summaries with dynamic label names
  m1_df <- m1 %>%
    summary() %>%
    coef() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    select(term, Estimate, contains("Pr")) %>%
    rename_with(~ "p", .cols = starts_with("Pr")) %>%
    rename_with(~ paste0(.x, "_", m1_label), .cols = -term)
  
  m2_df <- m2 %>%
    summary() %>%
    coef() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    select(term, Estimate, contains("Pr")) %>%
    rename_with(~ "p", .cols = starts_with("Pr")) %>%
    rename_with(~ paste0(.x, "_", m2_label), .cols = -term)
  
  # Full join to include all terms
  merged <- full_join(m1_df, m2_df, by = "term") %>%
    select(
      term,
      !!paste0("Estimate_", m1_label),
      !!paste0("Estimate_", m2_label),
      !!paste0("p_", m1_label),
      !!paste0("p_", m2_label)
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(term = gsub("relative_cue_pos", "rcp", term)) %>%
    mutate(term = gsub("age_group", "age", term)) %>%
    mutate(term = gsub("ValidCueProb", "ValidCueP", term)) %>%
    mutate(term = gsub("BlockType", "BlockT", term)) %>%
    mutate(
      is_different = case_when(
        !is.na(.data[[paste0("Estimate_", m1_label)]]) &
          !is.na(.data[[paste0("Estimate_", m2_label)]]) &
          (
            sign(.data[[paste0("Estimate_", m1_label)]]) != sign(.data[[paste0("Estimate_", m2_label)]]) |
              (.data[[paste0("p_", m1_label)]] < 0.05) != (.data[[paste0("p_", m2_label)]] < 0.05)
          ) ~ 1,
        TRUE ~ 0
      )
    )
  
  # Store is_different separately for bolding
  bold_rows <- which(merged$is_different == 1)
  
  # Drop is_different column before passing to make_apa_table
  merged_clean <- merged %>%
    select(-is_different)
  
  # Build and return table
  merged_clean %>%
    make_apa_table() %>%
    flextable::bold(i = bold_rows, part = "body") %>%
    flextable::colformat_double(j = c(paste0("p_", m1_label), paste0("p_", m2_label)), digits = 4) %>%
    flextable::colformat_double(j = paste0("Estimate_", m1_label), digits = 3) %>%
    flextable::colformat_double(j = paste0("Estimate_", m2_label), digits = 3) %>%
    flextable::add_header_lines(values = "Rows with differences in signs of estimates or binary significance (shared terms only) are bolded.")
}


export_flextable_to_word <- function(flextable_obj, filepath, title = NULL, subtitle = NULL) {
  library(officer)
  
  doc <- read_docx()
  
  if (!is.null(title)) {
    doc <- doc %>% body_add_par(title, style = "heading 1")
  }
  
  if (!is.null(subtitle)) {
    doc <- doc %>% body_add_par(subtitle, style = "Normal")
  }
  
  doc <- doc %>% body_add_flextable(flextable_obj)
  
  print(doc, target = filepath)
}
