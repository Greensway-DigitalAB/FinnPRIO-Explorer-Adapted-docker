## prepare cleanfinnprioresults
species_data <- simulations |> 
  # left_join(simulationSummaries) |> 
  left_join(assessments) |> 
  left_join(pests |> 
              left_join(quaran) |> 
              left_join(taxa, by = "idTaxa", suffix = c(".q",".t"))
  ) |> 
  # mutate(inEurope = as.logical(inEurope)) |> 
  mutate(inEurope = as.logical(inEurope)) |> 
  select(idSimulation, scientificName, name.t, endDate, name.q, eppoCode, inEurope) |> 
  rename("pest" = scientificName,
         "assessment_date" = endDate,
         "eppo_code" = eppoCode,
         "presence_in_europe" = inEurope, 
         "quarantine_status" = name.q,
         "taxonomic_group" = name.t)

sim_data <- simulations |> 
  ## pick the latest
  group_by(idAssessment) |> 
  arrange(desc(date), desc(idSimulation)) |> 
  slice(1) |> 
  ungroup() |> 
  ## complete the data
  left_join(simulationSummaries) |> 
  select(idSimulation, variable, q5, q25, median, mean, q75, q95) |> 
  # 2. Pivot wider so each variable becomes columns with suffixes
  pivot_wider(
    names_from = variable,
    values_from = c(q5, q25, median, mean, q75, q95),
    names_glue = "{variable}_{.value}"
  ) |> 
  select(idSimulation, 
         ENTRYA_q25, ENTRYA_median, ENTRYA_q75, ENTRYA_q5, ENTRYA_mean, ENTRYA_q95,
         ENTRYB_q25, ENTRYB_median, ENTRYB_q75, ENTRYB_q5, ENTRYB_mean, ENTRYB_q95,
         ESTABLISHMENT_q25, ESTABLISHMENT_median, ESTABLISHMENT_q75, ESTABLISHMENT_q5, ESTABLISHMENT_mean, ESTABLISHMENT_q95,
         INVASIONA_q25, INVASIONA_median, INVASIONA_q75, INVASIONA_q5, INVASIONA_mean, INVASIONA_q95,
         INVASIONB_q25, INVASIONB_median, INVASIONB_q75, INVASIONB_q5, INVASIONB_mean, INVASIONB_q95,
         IMPACT_q25, IMPACT_median, IMPACT_q75, IMPACT_q5, IMPACT_mean, IMPACT_q95,
         PREVENTABILITY_median, PREVENTABILITY_mean, 
         CONTROLLABILITY_median, CONTROLLABILITY_mean, 
         MANAGEABILITY_median,  MANAGEABILITY_mean, 
         RISKA_q25, RISKA_median,  RISKA_q75, RISKA_q5, RISKA_mean, RISKA_q95,
         RISKB_q25, RISKB_median,  RISKB_q75, RISKB_q5, RISKB_mean, RISKB_q95)
# 

cleanfinnprioresults <- species_data |> 
  left_join(sim_data) |>
  select(-idSimulation) |> 
  # 3. Rename columns to match your desired format
  rename(
         "entry_5perc" = ENTRYB_q5, 
         "entry_25perc" = ENTRYB_q25, 
         "entry_median" = ENTRYB_median, 
         "entry_mean" = ENTRYB_mean, 
         "entry_75perc" = ENTRYB_q75,
         "entry_95perc" = ENTRYB_q95,
         "establishment_and_spread_5perc" = ESTABLISHMENT_q5,
         "establishment_and_spread_25perc" = ESTABLISHMENT_q25, 
         "establishment_and_spread_median" = ESTABLISHMENT_median, 
         "establishment_and_spread_mean" = ESTABLISHMENT_mean,
         "establishment_and_spread_75perc" = ESTABLISHMENT_q75,
         "establishment_and_spread_95perc" = ESTABLISHMENT_q95,
         "invasion_5perc" = INVASIONB_q5,
         "invasion_25perc" = INVASIONB_q25, 
         "invasion_median" = INVASIONB_median, 
         "invasion_mean" = INVASIONB_mean,
         "invasion_75perc" = INVASIONB_q75,
         "invasion_95perc" = INVASIONB_q95,
         "impact_5perc" = IMPACT_q5,
         "impact_25perc" = IMPACT_q25, 
         "impact_median" = IMPACT_median, 
         "impact_mean" = IMPACT_mean,
         "impact_75perc" = IMPACT_q75,
         "impact_95perc" = IMPACT_q95,
         "preventability_median" = PREVENTABILITY_median,
         "preventability_mean" = PREVENTABILITY_mean,
         "controlability_median" = CONTROLLABILITY_median, 
         "controlability_mean" = CONTROLLABILITY_mean,
         "manageability_median" = MANAGEABILITY_median,
         "manageability_mean" = MANAGEABILITY_mean,
         "risk_5perc" = RISKB_q5,
         "risk_25perc" = RISKB_q25,
         "risk_median" = RISKB_median,
         "risk_mean" = RISKB_mean,
         "risk_75perc" = RISKB_q75,
         "risk_95perc" = RISKB_q95,
         )


# print(cleanfinnprioresults)
## Prepare pestquestions
questions_opt <- lapply(seq(1,nrow(questions_main)), function(i) {
  df <- fromJSON(questions_main$list[i])
  df$idQuestion <- questions_main$idQuestion[i]
  df |> select(opt, text, idQuestion)
}) |> bind_rows()
# 
# questions_entry
# 
# assessments
# answers_main


full_grid <- expand.grid(idAssessment = assessments$idAssessment,
                         idQuestion = questions_main$idQuestion)

# 2. Identify missing rows
missing_rows <- full_grid  |> 
  anti_join(answers_main, by = c("idAssessment","idQuestion")) |> 
  mutate(
    idAnswer = NA,  # or generate new IDs later
    min = NA,
    likely = NA,
    max = NA,
    justification = ""
  )

# 3. Append missing rows to answers
if (nrow(missing_rows) > 0) {
  answers_complete <- bind_rows(answers_main, missing_rows) |> 
    arrange(idAssessment, idQuestion)  
} else {
  answers_complete <- answers_main |> 
    arrange(idAssessment, idQuestion)  
}


# # Optional: generate new idAnswer for missing rows
# answers_complete <- answers_complete |> 
#   mutate(idAnswer = ifelse(is.na(idAnswer),
#                            max(answers_main$idAnswer, na.rm = TRUE) + row_number(),
#                            idAnswer))


# Assume you have data frames: questions, answers, assessments, pests
# 1. Join answers with questions
answers_long <- answers_complete |> 
  select(-idAnswer ) |> 
  left_join(questions_main, by = "idQuestion") |> 
  # 2. Join with assessments to get pest IDs
  right_join(assessments |>
              group_by(idPest) |>
              arrange(desc(valid), desc(endDate)) |>   # valid first, then latest date
              slice_max(order_by = endDate) |>         # pick the top row per group
              # slice(1) |>                              # pick the top row per group
              ungroup(),
            by = "idAssessment") |> 
  # 3. Join with pests to get pest names
  left_join(pests, by = "idPest") |> 
  # 4. Create a column for answer type (most likely, min, max)
  pivot_longer(cols = c(min, likely, max),
               names_to = "Answer for",
               values_to = "Answer")

# 5. Build the final table: group by question and spread pests as columns
final_table <- answers_long  |> 
  select(group, idQuestion, `Answer for`, scientificName , Answer)  |> 
  pivot_wider(names_from = scientificName, 
              values_from = Answer) |> 
  arrange(group, idQuestion) |> 
  rename("Codes" = group) |> 
  as.data.frame()


replace_opts <- function(column, idQuestion, lookup, questions) {
  # Join each value with its description
  sapply(seq_along(column), function(i) {
    val <- column[i]
    qid <- idQuestion[i]
    type <- questions$type[questions$idQuestion == qid]
    if (type == "minmax") {
      desc <- lookup$text[lookup$opt == val & lookup$idQuestion == qid]
      res <- if (length(desc) == 1) paste0(val, ". ", desc) else val
    } else {
      desc <- lookup$text[lookup$idQuestion == qid]
      res <- if (!is.na(val)) paste0(desc ,": Yes") else paste0(desc ,": No")
    }
    return(res)
  })
}

# Apply to pest columns
for (col in pests$scientificName) {
  if (col %in% colnames(final_table))
    final_table[[col]] <- replace_opts(final_table[[col]], final_table$idQuestion, questions_opt, questions_main)
}

pestquestions <- final_table |> 
  left_join(questions_main |> 
              select(idQuestion, group, number, question)) |> 
  mutate(Question = paste0(group, number, ": ", question)) |> 
  arrange(number)
pestquestions$Question <- ifelse(pestquestions$'Answer for' != "min", "", pestquestions$Question)
# select(Codes, idQuestion, Question, 'Answer for')


### Add Pathways to pestquestions
## Prepare pestquestions
questions_paths_opt <- lapply(seq(1,nrow(questions_entry)), function(i) {
  df <- fromJSON(questions_entry$list[i])
  df$idPathQuestion <- questions_entry$idPathQuestion[i]
  df |> select(opt, text, idPathQuestion)
}) |> bind_rows()

full_grid <- expand.grid(idEntryPathway = entryPathways$idEntryPathway ,
                         idPathQuestion = questions_entry$idPathQuestion)

# 2. Identify missing rows
missing_rows <- full_grid  |> 
  anti_join(answers_entry, by = c("idEntryPathway","idPathQuestion")) |> 
  mutate(
    idAnswer = NA,  # or generate new IDs later
    min = NA,
    likely = NA,
    max = NA,
    justification = ""
  )

# 3. Append missing rows to answers
if (nrow(missing_rows) > 0) {
  answers_complete <- bind_rows(answers_entry, missing_rows) |> 
    arrange(idEntryPathway, idPathQuestion)  
} else {
  answers_complete <- answers_entry |> 
    arrange(idEntryPathway, idPathQuestion)  
}


# Assume you have data frames: questions, answers, assessments, pests
# 1. Join answers with questions
answers_long <- answers_complete |> 
  select(-idPathAnswer) |> 
  left_join(questions_entry, by = "idPathQuestion") |> 
  # 2. Join with assessments to get pest IDs
  right_join(entryPathways |> 
               select(idAssessment, idPathway, idEntryPathway)) |> 
  right_join(assessments |>
               group_by(idPest) |>
               arrange(desc(valid), desc(endDate)) |>   # valid first, then latest date
               slice_max(order_by = endDate) |>         # pick the top row per group
               # slice(1) |>                              # pick the top row per group
               ungroup(),
             by = "idAssessment") |> 
  # 3. Join with pests to get pest names
  left_join(pests, by = "idPest") |> 
  # 4. Create a column for answer type (most likely, min, max)
  pivot_longer(cols = c(min, likely, max),
               names_to = "Answer for",
               values_to = "Answer")

# 5. Build the final table: group by question and spread pests as columns


# 1) Rank pathways *as they appear* within each species
answers_ranked <- answers_long |> 
  group_by(scientificName) |> 
  # match() against unique(idPathway) preserves first-seen order
  mutate(pathway_rank = match(idPathway, unique(idPathway))) |> 
  ungroup()

# Helper to build the section table for rank k

# make_section(k, pathways, all_species = NULL, group_label = "ENT")
# - k: integer pathway rank to build (1..5)
# - pathways: tibble with columns idPathway, name
# - all_species: optional character vector with the full set of species column names.
#                If NULL, will be taken from answers_ranked$scientificName (sorted).
# - group_label: value to use in "Codes" for the header row (default "ENT").

# make_section <- function(k) {
#   answers_ranked |> 
#     filter(pathway_rank == k) |> 
#     select(group, idPathQuestion, `Answer for`, scientificName, Answer) |> 
#     pivot_wider(
#       id_cols   = c(group, idPathQuestion, `Answer for`),
#       names_from  = scientificName,
#       values_from = Answer
#     ) |> 
#     arrange(group, idPathQuestion) |>    # (your original had idQuestion; using idPathQuestion here)
#     rename(Codes = group,
#            idQuestion = idPathQuestion) |> 
#     as.data.frame()
# }


make_section <- function(k, pathways, all_species = NULL, group_label = "ENT") {
  # 1) filter the k-th pathway and bring pathway name
  ar_k <- answers_ranked %>%
    filter(pathway_rank == k) %>%
    left_join(pathways %>% distinct(idPathway, name),
              by = "idPathway") %>%
    rename(pathway_name = name)
  
  # 2) build the wide table (species as columns)
  wide <- ar_k %>%
    select(group, idPathQuestion, `Answer for`, scientificName, Answer) %>%
    pivot_wider(
      id_cols    = c(group, idPathQuestion, `Answer for`),
      names_from = scientificName,
      values_from = Answer
    ) %>%
    arrange(group, idPathQuestion) %>%
    rename(Codes = group, idQuestion = idPathQuestion)
  
  # 3) ensure a consistent set/order of species columns (add missing as NA)
  species_universe <- if (is.null(all_species)) {
    sort(unique(answers_ranked$scientificName))
  } else {
    all_species
  }
  
  missing_cols <- setdiff(species_universe, names(wide))
  if (length(missing_cols)) {
    for (mc in missing_cols) wide[[mc]] <- NA_character_
  }
  wide <- wide %>%
    select(Codes, idQuestion, `Answer for`, all_of(species_universe)) |> 
    mutate(idQuestion = as.character(idQuestion))
  
  # 4) build the header row with pathway names under each species
  path_names_vec <- ar_k %>%
    distinct(scientificName, pathway_name) %>%
    tibble::deframe()  # named vector: names = species, values = pathway_name
  
  header_vals <- setNames(
    vapply(
      species_universe,
      function(s) if (s %in% names(path_names_vec)) path_names_vec[[s]] else NA_character_,
      FUN.VALUE = character(1)
    ),
    species_universe
  )
  
  header_row <- data.frame(
    Codes       = group_label,
    idQuestion    = paste0("Pathway ", k),
    check.names = FALSE
  )
  header_row[["Answer for"]] <- NA_character_
  for (s in species_universe) header_row[[s]] <- header_vals[[s]]
  
  # 5) prepend the header row
  final <- bind_rows(header_row, wide) %>% 
    as.data.frame(check.names = FALSE)
  final
}



# 2) Build up to 5 sections; drop empties
# final_table

# Build the list of species once, to keep columns consistent across all sections:
all_species <- sort(unique(answers_ranked$scientificName))

# Suppose you want sections for pathway ranks 1..5:
sections <- lapply(1:5, function(k) make_section(k, pathways, all_species))
names(sections) <- paste("Pathway", 1:5)
# Now `sections` is a named list of data frames:
# sections[["Pathway 1"]], sections[["Pathway 2"]], ... etc.

replace_opts <- function(column, idQuestion, lookup, questions) {
  # Join each value with its description
  sapply(seq_along(column), function(i) {
    val <- column[i]
    qid <- idQuestion[i]
    type <- questions$type[questions$idPathQuestion == qid]
    desc <- lookup$text[lookup$opt == val & lookup$idPathQuestion == qid]
    res <- if (length(desc) == 1) paste0(val, ". ", desc) else val
    
    return(res)
  })
}

pathways_by_section <- NULL
for (pw in 1:5) {
  # final_table <- 
  section_table <- sections[[pw]]
  # Apply to pest columns
  for (col in pests$scientificName) {
    if (col %in% colnames(section_table))
      section_table[[col]][-1] <- replace_opts(section_table[[col]][-1], section_table$idQuestion[-1], questions_paths_opt, questions_entry)
  }
  head <- section_table[1,] |> 
    mutate(group = NA,
           number = NA,
           question = NA, 
           Question = idQuestion)
  section_table_rest <- section_table[-1,] |> 
      left_join(questions_entry |> 
                  select(idPathQuestion, group, number, question) |> 
                  mutate(idPathQuestion = as.character(idPathQuestion)),
                by = c("idQuestion" = "idPathQuestion")) |> 
    mutate(Question = paste0(group, number, ": ", question)) |> 
    arrange(number)
  section_table <- rbind(head, section_table_rest)
  pathways_by_section <- rbind(pathways_by_section, section_table)
  
}

pestquestions <- rbind(pestquestions, 
                       pathways_by_section |> 
                         select(all_of(colnames(pestquestions))))


