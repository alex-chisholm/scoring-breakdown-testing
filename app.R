library(readr)
library(shiny)
library(plotly)
library(bslib)
library(showtext)
library(reactable)
library(shinyjs)
library(tidyr)

# Load data files (assuming same structure as DNA app)
squad.stats.for <- read_csv("squad.stats.for.csv")
zone.stats <- readRDS("zone_stats.RDS")

# Source the intercept and stoppage analysis functions
source("Intercept Chain Value.R")
source("Stoppage Chain Value.R")

## CREATE AGAINST AND DIFF DF'S FROM SQUAD.STATS.FOR
squad.stats.against <- squad.stats.for %>% 
  rename(squad.name = opp.squad, opp.squad = squad.name)

squad.stats.diff <- squad.stats.for %>%
  left_join(squad.stats.against, by = c("season.id", "match.id", "round.number", "venue.name", "squad.name", "name"), 
            suffix = c(".for", ".against")) %>%
  mutate(value = value.for - value.against) %>% 
  select(season.id, match.id, round.number, venue.name, squad.name, name, value)

# Get min and max rounds for the slider
min_round <- min(squad.stats.for$round.number, na.rm = TRUE)
max_round <- max(squad.stats.for$round.number, na.rm = TRUE)

# ADD SUFFIX TO RANKINGS
add_ordinal_suffix <- function(x) {
  suffix <- ifelse(x %% 100 %in% 11:13, "th",
                   ifelse(x %% 10 == 1, "st",
                          ifelse(x %% 10 == 2, "nd",
                                 ifelse(x %% 10 == 3, "rd", "th"))))
  paste0(x, suffix)
}

# Create reactive function to filter data based on period selection
create_filtered_stats <- function(period_filter, min_round, max_round) {
  
  # Determine round range based on period selection
  if (period_filter == "season") {
    round_range <- c(min_round, max_round)
  } else { # month
    round_range <- c(max_round - 3, max_round) # Last 4 rounds approximates a month
  }
  
  # Filter main datasets
  squad.stats.for.filtered <- squad.stats.for %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  
  squad.stats.against.filtered <- squad.stats.against %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  
  squad.stats.diff.filtered <- squad.stats.diff %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  
  # Filter zone stats for all zones
  zone.stats.filtered <- zone.stats
  zone.stats.filtered$D50$for.data <- zone.stats$D50$for.data %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  zone.stats.filtered$DM$for.data <- zone.stats$DM$for.data %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  zone.stats.filtered$AM$for.data <- zone.stats$AM$for.data %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  zone.stats.filtered$F50$for.data <- zone.stats$F50$for.data %>%
    filter(round.number >= round_range[1] & round.number <= round_range[2])
  
  return(list(
    squad.stats.for = squad.stats.for.filtered,
    squad.stats.against = squad.stats.against.filtered,
    squad.stats.diff = squad.stats.diff.filtered,
    zone.stats = zone.stats.filtered
  ))
}

# Move all stat calculations into a function that takes filtered data
calculate_stats <- function(filtered_data) {
  
  squad.stats.for <- filtered_data$squad.stats.for
  squad.stats.against <- filtered_data$squad.stats.against
  squad.stats.diff <- filtered_data$squad.stats.diff
  zone.stats <- filtered_data$zone.stats
  
  # Points metrics
  points.for <- squad.stats.for %>% 
    filter(name == "Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "For"
    )
  
  exp.points.for <- squad.stats.for %>% 
    filter(name == "Expected Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "For"
    )
  
  int.points.for <- squad.stats.for %>% 
    filter(name == "Intercept Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "For"
    )
  
  stp.points.for <- squad.stats.for %>% 
    filter(name == "Stoppage Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "For"
    )
  
  points.against <- squad.stats.against %>% 
    filter(name == "Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Against"
    )
  
  exp.points.against <- squad.stats.against %>% 
    filter(name == "Expected Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Against"
    )
  
  int.points.against <- squad.stats.against %>% 
    filter(name == "Intercept Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Against"
    )
  
  stp.points.against <- squad.stats.against %>% 
    filter(name == "Stoppage Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(value, ties.method = "min"),
      display.value = paste0(value, "pts"),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Against"
    )
  
  points.diff <- squad.stats.diff %>% 
    filter(name == "Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = if_else(value > 0, sprintf("+%.1fpts", value), sprintf("%.1fpts", value)),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Diff"
    )
  
  exp.points.diff <- squad.stats.diff %>% 
    filter(name == "Expected Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = if_else(value > 0, sprintf("+%.1fpts", value), sprintf("%.1fpts", value)),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Diff"
    )
  
  int.points.diff <- squad.stats.diff %>% 
    filter(name == "Intercept Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = if_else(value > 0, sprintf("+%.1fpts", value), sprintf("%.1fpts", value)),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Diff"
    )
  
  stp.points.diff <- squad.stats.diff %>% 
    filter(name == "Stoppage Points") %>%
    group_by(squad.name) %>% 
    summarise(value = round(mean(value, na.rm = TRUE), 1)) %>% 
    ungroup() %>% 
    mutate(
      rank = rank(-value, ties.method = "min"),
      display.value = if_else(value > 0, sprintf("+%.1fpts", value), sprintf("%.1fpts", value)),
      display.rank = add_ordinal_suffix(rank),
      display.title = "Diff"
    )
  
  # Calculate intercept and stoppage efficiency using existing functions
  intercept_results <- run_intercept_analysis(zone.stats)
  stoppage_results <- run_stoppage_analysis(zone.stats, squad.stats.for)
  
  # Return all calculated stats as a list
  return(list(
    points.for = points.for,
    exp.points.for = exp.points.for,
    int.points.for = int.points.for,
    stp.points.for = stp.points.for,
    points.against = points.against,
    exp.points.against = exp.points.against,
    int.points.against = int.points.against,
    stp.points.against = stp.points.against,
    points.diff = points.diff,
    exp.points.diff = exp.points.diff,
    int.points.diff = int.points.diff,
    stp.points.diff = stp.points.diff,
    intercept_results = intercept_results,
    stoppage_results = stoppage_results
  ))
}

## CREATE STAT CARD FUNCTION - FIXED for accordion ranks
statCard <- function(title, value, rank, info_text = NULL, extras = list(), plot_output_id = NULL, plot_in_accordion = FALSE, table_output_id = NULL) {
  ns <- paste0(gsub(" ", "_", tolower(title)))
  
  plot_output <- if (!is.null(plot_output_id)) plotlyOutput(plot_output_id, height = "90px")
  table_output <- if (!is.null(table_output_id)) reactableOutput(table_output_id)
  
  div(class = "stat-block",
      div(class = "stat-title",
          span(title),
          if (!is.null(info_text)) {
            tags$span(
              class = "info-icon",
              `data-bs-toggle` = "tooltip",
              `data-bs-placement` = "top",
              title = info_text,
              style = "margin-left: 6px; color: #999; cursor: pointer;",
              icon("info-circle")
            )
          },
          if (!is.null(table_output_id)) {
            tags$span(
              class = "table-icon",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = paste0("#", ns, "_table"),
              `aria-expanded` = "false",
              `aria-controls` = paste0(ns, "_table"),
              style = "margin-left: 6px; color: #999; cursor: pointer;",
              icon("table")
            )
          }
      ),
      
      # Main stat value + rank
      div(class = "stat-value",
          div(style = "display: flex; align-items: center; justify-content: center; gap: 5px;",
              value, "|", rank
          )
      ),
      
      # Plot (if not in accordion)
      if (!plot_in_accordion) plot_output,
      
      # Toggleable table section
      if (!is.null(table_output_id)) {
        div(
          id = paste0(ns, "_table"),
          class = "collapse mt-2",
          table_output
        )
      },
      
      # Accordion
      div(class = "accordion", id = paste0(ns, "_accordion"),
          div(class = "accordion-item",
              tags$h2(class = "accordion-header", id = paste0(ns, "_heading"),
                      tags$button(class = "accordion-button collapsed", type = "button",
                                  `data-bs-toggle` = "collapse",
                                  `data-bs-target` = paste0("#", ns, "_collapse"),
                                  `aria-expanded` = "false",
                                  `aria-controls` = paste0(ns, "_collapse"),
                                  "Insights")
              ),
              div(id = paste0(ns, "_collapse"), class = "accordion-collapse collapse",
                  `aria-labelledby` = paste0(ns, "_heading"),
                  div(class = "accordion-body",
                      if (plot_in_accordion) plot_output,
                      tagList(
                        lapply(extras, function(extra) {
                          tagList(
                            tags$hr(style = "margin: 5px 0;"),
                            tags$b(extra$title),
                            div(style = "display: flex; align-items: center; justify-content: center; font-size: 0.9em; margin-bottom: 1px; gap: 5px;",
                                extra$value, "|", extra$rank,
                                if (!is.null(extra$info_text)) {
                                  tags$span(
                                    class = "info-icon",
                                    `data-bs-toggle` = "tooltip",
                                    `data-bs-placement` = "top",
                                    title = extra$info_text,
                                    style = "margin-left: 5px; cursor: pointer;",
                                    icon("info-circle")
                                  )
                                }
                            ),
                            if (!is.null(extra$plot_output_id) && isTRUE(extra$plot_in_accordion)) {
                              plotlyOutput(extra$plot_output_id, height = "90px")
                            }
                          )
                        })
                      )
                  )
              )
          )
      )
  )
}

## CREATE RANK BOX FUNCTION (same as DNA app)
rank_box <- function(rank, is_statcard = TRUE) {
  # Handle missing or NULL values
  if (is.null(rank) || length(rank) == 0) {
    return('<div style="display: inline-block; background-color:#6c757d; color: white; font-size: 0.9em; padding: 2px 4px; border-radius: 3px; vertical-align: 1px; line-height: 1.3;">--</div>')
  }
  
  # Take the first element if it's a vector
  if (length(rank) > 1) {
    rank <- rank[1]
  }
  
  # Check for NA after taking first element
  if (is.na(rank)) {
    return('<div style="display: inline-block; background-color:#6c757d; color: white; font-size: 0.9em; padding: 2px 4px; border-radius: 3px; vertical-align: 1px; line-height: 1.3;">--</div>')
  }
  
  # Convert rank to character and extract just the number for conditional formatting
  rank_str <- as.character(rank)
  rank_num <- as.numeric(gsub("[^0-9]", "", rank_str))
  
  # Handle cases where extraction fails
  if (is.na(rank_num)) {
    return('<div style="display: inline-block; background-color:#6c757d; color: white; font-size: 0.9em; padding: 2px 4px; border-radius: 3px; vertical-align: 1px; line-height: 1.3;">--</div>')
  }
  
  color <- if (rank_num <= 4) {
    "#28a745"  # Green
  } else if (rank_num >= 15) {
    "#dc3545"  # Red
  } else {
    "#6c757d"  # Grey
  }
  
  font_size <- if (is_statcard) {
    "0.9em"
  } else {
    "0.9em"
  }
  
  # Add ordinal suffix
  rank_with_suffix <- add_ordinal_suffix(rank_num)
  
  sprintf(
    '<div style="display: inline-block; background-color:%s; color: white; font-size: %s; padding: 2px 4px; border-radius: 3px; vertical-align: 1px; line-height: 1.3;">%s</div>',
    color, font_size, rank_with_suffix
  )
}

# PLOTLY FUNCTION (same as DNA app)
plotly_function <- function(df, highlight_team = "Collingwood", highlight_team_2 = NULL, reverse = FALSE) {
  # Validate highlight teams
  is_highlight_1 <- !is.null(highlight_team) && highlight_team != ""
  is_highlight_2 <- !is.null(highlight_team_2) && highlight_team_2 != ""
  
  # Identify highlight rows
  highlight_row <- if (is_highlight_1) df %>% filter(squad.name == highlight_team) else NULL
  highlight_row_2 <- if (is_highlight_2) df %>% filter(squad.name == highlight_team_2) else NULL
  
  # Mean and SD for range
  sd_val <- sd(df$value, na.rm = TRUE)
  mean_val <- mean(df$value, na.rm = TRUE)
  x_range <- if (reverse) {
    c(mean_val + 3 * sd_val, mean_val - 3 * sd_val)
  } else {
    c(mean_val - 3 * sd_val, mean_val + 3 * sd_val)
  }
  
  # Base plot
  fig <- plot_ly()
  
  # Grey teams (excluding valid highlights only)
  excluded_teams <- c(
    if (is_highlight_1) highlight_team else NULL,
    if (is_highlight_2) highlight_team_2 else NULL
  )
  
  fig <- fig %>%
    add_trace(
      data = df %>% filter(!squad.name %in% excluded_teams),
      x = ~value,
      y = 0,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 20, color = 'rgba(160,160,160,0.5)'),
      hoverinfo = 'text',
      text = ~paste(squad.name, "<br>", display.title, display.value, "<br>Rank", display.rank),
      hoverlabel = list(
        font = list(color = "black"),
        bgcolor = "white",
        bordercolor = "black"
      ),
      showlegend = FALSE
    )
  
  # Highlight team 1
  # Add highlight team that is not Collingwood first
  if (is_highlight_1 && highlight_team != "Collingwood" && nrow(highlight_row) == 1) {
    fig <- fig %>% add_trace(
      data = highlight_row,
      x = ~value, y = 0,
      type = 'scatter', mode = 'markers',
      marker = list(size = 20, color = get.team.colours(highlight_team),
                    line = list(color = "#000000", width = 1.5)),
      hoverinfo = 'text',
      text = ~paste(squad.name, "<br>", display.title, ": ", display.value, "<br>Rank: ", display.rank),
      hoverlabel = list(font = list(color = "white"),
                        bgcolor = get.team.colours(highlight_team),
                        bordercolor = "white"),
      showlegend = FALSE
    )
  }
  if (is_highlight_2 && highlight_team_2 != "Collingwood" && nrow(highlight_row_2) == 1) {
    fig <- fig %>% add_trace(
      data = highlight_row_2,
      x = ~value, y = 0,
      type = 'scatter', mode = 'markers',
      marker = list(size = 20, color = get.team.colours(highlight_team_2),
                    line = list(color = "#000000", width = 1.5)),
      hoverinfo = 'text',
      text = ~paste(squad.name, "<br>", display.title, ": ", display.value, "<br>Rank: ", display.rank),
      hoverlabel = list(font = list(color = "white"),
                        bgcolor = get.team.colours(highlight_team_2),
                        bordercolor = "white"),
      showlegend = FALSE
    )
  }
  # Then ensure Collingwood is always last (on top)
  if ((highlight_team == "Collingwood" && nrow(highlight_row) == 1) ||
      (highlight_team_2 == "Collingwood" && nrow(highlight_row_2) == 1)) {
    fig <- fig %>% add_trace(
      data = if (highlight_team == "Collingwood") highlight_row else highlight_row_2,
      x = ~value, y = 0,
      type = 'scatter', mode = 'markers',
      marker = list(size = 20, color = get.team.colours("Collingwood"),
                    line = list(color = "#000000", width = 1.5)),
      hoverinfo = 'text',
      text = ~paste(squad.name, "<br>", display.title, ": ", display.value, "<br>Rank: ", display.rank),
      hoverlabel = list(font = list(color = "white"),
                        bgcolor = get.team.colours("Collingwood"),
                        bordercolor = "white"),
      showlegend = FALSE
    )
  }
  
  # Final layout
  fig <- fig %>%
    layout(
      shapes = list(
        list(
          type = "line",
          x0 = x_range[1],
          x1 = x_range[2],
          y0 = 0,
          y1 = 0,
          xref = "x",
          yref = "y",
          line = list(color = "rgba(160,160,160,0.3)", width = 1)
        ),
        list(
          type = "line",
          x0 = mean_val,
          x1 = mean_val,
          y0 = -0.01,
          y1 = 0.01,
          xref = "x",
          yref = "y",
          line = list(color = "rgba(160,160,160,0.9)", width = 1, dash = "dot")
        )
      ),
      xaxis = list(title = "", range = x_range, showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(l = 0, r = 0, t = 20, b = 0),
      height = 100
    ) %>%
    config(displayModeBar = FALSE)
  
  return(fig)
}

get.team.colours <- function(team.name) {
  case_when(
    team.name == "Adelaide Crows" ~ "#E1251B",
    team.name == "Brisbane Lions" ~ "#A30046",
    team.name == "Carlton" ~ "#002B5C",
    team.name == "Collingwood" ~ "#AA9767",
    team.name == "Essendon" ~ "#CC2031",
    team.name == "Fremantle" ~ "#2A0D54",
    team.name == "Geelong Cats" ~ "#002B5C",
    team.name == "Gold Coast SUNS" ~ "#E02112",
    team.name == "GWS GIANTS" ~ "#F47920",
    team.name == "Hawthorn" ~ "#FBBF15",
    team.name == "Melbourne" ~ "#CC2031",
    team.name == "North Melbourne" ~ "#1A3B8E",
    team.name == "Port Adelaide" ~ "#008AAB",
    team.name == "Richmond" ~ "#FFD200",
    team.name == "St Kilda" ~ "#ED1B2F",
    team.name == "Sydney Swans" ~ "#E1251B",
    team.name == "Western Bulldogs" ~ "#20539D",
    team.name == "West Coast Eagles" ~ "#003087",
    TRUE ~ "#999999"  # safe fallback
  )
}

# Create scatter plot card function for intercept and stoppage plots
scatterPlotCard <- function(title, plot_output_id, efficiency_type = "intercept") {
  ns <- paste0(gsub(" ", "_", tolower(title)))
  
  # Set descriptions based on type
  if (efficiency_type == "intercept") {
    scoring_desc <- HTML("<strong>Intercept Scoring Efficiency (ISE)</strong> measures how effectively a team converts intercepts into scores, adjusted for context — such as field position and scoring difficulty. Rather than simply counting points per intercept, ISE accounts for how likely a team should be to score based on where the intercept occurred. This allows for fair comparison across all teams, regardless of whether their intercepts typically occur near goal or further up the field. For example, an ISE of 1.2 means the team scores 20% more than expected from their intercept opportunities, while a value below 1.0 indicates underperformance relative to the league average.")
    
    defensive_desc <- HTML("<strong>Intercept Defensive Efficiency (IDE)</strong> applies the same logic to assess how well a team defends against intercepts — showing whether opponents score more or less than expected when they intercept against them. A lower IDE value indicates better defensive performance.")
  } else {
    scoring_desc <- HTML("<strong>Stoppage Scoring Efficiency (SSE)</strong> measures how effectively a team converts stoppages into scores, adjusted for context — such as field position and scoring difficulty. Rather than simply counting points per stoppage, SSE accounts for how likely a team should be to score based on where the stoppage occurred. This allows for fair comparison across all teams, regardless of whether their stoppages typically occur near goal or further up the field. For example, an SSE of 1.2 means the team scores 20% more than expected from their stoppage opportunities, while a value below 1.0 indicates underperformance relative to the league average.")
    
    defensive_desc <- HTML("<strong>Stoppage Defensive Efficiency (SDE)</strong> applies the same logic to assess how well a team defends stoppages — showing whether opponents score more or less than expected against them. A lower SDE value indicates better defensive performance.")
  }
  
  div(class = "stat-block",
      # Header with title and fullscreen button
      div(class = "stat-header", style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
          # Invisible spacer to balance the fullscreen button
          div(style = "width: 40px; height: 32px;"),
          
          div(class = "stat-title", style = "flex-grow: 1; text-align: center;",
              span(title)
          ),
          # Fullscreen button
          tags$button(
            class = "btn btn-sm btn-outline-secondary fullscreen-btn",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste0("#", ns, "_fullscreen_modal"),
            style = "border: none; background: transparent; color: #999; padding: 4px 8px; font-size: 16px; width: 40px; height: 32px;",
            icon("external-link-alt"),
            title = "View Fullscreen"
          )
      ),
      # Collapsible subtitle
      div(style = "text-align: center;",
          tags$a(
            tagList(
              icon("info-circle", style = "margin-right: 5px;"),
              "How is this measured?"
            ),
            class = "measurement-subtitle",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = paste0("#", ns, "_measurement"),
            `aria-expanded` = "false",
            `aria-controls` = paste0(ns, "_measurement"),
            style = "color: #666; font-size: 0.9em; text-decoration: none; cursor: pointer; display: inline-block; margin-bottom: 10px;"
          )
      ),
      # Collapsible measurement description
      div(
        id = paste0(ns, "_measurement"),
        class = "collapse",
        div(
          class = "measurement-content",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; text-align: left; font-size: 0.85em; line-height: 1.4;",
          tags$p(scoring_desc),
          tags$p(defensive_desc)
        )
      ),
      # Main plot
      div(class = "scatter-plot-container", style = "height: 550px; margin-top: 10px;",
          plotlyOutput(plot_output_id, height = "550px")
      ),
      
      # Fullscreen Modal
      div(
        class = "modal fade",
        id = paste0(ns, "_fullscreen_modal"),
        tabindex = "-1",
        `aria-labelledby` = paste0(ns, "_fullscreen_modal_label"),
        `aria-hidden` = "true",
        div(
          class = "modal-dialog modal-fullscreen",
          div(
            class = "modal-content",
            div(
              class = "modal-header",
              tags$h5(
                class = "modal-title",
                id = paste0(ns, "_fullscreen_modal_label"),
                paste(title, "- Fullscreen View")
              ),
              tags$button(
                type = "button",
                class = "btn-close",
                `data-bs-dismiss` = "modal",
                `aria-label` = "Close"
              )
            ),
            div(
              class = "modal-body",
              style = "padding: 20px;",
              plotlyOutput(paste0(plot_output_id, "_fullscreen"), height = "calc(100vh - 200px)")
            )
          )
        )
      )
  )
}

ui <- page_navbar(
  theme = bs_theme(bootswatch = "zephyr"),
  title = tags$div(
    class = "custom-navbar-brand d-flex align-items-center",
    tags$img(src = "https://i.postimg.cc/qqY0dg2L/COLLINGWOOD.png", height = "40px", style = "margin-right: 10px;"),
    tags$span("SCORING BREAKDOWN", class = "navbar-title")
  ),
  nav_spacer(),
  nav_item(
    div(class = "d-flex align-items-center",
        div(class = "me-3",
            selectInput(
              "period_filter",
              NULL,
              choices = c("Season" = "season", "Last Month" = "month"),
              selected = "season",
              width = "150px"
            )
        ),
        selectInput(
          "team2", 
          NULL,
          choices = c(
            "Select Opposition Team" = "",
            "Clear Selection" = "clear",
            setdiff(
              c(
                "Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood", "Essendon", 
                "Fremantle", "Geelong Cats", "Gold Coast SUNS", "GWS GIANTS", "Hawthorn", 
                "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", 
                "St Kilda", "Sydney Swans", "Western Bulldogs", "West Coast Eagles"
              ),
              "Collingwood"
            )
          ),
          selected = "",
          width = "200px"
        ),
        # Add JavaScript to fix dropdown text colors after page loads
        tags$script(HTML("
            $(document).ready(function() {
              $('#team2, #period_filter').css({
                'color': 'black',
                'background-color': 'white'
              });
            });
          "))
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML('
  $(document).ready(function () {
    var tooltipTriggerList = [].slice.call(document.querySelectorAll(\'[data-bs-toggle="tooltip"]\'))
    var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl)
    })
  });
')),
    tags$style(HTML("
      body {
        background-color: #D9D9D9;
      }
      .navbar {
        background-color: #000000 !important; 
        padding-top: 0.25rem !important;     
        padding-bottom: 0.25rem !important;   
        min-height: 30px !important;          
        display: flex !important;
        align-items: center !important;
      }

      .custom-navbar-brand {
        display: flex;
        align-items: center;
      }

      .navbar-title {
        font-family: 'DIN Condensed';
        font-weight: bold;
        font-size: 2em;
        color: white;
        align-items: center;
      }

      .stat-block {
        background-color: #ffffff;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 10px;
        box-shadow: 0 0 5px rgba(0,0,0,0.1);
        text-align: center;
      }

      .stat-title {
        font-weight: bold;
        margin-bottom: 0px;
      }

      main.container-fluid,
      main.container,
      .tab-content {
        padding-top: 0px !important;
        margin-top: 20px !important;   
        padding-left: 20px !important;
        padding-right: 20px !important;
      }

      .stat-value {
        font-size: 0.9em;
        margin-bottom: 10px;
      }

      .scatter-plot-container {
        display: flex;
        justify-content: center;
        align-items: center;
      }

      .accordion-body {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        text-align: center;
      }

      .accordion-button {
        display: block;
        width: 100%;
        text-align: center;
      }
      
      .zone-stat-mini {
        background-color: #f8f9fa;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
        text-align: center;
        border: 1px solid #e9ecef;
      }
      
      .zone-stat-mini b {
        display: block;
        margin-bottom: 5px;
        font-size: 0.9em;
        color: #495057;
      }
      
      .navbar .form-label {
        color: white !important;
        font-weight: 500;
        font-family: 'DIN Condensed';
      }
      
      /* Fix alignment and styling for navbar inputs */
      .navbar .form-group {
        margin-bottom: 0 !important;
      }
      
      .navbar .shiny-input-container {
        margin-bottom: 0 !important;
      }
      
      .navbar .form-select {
        margin-bottom: 0 !important;
        height: 38px;
      }
      
      .navbar select#team2 {
        color: black !important;
        background-color: white !important;
      }
      
      .navbar select#team2 option {
        color: black !important;
        background-color: white !important;
      }
      
      /* Style period filter dropdown to match */
      .navbar select#period_filter {
        color: black !important;
        background-color: white !important;
      }
      
      .navbar select#period_filter option {
        color: black !important;
        background-color: white !important;
      }
      
      /* Also target the select input wrapper */
      .navbar .form-select {
        color: black !important;
        background-color: white !important;
      }
      
      .navbar .form-select option {
        color: black !important;
        background-color: white !important;
      }
      
      /* Default state: white background, black text */
      select#period_filter,
      select#team2,
      .navbar .form-select {
        background-color: white !important;
        color: black !important;
        border: 1px solid #ccc !important;
        font-weight: 500;
        font-family: 'Helvetica', sans-serif;
      }

      /* Dropdown menu options: default state */
      select#period_filter option,
      select#team2 option,
      .navbar .form-select option {
        background-color: white !important;
        color: black !important;
      }

      /* Hover and focus state: black background, white text */
      select#period_filter:hover,
      select#team2:hover,
      select#period_filter:focus,
      select#team2:focus,
      .navbar .form-select:hover,
      .navbar .form-select:focus {
        background-color: black !important;
        color: white !important;
        border: 1px solid black !important;
      }

      /* Selected option inside dropdown (when dropdown is open) */
      select#period_filter option:checked,
      select#team2 option:checked {
        background-color: black !important;
        color: white !important;
      }

      /* Base appearance for selectize inputs */
      .selectize-input {
        background-color: white !important;
        color: black !important;
        border: 1px solid #ccc !important;
        font-family: 'Helvetica', sans-serif;
        font-weight: 500;
      }

      /* On hover: black bg, white text */
      .selectize-input:hover,
      .selectize-input.focus {
        background-color: black !important;
        color: white !important;
      }

      /* Dropdown menu options */
      .selectize-dropdown-content .option {
        background-color: white !important;
        color: black !important;
      }

      /* Hover over options */
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background-color: black !important;
        color: white !important;
      }

      /* Remove blue highlight/focus ring */
      .selectize-control.single .selectize-input.dropdown-active {
        border-color: black !important;
        box-shadow: none !important;
      }
    ")),
  ),
  
  # Main content layout
  fluidRow(
    # Three stat cards across the top
    column(4,
           statCard(
             "Points For", 
             uiOutput("points_for_value"), 
             uiOutput("points_for_rank"),
             info_text = "Average points scored per game",
             plot_output_id = "points_for_plot",
             table_output_id = "points_for_table",
             extras = list(
               list(
                 title = "Expected Points For",
                 value = uiOutput("exp_points_for_value"),
                 rank = uiOutput("exp_points_for_rank_accordion"),
                 info_text = "Expected points based on shot quality and position",
                 plot_output_id = "exp_points_for_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Intercept Points For",
                 value = uiOutput("int_points_for_value"),
                 rank = uiOutput("int_points_for_rank_accordion"),
                 info_text = "Points scored from intercept chains",
                 plot_output_id = "int_points_for_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Stoppage Points For",
                 value = uiOutput("stp_points_for_value"),
                 rank = uiOutput("stp_points_for_rank_accordion"),
                 info_text = "Points scored from stoppage chains",
                 plot_output_id = "stp_points_for_plot",
                 plot_in_accordion = TRUE
               )
             )
           )
    ),
    column(4,
           statCard(
             "Points Against", 
             uiOutput("points_against_value"), 
             uiOutput("points_against_rank"),
             info_text = "Average points conceded per game",
             plot_output_id = "points_against_plot",
             table_output_id = "points_against_table",
             extras = list(
               list(
                 title = "Expected Points Against",
                 value = uiOutput("exp_points_against_value"),
                 rank = uiOutput("exp_points_against_rank_accordion"),
                 info_text = "Expected points conceded based on opposition shot quality and position",
                 plot_output_id = "exp_points_against_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Intercept Points Against",
                 value = uiOutput("int_points_against_value"),
                 rank = uiOutput("int_points_against_rank_accordion"),
                 info_text = "Points conceded from opposition intercept chains",
                 plot_output_id = "int_points_against_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Stoppage Points Against",
                 value = uiOutput("stp_points_against_value"),
                 rank = uiOutput("stp_points_against_rank_accordion"),
                 info_text = "Points conceded from opposition stoppage chains",
                 plot_output_id = "stp_points_against_plot",
                 plot_in_accordion = TRUE
               )
             )
           )
    ),
    column(4,
           statCard(
             "Points Diff", 
             uiOutput("points_diff_value"), 
             uiOutput("points_diff_rank"),
             info_text = "Average point differential per game",
             plot_output_id = "points_diff_plot",
             table_output_id = "points_diff_table",
             extras = list(
               list(
                 title = "Expected Points Diff",
                 value = uiOutput("exp_points_diff_value"),
                 rank = uiOutput("exp_points_diff_rank_accordion"),
                 info_text = "Expected point differential based on shot quality",
                 plot_output_id = "exp_points_diff_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Intercept Points Diff",
                 value = uiOutput("int_points_diff_value"),
                 rank = uiOutput("int_points_diff_rank_accordion"),
                 info_text = "Point differential from intercept chains",
                 plot_output_id = "int_points_diff_plot",
                 plot_in_accordion = TRUE
               ),
               list(
                 title = "Stoppage Points Diff",
                 value = uiOutput("stp_points_diff_value"),
                 rank = uiOutput("stp_points_diff_rank_accordion"),
                 info_text = "Point differential from stoppage chains",
                 plot_output_id = "stp_points_diff_plot",
                 plot_in_accordion = TRUE
               )
             )
           )
    )
  ),
  
  # Two scatter plot cards below
  fluidRow(
    column(6,
           scatterPlotCard(
             "Intercept", 
             "intercept_scatter_plot",
             efficiency_type = "intercept"
           )
    ),
    column(6,
           scatterPlotCard(
             "Stoppage", 
             "stoppage_scatter_plot",
             efficiency_type = "stoppage"
           )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for filtered stats
  filtered_stats <- reactive({
    req(input$period_filter)
    filtered_data <- create_filtered_stats(input$period_filter, min_round, max_round)
    calculate_stats(filtered_data)
  })
  
  # Helper function for plots
  apply_plot <- function(data, reverse = FALSE) {
    plotly_function(data, highlight_team = "Collingwood", highlight_team_2 = input$team2, reverse = reverse)
  }
  
  # Helper function to safely extract rank
  get_team_rank <- function(data, team_name = "Collingwood") {
    team_data <- data %>% filter(squad.name == team_name)
    if(nrow(team_data) > 0) {
      rank_val <- team_data %>% pull(rank)
      if(length(rank_val) > 0 && !is.na(rank_val)) {
        return(rank_val[1])
      }
    }
    return(1)  # Default fallback
  }
  
  # Helper function to safely extract value
  get_team_value <- function(data, team_name = "Collingwood") {
    team_data <- data %>% filter(squad.name == team_name)
    if(nrow(team_data) > 0) {
      display_val <- team_data %>% pull(display.value)
      if(length(display_val) > 0 && !is.na(display_val)) {
        return(display_val[1])
      }
    }
    return("--")  # Default fallback
  }
  
  # Points For outputs - Main card
  output$points_for_value <- renderUI({
    stats <- filtered_stats()
    req(stats$points.for)
    HTML(stats$points.for %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$points_for_rank <- renderUI({
    stats <- filtered_stats()
    req(stats$points.for)
    rank_val <- get_team_rank(stats$points.for)
    HTML(rank_box(rank_val, is_statcard = TRUE))
  })
  
  output$points_for_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$points.for)
    apply_plot(stats$points.for)
  })
  
  # Expected Points For outputs - Main and accordion
  output$exp_points_for_value <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.for)
    HTML(stats$exp.points.for %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$exp_points_for_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.for)
    rank_val <- get_team_rank(stats$exp.points.for)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$exp_points_for_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$exp.points.for)
    apply_plot(stats$exp.points.for)
  })
  
  # Intercept Points For outputs - Main and accordion
  output$int_points_for_value <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.for)
    HTML(stats$int.points.for %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$int_points_for_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.for)
    rank_val <- get_team_rank(stats$int.points.for)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$int_points_for_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$int.points.for)
    apply_plot(stats$int.points.for)
  })
  
  # Stoppage Points For outputs - Main and accordion
  output$stp_points_for_value <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.for)
    HTML(stats$stp.points.for %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$stp_points_for_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.for)
    rank_val <- get_team_rank(stats$stp.points.for)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$stp_points_for_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$stp.points.for)
    apply_plot(stats$stp.points.for)
  })
  
  # Points Against outputs - Main card
  output$points_against_value <- renderUI({
    stats <- filtered_stats()
    req(stats$points.against)
    HTML(stats$points.against %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$points_against_rank <- renderUI({
    stats <- filtered_stats()
    req(stats$points.against)
    rank_val <- get_team_rank(stats$points.against)
    HTML(rank_box(rank_val, is_statcard = TRUE))
  })
  
  output$points_against_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$points.against)
    apply_plot(stats$points.against, reverse = TRUE)
  })
  
  # Expected Points Against outputs - Main and accordion
  output$exp_points_against_value <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.against)
    HTML(stats$exp.points.against %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$exp_points_against_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.against)
    rank_val <- get_team_rank(stats$exp.points.against)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$exp_points_against_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$exp.points.against)
    apply_plot(stats$exp.points.against, reverse = TRUE)
  })
  
  # Intercept Points Against outputs - Main and accordion
  output$int_points_against_value <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.against)
    HTML(stats$int.points.against %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$int_points_against_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.against)
    rank_val <- get_team_rank(stats$int.points.against)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$int_points_against_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$int.points.against)
    apply_plot(stats$int.points.against, reverse = TRUE)
  })
  
  # Stoppage Points Against outputs - Main and accordion
  output$stp_points_against_value <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.against)
    HTML(stats$stp.points.against %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$stp_points_against_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.against)
    rank_val <- get_team_rank(stats$stp.points.against)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$stp_points_against_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$stp.points.against)
    apply_plot(stats$stp.points.against, reverse = TRUE)
  })
  
  # Points Diff outputs - Main card
  output$points_diff_value <- renderUI({
    stats <- filtered_stats()
    req(stats$points.diff)
    HTML(stats$points.diff %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$points_diff_rank <- renderUI({
    stats <- filtered_stats()
    req(stats$points.diff)
    rank_val <- get_team_rank(stats$points.diff)
    HTML(rank_box(rank_val, is_statcard = TRUE))
  })
  
  output$points_diff_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$points.diff)
    apply_plot(stats$points.diff)
  })
  
  # Expected Points Diff outputs - Main and accordion
  output$exp_points_diff_value <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.diff)
    HTML(stats$exp.points.diff %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$exp_points_diff_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$exp.points.diff)
    rank_val <- get_team_rank(stats$exp.points.diff)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$exp_points_diff_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$exp.points.diff)
    apply_plot(stats$exp.points.diff)
  })
  
  # Intercept Points Diff outputs - Main and accordion
  output$int_points_diff_value <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.diff)
    HTML(stats$int.points.diff %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$int_points_diff_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$int.points.diff)
    rank_val <- get_team_rank(stats$int.points.diff)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$int_points_diff_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$int.points.diff)
    apply_plot(stats$int.points.diff)
  })
  
  # Stoppage Points Diff outputs - Main and accordion
  output$stp_points_diff_value <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.diff)
    HTML(stats$stp.points.diff %>% filter(squad.name == "Collingwood") %>% pull(display.value))
  })
  
  output$stp_points_diff_rank_accordion <- renderUI({
    stats <- filtered_stats()
    req(stats$stp.points.diff)
    rank_val <- get_team_rank(stats$stp.points.diff)
    HTML(rank_box(rank_val, is_statcard = FALSE))
  })
  
  output$stp_points_diff_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$stp.points.diff)
    apply_plot(stats$stp.points.diff)
  })
  
  # Scatter plots
  output$intercept_scatter_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$intercept_results)
    stats$intercept_results$plot
  })
  
  output$stoppage_scatter_plot <- renderPlotly({
    stats <- filtered_stats()
    req(stats$stoppage_results)
    stats$stoppage_results$plot
  })
  
  # Fullscreen scatter plots
  output$intercept_scatter_plot_fullscreen <- renderPlotly({
    stats <- filtered_stats()
    req(stats$intercept_results)
    stats$intercept_results$plot
  })
  
  output$stoppage_scatter_plot_fullscreen <- renderPlotly({
    stats <- filtered_stats()
    req(stats$stoppage_results)
    stats$stoppage_results$plot
  })
  
  # Reactive tables
  output$points_for_table <- renderReactable({
    stats <- filtered_stats()
    req(stats$points.for)
    
    reactable(
      stats$points.for %>%
        select(Squad = squad.name, Value = value, Rank = display.rank) %>%
        arrange(desc(Value)),
      pagination = FALSE,
      defaultSorted = "Value",
      defaultSortOrder = "desc",
      columns = list(
        Squad = colDef(name = "Squad"),
        Value = colDef(name = "Points"),
        Rank = colDef(name = "Rank")
      )
    )
  })
  
  output$points_against_table <- renderReactable({
    stats <- filtered_stats()
    req(stats$points.against)
    
    reactable(
      stats$points.against %>%
        select(Squad = squad.name, Value = value, Rank = display.rank) %>%
        arrange(Value),
      pagination = FALSE,
      defaultSorted = "Value",
      defaultSortOrder = "asc",
      columns = list(
        Squad = colDef(name = "Squad"),
        Value = colDef(name = "Points"),
        Rank = colDef(name = "Rank")
      )
    )
  })
  
  output$points_diff_table <- renderReactable({
    stats <- filtered_stats()
    req(stats$points.diff)
    
    reactable(
      stats$points.diff %>%
        select(Squad = squad.name, Value = value, Rank = display.rank) %>%
        arrange(desc(Value)),
      pagination = FALSE,
      defaultSorted = "Value",
      defaultSortOrder = "desc",
      columns = list(
        Squad = colDef(name = "Squad"),
        Value = colDef(name = "Points"),
        Rank = colDef(name = "Rank")
      )
    )
  })
}

shinyApp(ui = ui, server = server)
