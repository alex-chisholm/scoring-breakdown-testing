library(plotly)
library(dplyr)
library(tidyr)

squad.stats.for <- read_csv("squad.stats.for.csv")
zone.stats <- readRDS("zone_stats.RDS")

# INTERCEPT SCORING EFFICIENCY ANALYSIS
# ===========================================

# Define team logos
team_logos <- c(
  "Adelaide Crows" = "https://i.postimg.cc/pTn8Nn1b/Adelaide-Disc.png",
  "Brisbane Lions" = "https://i.postimg.cc/2jhv2hZn/Brisbane-Disc.png",
  "Carlton" = "https://i.postimg.cc/2yQZyND0/Carlton-Disc.png",
  "Collingwood" = "https://i.postimg.cc/TYxbS1VF/Collingwood-Disc.png",
  "Essendon" = "https://i.postimg.cc/c4c3NWTH/Essendon-Disc.png",
  "Fremantle" = "https://i.postimg.cc/fRT9zPgv/Fremantle-Disc.png",
  "Geelong Cats" = "https://i.postimg.cc/hty7f0kR/Geelong-Disc.png",
  "Gold Coast SUNS" = "https://i.postimg.cc/tgsnPxbZ/Gold-Coast-Disc.png",
  "GWS GIANTS" = "https://i.postimg.cc/d0gk9VQW/GWS-Disc.png",
  "Hawthorn" = "https://i.postimg.cc/brhZKVtk/Hawthorn-Disc.png",
  "Melbourne" = "https://i.postimg.cc/50vHKW6f/Melbourne-Disc.png",
  "North Melbourne" = "https://i.postimg.cc/8cm7DdR3/North-Melbourne-Disc.png",
  "Port Adelaide" = "https://i.postimg.cc/fWctxsGK/Port-Adelaide-Disc.png",
  "Richmond" = "https://i.postimg.cc/c4dKhjZN/Richmond-Disc.png",
  "St Kilda" = "https://i.postimg.cc/qMQN5LM6/St-Kilda-Disc.png",
  "Sydney Swans" = "https://i.postimg.cc/x1CXDzcx/Sydney-Disc.png",
  "Western Bulldogs" = "https://i.postimg.cc/xdNqqVGL/Western-Bulldogs-Disc.png",
  "West Coast Eagles" = "https://i.postimg.cc/L8bnH8PH/West-Coast-Disc.png"
)

# Team colors
get_team_color <- function(squad_name) {
  case_when(
    squad_name == "Adelaide Crows" ~ "#0F1432",
    squad_name == "Brisbane Lions" ~ "#A30046",
    squad_name == "Carlton" ~ "#031A29",
    squad_name == "Collingwood" ~ "#000000",
    squad_name == "Essendon" ~ "#CC2031",
    squad_name == "Fremantle" ~ "#2A0D54",
    squad_name == "Geelong Cats" ~ "#002B5C",
    squad_name == "Gold Coast SUNS" ~ "#E02112",
    squad_name == "GWS GIANTS" ~ "#F47920",
    squad_name == "Hawthorn" ~ "#4D2004",
    squad_name == "Melbourne" ~ "#0F1131",
    squad_name == "North Melbourne" ~ "#1A3B8E",
    squad_name == "Port Adelaide" ~ "#008AAB",
    squad_name == "Richmond" ~ "#FFD200",
    squad_name == "St Kilda" ~ "#ED1B2F",
    squad_name == "Sydney Swans" ~ "#E1251B",
    squad_name == "Western Bulldogs" ~ "#20539D",
    squad_name == "West Coast Eagles" ~ "#003087",
    TRUE ~ "#808080"
  )
}

# Add ordinal suffix to rankings
add_ordinal_suffix <- function(x) {
  suffix <- ifelse(x %% 100 %in% 11:13, "th",
                   ifelse(x %% 10 == 1, "st",
                          ifelse(x %% 10 == 2, "nd",
                                 ifelse(x %% 10 == 3, "rd", "th"))))
  paste0(x, suffix)
}

# Process intercept data for scoring (FOR)
process_intercept_stats <- function(df, zone) {
  df %>%
    filter(name %in% c("Intercept", "Intercept Score Launch")) %>%
    mutate(stat = case_when(
      name == "Intercept" ~ paste0(zone, ".int"),
      name == "Intercept Score Launch" ~ paste0(zone, ".int.scores")
    )) %>%
    group_by(squad.name, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
}

# Process intercept data for defense (AGAINST)
process_intercept_stats_against <- function(df, zone) {
  df %>%
    filter(name %in% c("Intercept", "Intercept Score Launch")) %>%
    mutate(stat = case_when(
      name == "Intercept" ~ paste0(zone, ".int.against"),
      name == "Intercept Score Launch" ~ paste0(zone, ".int.scores.against")
    )) %>%
    group_by(opp.squad, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
}

# Calculate Intercept Scoring Efficiency
calculate_ise <- function(zone_stats) {
  # Expected intercept scoring rates by zone
  expected_rates <- c(
    D50 = 0.096,
    DM = 0.186,
    AM = 0.284,
    F50 = 0.605
  )
  
  # Process FOR data
  d50_summary <- process_intercept_stats(zone_stats$D50$for.data, "D50")
  dm_summary <- process_intercept_stats(zone_stats$DM$for.data, "DM")
  am_summary <- process_intercept_stats(zone_stats$AM$for.data, "AM")
  f50_summary <- process_intercept_stats(zone_stats$F50$for.data, "F50")
  
  # Combine and pivot FOR data
  all_summaries <- bind_rows(d50_summary, dm_summary, am_summary, f50_summary)
  final_df <- all_summaries %>%
    pivot_wider(names_from = stat, values_from = total, values_fill = 0)
  
  # Compute ISE
  ise_df <- final_df %>%
    mutate(
      xSC = (D50.int * expected_rates["D50"]) +
        (DM.int * expected_rates["DM"]) +
        (AM.int * expected_rates["AM"]) +
        (F50.int * expected_rates["F50"]),
      
      aSC = D50.int.scores + DM.int.scores + AM.int.scores + F50.int.scores,
      
      ISE = round(ifelse(xSC > 0, aSC / xSC, NA), 3)
    ) %>%
    select(squad.name, ISE) %>%
    arrange(desc(ISE)) %>%
    mutate(
      rank = row_number(),
      display.value = paste0((ISE * 100), "%"),
      display.rank = add_ordinal_suffix(rank)
    )
  
  # Process AGAINST data
  d50_summary_against <- process_intercept_stats_against(zone_stats$D50$for.data, "D50")
  dm_summary_against <- process_intercept_stats_against(zone_stats$DM$for.data, "DM")
  am_summary_against <- process_intercept_stats_against(zone_stats$AM$for.data, "AM")
  f50_summary_against <- process_intercept_stats_against(zone_stats$F50$for.data, "F50")
  
  # Combine and pivot AGAINST data
  all_summaries_against <- bind_rows(d50_summary_against, dm_summary_against, 
                                     am_summary_against, f50_summary_against)
  final_df_against <- all_summaries_against %>%
    pivot_wider(names_from = stat, values_from = total, values_fill = 0)
  
  # Compute ISEA
  ise_against_df <- final_df_against %>%
    mutate(
      xSC_against = (D50.int.against * expected_rates["D50"]) +
        (DM.int.against * expected_rates["DM"]) +
        (AM.int.against * expected_rates["AM"]) +
        (F50.int.against * expected_rates["F50"]),
      
      aSC_against = D50.int.scores.against + DM.int.scores.against +
        AM.int.scores.against + F50.int.scores.against,
      
      ISEA = round(ifelse(xSC_against > 0, aSC_against / xSC_against, NA), 3)
    ) %>%
    select(squad.name = opp.squad, ISEA) %>%
    arrange(ISEA) %>%
    mutate(
      rank = row_number(),
      display.value = paste0((ISEA * 100), "%"),
      display.rank = add_ordinal_suffix(rank)
    )
  
  # Combine ISE and ISEA
  ise_combined <- ise_df %>%
    inner_join(ise_against_df, by = "squad.name") %>%
    mutate(box.colour = get_team_color(squad.name))
  
  return(ise_combined)
}

# Create intercept efficiency plot
create_intercept_plot <- function(ise_combined) {
  # Fixed axis ranges for consistent quadrants
  x_range <- c(0.7, 1.3)  # ISE range
  y_range <- c(1.3, 0.7)  # ISEA range (reversed for better=lower)
  
  p <- plot_ly(
    data = ise_combined,
    x = ~ISE,
    y = ~ISEA,
    type = "scatter",
    mode = "markers",
    text = ~paste(
      "<br>", squad.name, "<br>",
      "Scoring Rank: ", ise_combined$display.rank.x, "<br>",
      "Defending Rank: ", ise_combined$display.rank.y
    ),
    hoverinfo = "text",
    marker = list(size = 40, opacity = 0)
  )
  
  # Add team logos and layout
  p <- p %>%
    layout(
      images = lapply(1:nrow(ise_combined), function(i) {
        list(
          source = team_logos[ise_combined$squad.name[i]],
          x = ise_combined$ISE[i],
          y = ise_combined$ISEA[i],
          xref = "x",
          yref = "y",
          sizex = 0.03,
          sizey = 0.03,
          xanchor = "middle",
          yanchor = "middle",
          layer = "above"
        )
      }),
      shapes = list(
        # Vertical line at ISE = 1
        list(
          type = "line", 
          x0 = 1, x1 = 1, 
          y0 = y_range[1], y1 = y_range[2],
          line = list(dash = "dot", color = "#000000", width = 1.5),
          layer = "below"
        ),
        # Horizontal line at ISEA = 1
        list(
          type = "line", 
          x0 = x_range[1], x1 = x_range[2], 
          y0 = 1, y1 = 1,
          line = list(dash = "dot", color = "#000000", width = 1.5),
          layer = "below"
        )
      ),
      annotations = list(
        # Top-left: Score poorly, Defend poorly
        list(
          x = x_range[1] + 0.05, y = y_range[1] - 0.05,
          xref = "x", yref = "y",
          text = "<b>POOR OFFENSE</b><br><b>POOR DEFENSE</b>",
          showarrow = FALSE,
          font = list(size = 12, color = "white", family = "Arial"),
          align = "center",
          bgcolor = "rgba(203, 68, 74, 0.9)",
          bordercolor = "rgba(255, 255, 255, 0.8)",
          borderwidth = 1,
          borderradius = 8,
          pad = list(t = 8, b = 8, l = 12, r = 12),
          xanchor = "center",
          yanchor = "center"
        ),
        # Bottom-right: Score well, Defend well
        list(
          x = x_range[2] - 0.05, y = y_range[2] + 0.05,
          xref = "x", yref = "y",
          text = "<b>STRONG OFFENSE</b><br><b>STRONG DEFENSE</b>",
          showarrow = FALSE,
          font = list(size = 12, color = "white", family = "Arial"),
          align = "center",
          bgcolor = "rgba(76, 132, 82, 0.9)",
          bordercolor = "rgba(255, 255, 255, 0.8)",
          borderwidth = 1,
          borderradius = 8,
          pad = list(t = 8, b = 8, l = 12, r = 12),
          xanchor = "center",
          yanchor = "center"
        ),
        # Top-right: Score well, Defend poorly
        list(
          x = x_range[2] - 0.05, y = y_range[1] - 0.05,
          xref = "x", yref = "y",
          text = "<b>STRONG OFFENSE</b><br><b>POOR DEFENSE</b>",
          showarrow = FALSE,
          font = list(size = 12, color = "white", family = "Arial"),
          align = "center",
          bgcolor = "rgba(255, 165, 0, 0.9)",
          bordercolor = "rgba(255, 255, 255, 0.8)",
          borderwidth = 1,
          borderradius = 8,
          pad = list(t = 8, b = 8, l = 12, r = 12),
          xanchor = "center",
          yanchor = "center"
        ),
        # Bottom-left: Score poorly, Defend well
        list(
          x = x_range[1] + 0.05, y = y_range[2] + 0.05,
          xref = "x", yref = "y",
          text = "<b>POOR OFFENSE</b><br><b>STRONG DEFENSE</b>",
          showarrow = FALSE,
          font = list(size = 12, color = "white", family = "Arial"),
          align = "center",
          bgcolor = "rgba(255, 165, 0, 0.9)",
          bordercolor = "rgba(255, 255, 255, 0.8)",
          borderwidth = 1,
          borderradius = 8,
          pad = list(t = 8, b = 8, l = 12, r = 12),
          xanchor = "center",
          yanchor = "center"
        )
      ),
      hoverlabel = list(
        bgcolor = ~box.colour,
        font = list(family = "Arial", size = 12, color = "white"),
        bordercolor = "white",
        borderwidth = 1
      ),
      xaxis = list(
        title = "Intercept Scoring Efficiency (ISE)",
        range = x_range,
        tickfont = list(size = 11, family = "Arial", color = "#666666"),
        titlefont = list(size = 13, family = "Arial", color = "#333333"),
        ticks = "",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "rgba(204, 204, 204, 0.3)",
        gridwidth = 1,
        griddash = "dot",
        tickmode = "linear",
        dtick = 0.1
      ),
      yaxis = list(
        title = "Intercept Defensive Efficiency (IDE)",
        range = y_range,
        autorange = FALSE,
        tickfont = list(size = 11, family = "Arial", color = "#666666"),
        titlefont = list(size = 13, family = "Arial", color = "#333333"),
        ticks = "",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "rgba(204, 204, 204, 0.3)",
        gridwidth = 1,
        griddash = "dot",
        tickmode = "linear",
        dtick = 0.1
      ),
      plot_bgcolor = "#F8F8F8",
      paper_bgcolor = "#F8F8F8",
      margin = list(l = 80, r = 80, t = 50, b = 80)
    )
  
  return(p)
}

# Main function to run intercept analysis
# Usage: ise_plot <- run_intercept_analysis(zone_stats)
run_intercept_analysis <- function(zone_stats) {
  ise_combined <- calculate_ise(zone_stats)
  intercept_plot <- create_intercept_plot(ise_combined)
  
  return(list(
    data = ise_combined,
    plot = intercept_plot
  ))
}

# Then run the analysis with your zone_stats data
intercept_results <- run_intercept_analysis(zone.stats)

# Display the plot
intercept_results$plot
