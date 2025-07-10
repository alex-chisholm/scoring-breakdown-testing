library(plotly)
library(dplyr)
library(tidyr)

# squad.stats.for <- read_csv("squad.stats.for.csv")
# zone.stats <- readRDS("zone_stats.RDS")

# STOPPAGE SCORING EFFICIENCY ANALYSIS
# ====================================

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

# Process stoppage data for scoring (FOR)
process_squad_stats <- function(df, zone) {
  df %>%
    filter(name %in% c("Ball Up Clearance", "Throw In Clearance", 
                       "Ball Up Score Launch", "Throw In Score Launch")) %>%
    mutate(stat = case_when(
      name == "Ball Up Clearance" ~ paste0(zone, ".bu.clr"),
      name == "Throw In Clearance" ~ paste0(zone, ".ti.clr"),
      name == "Ball Up Score Launch" ~ paste0(zone, ".bu.clr.scores"),
      name == "Throw In Score Launch" ~ paste0(zone, ".ti.clr.scores")
    )) %>%
    group_by(squad.name, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
}

# Process stoppage data for defense (AGAINST)
process_squad_stats_against <- function(df, zone) {
  df %>%
    filter(name %in% c("Ball Up Clearance", "Throw In Clearance", 
                       "Ball Up Score Launch", "Throw In Score Launch")) %>%
    mutate(stat = case_when(
      name == "Ball Up Clearance" ~ paste0(zone, ".bu.clr.against"),
      name == "Throw In Clearance" ~ paste0(zone, ".ti.clr.against"),
      name == "Ball Up Score Launch" ~ paste0(zone, ".bu.clr.scores.against"),
      name == "Throw In Score Launch" ~ paste0(zone, ".ti.clr.scores.against")
    )) %>%
    group_by(opp.squad, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
}

# Calculate Stoppage Scoring Efficiency
calculate_sse <- function(zone_stats, squad_stats_for) {
  # Expected stoppage scoring rates
  expected_rates <- c(
    D50.bu = 0.075,
    D50.ti = 0.065,
    DM.bu = 0.157,
    DM.ti = 0.142,
    AM.bu = 0.255,
    AM.ti = 0.269,
    F50.bu = 0.577,
    F50.ti = 0.614,
    CB = 0.251
  )
  
  # Process FOR data from each zone
  d50_summary <- process_squad_stats(zone_stats$D50$for.data, "D50")
  dm_summary <- process_squad_stats(zone_stats$DM$for.data, "DM")
  am_summary <- process_squad_stats(zone_stats$AM$for.data, "AM")
  f50_summary <- process_squad_stats(zone_stats$F50$for.data, "F50")
  
  # Process centre bounce data separately
  cb_summary <- squad_stats_for %>%
    filter(name %in% c("Centre Bounce Clearance", "Centre Bounce Score Launch")) %>%
    mutate(stat = case_when(
      name == "Centre Bounce Clearance" ~ "CB.clr",
      name == "Centre Bounce Score Launch" ~ "CB.clr.scores"
    )) %>%
    group_by(squad.name, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Combine all summaries and pivot
  all_summaries <- bind_rows(d50_summary, dm_summary, am_summary, f50_summary, cb_summary)
  final_df <- all_summaries %>%
    pivot_wider(names_from = stat, values_from = total, values_fill = 0)
  
  # Compute SSE
  sse_df <- final_df %>%
    mutate(
      xSC = (D50.bu.clr * expected_rates["D50.bu"]) +
        (D50.ti.clr * expected_rates["D50.ti"]) +
        (DM.bu.clr * expected_rates["DM.bu"]) +
        (DM.ti.clr * expected_rates["DM.ti"]) +
        (AM.bu.clr * expected_rates["AM.bu"]) +
        (AM.ti.clr * expected_rates["AM.ti"]) +
        (F50.bu.clr * expected_rates["F50.bu"]) +
        (F50.ti.clr * expected_rates["F50.ti"]) +
        (CB.clr * expected_rates["CB"]),
      
      aSC = D50.bu.clr.scores + D50.ti.clr.scores +
        DM.bu.clr.scores + DM.ti.clr.scores +
        AM.bu.clr.scores + AM.ti.clr.scores +
        F50.bu.clr.scores + F50.ti.clr.scores +
        CB.clr.scores,
      
      SSE = round(ifelse(xSC > 0, aSC / xSC, NA), 3)
    ) %>%
    select(squad.name, SSE) %>%
    arrange(desc(SSE)) %>%
    mutate(
      rank = row_number(),
      display.value = paste0((SSE * 100), "%"),
      display.rank = add_ordinal_suffix(rank)
    )
  
  # Process AGAINST data
  d50_summary_against <- process_squad_stats_against(zone_stats$D50$for.data, "D50")
  dm_summary_against <- process_squad_stats_against(zone_stats$DM$for.data, "DM")
  am_summary_against <- process_squad_stats_against(zone_stats$AM$for.data, "AM")
  f50_summary_against <- process_squad_stats_against(zone_stats$F50$for.data, "F50")
  
  # Process centre bounce against data
  cb_summary_against <- squad_stats_for %>%
    filter(name %in% c("Centre Bounce Clearance", "Centre Bounce Score Launch")) %>%
    mutate(stat = case_when(
      name == "Centre Bounce Clearance" ~ "CB.clr.against",
      name == "Centre Bounce Score Launch" ~ "CB.clr.scores.against"
    )) %>%
    group_by(opp.squad, stat) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Combine and pivot AGAINST data
  all_summaries_against <- bind_rows(d50_summary_against, dm_summary_against,
                                     am_summary_against, f50_summary_against, cb_summary_against)
  final_df_against <- all_summaries_against %>%
    pivot_wider(names_from = stat, values_from = total, values_fill = 0)
  
  # Compute SSEA
  sse_against_df <- final_df_against %>%
    mutate(
      xSC_against = (D50.bu.clr.against * expected_rates["D50.bu"]) +
        (D50.ti.clr.against * expected_rates["D50.ti"]) +
        (DM.bu.clr.against * expected_rates["DM.bu"]) +
        (DM.ti.clr.against * expected_rates["DM.ti"]) +
        (AM.bu.clr.against * expected_rates["AM.bu"]) +
        (AM.ti.clr.against * expected_rates["AM.ti"]) +
        (F50.bu.clr.against * expected_rates["F50.bu"]) +
        (F50.ti.clr.against * expected_rates["F50.ti"]) +
        (CB.clr.against * expected_rates["CB"]),
      
      aSC_against = D50.bu.clr.scores.against + D50.ti.clr.scores.against +
        DM.bu.clr.scores.against + DM.ti.clr.scores.against +
        AM.bu.clr.scores.against + AM.ti.clr.scores.against +
        F50.bu.clr.scores.against + F50.ti.clr.scores.against +
        CB.clr.scores.against,
      
      SSEA = round(ifelse(xSC_against > 0, aSC_against / xSC_against, NA), 3)
    ) %>%
    select(squad.name = opp.squad, SSEA) %>%
    arrange(SSEA) %>%
    mutate(
      rank = row_number(),
      display.value = paste0((SSEA * 100), "%"),
      display.rank = add_ordinal_suffix(rank)
    )
  
  # Combine SSE and SSEA
  sse_combined <- sse_df %>%
    inner_join(sse_against_df, by = "squad.name") %>%
    mutate(box.colour = get_team_color(squad.name))
  
  return(sse_combined)
}

# Create stoppage efficiency plot
create_stoppage_plot <- function(sse_combined) {
  # Fixed axis ranges for consistent quadrants
  x_range <- c(0.7, 1.3)  # SSE range
  y_range <- c(1.3, 0.7)  # SSEA range (reversed for better=lower)
  
  p <- plot_ly(
    data = sse_combined,
    x = ~SSE,
    y = ~SSEA,
    type = "scatter",
    mode = "markers",
    text = ~paste(
      "<br>", squad.name, "<br>",
      "Scoring Rank: ", sse_combined$display.rank.x, "<br>",
      "Defending Rank: ", sse_combined$display.rank.y
    ),
    hoverinfo = "text",
    marker = list(size = 40, opacity = 0)
  )
  
  # Add team logos and layout
  p <- p %>%
    layout(
      images = lapply(1:nrow(sse_combined), function(i) {
        list(
          source = team_logos[sse_combined$squad.name[i]],
          x = sse_combined$SSE[i],
          y = sse_combined$SSEA[i],
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
        # Vertical line at SSE = 1
        list(
          type = "line",
          x0 = 1, x1 = 1,
          y0 = y_range[1], y1 = y_range[2],
          line = list(dash = "dot", color = "#000000", width = 1.5),
          layer = "below"
        ),
        # Horizontal line at SSEA = 1
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
        title = "Stoppage Scoring Efficiency (SSE)",
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
        title = "Stoppage Defensive Efficiency (SDE)",
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

# Main function to run stoppage analysis
# Usage: sse_plot <- run_stoppage_analysis(zone_stats, squad_stats_for)
run_stoppage_analysis <- function(zone_stats, squad_stats_for) {
  sse_combined <- calculate_sse(zone_stats, squad_stats_for)
  stoppage_plot <- create_stoppage_plot(sse_combined)
  
  return(list(
    data = sse_combined,
    plot = stoppage_plot
  ))
}

# Then run the analysis with your zone_stats data
# stoppage_results <- run_stoppage_analysis(zone.stats, squad.stats.for)

# Display the plot
# stoppage_results$plot
