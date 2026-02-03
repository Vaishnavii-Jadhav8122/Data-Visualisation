library(readxl)     # Read Excel files (.xlsx)
library(dplyr)      # Data manipulation (filter, select, mutate)
library(tidyr)      # Data reshaping (pivot_longer)
library(ggplot2)    # Data visualisation
library(scales)     # Axis and label formatting
library(viridis)   # Colour-blind friendly colour palettes
library(patchwork) # Combine multiple ggplots into one composite figure
library(janitor)   # Clean and standardise column names


# ===============================
# Set a global theme for all plots
# Ensures visual consistency across figures
# ===============================

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
)


# ===============================
# Import datasets from Excel file
# ===============================

# Annual aggregated data (used for Figures 1, 2 and 4)
annual <- read_excel(
  "C:/Users/User/Downloads/Delay_TrainClaim.xlsx",
  sheet = "Annual_Data"
)

# Periodic (shorter reporting period) data (used for Figure 3)
periodic <- read_excel(
  "C:/Users/User/Downloads/Delay_TrainClaim.xlsx",
  sheet = "Periodic_Data"
)


# ===============================
# Clean column names for consistency
# ===============================

annual <- clean_names(annual)
periodic <- clean_names(periodic)


# ===============================
# Create readable labels for train operators
# Used to improve axis readability in plots
# ===============================

operator_labels <- c(
  "great_britain" = "Great Britain",
  "avanti_west_coast" = "Avanti WC",
  "great_western_railway" = "GWR",
  "govia_thameslink_railway" = "GTR",
  "south_western_railway" = "SWR",
  "london_north_eastern_railway" = "LNER",
  "southeastern" = "Southeastern",
  "west_midlands_trains" = "West Midlands",
  "greater_anglia" = "Greater Anglia",
  "northern_trains" = "Northern",
  "east_midlands_railway" = "East Midlands",
  "trans_pennine_express" = "TPE",
  "cross_country" = "CrossCountry",
  "tfw_rail" = "TfW Rail",
  "scotrail" = "ScotRail",
  "chiltern_railways" = "Chiltern",
  "c2c" = "c2c",
  "lumo_note_2" = "Lumo",
  "hull_trains_note_1" = "Hull Trains",
  "grand_central_note_1" = "Grand Central",
  "heathrow_express" = "Heathrow Ex.",
  "london_overground_note_5" = "London Overground",
  "elizabeth_line_note_3_note5" = "Elizabeth Line",
  "caledonian_sleeper_note_1" = "Caledonian Sleeper",
  "merseyrail" = "Merseyrail"
)


# ===============================
# Identify most recent year in dataset
# ===============================

latest_year <- max(annual$time_period)


# ===============================
# Figure 1: National trend of delay compensation claims
# ===============================

# Filter data for Great Britain total claims
gb_received <- annual %>%
  filter(delay_compensation == "Volume of claims received within period") %>%
  select(time_period, great_britain)

# Create line chart showing annual trend
fig1 <- ggplot(gb_received,
               aes(x = time_period, y = great_britain, group = 1)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 2.5) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title = "Delay Compensation Claims Across Great Britain",
    subtitle = "Annual trend in total claims received",
    x = "Financial Year",
    y = "Number of Claims"
  )

fig1


# ===============================
# Figure 2: Claims by train operator (latest year)
# ===============================

# Prepare operator-level data for the most recent year
fig2_data <- annual %>%
  filter(
    time_period == latest_year,
    delay_compensation == "Volume of claims received within period"
  ) %>%
  select(where(is.numeric)) %>%        # Keep numeric operator columns only
  pivot_longer(
    cols = everything(),
    names_to = "operator",
    values_to = "claims"
  ) %>%
  filter(!is.na(claims))

# Create horizontal bar chart
fig2 <- ggplot(fig2_data,
               aes(x = reorder(operator, claims),
                   y = claims,
                   fill = claims)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(labels = operator_labels) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis(
    option = "C",
    direction = -1,
    labels = scales::comma,
    name = "Number of Claims"
  ) +
  labs(
    title = paste("Delay Compensation Claims by Train Operator (", latest_year, ")", sep = ""),
    subtitle = "Substantial inequality across operators",
    x = "Train Operator",
    y = "Number of Claims"
  )

fig2


# ===============================
# Figure 3: Distribution of periodic claims by operator
# ===============================

# Prepare periodic claims data
fig3_data <- periodic %>%
  filter(delay_compensation == "Volume of claims received within period") %>%
  select(where(is.numeric)) %>%        # Keep numeric operator columns only
  pivot_longer(
    cols = everything(),
    names_to = "operator",
    values_to = "claims"
  ) %>%
  filter(!is.na(claims))

# Create boxplot to show variability and outliers
fig3 <- ggplot(fig3_data,
               aes(x = operator, y = claims)) +
  geom_boxplot(
    fill = "#56B4E9",
    alpha = 0.7,
    outlier.color = "red"
  ) +
  coord_flip() +
  scale_x_discrete(labels = operator_labels) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Periodic Delay Compensation Claims",
    subtitle = "Extreme values highlight uneven passenger burden",
    x = "Train Operator",
    y = "Claims per Reporting Period"
  )

fig3


# ===============================
# Figure 4: Claim volume vs processing efficiency
# ===============================

# Prepare claims volume data
claims_data <- annual %>%
  filter(delay_compensation == "Volume of claims received within period") %>%
  select(time_period, where(is.numeric)) %>%
  pivot_longer(
    cols = -time_period,
    names_to = "operator",
    values_to = "claims"
  )

# Prepare processing efficiency data
efficiency_data <- annual %>%
  filter(delay_compensation == "Percentage closed within 20 working days") %>%
  select(time_period, where(is.numeric)) %>%
  pivot_longer(
    cols = -time_period,
    names_to = "operator",
    values_to = "closed_pct"
  )

# Merge claim volume and efficiency data
fig4_data <- left_join(
  claims_data,
  efficiency_data,
  by = c("time_period", "operator")
) %>%
  filter(operator != "great_britain")  # Remove national aggregate

# Create scatter plot
fig4 <- ggplot(fig4_data,
               aes(x = claims, y = closed_pct)) +
  geom_point(
    color = "#009E73",
    alpha = 0.6,
    size = 2
  ) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Claim Volume vs Processing Efficiency",
    subtitle = "Higher volume does not necessarily imply lower efficiency",
    x = "Number of Claims",
    y = "Percentage Closed Within 20 Working Days"
  )

fig4


# ===============================
# Combine all figures into a composite visualisation
# ===============================

composite_plot <- (fig1 | fig2) / (fig3 | fig4)
composite_plot

