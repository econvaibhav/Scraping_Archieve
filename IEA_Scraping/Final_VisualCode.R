library(readxl)
library(patchwork)
library(tidyverse)
library(scales)
library(treemapify)

energy <- read_excel("Projects/mani-work/mani-data/All_Energy_Indicators_Combined.xlsx")

df_long <- energy %>%
  rename(Country = `Country Name`, Indicator = `Indicator Name`) %>%
  filter(!(Country %in% c("Bangladesh", "Bhutan"))) %>% 
  mutate(across(matches("^(19|20)"), as.character)) %>%
  pivot_longer(cols = matches("^(19|20)"), names_to = "Year", values_to = "Value") %>%
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(Value)
  )

gap_data <- df_long %>%
  filter(Indicator %in% c("AnnualCO2 Emissions", 
                          "CO2 Emissions Per Capita (tonnes per person)")) %>%
  select(Country, Year, Indicator, Value) %>% 
  pivot_wider(names_from = Indicator, values_from = Value) 

colnames(gap_data) <- c("Country",                              
                        "Year",                                         
                        "AnnualCo2",
                        "C02Percap")


theme_dis <- theme(
  axis.text.x = element_text(face = 'bold', size = 10),
  axis.title.x = element_text(color = "black", size = 20, face = "bold"),
  axis.text.y = element_text(face = 'bold', size = 10),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  plot.title = element_text(color = "black", size = 14, hjust = 0.5))


make_plot <- function(target_year, column_name) {
  is_annual <- column_name == "AnnualCo2"
  
  gap_data %>%
    filter(Year == target_year) %>%
    slice_max(!!sym(column_name), n = 10) %>% 
    ggplot(aes(x = !!sym(column_name), y = reorder(Country, !!sym(column_name)))) +
    geom_col(fill = "grey30") +
    # Added Black Labels outside the bars
    geom_text(
      aes(label = if(is_annual) {
        paste0(round(!!sym(column_name) / 1e6, 1), "M")
      } else {
        round(!!sym(column_name), 2)
      }),
      hjust = -0.2,         # Moves text to the right, outside the bar
      vjust = 0.5,          # Centers text vertically with the bar
      color = "black",      # Changed to black for visibility
      fontface = "bold",
      size = 3.2           
    ) +
    scale_x_continuous(
      labels = if(is_annual) label_number(scale = 1e-6, suffix = "M") else label_number(),
      expand = expansion(mult = c(0, 0.15)) # Adds 15% extra space to the right for labels
    ) +
    labs(subtitle = target_year, x = NULL, y = NULL) +
    theme_bw() + 
    theme(
      axis.text.x   = element_text(face = 'bold'),
      axis.title.x  = element_text(color = "black", size = 10, face = "bold"),
      plot.subtitle = element_text(color = 'black', size = 16, hjust = 0.5),
      axis.title.y  = element_text(color = "black", size = 10, face = "bold"),
      plot.title    = element_text(color = "black", size = 20, hjust = 0.5),
      plot.caption  = element_text(face = "italic"),
      legend.position = 'bottom',
      panel.grid.minor = element_blank() # Cleans up the background
    ) 
}


p1 <- make_plot(2000, "AnnualCo2")
p2 <- make_plot(2015, "AnnualCo2")
p3 <- make_plot(2023, "AnnualCo2")
p4 <- make_plot(2000, "C02Percap")
p5 <- make_plot(2015, "C02Percap")
p6 <- make_plot(2023, "C02Percap")



row1_wrapped <- wrap_elements((p1 + p2 + p3) + 
                                plot_annotation(title = "Annual CO2 Emissions (Millions of Tonnes)") & 
                                theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)))


row2_wrapped <- wrap_elements(
  (p4 + p5 + p6) +
    plot_annotation(title = "CO2 Emissions Per Capita (Tonnes per person)") & 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)))


print(row1_wrapped / row2_wrapped)

##########################################################################
##########################################################################
gap_data <- df_long %>%
  filter(Indicator %in% c("Access to electricity, urban (% of urban population)", 
                          "Access to electricity, rural (% of rural population)",
                          "Access to electricity (% of population)")) %>%
  select(Country, Year, Indicator, Value) %>% 
  pivot_wider(names_from = Indicator, values_from = Value)


colnames(gap_data) <- c("Country","Year","Total","Urban","Rural")

gap_data_filtered <- gap_data %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(
    Rural = na.approx(Rural, na.rm = FALSE),
    Urban = na.approx(Urban, na.rm = FALSE),
    Total = na.approx(Total, na.rm = FALSE)) %>%
  filter(Year >= min(Year[!is.na(Rural)])) %>%
  ungroup()

ggplot(gap_data_filtered, aes(x = Year)) +
  geom_ribbon(aes(ymin = 0, ymax = Rural), fill = "#A4B1BA", alpha = 0.6) +
  geom_ribbon(aes(ymin = Rural, ymax = Urban), fill = "#D7A49A", alpha = 0.6) +
  geom_line(aes(y = Urban, color = "Urban"), linewidth = 1) +
  geom_line(aes(y = Total, color = "Total"), linewidth = 1, linetype = 1) +
  geom_line(aes(y = Rural, color = "Rural"), linewidth = 1, linetype = 1) +
  facet_wrap(~Country, scales = "free_x", ncol = 4) + 
  scale_x_continuous(breaks = seq(1800, 2030, by = 5)) +
  scale_color_manual(values = c("Urban" = "#D7A49A", "Rural" = "#A4B1BA", "Total" = "black")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Energy Access: Urban vs. Rural",
    subtitle = "Shaded area represents the gap in urban and rural electrification",
    y = "% Access", x = "Year", color = "Sector",
    caption = "Note: NA values replaced using averages from the previous and future years."
  ) + 
  theme_bw() + 
  theme(
    axis.text.x   = element_text(face = 'bold'),
    axis.title.x  = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = 'black', size = 8, hjust = 0.5),
    axis.title.y  = element_text(color = "black", size = 10, face = "bold"),
    plot.title    = element_text(color = "black", size = 12, hjust = 0.5),
    legend.position = 'bottom'
  )

##########################################################################
##########################################################################

bp <- "/home/vaibhavagarwal/Projects/mani-work/mani-data/All_Country_IEA"

energy_colors <- c(
  "Solar PV" = "#a1d99b",
  "Wind" = "#c7e9c0",
  "Hydropower" = "#7fcdbb",
  "Biofuels" = "#d9f0a3",
  "Waste" = "#e7f598",
  "Geothermal" = "#41b6c4",
  "Coal" = "#fc9272",
  "Oil" = "#fcbba1",
  "Natural gas" = "#fee0d2",
  "Nuclear" = "#bdbdbd"
)

# Uganda not present in the dataset I have currently. PLease add and test code. Good way to test as well :)

kenya <- read_csv(file.path(bp, "Kenya/International Energy Agency - renewable electricity generation by source non-combustible in Kenya.csv")) %>% mutate(Country = "Kenya")
india <- read_csv(file.path(bp, "India/International Energy Agency - renewable electricity generation by source non-combustible in India.csv")) %>% mutate(Country = "India")
ethiopia <- read_csv(file.path(bp, "Ethiopia/International Energy Agency - renewable electricity generation by source non-combustible in Ethiopia.csv")) %>% mutate(Country = "Ethiopia")
bangladesh <- read_csv(file.path(bp, "Bangladesh/International Energy Agency - renewable electricity generation by source non-combustible in Bangladesh.csv")) %>% mutate(Country = "Bangladesh")
ghana <- read_csv(file.path(bp, "Ghana/International Energy Agency - renewable electricity generation by source non-combustible in Ghana.csv")) %>% mutate(Country = "Ghana")
indonesia <- read_csv(file.path(bp, "Indonesia/International Energy Agency - renewable electricity generation by source non-combustible in Indonesia.csv")) %>% mutate(Country = "Indonesia")
peru <- read_csv(file.path(bp, "Peru/International Energy Agency - renewable electricity generation by source non-combustible in Peru.csv")) %>% mutate(Country = "Peru")
namibia <- read_csv(file.path(bp, "Namibia/International Energy Agency - renewable electricity generation by source non-combustible in Namibia.csv")) %>% mutate(Country = "Namibia")
burkina <- read_csv(file.path(bp, "Burkina_Faso/International Energy Agency - renewable electricity generation by source non-combustible in Burkina Faso.csv")) %>% mutate(Country = "Burkina Faso")
south_africa <- read_csv(file.path(bp, "South_Africa/International Energy Agency - renewable electricity generation by source non-combustible in South Africa.csv")) %>% mutate(Country = "South Africa")
# uganda <- read_csv(file.path(bp, "Uganda/International Energy Agency - renewable electricity generation by source non-combustible in Uganda.csv")) %>% mutate(Country = "Uganda")

list_of_dfs <- list(kenya, india, ethiopia, bangladesh, ghana, indonesia, peru, namibia, burkina, south_africa)

ren_energy_data <- bind_rows(lapply(list_of_dfs, function(df) {
  colnames(df)[1] <- "Source"
  return(df)
}))

# India over time
make_india_time_treemap <- function(target_year) {
  plot_data <- ren_energy_data %>%
    filter(Country == "India", Year == target_year, Value > 0) %>%
    mutate(perc = (Value / sum(Value)) * 100)
  
  if(nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(area = Value, fill = Source, 
                        label = paste0(Source, "\n", Value, " GWh\n(", round(perc, 1), "%)"))) +
    geom_treemap(color = "white", size = 2) + 
    geom_treemap_text(colour = "black", place = "centre", grow = TRUE, reflow = TRUE, 
                      size = 8, min.size = 2) + # Reduced font constraints
    scale_fill_manual(values = energy_colors) + 
    labs(title = paste("India -", target_year)) +
    theme_bw() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 10), # Reduced subtitle size
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
}

# All country over time 
make_country_2023_treemap <- function(country_name) {
  plot_data <- ren_energy_data %>%
    filter(Country == country_name, Year == 2023, Value > 0) %>%
    mutate(perc = (Value / sum(Value)) * 100)
  
  if(nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(area = Value, fill = Source, 
                        label = paste0(Source, "\n", Value, " GWh\n(", round(perc, 1), "%)"))) +
    geom_treemap(color = "white", size = 3) + 
    geom_treemap_text(colour = "black", place = "centre", grow = TRUE, reflow = TRUE, 
                      size = 8, min.size = 2) + # Reduced font constraints
    scale_fill_manual(values = energy_colors) + 
    labs(title = country_name) +
    theme_bw() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 10), # Reduced subtitle size
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
}

# generate plots
years_to_plot <- c(2000, 2005, 2010, 2015, 2020, 2023)
india_plots <- lapply(years_to_plot, make_india_time_treemap)
final_india_grid <- wrap_plots(india_plots, ncol = 3) + 
  plot_annotation(
    title = "Renewable Electricity Generation by Source (Non-Combustible) – India",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

countries_to_plot <- sort(unique(ren_energy_data$Country))
country_plots <- lapply(countries_to_plot, make_country_2023_treemap)

final_country_grid <- wrap_plots(country_plots, ncol = 3) + 
  plot_annotation(
    title = "Renewable Electricity Generation by Source (Non-Combustible)",
    subtitle = "Data for 2023",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

final_india_grid
final_country_grid


##########################################################################
##########################################################################

bp <- "/home/vaibhavagarwal/Projects/mani-work/mani-data/All_Country_IEA"

country_info <- list(
  "Bangladesh" = "Bangladesh",
  "Burkina Faso" = "Burkina_Faso",
  "Ethiopia" = "Ethiopia",
  "Ghana" = "Ghana",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Kenya" = "Kenya",
  "Namibia" = "Namibia",
  "Peru" = "Peru",
  "South Africa" = "South_Africa"
)

country_names <- sort(names(country_info))
all_data_list <- list()

# Define the 5-year increment sequence
year_breaks <- seq(1900, 2030, by = 5)

for (c_name in country_names) {
  c_folder <- country_info[[c_name]]
  
  file_name <- paste0("International Energy Agency - CO2 emissions by fuel in ", c_name, ".csv")
  file_path <- file.path(bp, c_folder, file_name)
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path) %>%
      mutate(Country = c_name) %>%
      rename(Fuel = 1) %>% 
      filter(!is.na(Value)) %>% 
      filter(Fuel != "Other")
    
    all_data_list[[c_name]] <- df
    
    p_individual <- ggplot(df, aes(x = Year, y = Value, fill = Fuel)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black") +
      geom_text(aes(label = ifelse(round(Value, 0) == 0, "", round(Value, 0))), 
                position = position_stack(vjust = 0.5), 
                size = 3, fontface = "bold") +
      scale_x_continuous(breaks = year_breaks) +
      scale_fill_manual(values = c(
        "Coal" = "#fc9272",        
        "Oil" = "#fcbba1",         
        "Natural gas" = "#fee0d2" )) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = c_name,
        x = "Year",
        y = "MtCO2",
        fill = "Fuel Source",
        caption = "Source: International Energy Agency (IEA)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    print(p_individual)
  }
}

combined_data <- bind_rows(all_data_list) %>%
  mutate(Country = factor(Country, levels = country_names))

p_combined <- ggplot(combined_data, aes(x = Year, y = Value, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black") +
  # Using scales = "free" ensures the x-axis labels are printed on every single facet
  facet_wrap(~Country, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = year_breaks) +
  scale_fill_manual(values = c(
    "Coal" = "#fc9272",        
    "Oil" = "#fcbba1",         
    "Natural gas" = "#fee0d2" )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "CO2 Emissions by Fuel Type: Combined Analysis",
    x = "Year",
    y = "MtCO2",
    fill = "Fuel Source",
    caption = "Source: International Energy Agency (IEA)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    # Grey background and black text for facet headers (strips)
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p_combined)

##########################################################################
##########################################################################


bp <- "/home/vaibhavagarwal/Projects/mani-work/mani-data/All_Country_IEA"

energy_colors <- c(
  "Solar PV"    = "#a1d99b", "Wind"        = "#c7e9c0", 
  "Hydropower"  = "#7fcdbb", "Biofuels"    = "#d9f0a3", 
  "Waste"       = "#e7f598", "Geothermal"  = "#41b6c4",
  "Coal"        = "#fc9272", "Oil"         = "#fcbba1", 
  "Natural gas" = "#fee0d2", "Nuclear"     = "#bdbdbd"
)

kenya <- read_csv(file.path(bp, "Kenya/International Energy Agency - electricity generation sources in Kenya.csv")) %>% mutate(Country = "Kenya")
india <- read_csv(file.path(bp, "India/International Energy Agency - electricity generation sources in India.csv")) %>% mutate(Country = "India")
ethiopia <- read_csv(file.path(bp, "Ethiopia/International Energy Agency - electricity generation sources in Ethiopia.csv")) %>% mutate(Country = "Ethiopia")
bangladesh <- read_csv(file.path(bp, "Bangladesh/International Energy Agency - electricity generation sources in Bangladesh.csv")) %>% mutate(Country = "Bangladesh")
ghana <- read_csv(file.path(bp, "Ghana/International Energy Agency - electricity generation sources in Ghana.csv")) %>% mutate(Country = "Ghana")
indonesia <- read_csv(file.path(bp, "Indonesia/International Energy Agency - electricity generation sources in Indonesia.csv")) %>% mutate(Country = "Indonesia")
peru <- read_csv(file.path(bp, "Peru/International Energy Agency - electricity generation sources in Peru.csv")) %>% mutate(Country = "Peru")
namibia <- read_csv(file.path(bp, "Namibia/International Energy Agency - electricity generation sources in Namibia.csv")) %>% mutate(Country = "Namibia")
burkina <- read_csv(file.path(bp, "Burkina_Faso/International Energy Agency - electricity generation sources in Burkina Faso.csv")) %>% mutate(Country = "Burkina Faso")
south_africa <- read_csv(file.path(bp, "South_Africa/International Energy Agency - electricity generation sources in South Africa.csv")) %>% mutate(Country = "South Africa")

list_of_dfs <- list(kenya, india, ethiopia, bangladesh, ghana, indonesia, peru, namibia, burkina, south_africa)

energy_data <- bind_rows(lapply(list_of_dfs, function(df) {
  colnames(df)[1] <- "Source" 
  return(df)
}))

make_donut_chart <- function(country_name) {
  
  display_name <- case_when(
    country_name == "South Africa" ~ "SOUTH\nAFRICA",
    country_name == "Burkina Faso" ~ "BURKINA\nFASO",
    TRUE ~ toupper(country_name)
  )
  
  plot_data <- energy_data %>%
    filter(Country == country_name, Year == 2023) %>%
    filter(!Source %in% c("Total", "Total generation", "Total electricity generation")) %>%
    filter(Value > 0) %>%
    rename(category = Source, count = Value) %>%
    mutate(
      fraction = count / sum(count),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n=-1)),
      labelPosition = (ymax + ymin) / 2,
      label = ifelse(fraction >= 0.05, 
                     paste0(count, " GWh\n(", percent(fraction, accuracy = 0.1), ")"), 
                     "")
    )
  
  if(nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2, fill=category)) +
    geom_rect(color = "black") +
    geom_text(x=3, aes(y=labelPosition, label=label), size=2.5, fontface="bold", lineheight = 0.8) +
    scale_fill_manual(values = energy_colors) + 
    annotate("text", x = 0, y = 0, label = display_name, size = 3, fontface = "bold", lineheight = 0.8) +
    coord_polar(theta="y") +
    xlim(c(0, 5)) +
    theme_void() + 
    theme(legend.position = 'none')
}

countries_to_plot <- sort(unique(energy_data$Country))
all_donuts <- lapply(countries_to_plot, make_donut_chart)

final_donut_grid <- wrap_plots(all_donuts, ncol = 4) + 
  plot_annotation(
    title = "Electricity Generation Sources (2023)",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

final_donut_grid


##########################################################################
##########################################################################
bp <- "/home/vaibhavagarwal/Projects/mani-work/mani-data/All_Country_IEA"

energy_colors <- c(
  "Biofuels"    = "#d9f0a3", 
  "Coal"        = "#fc9272", 
  "Geothermal"  = "#41b6c4",
  "Hydropower"  = "#7fcdbb", 
  "Natural gas" = "#fee0d2", 
  "Nuclear"     = "#bdbdbd",
  "Oil"         = "#fcbba1", 
  "Solar PV"    = "#a1d99b", 
  "Waste"       = "#e7f598", 
  "Wind"        = "#c7e9c0"
)

country_info <- list(
  "Bangladesh"   = "Bangladesh",
  "Burkina Faso" = "Burkina_Faso",
  "Ethiopia"     = "Ethiopia",
  "Ghana"        = "Ghana",
  "India"        = "India",
  "Indonesia"    = "Indonesia",
  "Kenya"        = "Kenya",
  "Namibia"      = "Namibia",
  "Peru"         = "Peru",
  "South Africa" = "South_Africa"
)

country_names <- sort(names(country_info))
all_data_list <- list()
year_breaks <- seq(2000, 2023, by = 5)

for (c_name in country_names) {
  c_folder <- country_info[[c_name]]
  
  file_name <- paste0("International Energy Agency - electricity generation sources in ", c_name, ".csv")
  file_path <- file.path(bp, c_folder, file_name)
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path) %>%
      mutate(Country = c_name) %>%
      rename(Source = 1) %>% 
      filter(!is.na(Value)) %>% 
      filter(!Source %in% c("Total", "Total generation", "Total electricity generation")) %>%
      filter(Value > 0) %>%
      group_by(Year) %>%
      mutate(total_year = sum(Value)) %>%
      ungroup()
    
    all_data_list[[c_name]] <- df
    
    p_individual <- ggplot(df, aes(x = Year, y = Value, fill = Source)) +
      geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black", size = 0.1) +
      # Labels rounded to nearest 0.1 with K/M scaling
      geom_text(aes(label = ifelse(Value/total_year > 0.05, 
                                   label_number(accuracy = 0.1, scale_cut = cut_short_scale())(Value), 
                                   "")), 
                position = position_stack(vjust = 0.5), 
                size = 2.5, fontface = "bold") +
      scale_x_continuous(breaks = year_breaks) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), 
                         expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = energy_colors) +
      labs(
        title = c_name,
        x = "Year",
        y = "GWh",
        fill = "Energy Source",
        caption = "Source: International Energy Agency (IEA), Labels rounded to nearest 0.1"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    print(p_individual)
  }
}


combined_data <- bind_rows(all_data_list) %>%
  mutate(Country = factor(Country, levels = country_names))

p_combined <- ggplot(combined_data, aes(x = Year, y = Value, fill = Source)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black", size = 0.1) +
  facet_wrap(~Country, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = year_breaks) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = energy_colors) +
  labs(
    title = "Electricity Generation Sources",
    x = "Year",
    y = "GWh",
    fill = "Energy Source",
    caption = "Source: International Energy Agency (IEA)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 8),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(color = "black", face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p_combined)

##########################################################################
##########################################################################
bp <- "/home/vaibhavagarwal/Projects/mani-work/mani-data/All_Country_IEA"

supply_colors <- c(
  "Biofuels and waste"               = "#d9f0a3", 
  "Coal and coal products"           = "#fc9272", 
  "Hydropower"                       = "#7fcdbb", 
  "Natural gas"                      = "#fee0d2", 
  "Oil and oil products"             = "#fcbba1", 
  "Solar, wind and other renewables" = "#a1d99b",
  "Nuclear"                          = "lightgrey"
)

country_info <- list(
  "Bangladesh"   = "Bangladesh", "Burkina Faso" = "Burkina_Faso",
  "Ethiopia"     = "Ethiopia",   "Ghana"        = "Ghana",
  "India"        = "India",       "Indonesia"    = "Indonesia",
  "Kenya"        = "Kenya",       "Namibia"      = "Namibia",
  "Peru"         = "Peru",        "South Africa" = "South_Africa",
  "Uganda"       = "Uganda"
)

country_names <- sort(names(country_info))
all_data_list <- list()

for (c_name in country_names) {
  c_folder <- country_info[[c_name]]
  file_name <- paste0("International Energy Agency - total energy supply in ", c_name, ".csv")
  file_path <- file.path(bp, c_folder, file_name)
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path) %>%
      mutate(Country = c_name) %>%
      rename(Source = 1) %>% 
      filter(!is.na(Value), Value > 0) %>%
      filter(!Source %in% c("Total", "Total energy supply", "Total supply")) %>%
      mutate(Source = case_when(
        Source %in% c("Oil", "Oil products") ~ "Oil and oil products",
        Source %in% c("Coal") ~ "Coal and coal products",
        Source %in% c("Biofuels", "Waste") ~ "Biofuels and waste",
        Source %in% c("Solar PV", "Wind", "Geothermal") ~ "Solar, wind and other renewables",
        TRUE ~ Source
      ))
    all_data_list[[c_name]] <- df
  }
}

combined_data <- bind_rows(all_data_list)

timeline_data <- combined_data %>%
  mutate(Country = factor(Country, levels = country_names))

p_timeline <- ggplot(timeline_data, aes(x = Year, y = Value, fill = Source)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black", size = 0.1) +
  facet_wrap(~Country, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = supply_colors) +
  labs(title = "Total Energy Supply", x = "Year", y = "TJ", fill = "Source") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold"))

snapshot_data <- combined_data %>%
  filter(Year == 2023) %>%
  group_by(Country) %>%
  mutate(perc = Value / sum(Value)) %>%
  ungroup() %>%
  mutate(Country = factor(Country, levels = rev(country_names)))

p_snapshot_2023 <- ggplot(snapshot_data, aes(y = Country, x = Value, fill = Source)) +
  geom_col(position = "fill", color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(perc > 0.01, paste0(round(perc*100, 0), "%"), "")), 
            position = position_fill(vjust = 0.5), size = 3, fontface = "bold") +
  scale_x_continuous(labels = percent, expand = c(0,0)) +
  scale_fill_manual(values = supply_colors) +
  labs(
    title = "Energy Consumption Mix By Country (2023)",
    x = "Percentage Share",
    y = "Country",
    fill = "Source"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

print(p_timeline)
print(p_snapshot_2023)

##########################################################################
##########################################################################