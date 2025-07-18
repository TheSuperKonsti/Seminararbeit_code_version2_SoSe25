rm(list = ls())
set.seed(151)

library(readxl)
library(dplyr)
library(countrycode)
library(ggplot2)
library(tidyr)
library(xtable)

benford_probs_1digit <- log10(1 + 1 / (1:9))
benford_probs_2digits <- log10(1 + 1 / (10:99))
years <- c(2002, 2007, 2012, 2017, 2022)

wpp_data_raw <- read_excel("WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx",
                           sheet = "Estimates", skip = 16)

wpp_data <- wpp_data_raw[, c("Region, subregion, country or area *",
                             "Total Population, as of 1 January (thousands)",
                             "Year")]
colnames(wpp_data) <- c("Country", "Population", "Year")
wpp_data <- na.omit(wpp_data)

all_countries <- sort(unique(wpp_data$Country))
export_df <- data.frame(Country_or_Region = all_countries, Remove = FALSE, stringsAsFactors = FALSE)
write.csv(export_df, "benford_regions_all_unfiltered.csv", row.names = FALSE)

un_members <- read.csv("benford_regions_un_members.csv", stringsAsFactors = FALSE)
un_members$Remove <- toupper(un_members$Remove) == "TRUE"
filtered_countries <- un_members[!un_members$Remove, "Country_or_Region", drop = FALSE]

filtered_countries$Region <- countrycode(filtered_countries$Country_or_Region,
                                         origin = "country.name",
                                         destination = "continent")

write.csv(filtered_countries, "benford_regions_with_continent.csv", row.names = FALSE)

data_filtered <- wpp_data %>%
  filter(Country %in% filtered_countries$Country_or_Region)

extract_digits <- function(x, digits = 1) {
  x <- gsub("[^0-9]", "", as.character(floor(as.numeric(x))))
  if (digits == 1) substr(x, 1, 1) else substr(x, 1, 2)
}

results_by_year <- data.frame()

for (year in years) {
  pop_values <- data_filtered %>% filter(Year == year) %>% pull(Population) %>% as.numeric()
  d1 <- extract_digits(pop_values, 1)
  d1 <- d1[d1 %in% as.character(1:9)]
  freq_1 <- table(factor(d1, levels = as.character(1:9)))
  rel_freq_1 <- freq_1 / sum(freq_1)
  chi_1 <- chisq.test(freq_1, p = benford_probs_1digit, rescale.p = TRUE)
  ks_1 <- max(abs(cumsum(rel_freq_1) - cumsum(benford_probs_1digit)))
  mad_1 <- mean(abs(rel_freq_1 - benford_probs_1digit))
  mad_1_class <- ifelse(mad_1 < 0.006, "Close",
                        ifelse(mad_1 < 0.012, "Acceptable",
                               ifelse(mad_1 < 0.015, "Marginal", "Nonconformity")))
  
  d2 <- extract_digits(pop_values, 2)
  d2 <- d2[d2 %in% as.character(10:99)]
  freq_2 <- table(factor(d2, levels = as.character(10:99)))
  rel_freq_2 <- freq_2 / sum(freq_2)
  chi_2 <- chisq.test(freq_2, p = benford_probs_2digits, rescale.p = TRUE)
  ks_2 <- max(abs(cumsum(rel_freq_2) - cumsum(benford_probs_2digits)))
  mad_2 <- mean(abs(rel_freq_2 - benford_probs_2digits))
  mad_2_class <- ifelse(mad_2 < 0.006, "Close",
                        ifelse(mad_2 < 0.012, "Acceptable",
                               ifelse(mad_2 < 0.015, "Marginal", "Nonconformity")))
  
  results_by_year <- rbind(results_by_year, data.frame(
    Year = year,
    KS_1st = round(ks_1, 4),
    MAD_1st = round(mad_1, 4),
    MAD_1st_Class = mad_1_class,
    Chi2_1st_p = round(chi_1$p.value, 4),
    KS_1to2 = round(ks_2, 4),
    MAD_1to2 = round(mad_2, 4),
    MAD_1to2_Class = mad_2_class,
    Chi2_1to2_p = round(chi_2$p.value, 4)
  ))
}

write.csv(results_by_year, "benford_results_yearly_un_members.csv", row.names = FALSE)

results_by_region <- data.frame()
data_with_region <- merge(data_filtered, filtered_countries, by.x = "Country", by.y = "Country_or_Region")
data_subset <- data_with_region %>% filter(!is.na(Region) & Year %in% years)
region_names <- unique(data_subset$Region)

for (region in region_names) {
  pop_vals <- data_subset %>% filter(Region == region) %>% pull(Population) %>% as.numeric()
  if (length(pop_vals) < 20) next
  
  d1 <- extract_digits(pop_vals, 1)
  d1 <- d1[d1 %in% as.character(1:9)]
  freq_1 <- table(factor(d1, levels = as.character(1:9)))
  rel_freq_1 <- freq_1 / sum(freq_1)
  chi_1 <- suppressWarnings(chisq.test(freq_1, p = benford_probs_1digit, rescale.p = TRUE))
  ks_1 <- max(abs(cumsum(rel_freq_1) - cumsum(benford_probs_1digit)))
  mad_1 <- mean(abs(rel_freq_1 - benford_probs_1digit))
  mad_1_class <- ifelse(mad_1 < 0.006, "Close",
                        ifelse(mad_1 < 0.012, "Acceptable",
                               ifelse(mad_1 < 0.015, "Marginal", "Nonconformity")))
  
  d2 <- extract_digits(pop_vals, 2)
  d2 <- d2[d2 %in% as.character(10:99)]
  freq_2 <- table(factor(d2, levels = as.character(10:99)))
  rel_freq_2 <- freq_2 / sum(freq_2)
  chi_2 <- suppressWarnings(chisq.test(freq_2, p = benford_probs_2digits, rescale.p = TRUE))
  ks_2 <- max(abs(cumsum(rel_freq_2) - cumsum(benford_probs_2digits)))
  mad_2 <- mean(abs(rel_freq_2 - benford_probs_2digits))
  mad_2_class <- ifelse(mad_2 < 0.006, "Close",
                        ifelse(mad_2 < 0.012, "Acceptable",
                               ifelse(mad_2 < 0.015, "Marginal", "Nonconformity")))
  
  results_by_region <- rbind(results_by_region, data.frame(
    Region = region,
    Observations = length(pop_vals),
    KS_1st = round(ks_1, 4),
    MAD_1st = round(mad_1, 4),
    MAD_1st_Class = mad_1_class,
    Chi2_1st_p = round(chi_1$p.value, 4),
    KS_1to2 = round(ks_2, 4),
    MAD_1to2 = round(mad_2, 4),
    MAD_1to2_Class = mad_2_class,
    Chi2_1to2_p = round(chi_2$p.value, 4)
  ))
}

write.csv(results_by_region, "benford_results_region_aggregated.csv", row.names = FALSE)

pop_2022 <- data_filtered %>% filter(Year == 2022) %>% pull(Population) %>% as.numeric()
d1_2022 <- extract_digits(pop_2022, 1)
d1_2022 <- d1_2022[d1_2022 %in% as.character(1:9)]
rel_1 <- table(factor(d1_2022, levels = as.character(1:9))) / length(d1_2022)
df_1 <- data.frame(Digit = factor(1:9), Observed = as.numeric(rel_1), Expected = benford_probs_1digit)
df_1_long <- pivot_longer(df_1, cols = c("Observed", "Expected"), names_to = "Type", values_to = "Value")

pdf("first_digit_2022.pdf", width = 6, height = 4)
ggplot(df_1_long, aes(x = Digit, y = Value, group = Type)) +
  geom_col(data = subset(df_1_long, Type == "Observed"), aes(fill = Type), color = "black") +
  geom_line(data = subset(df_1_long, Type == "Expected"), aes(color = Type), size = 1.2) +
  scale_fill_manual(values = c("Observed" = "steelblue1")) +
  scale_color_manual(values = c("Expected" = "orangered")) +
  labs(title = "First Digit Distribution vs. Benford (2022)", x = "Digit", y = "Relative Frequency") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

d2_2022 <- extract_digits(pop_2022, 2)
d2_2022 <- d2_2022[d2_2022 %in% as.character(10:99)]
rel_2 <- table(factor(d2_2022, levels = as.character(10:99))) / length(d2_2022)
df_2 <- data.frame(Digit = 10:99, Observed = as.numeric(rel_2), Expected = benford_probs_2digits)
df_2_long <- pivot_longer(df_2, cols = c("Observed", "Expected"), names_to = "Type", values_to = "Value")

pdf("two_digits_2022.pdf", width = 8, height = 4)
ggplot(df_2_long, aes(x = Digit, y = Value, group = Type)) +
  geom_col(data = subset(df_2_long, Type == "Observed"), aes(fill = Type), color = "black") +
  geom_line(data = subset(df_2_long, Type == "Expected"), aes(color = Type), size = 1.2) +
  scale_fill_manual(values = c("Observed" = "steelblue1")) +
  scale_color_manual(values = c("Expected" = "orangered")) +
  labs(title = "First Two Digits Distribution vs. Benford (2022)", x = "Digit Pair", y = "Relative Frequency") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

ks_crit_year <- 1.36 / sqrt(193)

pdf("ks_by_year.pdf", width = 6, height = 4)
ggplot(results_by_year, aes(x = factor(Year))) +
  geom_col(aes(y = KS_1st, fill = "First Digit"), width = 0.4,
           position = position_nudge(x = -0.2), color = "black") +
  geom_col(aes(y = KS_1to2, fill = "First Two Digits"), width = 0.4,
           position = position_nudge(x = 0.2), color = "black") +
  geom_hline(yintercept = ks_crit_year, linetype = "dashed", color = "orangered", size = 0.6) +
  geom_text(aes(x = 3, y = ks_crit_year - 0.004, label = paste0(round(ks_crit_year, 3))),
            color = "orangered4", size = 3.5, hjust = 0.5, vjust = 1.2) +
  scale_fill_manual(values = c("First Digit" = "steelblue1", "First Two Digits" = "lightgreen")) +
  labs(title = "KS Statistic by Year", x = "Year", y = "KS Statistic") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

pdf("mad_by_year.pdf", width = 6, height = 4)
ggplot(results_by_year, aes(x = factor(Year))) +
  geom_col(aes(y = MAD_1st, fill = "First Digit"), width = 0.4,
           position = position_nudge(x = -0.2), color = "black") +
  geom_col(aes(y = MAD_1to2, fill = "First Two Digits"), width = 0.4,
           position = position_nudge(x = 0.2), color = "black") +
  geom_hline(yintercept = c(0.006, 0.015),
             linetype = c("dashed", "dotted"), color = "orangered", size = 0.6) +
  geom_text(aes(x = 1, y = 0.006, label = "Acceptable"),
            color = "orangered4", size = 3.5, hjust = 0, vjust = -0.8) +
  geom_text(aes(x = 1, y = 0.015, label = "Nonconforming"),
            color = "orangered4", size = 3.5, hjust = 0, vjust = -0.8) +
  scale_fill_manual(values = c("First Digit" = "steelblue1", "First Two Digits" = "lightgreen")) +
  labs(title = "MAD Values by Year", x = "Year", y = "MAD") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

results_by_region$X <- as.numeric(factor(results_by_region$Region))
results_by_region$KS_Crit <- 1.36 / sqrt(results_by_region$Observations)

pdf("ks_by_region.pdf", width = 6, height = 4)
ggplot(results_by_region, aes(x = Region)) +
  geom_col(aes(y = KS_1st, fill = "First Digit"), position = position_nudge(x = -0.15),
           width = 0.3, color = "black") +
  geom_col(aes(y = KS_1to2, fill = "First Two Digits"), position = position_nudge(x = 0.15),
           width = 0.3, color = "black") +
  geom_segment(data = results_by_region,
               aes(x = X - 0.35, xend = X + 0.35, y = KS_Crit, yend = KS_Crit),
               inherit.aes = FALSE, color = "orangered", linetype = "dashed", size = 0.6) +
  geom_text(data = results_by_region,
            aes(x = X, y = KS_Crit + 0.01, label = paste0(round(KS_Crit, 3))),
            inherit.aes = FALSE, color = "orangered4", size = 4.5, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c("First Digit" = "steelblue1", "First Two Digits" = "lightgreen")) +
  labs(title = "KS Statistic by Region (with individual thresholds)", x = "Region", y = "KS Statistic") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

pdf("mad_by_region.pdf", width = 6, height = 4)
ggplot(results_by_region, aes(x = Region)) +
  geom_col(aes(y = MAD_1st, fill = "First Digit"), position = position_nudge(x = -0.15),
           color = "black", width = 0.3) +
  geom_col(aes(y = MAD_1to2, fill = "First Two Digits"), position = position_nudge(x = 0.15),
           color = "black", width = 0.3) +
  geom_hline(yintercept = c(0.006, 0.015), linetype = c("dashed", "dotted"),
             color = "orangered", size = 0.6) +
  geom_text(aes(x = 1, y = 0.006, label = "Acceptable"), color = "orangered4",
            size = 3.5, hjust = 0, vjust = -0.8) +
  geom_text(aes(x = 1, y = 0.015, label = "Nonconforming"), color = "orangered4",
            size = 3.5, hjust = 0, vjust = -0.8) +
  scale_fill_manual(values = c("First Digit" = "steelblue1", "First Two Digits" = "lightgreen")) +
  labs(title = "MAD by Region", x = "Region", y = "MAD") +
  theme_minimal() + theme(legend.position = "top")
dev.off()

colnames(results_by_year) <- c(
  "Year", "KS_1st", "MAD_1st", "Class_1st",
  "Chi2_1st_p", "KS_1to2", "MAD_1to2", "Class_1to2", "Chi2_1to2_p"
)

xtable_year <- xtable(
  results_by_year,
  caption = "Goodness-of-fit results for Benford analysis (2002--2022, global sample of 193 UN member states).",
  digits = 4
)

print(xtable_year, include.rownames = FALSE)

colnames(results_by_region) <- c(
  "Region", "Observations",
  "KS_1st", "MAD_1st", "Class_1st", "Chi2_1st_p",
  "KS_1to2", "MAD_1to2", "Class_1to2", "Chi2_1to2_p"
)

xtable_region <- xtable(
  results_by_region,
  caption = "Aggregated Benford results by continent (years 2002--2022 combined).",
  digits = 4
)

print(xtable_region, include.rownames = FALSE)

ks_threshold_year <- data.frame(
  Group = "Global (per year)",
  Observations = 193,
  KS_Threshold = round(1.36 / sqrt(193), 4)
)

ks_threshold_regions <- results_by_region[, c("Region", "Observations")]
ks_threshold_regions$KS_Threshold <- round(1.36 / sqrt(ks_threshold_regions$Observations), 4)
colnames(ks_threshold_regions) <- c("Group", "Observations", "KS_Threshold")

ks_threshold_all <- rbind(ks_threshold_year, ks_threshold_regions)

xtable_ks <- xtable(
  ks_threshold_all,
  caption = "KS critical values by region and globally (per year), based on sample size.",
  label = "tab:ks_thresholds",
  digits = c(0, 0, 0, 4)
)

print(xtable_ks, include.rownames = FALSE)

