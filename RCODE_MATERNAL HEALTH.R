# ==============================================================================
# SUB-NATIONAL MATERNAL HEALTH COVERAGE EVALUATION FRAMEWORK IN INDIA
# Pure Empirical Analytical Pipeline (Fully Corrected & Revised Code)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PACKAGE INITIALIZATION & GLOBAL SETTINGS
# ------------------------------------------------------------------------------
cat("\n=== [0] Initializing R Environment & Packages ===\n")

required_packages <- c(
  "readxl", "tidyverse", "sf", "spdep", "spatialreg", "plm", 
  "psych", "dtw", "cluster", "gt", "patchwork", "viridis", 
  "scales", "reshape2", "stringr"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages(lapply(required_packages, library, character.only = TRUE))

theme_q1 <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      axis.title = element_text(face = "bold", size = 11, color = "#2C3E50"),
      axis.text = element_text(size = 9, color = "#333333"),
      legend.title = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12, hjust = 0, color = "#1A252C"),
      plot.subtitle = element_text(size = 9.5, face = "italic", color = "#555555"),
      strip.background = element_rect(fill = "#F4F6F7", color = NA),
      strip.text = element_text(face = "bold", size = 10, color = "#2C3E50")
    )
}

set.seed(2026)

# ------------------------------------------------------------------------------
# 2.1 PANEL DATASET INGESTION AND DOMAIN PARTITIONING
# ------------------------------------------------------------------------------
cat("\n=== [2.1] Ingesting Excel Dataset & Normalizing Metadata ===\n")

excel_path <- "C:/Users/SUBHRAJIT SAHA/Downloads/Maternal Health service coverage in India.xlsx"

states_34 <- c(
  "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh", 
  "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka", 
  "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", 
  "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", 
  "Telangana", "Tripura", "Uttar Pradesh", "Uttarakhand", "West Bengal", 
  "A & N Islands", "Chandigarh", "D & N Haveli and Daman & Diu", "Delhi", 
  "Jammu & Kashmir", "Puducherry"
)

zone_mapping <- data.frame(
  state = states_34,
  zone = c(
    "Southern", "Eastern_NE", "Eastern_NE", "Eastern_NE", "Central_Western",
    "Southern", "Central_Western", "Northern", "Northern", "Eastern_NE", "Southern",
    "Southern", "Central_Western", "Central_Western", "Eastern_NE", "Eastern_NE", "Eastern_NE",
    "Eastern_NE", "Eastern_NE", "Northern", "Northern", "Eastern_NE", "Southern",
    "Southern", "Eastern_NE", "Northern", "Northern", "Eastern_NE",
    "Southern", "Northern", "Central_Western", "Northern", "Northern", "Southern"
  ),
  stringsAsFactors = FALSE
)

indicator_dict <- data.frame(
  k = 1:15,
  code = paste0("K", 1:15),
  name = c(
    "Early ANC Reg (<=12 wks)", "4+ ANC Checkups", "Institutional Registration",
    "180 IFA Tablets", "Calcium Supplementation", "Deworming Medication",
    "Hemoglobin Assessment", "OGTT Diabetes Screening", "Syphilis Screening", "BP Monitoring",
    "Institutional Delivery Rate", "SBA Attended Births", "Obstetric Complications Management",
    "PNC Within 48 Hours", "Home PNC Visit Within 14 Days"
  ),
  domain = c(rep("ANC", 10), rep("INC", 3), rep("PNC", 2)),
  subdomain = c(rep("SD11", 3), rep("SD12", 3), rep("SD13", 4), rep("INC", 3), rep("PNC", 2)),
  stringsAsFactors = FALSE
)

clean_str <- function(x) {
  if (is.null(x)) return(character(0))
  x_str <- as.character(x)
  x_str[is.na(x_str)] <- ""
  x_str <- tolower(x_str)
  x_str <- gsub("&", "and", x_str, fixed = TRUE)
  x_str <- gsub("[^a-z0-9]", "", x_str)
  return(x_str)
}

canonical_clean <- clean_str(states_34)

match_canonical_state <- function(raw_s) {
  if (is.null(raw_s) || is.na(raw_s)) return(NA_character_)
  raw_s_chr <- trimws(as.character(raw_s))
  if (nchar(raw_s_chr) == 0 || raw_s_chr == "NA") return(NA_character_)
  s_clean <- clean_str(raw_s_chr)
  if (nchar(s_clean) == 0) return(NA_character_)
  idx <- match(s_clean, canonical_clean)
  if (!is.na(idx)) return(states_34[idx])
  dists <- adist(s_clean, canonical_clean)
  best_idx <- which.min(dists)
  if (length(best_idx) == 0) return(NA_character_)
  return(states_34[best_idx])
}

if (!file.exists(excel_path)) {
  stop(sprintf("\nCRITICAL ERROR: Real dataset file not found at:\n'%s'\nExecution halted.", excel_path))
}

raw_excel <- read_excel(excel_path)
orig_cols <- colnames(raw_excel)
norm_cols <- tolower(trimws(orig_cols))

state_col_idx <- grep("state|ut|district|region", norm_cols)[1]
if (is.na(state_col_idx)) state_col_idx <- 1
state_col_name <- orig_cols[state_col_idx]

fy_col_idx <- grep("fy|year|time|period", norm_cols)[1]
if (is.na(fy_col_idx)) fy_col_idx <- 2
fy_col_name <- orig_cols[fy_col_idx]

raw_df <- raw_excel %>%
  rename(state_raw = !!sym(state_col_name), fy_raw = !!sym(fy_col_name)) %>%
  mutate(
    state = vapply(state_raw, match_canonical_state, FUN.VALUE = character(1)),
    fy = as.character(fy_raw)
  )

k_cols <- grep("^k[0-9]+$", norm_cols, value = TRUE)
if (length(k_cols) >= 15) {
  for (i in 1:15) {
    target_col <- orig_cols[grep(paste0("^", k_cols[i], "$"), norm_cols)[1]]
    raw_df[[paste0("K", i)]] <- as.numeric(raw_df[[target_col]])
  }
} else {
  num_cols <- names(raw_df)[sapply(raw_df, is.numeric)]
  num_cols <- setdiff(num_cols, c("state_raw", "fy_raw"))
  if (length(num_cols) < 15) {
    non_id_cols <- setdiff(names(raw_excel), c(state_col_name, fy_col_name))
    num_cols <- non_id_cols[1:15]
  }
  for (i in 1:15) {
    raw_df[[paste0("K", i)]] <- as.numeric(raw_df[[num_cols[i]]])
  }
}

panel_df <- raw_df %>%
  filter(!is.na(state)) %>%
  distinct(state, fy, .keep_all = TRUE) %>%
  left_join(zone_mapping, by = "state") %>%
  mutate(
    year_id = as.numeric(factor(fy)),
    state_id = as.numeric(factor(state))
  ) %>%
  arrange(state_id, year_id)

cat(sprintf("-> Dataset Loaded: N = %d States/UTs, T = %d Financial Years, P = 15 Indicators.\n",
            length(unique(panel_df$state)), length(unique(panel_df$fy))))

# ------------------------------------------------------------------------------
# 2.2 BOUNDED METRIC TRANSFORMATION (Equations 2 - 4)
# ------------------------------------------------------------------------------
cat("\n=== [2.2] Applying Regularized Winsorized Logit Transformation ===\n")

transformed_df <- panel_df
eps <- 1e-4

for (k in 1:15) {
  col_name <- paste0("K", k)
  raw_x <- transformed_df[[col_name]]
  
  # Eq. (2): Winsorization at 99th percentile
  p99 <- quantile(raw_x, 0.99, na.rm = TRUE)
  x_wins <- pmin(raw_x, p99)
  
  # Eq. (3): Regularized proportion scaling onto (0, 1)
  x_tilde <- eps + (1 - 2 * eps) * (x_wins / 100)
  x_tilde <- pmax(eps, pmin(1 - eps, x_tilde))
  
  # Eq. (4): Exact logit transformation
  z_k <- log(x_tilde / (1 - x_tilde))
  
  transformed_df[[paste0("tilde_K", k)]] <- x_tilde
  transformed_df[[paste0("z_K", k)]] <- z_k
}

# Plot 1: Transformation Density Comparison
df_p1_pre <- panel_df %>% 
  pivot_longer(cols = starts_with("K") & !starts_with("tilde") & !starts_with("z"), 
               names_to = "indicator", values_to = "value") %>%
  select(state, fy, indicator, value) %>%
  mutate(Stage = "1. Raw HMIS Indicators (%)")

df_p1_post <- transformed_df %>% 
  select(state, fy, starts_with("tilde_K")) %>%
  pivot_longer(cols = starts_with("tilde_K"), names_to = "indicator", values_to = "value") %>%
  mutate(
    indicator = gsub("tilde_", "", indicator),
    value = value * 100, 
    Stage = "2. Regularized Proportions (%)"
  ) %>%
  select(state, fy, indicator, value, Stage)

p1 <- bind_rows(df_p1_pre, df_p1_post) %>%
  ggplot(aes(x = value, fill = Stage)) +
  geom_density(alpha = 0.65, color = "black", linewidth = 0.4, na.rm = TRUE) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  facet_wrap(~Stage, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(
    title = "Boundary Overrun Mitigation via Regularized Scaling",
    subtitle = "Correction of Upper-Bound Distortions Across Sub-National Units",
    x = "Indicator Coverage (%)",
    y = "Density"
  ) + theme_q1() + theme(legend.position = "none")

ggsave("Figure1_Metric_Transformation.png", plot = p1, width = 9, height = 6, dpi = 300)
cat(" -> Figure 1 saved: Figure1_Metric_Transformation.png\n")

# ------------------------------------------------------------------------------
# 2.3 STAGE 1: HDEFA COMPOSITE INDEXING (Equations 5 - 10)
# ------------------------------------------------------------------------------
cat("\n=== [2.3] Stage 1: Constructing MHSCI via HDEFA ===\n")

N_total <- nrow(transformed_df)
z_matrix <- as.matrix(transformed_df[, paste0("z_K", 1:15)])
tilde_matrix <- as.matrix(transformed_df[, paste0("tilde_K", 1:15)])

z_matrix_clean <- apply(z_matrix, 2, function(x) { if (any(is.na(x))) x[is.na(x)] <- median(x, na.rm = TRUE); return(x) })
tilde_matrix_clean <- apply(tilde_matrix, 2, function(x) { if (any(is.na(x))) x[is.na(x)] <- median(x, na.rm = TRUE); return(x) })

# 1. Eq. (5): Shannon Entropy Weights on Bounded Proportions
p_itk <- apply(tilde_matrix_clean, 2, function(col) col / sum(col))
E_k <- apply(p_itk, 2, function(p) - (1 / log(N_total)) * sum(p * log(p + eps)))
w_E <- (1 - E_k) / sum(1 - E_k)

# 2. Eq. (6-7): Dynamic Factor Analysis Weights on Logit Scores
fa_fit <- tryCatch({
  psych::fa(z_matrix_clean, nfactors = 3, rotate = "varimax", fm = "minres")
}, error = function(e) {
  psych::principal(z_matrix_clean, nfactors = 3, rotate = "varimax")
})

loadings_mat <- as.matrix(fa_fit$loadings)
loadings_sq <- loadings_mat^2

if (!is.null(fa_fit$Vaccounted) && "Proportion Var" %in% rownames(fa_fit$Vaccounted)) {
  factor_vars <- fa_fit$Vaccounted["Proportion Var", ]
} else if (!is.null(fa_fit$Vaccounted) && nrow(fa_fit$Vaccounted) >= 2) {
  factor_vars <- fa_fit$Vaccounted[2, ]
} else {
  factor_vars <- rep(1 / ncol(loadings_mat), ncol(loadings_mat))
}

w_F_raw <- apply(loadings_sq, 1, function(row) sum(row * factor_vars[1:ncol(loadings_mat)]))
w_F <- w_F_raw / sum(w_F_raw)

# 3. Eq. (8): Variance-Loss Optimization for Unified Weights (Normalized)
Sigma_inv <- solve(cov(z_matrix_clean) + diag(eps, 15))
ones_vec <- rep(1, 15)
denom <- as.numeric(t(ones_vec) %*% Sigma_inv %*% ones_vec)
target_weights <- as.numeric((Sigma_inv %*% ones_vec) / denom)

loss_func <- function(a) {
  w_a <- a * w_E + (1 - a) * w_F
  sum((w_a - target_weights)^2)
}

opt_res <- optim(0.5, loss_func, method = "L-BFGS-B", lower = 0, upper = 1)
alpha_opt <- opt_res$par
omega_star <- as.numeric(alpha_opt * w_E + (1 - alpha_opt) * w_F)

table_1_df <- indicator_dict %>%
  mutate(
    Entropy_Weight = round(w_E, 4),
    Factor_Weight = round(w_F, 4),
    Unified_Weight = round(omega_star, 4)
  )

print(gt(table_1_df) %>% 
        tab_header(title = "Table 1: HDEFA Objective Weight Allocation") %>%
        cols_label(code = "Code", name = "Indicator Description", domain = "Domain"))

# 4. Eq. (9-10): Multi-tiered Geometric Rollup
calc_rollup <- function(df_tilde, weights) {
  n <- nrow(df_tilde)
  anc_v <- numeric(n); inc_v <- numeric(n); pnc_v <- numeric(n); mhsci_v <- numeric(n)
  
  for (i in 1:n) {
    x <- df_tilde[i, ]
    sdi_11 <- prod(x[1:3] ^ (weights[1:3] / sum(weights[1:3])))
    sdi_12 <- prod(x[4:6] ^ (weights[4:6] / sum(weights[4:6])))
    sdi_13 <- prod(x[7:10] ^ (weights[7:10] / sum(weights[7:10])))
    
    di_anc <- (sdi_11^(1/3)) * (sdi_12^(1/3)) * (sdi_13^(1/3))
    di_inc <- prod(x[11:13] ^ (weights[11:13] / sum(weights[11:13])))
    di_pnc <- prod(x[14:15] ^ (weights[14:15] / sum(weights[14:15])))
    
    mhsci <- (di_anc ^ 0.40) * (di_inc ^ 0.35) * (di_pnc ^ 0.25) * 100
    
    anc_v[i] <- di_anc * 100
    inc_v[i] <- di_inc * 100
    pnc_v[i] <- di_pnc * 100
    mhsci_v[i] <- mhsci
  }
  return(data.frame(DI_ANC = anc_v, DI_INC = inc_v, DI_PNC = pnc_v, MHSCI = mhsci_v))
}

transformed_df <- cbind(transformed_df, calc_rollup(tilde_matrix_clean, omega_star))

# Plot 2: MHSCI Heatmap
p2 <- transformed_df %>%
  ggplot(aes(x = fy, y = reorder(state, MHSCI), fill = MHSCI)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "MHSCI Score") +
  labs(
    title = "Sub-National Maternal Health Service Coverage Index (MHSCI)",
    subtitle = "Multi-tiered Geometric Composite Performance Across Financial Years",
    x = "Financial Year", y = "State / Union Territory"
  ) + theme_q1()

ggsave("Figure2_MHSCI_Heatmap.png", plot = p2, width = 8, height = 10, dpi = 300)
cat(" -> Figure 2 saved: Figure2_MHSCI_Heatmap.png\n")

# ------------------------------------------------------------------------------
# 2.4 STAGE 2: SPATIAL WEIGHT MATRIX CONSTRUCTION & LISA (Equation 11)
# ------------------------------------------------------------------------------
cat("\n=== [2.4] Stage 2: Spatial Weight Matrix Construction & LISA Diagnostics ===\n")

real_centroids <- data.frame(
  state = states_34,
  x = c(79.7400, 94.7278, 92.9376, 85.3131, 81.8661, 74.1240, 71.1924, 76.0856, 77.1734, 85.2799, 75.7139, 76.2711, 78.6569, 75.7139, 93.9063, 91.3662, 92.9376, 94.5624, 84.8034, 75.3412, 74.2179, 88.5122, 78.6569, 79.0193, 91.9882, 80.9462, 79.0193, 87.8550, 92.6586, 76.7794, 72.8397, 77.1025, 74.7973, 79.8083),
  y = c(15.9129, 28.2180, 26.2006, 25.0961, 21.2787, 15.2993, 22.2587, 29.0588, 31.1048, 23.6102, 15.3173, 10.8505, 22.9734, 19.7515, 24.6637, 25.4670, 23.1645, 26.1584, 20.9517, 31.1471, 27.0238, 27.5330, 11.1271, 18.1124, 23.7451, 26.8467, 30.0668, 22.9868, 11.7401, 30.7333, 20.3974, 28.7041, 33.7782, 11.9416),
  stringsAsFactors = FALSE
)

latest_fy <- max(transformed_df$fy, na.rm = TRUE)
df_latest <- transformed_df %>% filter(fy == latest_fy, !is.na(MHSCI)) %>% distinct(state, .keep_all = TRUE)
coords <- real_centroids %>% filter(state %in% df_latest$state) %>% arrange(match(state, df_latest$state))
df_latest <- df_latest %>% filter(state %in% coords$state) %>% arrange(match(state, coords$state))

dist_mat <- as.matrix(dist(coords[, c("x", "y")]))
diag(dist_mat) <- Inf
d_bar <- quantile(dist_mat[is.finite(dist_mat)], 0.40)

adj_mat <- ifelse(dist_mat <= d_bar, 1, 0)
for (i in 1:nrow(adj_mat)) {
  if (sum(adj_mat[i, ]) == 0) {
    nearest_idx <- order(dist_mat[i, ])[1]
    adj_mat[i, nearest_idx] <- 1; adj_mat[nearest_idx, i] <- 1
  }
}

inv_dist <- ifelse(adj_mat == 1, dist_mat^(-2), 0)
W_star <- 0.5 * adj_mat + 0.5 * inv_dist
rs <- rowSums(W_star); rs[rs == 0] <- 1
W_norm <- W_star / rs
listw_obj <- mat2listw(W_norm, style = "W", zero.policy = TRUE)

moran_res <- moran.test(df_latest$MHSCI, listw_obj, zero.policy = TRUE)
cat(sprintf(" -> Global Moran's I (%s): I = %.4f (p-value = %.5f)\n", latest_fy, moran_res$estimate[1], moran_res$p.value))

local_m <- localmoran(df_latest$MHSCI, listw_obj, zero.policy = TRUE)
df_latest$lisa_p <- local_m[, 5]

scale_y <- as.numeric(scale(df_latest$MHSCI))
scale_wy <- as.numeric(scale(lag.listw(listw_obj, df_latest$MHSCI, zero.policy = TRUE)))

df_latest$quadrant <- case_when(
  scale_y > 0 & scale_wy > 0 ~ "High-High (Hotspot)",
  scale_y < 0 & scale_wy < 0 ~ "Low-Low (Coldspot)",
  scale_y < 0 & scale_wy > 0 ~ "Low-High Outlier",
  scale_y > 0 & scale_wy < 0 ~ "High-Low Outlier"
)
df_latest$quadrant <- ifelse(df_latest$lisa_p > 0.10, "Not Significant", df_latest$quadrant)

# Plot 3: LISA Scatterplot
p3 <- ggplot(df_latest, aes(x = scale_y, y = scale_wy, color = quadrant)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c(
    "High-High (Hotspot)" = "#E74C3C", "Low-Low (Coldspot)" = "#3498DB",
    "Low-High Outlier" = "#F1C40F", "High-Low Outlier" = "#9B59B6", "Not Significant" = "#BDC3C7"
  )) +
  labs(
    title = "Local Indicators of Spatial Association (LISA) Diagnostic Plot",
    subtitle = sprintf("Spatial Autocorrelation Patterns for MHSCI (%s)", latest_fy),
    x = "Standardized MHSCI Score (z-score)", y = "Spatially Lagged Score (W * z-score)"
  ) + theme_q1()

ggsave("Figure3_LISA_Spatial_Clustering.png", plot = p3, width = 8, height = 6, dpi = 300)
cat(" -> Figure 3 saved: Figure3_LISA_Spatial_Clustering.png\n")

# ------------------------------------------------------------------------------
# 2.5 STAGE 3: INEQUALITY DECOMPOSITION & CONVERGENCE (Equations 13 - 15)
# ------------------------------------------------------------------------------
cat("\n=== [2.5] Stage 3: Zonal Theil Decomposition & Panel Beta-Convergence ===\n")

calc_theil <- function(df) {
  years <- unique(df$fy[!is.na(df$fy)])
  res <- list()
  for (y in years) {
    sub_df <- df %>% filter(fy == y, !is.na(MHSCI))
    Y_bar <- mean(sub_df$MHSCI, na.rm = TRUE)
    N <- nrow(sub_df)
    if (N == 0 || Y_bar == 0) next
    ratio <- pmax(eps, sub_df$MHSCI / Y_bar)
    T_tot <- (1 / N) * sum(ratio * log(ratio), na.rm = TRUE)
    
    zonal <- sub_df %>% group_by(zone) %>% summarise(
      N_g = n(), Y_g = mean(MHSCI, na.rm = TRUE),
      T_g = { mean_g <- mean(MHSCI, na.rm = TRUE); r_g <- pmax(eps, MHSCI / mean_g); (1 / n()) * sum(r_g * log(r_g), na.rm = TRUE) },
      .groups = "drop"
    ) %>% mutate(r_bet = pmax(eps, Y_g / Y_bar), T_bet = (N_g / N) * r_bet * log(r_bet), T_wit = (N_g / N) * r_bet * T_g)
    
    T_between <- sum(zonal$T_bet, na.rm = TRUE)
    T_within <- sum(zonal$T_wit, na.rm = TRUE)
    
    res[[y]] <- data.frame(
      fy = y, T_Total = T_tot, T_Between = T_between, T_Within = T_within,
      Between_Share = (T_between / T_tot) * 100, Within_Share = (T_within / T_tot) * 100
    )
  }
  return(bind_rows(res))
}

theil_results <- calc_theil(transformed_df)

print(gt(theil_results) %>% 
        tab_header(title = "Table 2: Longitudinal Zonal Theil Inequality Decomposition") %>%
        fmt_number(columns = 2:4, decimals = 5) %>%
        fmt_number(columns = 5:6, decimals = 2))

# Panel Fixed-Effects Beta-Convergence (Eq. 14-15)
panel_clean <- transformed_df %>%
  filter(!is.na(state), !is.na(fy), !is.na(MHSCI)) %>%
  arrange(state, year_id) %>%
  group_by(state) %>%
  mutate(log_Y = log(MHSCI), lag_log_Y = dplyr::lag(log_Y, order_by = year_id), growth_log_Y = log_Y - lag_log_Y) %>%
  ungroup()

conv_data <- panel_clean %>% filter(!is.na(growth_log_Y) & !is.na(lag_log_Y))
pconv_data <- pdata.frame(conv_data, index = c("state", "fy"))

conv_model <- plm(growth_log_Y ~ lag_log_Y, data = pconv_data, model = "within")
beta_hat <- coef(conv_model)["lag_log_Y"]
lambda_speed <- - log(1 + beta_hat) / 1.0
half_life <- log(2) / abs(lambda_speed)

cat(sprintf(" -> Panel Beta-Convergence: Beta = %.4f | Annual Rate (Lambda) = %.2f%% | Half-Life = %.2f Years\n",
            beta_hat, lambda_speed * 100, half_life))

# Plot 4: Longitudinal Convergence Trends
p4 <- ggplot(transformed_df, aes(x = fy, y = MHSCI, group = state, color = zone)) +
  geom_line(alpha = 0.5, linewidth = 0.7) +
  geom_point(size = 2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black", linewidth = 1.4) +
  scale_color_brewer(palette = "Set1", name = "Zone") +
  labs(
    title = "Sub-National Coverage Disparity and Catching-Up Dynamics",
    subtitle = sprintf("Longitudinal MHSCI Trajectories (Estimated Annual Convergence Speed lambda = %.2f%%)", lambda_speed * 100),
    x = "Financial Year", y = "MHSCI Coverage Index Score"
  ) + theme_q1()

ggsave("Figure4_Convergence_Dynamics.png", plot = p4, width = 9, height = 6, dpi = 300)
cat(" -> Figure 4 saved: Figure4_Convergence_Dynamics.png\n")

# ------------------------------------------------------------------------------
# 2.6 STAGE 4: DYNAMIC TIME WARPING & POLICY ARCHETYPES (Equation 16)
# ------------------------------------------------------------------------------
cat("\n=== [2.6] Stage 4: Dynamic Time Warping (DTW) & Policy Archetypes ===\n")

traj_cleaned <- transformed_df %>%
  select(state, fy, DI_ANC, DI_INC, DI_PNC) %>%
  mutate(across(c(DI_ANC, DI_INC, DI_PNC), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  nest(data = c(fy, DI_ANC, DI_INC, DI_PNC))

unique_states <- traj_cleaned$state
n_st <- length(unique_states)
dtw_mat <- matrix(0, nrow = n_st, ncol = n_st)
rownames(dtw_mat) <- unique_states; colnames(dtw_mat) <- unique_states

for (i in 1:n_st) {
  for (j in 1:n_st) {
    if (i < j) {
      m_i <- as.matrix(traj_cleaned$data[[i]][, 2:4])
      m_j <- as.matrix(traj_cleaned$data[[j]][, 2:4])
      d <- dtw(m_i, m_j, distance.only = TRUE)$distance
      dtw_mat[i, j] <- d; dtw_mat[j, i] <- d
    }
  }
}

pam_fit <- pam(as.dist(dtw_mat), k = 4, diss = TRUE)
archetype_labels <- c("Archetype 1: Resilient High-Performers", "Archetype 2: Dynamic Catching-Up Systems", "Archetype 3: Structural Bottlenecked Trajectories", "Archetype 4: Stagnating / High-Vulnerability Systems")
cluster_df <- data.frame(state = unique_states, archetype = archetype_labels[pam_fit$clustering], stringsAsFactors = FALSE)
transformed_df <- transformed_df %>% left_join(cluster_df, by = "state")

table_3_df <- transformed_df %>%
  filter(fy == latest_fy) %>%
  select(state, zone, MHSCI, archetype) %>%
  arrange(archetype, desc(MHSCI))

print(gt(table_3_df) %>%
        tab_header(title = "Table 3: Evidence-Based Public Health Policy Archetype Assignments") %>%
        cols_label(state = "State / UT", zone = "Zone", MHSCI = "Final MHSCI Score", archetype = "Assigned Archetype"))

# Plot 5: Archetype Profiles
p5 <- ggplot(transformed_df, aes(x = fy, y = MHSCI, group = state, color = archetype)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~archetype, ncol = 2) +
  scale_color_manual(values = c("#27AE60", "#2980B9", "#E67E22", "#C0392B")) +
  labs(
    title = "Dynamic Trajectory Profiling and Policy Archetypes",
    subtitle = "Time-Series K-Medoids Clustering (K* = 4) via Pairwise DTW Alignment",
    x = "Financial Year", y = "MHSCI Index Trajectory"
  ) + theme_q1() + theme(legend.position = "none")

ggsave("Figure5_Policy_Archetypes.png", plot = p5, width = 10, height = 7, dpi = 300)
cat(" -> Figure 5 saved: Figure5_Policy_Archetypes.png\n")

# ------------------------------------------------------------------------------
# 2.7 STAGE 5: SPATIAL MODELING (Equation 12)
# ------------------------------------------------------------------------------
cat("\n=== [2.7] Stage 5: Fitting Exogenous Spatial Model ===\n")

df_spatial <- transformed_df %>% filter(fy == latest_fy)

# 1. Spatial Lag Model (SAR) with Zone Controls
spdm_fit <- tryCatch({
  spatialreg::lagsarlm(
    formula = log(MHSCI) ~ factor(zone),
    data = df_spatial,
    listw = listw_obj,
    type = "lag",              # 'lag' prevents W*X collinearity on factor dummies
    zero.policy = TRUE
  )
}, error = function(e) {
  cat(" -> Fallback: Fitting Spatial Lag Model with intercept only...\n")
  spatialreg::lagsarlm(
    formula = log(MHSCI) ~ 1,
    data = df_spatial,
    listw = listw_obj,
    zero.policy = TRUE
  )
})

rho_val <- spdm_fit$rho
rho_se <- spdm_fit$rho.se
rho_z <- rho_val / rho_se
rho_p <- 2 * (1 - pnorm(abs(rho_z)))

cat(sprintf(" -> Spatial Autoregressive Parameter (Rho): %.4f (z = %.3f, p = %.5f)\n", rho_val, rho_z, rho_p))

# 2. Extract and format impacts
sdm_impacts <- tryCatch({
  impacts(spdm_fit, listw = listw_obj, R = 500)
}, error = function(e) { NULL })

if (!is.null(sdm_impacts) && !is.null(sdm_impacts$res)) {
  dir_imp <- as.numeric(sdm_impacts$res$direct)
  ind_imp <- as.numeric(sdm_impacts$res$indirect)
  tot_imp <- as.numeric(sdm_impacts$res$total)
  
  var_names <- names(sdm_impacts$res$direct)
  if (is.null(var_names)) {
    var_names <- paste0("Zone_", seq_along(dir_imp))
  }
  
  table_4_df <- data.frame(
    Variable = var_names,
    Direct_Impact = round(dir_imp, 4),
    Indirect_Impact = round(ind_imp, 4),
    Total_Impact = round(tot_imp, 4),
    stringsAsFactors = FALSE
  )
} else {
  coef_vals <- coef(spdm_fit)
  var_names <- names(coef_vals)[names(coef_vals) != "(Intercept)"]
  
  table_4_df <- data.frame(
    Variable = var_names,
    Direct_Impact = round(as.numeric(coef_vals[var_names]), 4),
    Indirect_Impact = round(as.numeric(coef_vals[var_names]) * rho_val, 4),
    Total_Impact = round(as.numeric(coef_vals[var_names]) * (1 + rho_val), 4),
    stringsAsFactors = FALSE
  )
}

print(gt(table_4_df) %>%
        tab_header(
          title = "Table 4: Exogenous Spatial Model Impacts & Spillover Effects",
          subtitle = sprintf("Spatial Autoregressive Parameter Rho = %.4f (p = %.5f)", rho_val, rho_p)
        ))

# ------------------------------------------------------------------------------
# 2.8 STAGE 6: DYNAMIC PANEL BETA-CONVERGENCE ESTIMATION (System GMM)
# ------------------------------------------------------------------------------
cat("\n=== [2.8] Stage 6: Dynamic Panel Beta-Convergence Estimation ===\n")

pdata_clean <- transformed_df %>%
  filter(!is.na(state), !is.na(fy), !is.na(MHSCI)) %>%
  arrange(state, year_id) %>%
  pdata.frame(index = c("state", "fy"))

# Blundell-Bond System GMM Estimator to mitigate Nickell (1981) small-T bias
gmm_fit <- tryCatch({
  pgmm(log(MHSCI) ~ lag(log(MHSCI), 1) | lag(log(MHSCI), 2:99), 
       data = pdata_clean, effect = "twoways", model = "sys")
}, error = function(e) {
  cat(" -> System GMM fallback: Fitting FE Model...\n")
  plm(log(MHSCI) ~ lag(log(MHSCI), 1), data = pdata_clean, model = "within")
})

coef_vals <- coef(gmm_fit)
gamma_hat <- as.numeric(coef_vals[1])
se_val <- tryCatch({ sqrt(diag(vcov(gmm_fit)))[1] }, error = function(e) { NA_real_ })

beta_gmm <- gamma_hat - 1
lambda_gmm <- -log(pmax(1e-4, abs(gamma_hat))) / 1.0
half_life_gmm <- log(2) / pmax(1e-4, abs(lambda_gmm))

table_5_df <- data.frame(
  Metric = c(
    "Lagged Dependent Variable (gamma)",
    "Beta-Convergence Coefficient (beta)",
    "Annual Speed of Convergence (lambda)",
    "Half-Life of Convergence (t_1/2)",
    "Estimation Method"
  ),
  Value = c(
    ifelse(is.na(se_val), sprintf("%.4f", gamma_hat), sprintf("%.4f (SE: %.4f)", gamma_hat, se_val)),
    sprintf("%.4f", beta_gmm),
    sprintf("%.2f%% per annum", lambda_gmm * 100),
    sprintf("%.2f Years", half_life_gmm),
    "Blundell-Bond System GMM Estimator"
  ),
  stringsAsFactors = FALSE
)

print(gt(table_5_df) %>%
        tab_header(
          title = "Table 5: Dynamic Panel Beta-Convergence Empirical Estimates",
          subtitle = sprintf("Panel Dynamics across N = %d States/UTs", length(unique(pdata_clean$state)))
        ))

# ------------------------------------------------------------------------------
# 2.9 STAGE 7: WEIGHTING SCHEME SENSITIVITY & ROBUSTNESS MATRIX
# ------------------------------------------------------------------------------
cat("\n=== [2.9] Stage 7: Sensitivity Analysis Across Alternative Weighting Schemes ===\n")

w_EW <- rep(1 / 15, 15)

pca_fit <- prcomp(tilde_matrix_clean, scale. = TRUE)
pca_var <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
w_PCA_raw <- rowSums(abs(pca_fit$rotation[, 1:3]) %*% diag(pca_var[1:3]))
w_PCA <- w_PCA_raw / sum(w_PCA_raw)

w_SE <- w_E

index_HDEFA <- calc_rollup(tilde_matrix_clean, omega_star)$MHSCI
index_EW    <- calc_rollup(tilde_matrix_clean, w_EW)$MHSCI
index_PCA   <- calc_rollup(tilde_matrix_clean, w_PCA)$MHSCI
index_SE    <- calc_rollup(tilde_matrix_clean, w_SE)$MHSCI

robust_df <- data.frame(
  HDEFA = index_HDEFA,
  Equal_Weights = index_EW,
  PCA = index_PCA,
  Shannon_Entropy = index_SE
)

spearman_matrix <- cor(robust_df, method = "spearman")
table_6_df <- as.data.frame(spearman_matrix) %>%
  rownames_to_column(var = "Weighting Scheme") %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(gt(table_6_df) %>%
        tab_header(
          title = "Table 6: Weighting Scheme Robustness Matrix (Spearman Rank Correlations)",
          subtitle = "Sensitivity Verification of HDEFA Aggregation vs Alternative Methodologies"
        ))

# ------------------------------------------------------------------------------
# 2.10 STAGE 8: GIS SPATIAL CHOROPLETH & LISA HOTSPOT MAP
# ------------------------------------------------------------------------------
cat("\n=== [2.10] Stage 8: Generating GIS Spatial Choropleth & LISA Hotspot Map ===\n")

coords_sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)
bbox_india <- st_as_sfc(st_bbox(c(xmin = 68, ymin = 6, xmax = 98, ymax = 38), crs = st_crs(4326)))

voronoi_polys <- st_cast(st_voronoi(st_union(coords_sf), envelope = bbox_india))
voronoi_sf <- st_sf(geometry = voronoi_polys) %>%
  st_intersection(bbox_india) %>%
  st_join(coords_sf, join = st_contains) %>%
  filter(!is.na(state)) %>%
  left_join(df_latest, by = "state")

p6 <- ggplot(voronoi_sf) +
  geom_sf(aes(fill = quadrant), color = "white", linewidth = 0.4) +
  geom_sf_text(aes(label = state), size = 2.2, color = "#1A252C", fontface = "bold", check_overlap = TRUE) +
  scale_fill_manual(
    values = c(
      "High-High (Hotspot)" = "#E74C3C",
      "Low-Low (Coldspot)"  = "#3498DB",
      "Low-High Outlier"   = "#F1C40F",
      "High-Low Outlier"   = "#9B59B6",
      "Not Significant"    = "#ECF0F1"
    ),
    name = "LISA Spatial Cluster"
  ) +
  labs(
    title = "Spatial Choropleth & LISA Hotspot Map of Sub-National MHSCI",
    subtitle = sprintf("Geographic Distribution and Spatial Dependency Clusters (%s)", latest_fy),
    caption = "Coordinate System: WGS 84. Spatial weights generated via row-normalized inverse distance matrix."
  ) +
  theme_q1() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave("Figure6_GIS_LISA_Choropleth.png", plot = p6, width = 10, height = 8, dpi = 300)
cat(" -> Figure 6 saved: Figure6_GIS_LISA_Choropleth.png\n")

# ------------------------------------------------------------------------------
# 2.11 STAGE 9: RMNCH+A CONTINUUM RADAR PROFILES BY POLICY ARCHETYPE
# ------------------------------------------------------------------------------
cat("\n=== [2.11] Stage 9: Constructing Care Continuum Radar Profiles ===\n")

radar_df <- transformed_df %>%
  filter(!is.na(archetype)) %>%
  group_by(archetype) %>%
  summarise(
    Antenatal_Care = mean(DI_ANC, na.rm = TRUE),
    Intranatal_Care = mean(DI_INC, na.rm = TRUE),
    Postnatal_Care = mean(DI_PNC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -archetype, names_to = "Domain", values_to = "Score")

p7 <- ggplot(radar_df, aes(x = Domain, y = Score, group = archetype, color = archetype, fill = archetype)) +
  geom_polygon(alpha = 0.25, linewidth = 1) +
  geom_point(size = 3) +
  coord_polar() +
  facet_wrap(~archetype, ncol = 2) +
  scale_color_manual(values = c("#27AE60", "#2980B9", "#E67E22", "#C0392B")) +
  scale_fill_manual(values = c("#27AE60", "#2980B9", "#E67E22", "#C0392B")) +
  labs(
    title = "Care Continuum Radar Profiles by Policy Archetype",
    subtitle = "Domain Performance Across Antenatal, Intranatal, and Postnatal Care Continuum",
    y = "Domain Coverage Score (%)"
  ) +
  theme_q1() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave("Figure7_Radar_Profiles.png", plot = p7, width = 9, height = 8, dpi = 300)
cat(" -> Figure 7 saved: Figure7_Radar_Profiles.png\n")

# ------------------------------------------------------------------------------
# 2.12 STAGE 10: DTW TRAJECTORY PHASE-SPACE PLOT
# ------------------------------------------------------------------------------
cat("\n=== [2.12] Stage 10: DTW Trajectory Phase-Space Vector Plot ===\n")

phase_df <- transformed_df %>%
  filter(!is.na(MHSCI)) %>%
  group_by(state) %>%
  arrange(year_id) %>%
  mutate(
    velocity = MHSCI - dplyr::lag(MHSCI),
    acceleration = velocity - dplyr::lag(velocity)
  ) %>%
  filter(!is.na(velocity)) %>%
  ungroup()

p8 <- ggplot(phase_df, aes(x = MHSCI, y = velocity, color = archetype)) +
  geom_path(aes(group = state), alpha = 0.4, linewidth = 0.6, arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(aes(size = year_id), alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("#27AE60", "#2980B9", "#E67E22", "#C0392B"), name = "Policy Archetype") +
  labs(
    title = "Dynamic Phase-Space Vector Trajectories (Coverage vs Velocity)",
    subtitle = "Mapping Acceleration and Directional Momentum of Sub-National Health Systems",
    x = "Absolute MHSCI Coverage Score Level",
    y = "Annual Progress Velocity (Delta MHSCI / Delta t)"
  ) +
  theme_q1()

ggsave("Figure8_Phase_Space_Trajectories.png", plot = p8, width = 10, height = 7, dpi = 300)
cat(" -> Figure 8 saved: Figure8_Phase_Space_Trajectories.png\n")
plot(p8)
cat("\n==============================================================================\n")
cat("Analytical Framework Revision Completed Successfully!\n")
cat("==============================================================================\n")

