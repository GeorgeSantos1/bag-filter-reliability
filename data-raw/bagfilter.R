# ==============================================================================
# Data Extraction & Preparation: 'bagfilter' Dataset (Recorte 02 - Article)
#
# Source: Raw industrial process data from a bag filter system
# (Bagfilter_Dataset.xlsx)
# ==============================================================================

library(readxl)
library(dplyr)
library(lubridate)

# 1. Path to raw process spreadsheet in data-raw
raw_file <- "data-raw/Bagfilter_Dataset.xlsx"
if (!file.exists(raw_file)) {
  stop("Raw data file not found: ", raw_file)
}

# 2. Read raw process data
df <- read_excel(raw_file)
df <- df[-1, ]  # Remove auxiliary unit row

# 3. Target time horizon for Recorte 02 (as analyzed in the article/dissertation)
i_time <- ymd_hms("2024-05-04 14:40:08")
f_time <- ymd_hms("2024-05-05 00:00:00")

dh <- df[["Data Hora"]]
df_aux <- df[dh >= i_time & dh < f_time, ]

# 4. Sampling / Thinning step (30 measurement steps)
step <- 30
index <- seq(1, nrow(df_aux), by = step)

# Exact indices immediately preceding and succeeding maintenance pulse actions
cx <- c(1, 391, 392, 781, 783, 1171, 1173)

# Combine and sort unique measurement points
index <- sort(unique(c(index, cx)))
df_aux <- df_aux[index, ]

# 5. Discrete inspection time scale with maintenance actions at t = 13, 26, 39
s1 <- seq(1, 14)
for (i in 1:3) {
  s2 <- seq(max(s1), max(s1) + 13)
  s1 <- c(s1, s2)
}

df_aux[["Time"]] <- s1[1:nrow(df_aux)] - 1

# 6. Format final data frame preserving all process variables
bagfilter <- as.data.frame(df_aux)
names(bagfilter)[names(bagfilter) == "Data Hora"] <- "Data_Hora"
names(bagfilter)[5] <- "Y"
bagfilter[["Objeto"]] <- "OBJ_001"

num_cols <- c(
  "Batimento_FM_Passo",
  "Entrada_mmCa_800PT8101",
  "Saida_mmCa_800PT8102",
  "Y",
  "Corrente_exaustor_AIC800EXA001",
  "Time"
)

for (col in num_cols) {
  if (col %in% names(bagfilter)) {
    bagfilter[[col]] <- as.numeric(bagfilter[[col]])
  }
}

# 7. Save compressed dataset into data/
if (!dir.exists("data")) {
  dir.create("data")
}
save(bagfilter, file = "data/bagfilter.rda", compress = "xz")
message("Successfully generated 'bagfilter.rda' with ", nrow(bagfilter), " rows and ", ncol(bagfilter), " columns.")
