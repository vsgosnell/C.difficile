library(ggplot2)
library(readr)
library(dplyr)

read_xvg <- function(file) {
  # Read all lines from file
  lines <- readLines(file)

  # Filter out comment lines that start with @ or #
  data_lines <- lines[!grepl("^[@#]", lines)]

  # Combine data lines and read as table
  read_table2(paste(data_lines, collapse = "\n"), col_names = FALSE)
}


# Radius of Gyration
rg_data <- read_xvg("/Users/veronicagosnell/Desktop/R/C.difficile/Images/radius_of_gyration.xvg")

# Rename if it has 2 or 5 columns:
if (ncol(rg_data) == 2) {
  colnames(rg_data) <- c("time", "Rg")
} else if (ncol(rg_data) >= 5) {
  colnames(rg_data)[1:5] <- c("time", "RgX", "RgY", "RgZ", "Rg")
}


# Make sure it's numeric
rg_data <- rg_data %>% mutate(across(everything(), as.numeric))


# H-bonds within the protein
hb_data <- read_xvg("/Users/veronicagosnell/Desktop/R/C.difficile/Images/hbnum_intra.xvg")

# Read RMSD
rmsdev_data <- read_xvg("/Users/veronicagosnell/Desktop/R/C.difficile/Images/rmsdev.xvg")

# Read per-residue RMSF
rmsf_data <- read_xvg("/Users/veronicagosnell/Desktop/R/C.difficile/Images/rmsf-per-res.xvg")


# plot Radius of Gyration
ggplot(rg_data, aes(x = time, y = Rg)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  labs(title = "Radius of Gyration (Rg)",
       x = "Time (ps)",
       y = "Rg (nm)") +
  theme_minimal(base_size = 14)



# Rename columns for clarity
hb_data <- hb_data %>%
  rename(time = X1, intramol = X2, total = X3)

# Plot intramolecular H-bonds
ggplot(hb_data, aes(x = time, y = intramol)) +
  geom_line(color = "#B03A2E", size = 1) +
  labs(title = "Intramolecular Hydrogen Bonds",
       x = "Time (ps)",
       y = "Number of H-bonds") +
  theme_minimal(base_size = 14)


# Rename columns for clarity if not done already
rmsdev_data <- rmsdev_data %>%
  rename(time = X1, rmsd = X2)


# Plot RMSD
ggplot(rmsdev_data, aes(x = time, y = rmsf_deviation)) +
  geom_line(color = "#884EA0", linewidth = 1) +
  labs(title = "RMSD",
       x = "Time (ps)",
       y = "RMSD (nm)") +
  theme_minimal(base_size = 14)



# rename columns for clarity
rmsf_data <- rmsf_data %>%
  rename(time = X1, rmsf_deviation = X2)


# plot RMSF per residue
# highlights flexibility of individual residues
ggplot(rmsf_data, aes(x = time, y = rmsf_deviation)) +
  geom_line(color = "#884EA0", linewidth = 1) +
  labs(title = "RMSF per-residue",
       x = "Time (ps)",
       y = "RMSF Deviation (nm)") +
  theme_minimal(base_size = 14)





