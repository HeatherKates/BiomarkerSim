# Load necessary libraries
library(lme4)
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Parameters
n_patients <- 50
n_weeks <- 12
n_technicians <- 3

# Biomarker baseline values (mean and sd)
biomarkers <- data.frame(
  biomarker = c("Aβ42", "Tau", "NfL"),
  mean = c(200, 50, 30),
  sd = c(30, 10, 5)
)

# Generate patient-specific random effects
patient_effects <- rnorm(n_patients, mean = 0, sd = 10)
technician_effects <- rnorm(n_technicians, mean = 0, sd = 5)

# Generate data
data <- expand.grid(
  patient = 1:n_patients,
  week = 1:n_weeks,
  technician = 1:n_technicians
) %>%
  mutate(
    biomarker = rep(biomarkers$biomarker, each = n_patients * n_weeks * n_technicians / 3),
    baseline = rep(biomarkers$mean, each = n_patients * n_weeks * n_technicians / 3),
    sd = rep(biomarkers$sd, each = n_patients * n_weeks * n_technicians / 3),
    patient_effect = rep(patient_effects, each = n_weeks * n_technicians * 3 / 3),
    technician_effect = rep(technician_effects, times = n_patients * n_weeks * 3 / 3),
    time_effect = rnorm(n_patients * n_weeks * n_technicians, mean = 0, sd = 5),
    measurement = baseline + patient_effect + technician_effect + time_effect + rnorm(n_patients * n_weeks * n_technicians, mean = 0, sd = sd)
  )

# Summarize the first few rows of the data
head(data)

# Fit the mixed-effects model
model <- lmer(measurement ~ week + (1 | patient) + (1 | technician), data = data)

# Summarize the model
summary(model)

