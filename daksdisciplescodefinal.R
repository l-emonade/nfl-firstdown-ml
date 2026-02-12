# Libraries
library(corrplot)
library(car)

nfl <- read.csv("NFLData.csv")

# Keep run/pass plays only
nfl_clean <- subset(nfl, play_type %in% c("run", "pass"))

# Select variables
nfl_model <- nfl_clean[, c(
  "yards_gained", "down", "ydstogo", "qtr",
  "score_differential", "play_type", "game_seconds_remaining"
)]

# Remove missing values
nfl_model <- na.omit(nfl_model)

# Remove invalid football values
nfl_model <- subset(
  nfl_model,
  down %in% 1:4 &
    qtr %in% 1:4 &
    ydstogo >= 0 &
    game_seconds_remaining >= 0 &
    yards_gained >= 0 & yards_gained <= 100
)

# Sample for plotting/modeling
set.seed(123)
nfl_model <- nfl_model[sample(nrow(nfl_model), 2000), ]

# Convert to factors
nfl_model$down <- as.factor(nfl_model$down)
nfl_model$qtr <- as.factor(nfl_model$qtr)
nfl_model$play_type <- as.factor(ifelse(nfl_model$play_type == "pass", 1, 0))

# Save cleaned data
write.csv(nfl_model, "nfl_model_cleaned.csv", row.names = FALSE)

pdf("EDA_plots_full.pdf", width = 10, height = 6)

# Response distribution
hist(nfl_model$yards_gained,
     main = "Distribution of Yards Gained",
     xlab = "Yards Gained",
     breaks = 30)

# Scatterplots (numeric predictors)
par(mfrow = c(1,3))
plot(nfl_model$ydstogo, nfl_model$yards_gained,
     main = "Yards vs Yards To Go", xlab = "Yards To Go", ylab = "Yards Gained")
plot(nfl_model$game_seconds_remaining, nfl_model$yards_gained,
     main = "Yards vs Time Remaining", xlab = "Seconds Remaining", ylab = "Yards Gained")
plot(nfl_model$score_differential, nfl_model$yards_gained,
     main = "Yards vs Score Differential", xlab = "Score Differential", ylab = "Yards Gained")
par(mfrow = c(1,1))

# Boxplots (categorical predictors)
par(mfrow = c(1,3))
boxplot(yards_gained ~ down, data = nfl_model, main = "Yards by Down")
boxplot(yards_gained ~ play_type, data = nfl_model, main = "Yards by Play Type")
boxplot(yards_gained ~ qtr, data = nfl_model, main = "Yards by Quarter")
par(mfrow = c(1,1))

# Correlation heatmap (numeric only)
num_vars <- nfl_model[, c("yards_gained", "ydstogo",
                          "score_differential", "game_seconds_remaining")]
cor_matrix <- cor(num_vars)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", addCoef.col = "black")

dev.off()

fit_linear <- lm(
  yards_gained ~ ydstogo + down + play_type +
    score_differential + game_seconds_remaining + qtr,
  data = nfl_model
)

summary(fit_linear)

# Diagnostics
par(mfrow = c(2,2))
plot(fit_linear)
par(mfrow = c(1,1))

nfl_pos <- subset(nfl_model, yards_gained > 0)

fit_log <- lm(
  log(yards_gained) ~ ydstogo + down + play_type +
    score_differential + game_seconds_remaining + qtr,
  data = nfl_pos
)

summary(fit_log)

pdf("log_model_diagnostics.pdf", width = 10, height = 8)

hist(log(nfl_pos$yards_gained),
     main = "Distribution of log(Yards Gained)",
     xlab = "log(Yards Gained)")

par(mfrow = c(2,2))
plot(fit_log)
par(mfrow = c(1,1))

dev.off()

gvif_vals <- vif(fit_log)

gvif_scaled <- gvif_vals
gvif_scaled[, "GVIF^(1/(2*Df))"] <-
  gvif_vals[, "GVIF"]^(1 / (2 * gvif_vals[, "Df"]))

gvif_scaled

fit_log_step <- step(
  fit_log,
  direction = "both",
  trace = FALSE
)

summary(fit_log_step)

log(yards_gained) ~ ydstogo + play_type

fit_log_final <- lm(
  log(yards_gained) ~ ydstogo + play_type,
  data = nfl_pos
)

summary(fit_log_final)

# Sequential ANOVA
anova(fit_log_final)

# Nested ANOVA
fit_log_reduced <- lm(log(yards_gained) ~ play_type, data = nfl_pos)
anova(fit_log_reduced, fit_log_final)


