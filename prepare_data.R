# STAT 442 | Eli Homan | 2024 rural and urban fatal crash comparison
raw <- read.csv("accident_2024.csv", stringsAsFactors = FALSE)
stopifnot(all(c("STATE", "ST_CASE", "YEAR", "STATENAME", "RUR_URBNAME",
                "FUNC_SYSNAME", "MAN_COLLNAME") %in% names(raw)))
stopifnot(!anyDuplicated(raw[c("STATE", "ST_CASE", "YEAR")]))
stopifnot(all(raw$YEAR == 2024))

collision_labels <- c(
  "Angle" = "Angle collision",
  "Front-to-Front" = "Head-on collision",
  "Front-to-Rear" = "Rear-end collision",
  "Sideswipe - Opposite Direction" = "Sideswipe: opposite direction",
  "Sideswipe - Same Direction" = "Sideswipe: same direction",
  "Rear-to-Rear" = "Other vehicle collision",
  "Rear-to-Side" = "Other vehicle collision",
  "Other" = "Other vehicle collision",
  "The First Harmful Event was Not a Collision with a Motor Vehicle in Transport" =
    "First event: not a moving-vehicle collision"
)
clean <- data.frame(state = raw$STATENAME, area = raw$RUR_URBNAME,
                    road = raw$FUNC_SYSNAME,
                    collision = unname(collision_labels[raw$MAN_COLLNAME]),
                    stringsAsFactors = FALSE)
clean$collision[is.na(clean$collision)] <- "Unknown / not reported"
clean$n <- 1L
clean <- aggregate(n ~ state + area + road + collision, clean, sum)
dir.create("data", showWarnings = FALSE)
saveRDS(clean, "data/crash_summary.rds")
message("Saved ", sum(clean$n), " crashes in data/crash_summary.rds")