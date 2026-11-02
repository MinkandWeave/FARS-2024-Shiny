
################################################################
# Shiny app by Eli Homan | STAT 442 | November 2026
# Compares collision patterns among 2024 rural and urban fatal crashes.
# Source: NHTSA FARS accident file.
# Percentages describe fatal crash patterns, not driving risk.
################################################################

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Load prepared data
if (!file.exists("data/crash_summary.rds")) {
  stop("Run source('prepare_data.R') before starting the app.")
}

crashes <- readRDS("data/crash_summary.rds")

areas <- c("Rural", "Urban")
unknown <- "Unknown / not reported"

categories <- c(
  "Angle collision",
  "Head-on collision",
  "Rear-end collision",
  "Sideswipe: opposite direction",
  "Sideswipe: same direction",
  "Other vehicle collision",
  "First event: not a moving-vehicle collision",
  unknown
)

# Shorter graph labels
short_labels <- c(
  "Angle collision" = "Angle",
  "Head-on collision" = "Head-on",
  "Rear-end collision" = "Rear-end",
  "Sideswipe: opposite direction" = "Sideswipe (opposite)",
  "Sideswipe: same direction" = "Sideswipe (same)",
  "Other vehicle collision" = "Other vehicle collision",
  "First event: not a moving-vehicle collision" = "Other first harmful event",
  "Unknown / not reported" = "Unknown / not reported"
)

# Available roadway classifications
roads <- sort(setdiff(
  unique(crashes$road),
  c(
    "Unknown",
    "Not Reported",
    "Trafficway Not in State Inventory"
  )
))

# Friendly display labels for roadway types.
# Original classification values are preserved for filtering.
road_labels <- c(
  "Interstate" = "Interstate highway",
  "Local" = "Local street or road",
  "Major Collector" = "Major connecting road",
  "Minor Collector" = "Minor connecting road",
  "Minor Arterial" = "Minor arterial (smaller major road)",
  "Principal Arterial" = "Principal arterial (major road)"
)

# Named choices: visible label = original roadway value
road_display <- roads
matched <- roads %in% names(road_labels)
road_display[matched] <- unname(road_labels[roads[matched]])

road_choices <- setNames(roads, road_display)

colors <- c(
  Rural = "#24756B",
  Urban = "#8055A6"
)

################################################################
# USER INTERFACE
################################################################

ui <- page_sidebar(
  
  title = "How do Rural and Urban Fatal Crashes Differ?",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#24756B"
  ),
  
  fillable = FALSE,
  
  sidebar = sidebar(
    
    title = "Choose a comparison",
    
    selectInput(
      "state",
      "State",
      choices = c(
        "All states",
        sort(unique(crashes$state))
      )
    ),
    
    selectInput(
      "road",
      "Road type",
      choices = c(
        "All road types" = "All road types",
        road_choices
      )
    ),
    
    p(
      "Compare rural and urban crashes within the same selected location and road category."
    ),
    
    hr(),
    
    p("2024 | NHTSA FARS"),
    
    helpText(
      "Road types describe how roads connect places, from major highways to neighborhood streets."
    )
  ),
  
  # Introduction
  p(
    class = "lead",
    paste(
      "Explore how collision patterns differ among recorded",
      "fatal crashes. These comparisons do not measure",
      "the chance of crashing."
    )
  ),
  
  # Summary numbers
  layout_columns(
    
    col_widths = c(6, 6),
    
    div(
      class = "p-4 rounded",
      style = "background-color: #24756B; color: white;",
      
      h4(
        "Rural fatal crashes",
        style = "color: white;"
      ),
      
      h2(
        textOutput("rural_n", inline = TRUE),
        style = "color: white; margin-bottom: 0;"
      )
    ),
    
    div(
      class = "p-4 rounded",
      style = "background-color: #8055A6; color: white;",
      
      h4(
        "Urban fatal crashes",
        style = "color: white;"
      ),
      
      h2(
        textOutput("urban_n", inline = TRUE),
        style = "color: white; margin-bottom: 0;"
      )
    )
  ),
  
  # Main takeaway
  div(
    class = "border rounded p-3 bg-light",
    
    h4("What stands out?"),
    
    textOutput("takeaway"),
    
    div(
      class = "mt-2 text-muted",
      textOutput("small_sample")
    )
  ),
  
  # Main chart
  div(
    class = "border rounded p-3",
    
    h4("Share of fatal crashes by collision type"),
    
    p(
      paste(
        "Compare the percentage of fatal crashes in each",
        "category for rural and urban areas."
      )
    ),
    
    plotOutput(
      "comparison",
      height = "540px",
      width = "100%"
    ),
    
    p(
      class = "text-muted",
      paste(
        "Each percentage is calculated separately within",
        "rural or urban fatal crashes.",
        "Both groups use the same scale."
      )
    ),
    
    p(
      tags$strong("Note: "),
      paste(
        "The 'Other first harmful event' category includes",
        "crashes where the first harmful event was not a",
        "collision with another moving motor vehicle.",
        "Examples can include pedestrians, fixed objects,",
        "or overturning."
      )
    )
  ),
  
  # Additional information
  accordion(
    
    accordion_panel(
      "See the numbers",
      tableOutput("details")
    ),
    
    accordion_panel(
      "How to interpret this comparison",
      
      p(
        paste(
          "Percentages use all recorded fatal crashes within",
          "the selected rural or urban group as the denominator."
        )
      ),
      
      p(
        paste(
          "Unknown collision classifications are included",
          "in the denominator and displayed separately."
        )
      ),
      
      textOutput("coverage"),
      
      p(
        paste(
          "Selecting a state and road category makes the",
          "comparison more focused. However, differences",
          "in traffic volume, speed, and driving conditions remain."
        )
      ),
      
      p(
        paste(
          "These percentages do not measure crash risk",
          "per trip or mile driven and do not establish causes."
        )
      ),
      
      p(
        "FARS records crashes involving a death, not all traffic crashes."
      ),
      
      a(
        "Source: NHTSA Fatality Analysis Reporting System",
        href = paste0(
          "https://www.nhtsa.gov/research-data/",
          "fatality-analysis-reporting-system-fars"
        ),
        target = "_blank"
      )
    )
  )
)

################################################################
# SERVER
################################################################

server <- function(input, output, session) {
  
  # Filter by selected state and road type
  selected <- reactive({
    
    x <- crashes
    
    if (input$state != "All states") {
      x <- filter(x, state == input$state)
    }
    
    if (input$road != "All road types") {
      x <- filter(x, road == input$road)
    }
    
    x
  })
  
  # Count rural and urban fatal crashes
  totals <- reactive({
    
    x <- selected()
    
    setNames(
      vapply(
        areas,
        function(a) sum(x$n[x$area == a]),
        numeric(1)
      ),
      areas
    )
  })
  
  # Calculate percentages within each group
  comparison <- reactive({
    
    counts <- selected() |>
      filter(area %in% areas) |>
      group_by(area, collision) |>
      summarise(
        n = sum(n),
        .groups = "drop"
      )
    
    expand.grid(
      area = areas,
      collision = categories,
      stringsAsFactors = FALSE
    ) |>
      left_join(
        counts,
        by = c("area", "collision")
      ) |>
      mutate(
        n = coalesce(n, 0L),
        total = unname(totals()[area]),
        pct = ifelse(
          total > 0,
          100 * n / total,
          NA_real_
        )
      )
  })
  
  # Summary numbers
  output$rural_n <- renderText({
    format(
      totals()[["Rural"]],
      big.mark = ","
    )
  })
  
  output$urban_n <- renderText({
    format(
      totals()[["Urban"]],
      big.mark = ","
    )
  })
  
  # Identify largest observed percentage difference
  output$takeaway <- renderText({
    
    if (any(totals() == 0)) {
      return(
        "Choose a broader selection to compare both rural and urban crashes."
      )
    }
    
    x <- comparison() |>
      filter(collision != unknown)
    
    r <- filter(x, area == "Rural")
    u <- filter(x, area == "Urban")
    
    u <- u[match(r$collision, u$collision), ]
    
    i <- which.max(abs(r$pct - u$pct))
    
    sprintf(
      paste0(
        "Largest observed gap: %s accounts for %.1f%% ",
        "of rural fatal crashes and %.1f%% of urban fatal crashes. ",
        "That is a %.1f percentage-point difference."
      ),
      r$collision[i],
      r$pct[i],
      u$pct[i],
      abs(r$pct[i] - u$pct[i])
    )
  })
  
  # Small sample warning
  output$small_sample <- renderText({
    
    if (any(totals() < 100)) {
      
      paste(
        "Caution: At least one group has fewer than 100 crashes.",
        "Its percentages may change noticeably with only a few crashes.",
        "This is not a statistical significance test."
      )
      
    } else {
      
      paste(
        "These are observed differences,",
        "not tests of statistical significance."
      )
    }
  })
  
  # Main comparison chart
  output$comparison <- renderPlot({
    
    validate(
      need(
        all(totals() > 0),
        "No comparison available. Try a broader selection."
      )
    )
    
    x <- comparison() |>
      mutate(
        collision = factor(
          collision,
          levels = rev(categories),
          labels = rev(unname(short_labels[categories]))
        ),
        area = factor(
          area,
          levels = areas
        )
      )
    
    ggplot(
      x,
      aes(
        x = pct,
        y = collision,
        color = area
      )
    ) +
      geom_point(
        position = position_dodge(width = 0.6),
        size = 3.5
      ) +
      scale_color_manual(
        values = colors,
        drop = FALSE
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(v) paste0(v, "%")
      ) +
      labs(
        x = "Share of fatal crashes",
        y = NULL,
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(15, 20, 15, 15)
      )
    
  }, res = 110)
  
  # Detailed table
  output$details <- renderTable({
    
    x <- comparison()
    
    r <- filter(x, area == "Rural")
    u <- filter(x, area == "Urban")
    
    u <- u[match(r$collision, u$collision), ]
    
    pct_label <- function(v) {
      ifelse(
        is.na(v),
        "No crashes",
        sprintf("%.1f%%", v)
      )
    }
    
    data.frame(
      Category = r$collision,
      `Rural count` = r$n,
      `Rural share` = pct_label(r$pct),
      `Urban count` = u$n,
      `Urban share` = pct_label(u$pct),
      check.names = FALSE
    )
    
  }, striped = TRUE, spacing = "s")
  
  # Explain data coverage
  output$coverage <- renderText({
    
    x <- selected()
    
    excluded <- sum(
      x$n[!x$area %in% areas]
    )
    
    missing_collision <- sum(
      x$n[
        x$area %in% areas &
          x$collision == unknown
      ]
    )
    
    sprintf(
      paste0(
        "In this selection: %s crashes excluded because rural/urban ",
        "classification is unavailable; %s included crashes have unknown ",
        "or unreported collision type. 'All road types' includes ",
        "unknown roadway functions."
      ),
      format(excluded, big.mark = ","),
      format(missing_collision, big.mark = ",")
    )
  })
}


shinyApp(ui, server)

