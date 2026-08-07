library(shiny)
library(bslib)
library(tidyverse)

gold <- readr::read_csv( 'https://raw.githubusercontent.com/datasets/gold-prices/refs/heads/main/data/monthly-processed.csv') |>
    rename(gold_price = Price)


oil <- readr::read_csv('https://raw.githubusercontent.com/datasets/oil-prices/refs/heads/main/data/wti-monthly.csv') |>
    rename(oil_price = Price)


nat_gas <- readr::read_csv('https://raw.githubusercontent.com/datasets/natural-gas/refs/heads/main/data/monthly-processed.csv') |>
    rename(gas_price = Price)


vix <- readr::read_csv('https://raw.githubusercontent.com/datasets/finance-vix/refs/heads/main/data/vix-monthly.csv') |>
    rename(vol_index = Close)


bond <- readr::read_csv('https://raw.githubusercontent.com/datasets/bond-yields-us-10y/refs/heads/main/data/monthly.csv') |>
    rename(bond_rate = Rate)


temp <- readr::read_csv('https://raw.githubusercontent.com/datasets/global-temp/refs/heads/main/data/monthly.csv') |>
    mutate(Date = lubridate::as_date(paste0(Year, "-01"))) |>
    rename(temp_diff = Mean) |>
    select(-Year, -Source)


sp <- readr::read_csv('https://raw.githubusercontent.com/datasets/s-and-p-500/refs/heads/main/data/data.csv') |>
    select(-PE10, -Dividend, -Earnings)

full_data <- gold |>
    full_join(oil) |>
    full_join(nat_gas) |>
    full_join(vix) |>
    full_join(bond) |>
    full_join(temp) |>
    full_join(sp)

y_choices <- setdiff(names(full_data), "Date")

plot_ts <- function(variable, log = TRUE) {
 p <- full_data |>
   select(Date, .data[[variable]]) |>
   drop_na() |>
   ggplot(aes(x = Date, y = .data[[variable]])) +
   geom_line() +
   theme_bw() +
   labs(x = "date", y = variable)


 if (log) {
   p <- p + scale_y_log10()
 }
 p
}


## ---- UI ----
ui <- page_sidebar(
 title = "Market Indicators Explorer",
 sidebar = sidebar(
   selectInput("rainbow", "Variable to plot:", choices = y_choices),
   checkboxInput("unicorn", "Log scale (y-axis)", value = TRUE)
 ),
 plotOutput("the_plot")
)


## ---- Server ----
server <- function(input, output, session) {
 output$the_plot <- renderPlot({
   plot_ts(input$rainbow, log = input$unicorn)
 })
}


shinyApp(ui, server)


