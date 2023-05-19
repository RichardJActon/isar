# https://jiwanheo.rbind.io/post/2022-02-06-pass-around-data-between-shiny-modules-with-r6/
library(dplyr)
library(ggplot2)

IrisR6 <- R6::R6Class(
	"IrisR6",
	public = list(
		n_rows = NULL,
		multiplier = NULL,
		orig_data = iris,
		res_data = NULL,
		manip_data = function(dat) {
			dat %>%
				head(self$n_rows) %>%
				mutate(Sepal.Length = Sepal.Length * self$multiplier)
		}
	)
)

mod_manip_ui <- function(id) {
	ns <- NS(id)

	tagList(
		numericInput(
			ns("n_rows"),
			"Number of rows to display",
			value = 10,
			min = 1,
			max = 150
		),

		numericInput(
			ns("multiplier"),
			"A random calculation",
			value = 1,
			min = 1,
			max = 10
		),
		actionButton(ns("go"), "Go!")
	)
}

mod_manip_server <- function(id, r6) {
	moduleServer(id, function(input, output, session) {

		observeEvent(input$go, {
			r6$n_rows <- input$n_rows
			r6$multiplier <- input$multiplier

			new_data <- r6$manip_data(dat = r6$orig_data)
			r6$res_data <- new_data

			gargoyle::trigger("update_iris")
		})

	})
}
mod_table_ui <- function(id) {
	ns <- NS(id)
	tagList(
		textOutput(ns("text")),
		tableOutput(ns("table"))
	)
}

mod_table_server <- function(id, r6) {
	moduleServer(id, function(input, output, session) {

		observeEvent(gargoyle::watch("update_iris"), {

			output$text <- renderText(paste("Multiplier:", r6$multiplier))

			output$table <- renderTable({
				req(!is.null(r6$res_data))

				r6$res_data
			})

		})
	})
}

mod_graph_ui <- function(id) {
	ns <- NS(id)
	plotOutput(ns("graph"))
}

mod_graph_server <- function(id, r6) {
	moduleServer(id, function(input, output, session) {

		observeEvent(gargoyle::watch("update_iris"), {

			output$graph <- renderPlot({
				req(!is.null(r6$res_data))

				r6$res_data %>%
					ggplot(aes(Sepal.Length, Sepal.Width)) +
					geom_point()
			})

		})
	})
}

ui <- fluidPage(
	column(12, mod_manip_ui("mod_manip_1")),
	column(6, mod_table_ui("mod_table_1")),
	column(6, mod_graph_ui("mod_graph_1"))
)

server <- function(session, input, output) {

	r6 <- IrisR6$new()
	gargoyle::init("update_iris")

	mod_manip_server("mod_manip_1", r6 = r6)
	mod_table_server("mod_table_1", r6 = r6)
	mod_graph_server("mod_graph_1", r6 = r6)
}

shinyApp(ui, server)
