# bs4Dash UI components ----

## Sidebar ----
sidebar <- bs4Dash::dashboardSidebar(
	bs4Dash::sidebarMenu(
		id = "sidebar_menu",
		#bs4Dash::sidebarHeader("Investigations"),
		bs4Dash::bs4SidebarMenuItem(
			text = "Investigations",
			startExpanded = TRUE,
			#text = "example investigation",
			#tabName = "example_investigation",
			bs4Dash::bs4SidebarMenuItem(
				text = "example investigation",
				tabName = "example_investigation"
			),
			bs4Dash::menuSubItem(text = "study 1", tabName = "a"),
			bs4Dash::menuSubItem(text = "study 2", tabName = "b"),
			bs4Dash::menuSubItem(text = "study 3", tabName = "c")
		),
		bs4Dash::bs4SidebarMenuItem(text = "about", tabName = "about")
	)
)

## Footer ----
footer <- bs4Dash::dashboardFooter(
	left = shiny::markdown("&copy; 2023 [HDBI](https://hdbi.org/). License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)"),
	right = shiny::img(
		alt = "Wellcome Trust",
		src = "./inst/app/img/Wellcome_Trust_logo.svg",
		href = "https://wellcome.org/",
		width = 32
	)
)

## Investigation tab ----
investigation_tab <- bs4Dash::tabItem(
	tabName = "example_investigation",
	shiny::fluidRow(
		bs4Dash::box(
			title = "Example Investigation",
			width = 12,
			sidebar = bs4Dash::boxSidebar(
				# startOpen = FALSE,
				width = 70,
				bs4Dash::socialBox(
					collapsible = FALSE,
					title = "comments", width = 12,
					comments = shiny::tagList(
						bs4Dash::boxComment(
							image = NULL,
							title = "a title",
							"body of the comment"
						),
						bs4Dash::boxComment(
							image = NULL,
							title = "a title",
							"body of the comment"
						)
					),
					shiny::actionButton("add_comment","Add")
				),
				icon = shiny::icon("comments")
			),
			shiny::fluidRow(
				shiny::column(
					width = 9,
					"description text here..."
				),
				shiny::column(
					width = 3,
					bs4Dash::boxPad(
						bs4Dash::descriptionBlock(
							header = "submission date",
							text = "2023-05-05",
							rightBorder = FALSE,
							marginBottom = TRUE
						),
						bs4Dash::descriptionBlock(
							header = "release date",
							text = "2023-05-15",
							rightBorder = FALSE,
							marginBottom = TRUE
						)
					)
				)
			)
		)
	),
	shiny::fluidRow(
		bs4Dash::box(
			title = "Studies", width = 12,
			footer = shiny::fluidRow(
				column(
					width = 6,
					bs4Dash::descriptionBlock(
						# number = "17%",
						# number_color = "green",
						header = "Study A",
						text = "...",
						rightBorder = TRUE,
						marginBottom = FALSE
					)
				),
				column(
					width = 6,
					bs4Dash::descriptionBlock(
						# number = "18%",
						# number_color = "red",
						header = "Study B",
						text = "...",
						rightBorder = TRUE,
						marginBottom = FALSE
					)
				)
			)
		)
	),
	shiny::fluidRow(
		bs4Dash::box(
			title = "Publications", width = 6
		),
		bs4Dash::box(
			title = "Contacts", width = 6,
			bs4Dash::userList(
				bs4Dash::userListItem(
					image = NULL,
					title = "Steve",
					subtitle = "0000-0000-0000-0000"
				),
				bs4Dash::userListItem(
					image = NULL,
					title = "Jane",
					subtitle = "0000-0000-0000-0001"
				)
			)
		),
		bs4Dash::box(
			title = "Factors", width = 6
		),
		bs4Dash::box(
			title = "Protocols", width = 6
		)
	)

)

## Study Tabs ----
study1_tab <- bs4Dash::tabItem(
	tabName = "a",
	shiny::fluidRow(
		bs4Dash::box(
			title = "Study 1", width = 12,
			sidebar = bs4Dash::boxSidebar(
				# startOpen = FALSE,
				width = 70,
				bs4Dash::socialBox(
					collapsible = FALSE,
					title = "comments", width = 12,
					comments = shiny::tagList(
						bs4Dash::boxComment(
							image = NULL,
							title = "a title",
							"body of the comment"
						),
						bs4Dash::boxComment(
							image = NULL,
							title = "a title",
							"body of the comment"
						)
					),
					shiny::actionButton("add_comment","Add")
				),
				icon = shiny::icon("comments")
			),
			shiny::fluidRow(
				shiny::column(
					width = 9,
					"description text here..."
				),
				shiny::column(
					width = 3,
					bs4Dash::boxPad(
						bs4Dash::descriptionBlock(
							header = "submission date",
							text = "2023-05-05",
							rightBorder = FALSE,
							marginBottom = TRUE
						),
						bs4Dash::descriptionBlock(
							header = "release date",
							text = "2023-05-15",
							rightBorder = FALSE,
							marginBottom = TRUE
						)
					)
				)
			)
		)
	),
	shiny::fluidRow(
		bs4Dash::tabBox(
			title = "Samples",
			width = 12,
			shiny::tabPanel(title = "Sample 1"),
			shiny::tabPanel(title = "Sample 2")
		)
	),
	shiny::fluidRow(
		bs4Dash::tabBox(
			title = "Assays",
			width = 12,
			shiny::tabPanel(title = "Assay 1"),
			shiny::tabPanel(title = "Assay 2")
		)
	),
	shiny::fluidRow(
		bs4Dash::box(
			title = "Process Sequence",
			width = 12,
			bs4Dash::timelineBlock(
				bs4Dash::timelineStart(color = "gray"),
				bs4Dash::timelineLabel(1, color = "teal"),
				bs4Dash::timelineItem(
					title = "Item 1",
					icon = shiny::icon("gears"),
					# color = "olive",
					# # time = "now",
					# # footer = "Here is the footer",
					"This is the body"
				),
				bs4Dash::timelineLabel(2, color = "teal"),
				bs4Dash::timelineItem(
					title = "Item 2",
					#border = FALSE
					icon = shiny::icon("gears"),
					color = "olive"
					# bs4Dash::timelineItemMedia(src = "http://placehold.it/150x100")
				),
				bs4Dash::timelineEnd(color = "gray")
			)
		)
	)
)

## Body ----
body <- bs4Dash::dashboardBody(
	bs4Dash::tabItems(
		investigation_tab,
		study1_tab,
		bs4Dash::tabItem(
			tabName = "b", bs4Dash::box(
				title = "Study 2", width = 12
			)
		),
		bs4Dash::tabItem(
			tabName = "c", bs4Dash::box(
				title = "Study 3", width = 12
			)
		),

		bs4Dash::tabItem(
			tabName = "about",
			shiny::fluidRow(
				bs4Dash::userBox(
					width = 12, collapsible = FALSE,
					title = bs4Dash::userDescription(
						title = "{shinyisar}",
						subtitle = "v0.0.0-9000",
						image = NULL # shinyisar Hex
					)
				),
				bs4Dash::userBox(
					width = 12, collapsible = FALSE,
					title = bs4Dash::userDescription(
						title = "{isar}",
						subtitle = "v0.0.0-9000",
						image = NULL # isar Hex
					)
				)
			)
		)
	)
)

# Combined UI ----
ui <- bs4Dash::dashboardPage(
	title = "Shiny ISA R",
	header = bs4Dash::dashboardHeader(),
	sidebar = sidebar,
	controlbar = bs4Dash::dashboardControlbar(),
	footer = footer,
	body = body

)

# Server ----
server <- function(input, output, session) {

}

# Run App ----
shinyApp(ui, server)
