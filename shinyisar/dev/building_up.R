library(shiny)

## self-contained example of dynamically generated input fields updating an R6 object ----
# 
# ui <- fluidPage(
#   uiOutput("comments"),
#   uiOutput("display")
# )
# 
# server <- function(input, output, session) {
#   
#   exinv <- Investigation$new(
#     comments = list(test = "test content", another = "lorem ipsum")
#   )
#   gargoyle::init("gtest")
#   
#   observeEvent(gargoyle::watch("gtest"), {
#     output$comments <- renderUI({
#       bx <- purrr::imap(exinv$comments, ~{
#         textInput(inputId = .y, label = .y, value = .x)
#       })
#       tagList(bx, actionButton("go","go")) 
#     })
#   })
#   
#   observeEvent(input$go, {
#     new_comments <- names(exinv$comments) %>% 
#       purrr::set_names() %>% 
#       purrr::map(~{input[[.x]]})
#     exinv$set_comments(new_comments)
#     gargoyle::trigger("gtest")
#   })
#   
#   output$display <- renderUI({
#     gargoyle::watch("gtest")
#     purrr::imap(exinv$comments, ~p(.x))
#   })
#   
# }
# 
# shinyApp(ui, server)


## modularised ----

mod_comments_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("comments"))
}

mod_comments_server <- function(id, exinv) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    observeEvent(gargoyle::watch("gtest"), {
      output$comments <- renderUI({
        bx <- purrr::imap(exinv$comments, ~{
          textInput(inputId = ns(.y), label = .y, value = .x)
        })
        tagList(bx, actionButton(ns("go"),"go")) 
      })
    })
    
    observeEvent(input$go, {
      new_comments <- names(exinv$comments) %>% 
        #purrr::map_chr(ns) %>%
        purrr::set_names() %>% 
        purrr::map(~{input[[.x]]})
      exinv$set_comments(new_comments)
      gargoyle::trigger("gtest")
    })
  })
}

mod_display_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("display"))
}

mod_display_server <- function(id, exinv) {
  moduleServer(id, function(input, output, session) {
    #ns <- NS(id)
    output$display <- renderUI({
      gargoyle::watch("gtest")
      purrr::map(exinv$comments, ~p(.x))
    })
  })
}

ui <- fluidPage(
  mod_comments_ui("comms"),
  mod_display_ui("disp")
)

server <- function(session, input, output) {
  exinv <- Investigation$new(
    comments = list(test = "test content", another = "lorem ipsum")
  )
  gargoyle::init("gtest")
  
  mod_comments_server("comms", exinv = exinv)
  mod_display_server("disp", exinv = exinv)
}

shinyApp(ui, server)
