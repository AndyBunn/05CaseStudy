# libraries that we need
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# check to see if the data exist in the environment
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

# code to get the products for filtering
prod_codes <- setNames(products$prod_code, products$title)

# useful factor lumping function
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{ n }},
                                 other_level = "Sum of All Other Categories")) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}


# start the UI
ui <- fluidPage(
  h1("Introduction"),
  p("This app will allow you to explore data from the National Electronic Injury Surveillance System (NEISS),
    collected by the Consumer Product Safety Commission. This is a long-term study that recorded all
    accidents seen in a representative sample of hospitals in the United States from 2013 to 2017. Here we use
    the data from 2017 only.You can find out more
    about this dataset at:"),
  a(href="github.com/hadley/neiss", "https://github.com/hadley/neiss"),
  hr(),
  p("To begin, select a Product Code. This is the primary product associated
    with the injury (e.g., skateboards)."),
  fluidRow(
    # user input select product
    column(4,
           selectInput("code", "Product Code",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    )
  ),
  hr(),
  h1("Summarize"),
  p("Here we present simply summary information on the location, body part, and
    diagnosis of selected injuries. To simplify the tables, you can
    truncate the information shown."),
  fluidRow(
    column(1),
    # user input to select n
    column(4, sliderInput(inputId = "nRows", label = "Number of unique rows",
                          min = 2,max = 20,step = 1,value = 5))
  ),
  fluidRow(
    # output from the server as tables
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  hr(),
  h1("Plot by sex and age"),
  p("The accident data are stored with some demographic information. Here we
    plot age and sex of the
    person who experienced the accident."),
  fluidRow(
    column(1),
    column(4, selectInput("y", "Y Axis to Plot",
                          c("Injuries per 10,000 people",
                            "Estimated number of injuries")))
  ),
  fluidRow(
    # output from server as a plot
    column(12, plotOutput("age_sex"))
  ),
  hr(),
  h1("Narrative"),
  p("Each observation in the data is accompanied by a short narrative that explains
    how the accident occurred.These can be somewhat cryptic in nature."),
  br(),
  fluidRow(
    column(1), # space
    column(2, actionButton("story", "Random narrative")),
    column(9,textOutput("narrative"))
  ),
  br(),
  p("Or you can scroll through the narrratives in order."),
  br(),
  fluidRow(
    column(1), # some empty space
    column(2,actionButton("prevStory", "Previous")),
    column(5,textOutput("narrative2")),
    column(1,textOutput("narrative2n")),
    column(2,actionButton("nextStory", "Next")),
    column(1) # some empty space
  )
)

# server
server <- function(input, output, session) {
  # make a reactive to select a product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  # pull the narratives
  selectedNarratives <- reactive(selected() %>% pull(narrative))

  # output table for diag
  output$diag <- renderTable({
    diagTable <- count_top(df = selected(), var = diag, n = input$nRows - 1)
    colnames(diagTable) <- c("Diagnosis","Number")
    diagTable

  }, width = "100%")

  # output table for body part
  output$body_part <- renderTable({
    bodyTable <- count_top(df = selected(), var = body_part, n = input$nRows - 1)
    colnames(bodyTable) <- c("Body part injured","Number")
    bodyTable
  }, width = "100%")

  # output table for location
  output$location <- renderTable({
    locationTable <- count_top(df = selected(), var = location, n = input$nRows - 1)
    colnames(locationTable) <- c("Location of injury","Number")
    locationTable

  }, width = "100%")

  # Reactive for getting rate of injury per 10k and raw number
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  # output a plot
  output$age_sex <- renderPlot({
    if (input$y == "Estimated number of injuries") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people", x = "Age")
    }
  }, res = 96)

  # tell a story based on action button
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selectedNarratives() %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())


  # loop stories based on action button
  res <- reactiveValues(narrativeIndex = 1)

  observeEvent(input$nextStory,{
    if(res$narrativeIndex > length(selectedNarratives())) res$narrativeIndex <- 1
    else res$narrativeIndex <- res$narrativeIndex + 1
  })

  observeEvent(input$prevStory,{
    if(res$narrativeIndex < 2) res$narrativeIndex <- length(selectedNarratives())
    else res$narrativeIndex <- res$narrativeIndex - 1
  })

  output$narrative2 <- renderText({
    theNarratives <- selectedNarratives()
    theNarratives[res$narrativeIndex]
  })

  output$narrative2n <- renderText({
    paste0(res$narrativeIndex," of ",length(selectedNarratives()))
  })

}


shinyApp(ui, server)