library(shiny)
library(DT)
library(rhandsontable)

# Define UI ----
ui <- fluidPage(title = "Multi-Criteria Decision Analysis (MCDA)",
  titlePanel(h1("Best management practices for multiple ecosystem services: 
                multi-criteria decision analysis for Mediterranean-type 
                farmland and rangeland")),
  tabsetPanel(type = "tabs",
    tabPanel("Instructions",
      h2("Instructions"),
      HTML(
        '<p>This is a tool for making decisions about how best to manage 
          farmland and rangeland, when considering multiple criteria (cost, 
          crop yield, soil, water, pest regulation, pollination, biodiversity 
          conservation, and climate regulation). This tool is based on 
          evidence that was summarized by <a href="https://www.conservationevidence.com/data/index/?synopsis_id[]=22">
          Conservation Evidence</a> in <a href="https://www.conservationevidence.com/synopsis/download/22">
          <em>Sustainable Agriculture in California and Mediterranean 
          Climates: Evidence for the effects of selected interventions</em></a>
          (Shackelford <em>et al.</em> 2017).
        </p>
        <br />
        <p>To use this tool, please click on the "Priorities" tab. You will 
          see the management practices in order, from highest priority to 
          lowest priority, based on our assessment of <a href="https://www.conservationevidence.com/data/index/?synopsis_id[]=22">
          the evidence</a>. The priority of each practice depends on how much 
          you value each of the criteria for which we have evidence. By 
          default, these criteria have equal values. However, you can use the
          sliders to change their values. 
        </p>
        <br />
        <p>You can also click on the tabs for "Effectiveness" and "Cost" to 
          see the data on which these priorities are based. If you would like
          to assess the evidence for yourself, you can change the scores 
          ("benefits", "harms", and "certainty") and the cost of each 
          practice.
        </p>
        <br />
        <p>Please contact Gorm Shackelford (gorm.shackelford@gmail.com) for 
          more information.
        </p>
        <br />
        <p>Shackelford, G. E., Kelsey, R., Robertson, R. J., Williams, D. R. 
          & Dicks, L. V. (2017) Sustainable Agriculture in California and 
          Mediterranean Climates: Evidence for the effects of selected 
          interventions. Synopses of Conservation Evidence Series. University
          of Cambridge, Cambridge, UK.
        </p>'
      )
    ),
    tabPanel("Priorities",
      sidebarLayout(position = "right",
        sidebarPanel(
          h2("Criteria"),
          p("How much should we value these criteria, from 0 (no value) to 1
            (highest value)? By default, all criteria have equal value."),
          sliderInput(
            "Cost weight", "Cost", min = 0, 
            max = 1, value = 1
          ),
          sliderInput(
            "Crop production weight", "Crop production", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Soil regulation weight", "Soil regulation", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Water regulation weight", "Water regulation", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Pest regulation weight", "Pest regulation", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Pollination weight", "Pollination", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Biodiversity conservation weight", "Biodiversity conservation", 
            min = 0, max = 1, value = 1
          ),
          sliderInput(
            "Climate regulation weight", "Climate regulation", 
            min = 0, max = 1, value = 1
          ),
          h2("Evidence"),
          p("How much weight should we give to the benefits vs the harms of 
            each practice, from 0 (no weight) to 1 (full weight)? By default, 
            benefits and harms have equal weight."),
          sliderInput(
            "benefits_weight", "Benefits", min = 0, max = 1, value = 1
          ),
          sliderInput(
            "harms_weight", "Harms", min = 0, max = 1, value = 1
          ),
          p("How certain should we be about the effects of each practice? We 
            will only consider evidence about which we are at least this 
            certain (see the \"Effectiveness\" tab for certainty scores). In
            the Conservation Evidence project, practices with a certainty 
            score < 40 are considered to have \"unknown effectiveness\"."),
          sliderInput(
            "certainty_min", "Minimum certainty", 
            min = 0, max = 100, value = 40
          ),
          p("Should we use the certainty scores to weight the benefits and 
            harms scores?"),
          checkboxInput(
            "weight_by_certainty", "Weight by certainty", value = TRUE
          ),
          p("What should we do about missing data? If you tick this box, we 
            will assume that practices for which we have no evidence should 
            have a priority score of 0.5 (halfway between 0 and 1). By 
            default, we will assume that these practices should have a 
            priority score of 0."),
          checkboxInput(
            "impute_missing_data", "Impute missing data", value = FALSE
          )
        ),
        mainPanel(
          h2("Priorities"),
          p("The practices for which we have evidence are shown in order, 
            from highest priority to lowest priority. If you move the sliders 
            on this tab, or change the data on the other tabs 
            (\"Effectiveness\" and \"Cost\"), then the priority order will 
            also change."),
          DT::dataTableOutput("bmps")
        )
      )
    ),
    tabPanel("Weights", 
             h2("Weights"),
             p("These weights are calculated from the sliders on the 
              \"Priorities\" tab. They are \"normalized\" (they add up to 1).
              The scores on the \"Performance\" tab are multiplied by these 
              weights when the \"Priorities\" are calculated."),
             DT::dataTableOutput("weights")),
    tabPanel("Effectiveness", 
             h2("Effectiveness"),
             HTML('<p>These scores are based on <a href="https://www.conservationevidence.com/data/index/?synopsis_id[]=22">
              Conservation Evidence</a>.</p>'),
             rHandsontableOutput("assessment")),
    tabPanel("Cost", 
             h2("Cost"),
             p("These costs are based on the Environmental Quality Incentives 
               Program (EQIP) Payment Schedule for California in 2017. They
               are costs per acre in US Dollars."),
             rHandsontableOutput("cost")),
    tabPanel("Performance", 
             h2("Performance"),
             p("These scores are a function of the scores on the
               \"Effectiveness\" and \"Cost\" tabs. The scores in each column
               are \"idealized\" (they are a proportion of the highest score
               in that column). Thus, the highest priority practice for each
               criterion has a score of 1. If you selected \"impute missing 
               data\" on the \"Priorities\" tab, then the missing data in 
               each column have been replaced by scores of 0.5 (halfway 
               between 0 and 1)."),
             DT::dataTableOutput("calculations"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  assessment <- read.csv("data/assessment.csv")
  cost <- read.csv("data/cost.csv")
  values <- reactiveValues()
  
  # Reactive data from the effectiveness tab
  observe({
    if (!is.null(input$assessment)) {
      assessment = hot_to_r(input$assessment)
    } else {
      if (is.null(values[["assessment"]]))
        assessment <- assessment
      else
        assessment <- values[["assessment"]]
    }
    values[["assessment"]] <- assessment
  })
  output$assessment <- renderRHandsontable({
    assessment <- values[["assessment"]]
    if (!is.null(assessment)) {
      rhandsontable(assessment, readOnly = FALSE, contextMenu = FALSE) %>%
        hot_col("practice", readOnly = TRUE) %>%
        hot_col("service", readOnly = TRUE)
    }
  })
  
  # Reactive data from the cost tab
  observe({
    if (!is.null(input$cost)) {
      cost = hot_to_r(input$cost)
    } else {
      if (is.null(values[["cost"]]))
        cost <- cost
      else
        cost <- values[["cost"]]
    }
    values[["cost"]] <- cost
  })
  output$cost <- renderRHandsontable({
    cost <- values[["cost"]]
    if (!is.null(cost)) {
      rhandsontable(cost, readOnly = TRUE, contextMenu = FALSE) %>%
        hot_col("cost", readOnly = FALSE, format = "$0,0.00")
    }
  })
  
  # Transform the reactive data from the effectiveness tab.
  get_assessment <- reactive({
    assessment <- values[["assessment"]]
    # Get a subset of the data, based on user input.
    assessment <- assessment[assessment$certainty >= input$certainty_min, ]
    assessment
  })
  
  # Transform the reactive data from the cost tab.
  get_cost <- reactive({
    cost <- values[["cost"]]
    # For criteria that should be minimized (not maximized), subtract the 
    # scores from the maximum score, and then set the maximum score to 1.
    cost$cost <- max(cost$cost, na.rm = TRUE) - cost$cost
    cost$cost <- cost$cost / max(cost$cost, na.rm = TRUE)
    cost
  })
  
  # Transform the reactive data from the weights sliders.
  weights <- reactive({
    d <- get_assessment()
    c <- get_cost()
    practices <- as.character(factor(unique(d$practice)))
    services <- as.character(factor(unique(d$service)))
    criteria <- c("Cost", services)
    
    # Get the reactive weights.
    weights <- matrix(nrow = 2, ncol = length(criteria))
    weights <- data.frame(weights)
    colnames(weights) <- criteria
    rownames(weights) <- c("Your values", "Normalized weights")
    for (i in 1:length(criteria)) {
      criterion <- as.character(criteria[i])
      weight <- paste(criterion, "weight", sep = " ")
      weights[1, criterion] <- input[[weight]]
    }
    # Normalize the reactive weights.
    weights[2, ] <- weights[1, ] / sum(weights[1, ])
    weights
  })
  
  ##############
  # Value scores
  ##############
  
  calculations <- reactive({
    d <- get_assessment()  # Reactive assessment data
    req(d$practice)  # return(NULL) if there is no reactive data.
    c <- get_cost()  # Reactive cost data
    practices <- as.character(factor(unique(d$practice)))
    services <- as.character(factor(unique(d$service)))
    criteria <- c("Cost", services)
    # Create the empty data.frame
    calculations <- matrix(nrow = length(practices), 
                           ncol = length(criteria))
    calculations <- data.frame(calculations, row.names = practices)
    colnames(calculations) <- criteria

    # Value scores for cost
    for (i in 1:length(practices)) {  # For each practice
      practice <- practices[i]  # This practice
      if (!is.na(c[c$practice==practice, "cost"])) {
        calculations[practice, "Cost"] <- c[c$practice==practice, "cost"]
      } else {
        calculations[practice, "Cost"] <- NA
      }
    }

    # Value scores for services
    for (i in 1:length(services)) {  # For each service
      service <- services[i]  # This service
      service_d <- d[d$service == service, ]  # Data for this service
      # Value function for services
      # For criteria that should be maximized, set the maximum score to 1.
      service_d$benefits <- service_d$benefits / max(service_d$benefits)
      # For criteria that should be minimized, subtract the scores from the 
      # maximum possible score (100), and then set the maximum score to 1.
      service_d$harms <- 100 - service_d$harms
      service_d$harms <- service_d$harms / max(service_d$harms)
      practices <- as.character(factor(unique(service_d$practice)))  # Practices for this service
      if (length(practices) > 0) {  # If there is at least one practice
        for (j in 1:length(practices)) {  # For each practice
          practice <- practices[j]  # This practice
          practice_d <- service_d[service_d$practice==practice 
                                  & service_d$service==service, ]
          # Calculate the value scores, weighted by user inputs.
          benefits <- practice_d$benefits * input[["benefits_weight"]]
          harms <- practice_d$harms * input[["harms_weight"]]
          certainty <- practice_d$certainty
          # For services, the value score is the average of the value scores
          # for benefits and harms.
          score <- (benefits + harms) / 2
          if (input[["weight_by_certainty"]]) {  # If the user has selected "weight by certainty" (the default)
            score <- (score * certainty) / 100  # Weight the scores by the certainty of the evidence.
          }
          calculations[practice, service] <- score
        }
      }
      # Transform the scores to a proportion of the maximum score.
      calculations[service] <- calculations[service] / max(calculations[service], na.rm = TRUE)
    }
    # Impute missing data
    if (input[["impute_missing_data"]]) {  # If the user has selected "impute missing data"
      calculations[is.na(calculations)] <- 0.5
    }
    
    calculations
  })
  
  priorities <- reactive({
    calculations <- calculations()
    d <- get_assessment()  # Reactive assessment data, transformed
    w <- weights()  # Reactive weight data, transformed
    practices <- as.character(factor(unique(d$practice)))
    services <- as.character(factor(unique(d$service)))
    criteria <- c("Cost", services)

    # Weight the scores by user values for each criterion.
    for (i in 1:length(criteria)) {  # For each criterion
      criterion <- criteria[i]  # This criterion
      weight <- w["Normalized weights", criterion]  # Weight for this criterion
      calculations[, criterion] <- calculations[, criterion] * weight
    }
    
    # Calculate the overall priority scores (the sum of value scores).
    for (i in 1:length(practices)) {  # For each practice
      practice <- practices[i]
      calculations[practice, "Priority"] <- 
        sum(calculations[practice, 1:length(criteria)], na.rm = TRUE)
    }
    # Normalize the overall priority scores.
    calculations$Priority <- calculations$Priority / sum(calculations$Priority)
    # Idealize the overall priority scores.
    calculations$Priority <- calculations$Priority / max(calculations$Priority)

    calculations
  })
  
  
  #########
  # Outputs
  #########
  
  # Weights
  output$weights <- DT::renderDataTable({
    datatable(weights(), rownames = TRUE,
              options = list(pageLength = 25)) %>%
      formatRound(1:length(weights()), digits = 3)
  })
  
  # Calculations
  output$calculations <- DT::renderDataTable({
    datatable(calculations(), rownames = TRUE,
      options = list(pageLength = 25)) %>%
      formatRound(1:length(calculations()), digits = 2)
  })
  
  # Priorities
  output$bmps <- DT::renderDataTable({
    data <- priorities()
    data$Practice <- row.names(data)
    data <- data[, c("Practice", "Priority")]
    data <- data[order(-data$Priority), ]
    datatable(data, rownames = FALSE, options = list(pageLength = 25)) %>%
      formatRound(2, digits = 2)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)