header = dashboardHeader(title = "Bayes Belief Network",
                         titleWidth = "400px")

sidebar = dashboardSidebar(width = "400px",
                           box(width = 12, height = 400, status = "info",
                               conditionalPanel(
                                 condition = "input.useType == 'CP Table'",
                                 verbatimTextOutput("shiny_return"),
                                 tags$head(tags$style(HTML("#shiny_return {
                                                           font-size: 12px;
                                                           overflow-y:scroll;
                                                           max-height: 375px;
                                                           background: ghostwhite;
                                                           }")))
                               ),
                               conditionalPanel(
                                 condition = "input.useType == 'BN Score'",
                                 verbatimTextOutput("bnScoreTextBox"),
                                 tags$head(tags$style(HTML("#bnScoreTextBox {
                                font-size: 12px;
                                overflow-y:scroll;
                                max-height: 375px;
                                background: ghostwhite;
                                }")))
                               )
                           ),
                           #h3("Actions"),
                           radioButtons("useType", "Select Output", c("CP Table", "BN Score")),
                           actionButton("debugButton", "Freeze"),
                           actionButton("learnNetButton", "Learn Network")
                           #,actionButton("savePriorButton", "Save Priors")
)

body = dashboardBody(
  tabBox(
    side = "right",
    height = "485px", width = "200px",
    id = "bodyTab",
    selected = "Network",
    tabPanel("Settings",
             id = "settings",
             radioButtons("netScore", "Select Network Structure Score",
                          c("Multinomial Log-Likelihood" = "loglik",
                            "Akaike Information Criterion" = "aic",
                            "Bayesian Information Criterion" = "bic",
                            "Bayesian Dirichlet equivalent" = "bde",
                            "Bayesian Dirichlet sparse" = "bds",
                            "Modified Bayesian Dirichlet equivalent" = "mbde",
                            "Locally averaged Bayesian Dirichlet" = "bdla",
                            "K2 algorithm" = "k2"
                          )
                        )
    ),
    tabPanel("Set CPT",
             id = "cptTab",
             uiOutput("selectState"),
             #             div(style="display:inline-block", uiOutput("selectState")),
             uiOutput("saveState")
    ),
    tabPanel("Graph",
             id = "graphTab",
             plotOutput("priorPlot")
    ),
    tabPanel("Network",
             visNetworkOutput("myNetId",
                              height = "450px", width = "480"
             ))
  ),
  h4("Prior Beta Distribution"),
  # sliderInput(inputId = "ciSlider",
  #           label = "Confidence Interval Slider",
  #          post = "%",
  #         min = 0, max = 100, value = c(0, 100),
  #        step = 0.5, ticks = FALSE),
  div(style="display:inline-block", rHandsontableOutput("hot")),
  div(style="display:inline-block", uiOutput("savePrior"))
)

ui <- fluidPage(
  dashboardPage(header, sidebar, body)
)