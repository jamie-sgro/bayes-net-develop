header = dashboardHeader(
  title = "Bayesian Network",
  titleWidth = "400px"
)

sidebar = dashboardSidebar(
  collapsed = TRUE,
  width = "400px",
  box(width = 12, height = 400, status = "info",
    conditionalPanel(
      condition = "input.useType == 'CP Table'",
      verbatimTextOutput("cptTextBox"),
      tags$head(tags$style(HTML("#cptTextBox {
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
    ),
    conditionalPanel(
      condition = "input.useType == 'Evaluate'",
      verbatimTextOutput("evalTextBox"),
      tags$head(tags$style(HTML("#evalTextBox {
        font-size: 12px;
        overflow-y:scroll;
        max-height: 375px;
        background: ghostwhite;
      }")))
    )
  ),
  radioButtons("useType", "Select Output", c("CP Table", "BN Score", "Evaluate")),
  actionButton("debugButton", "Freeze"),
  actionButton("learnNetButton", "Learn Network")
)

body = dashboardBody(
  tabBox(
    side = "right",
    height = "485px", width = "200px",
    id = "bodyTab",
    selected = "File",
    tabsetPanel(id = "tabset",
      tabPanel("File",
        id = "fileTab",
        verticalLayout(
          # actionButton("fileTabNew", "New Network"),
          # actionButton("fileTabSave", "Save Network"),
          # actionButton("fileTabLoad", "Load Network")
          radioButtons("fileTabType", "Select Output", c("New Network", "Save Network", "Load Network"))
        ),
        conditionalPanel(
          condition = "input.fileTabType == 'New Network'",
          fileInput("newCsv", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")
          )
        ),
        conditionalPanel(
          condition = "input.fileTabType == 'Save Network'",
          # Dynamic dropdown for all RData files that can be saved
          uiOutput("saveNetworkSelect"),
          conditionalPanel(
            condition = "input.saveNetworkSelect == 'Save as new:'",
            textInput("saveNetworkFileName", "New File Name", "")
          ),
          conditionalPanel(
            condition = "input.saveNetworkSelect != 'Save as new:' || input.saveNetworkFileName != ''",
            actionButton("saveNetworkBtn", "Save")
          )
        ),
        conditionalPanel(
          condition = "input.fileTabType == 'Load Network'",
          # Dynamic dropdown for all RData files that can be loaded
          uiOutput("loadNetworkSelect"),
          conditionalPanel(
            condition = "input.loadNetworkSelect != 'Select one:'",
            actionButton("loadNetworkBtn", "Load")
          )
        )
      ),
      tabPanel("Network",
        visNetworkOutput("myNetId", height = "450px", width = "480")
      ),
      tabPanel("Graph",
        id = "graphTab",
        plotOutput("priorPlot")
      ),
      tabPanel("Set_CPT",
        id = "cptTab",
        uiOutput("selectState"),
        uiOutput("saveState")
      ),
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
            "Cooper & Herskovits' K2 algorithm" = "k2"
          ),
          selected = DEFAULT_NETSCORE
        ),
        radioButtons("strucAlgo", "Select Structure Learning Algorithm",
        c("Hill-Climbing" = "hc",
          "Tabu Search" = "tabu",
          "Max-Min Hill-Climbing" = "mmhc",
          "Restricted Maximization (HC Maximizing)" = "rsmax2_hc",
          "Restricted Maximization (Tabu Maximizing)" = "rsmax2_tabu",
          "Hybrid HPC" = "h2pc"
        ),
        selected = DEFAULT_STRUCALGO
        )
      )
    )
  ),
  h4("Prior Beta Distribution"),
  div(style="display:inline-block", rHandsontableOutput("hot")),
  div(style="display:inline-block", uiOutput("savePrior"))
)

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("disableTab", "enableTab")),
  inlineCSS(css),
  dashboardPage(header, sidebar, body)
)
