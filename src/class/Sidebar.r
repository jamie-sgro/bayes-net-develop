Sidebar = R6Class("Sidebar",
  public = list(
    printActiveTextBox = function(input, output, msg) {
      if (input$useType == "CP Table") {
        output$cptTextBox = renderPrint({
          print(msg)
        })
      } else if (input$useType == "BN Score") {
        output$bnScoreTextBox <- renderPrint({
          print(msg)
        })
      } else if (input$useType == "Evaluate") {
        output$evalTextBox <- renderPrint({
          print(msg)
        })
      }
    }
  ),
  active = list(
    collapse = function() {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    },
    expand = function() {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    },
    toggle = function() {
      runjs("document.getElementsByClassName('sidebar-toggle')[0].click();")
    }
  ),
  private= list()
)
