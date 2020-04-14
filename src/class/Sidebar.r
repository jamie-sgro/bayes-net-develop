Sidebar = R6Class("Sidebar",
  public = list(),
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
