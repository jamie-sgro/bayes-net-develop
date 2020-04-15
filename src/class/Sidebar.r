Sidebar = R6Class("Sidebar",
  public = list(
    jsCode = "
    shinyjs.disableTab = function(name) {
      var tab = $('.nav li a[data-value=' + name + ']');
      tab.bind('click.tab', function(e) {
        e.preventDefault();
        return false;
      });
      tab.addClass('disabled');
    }

    shinyjs.enableTab = function(name) {
      var tab = $('.nav li a[data-value=' + name + ']');
      tab.unbind('click.tab');
      tab.removeClass('disabled');
    }
    ",
    css = "
    .nav li a.disabled {
      background-color: #aaa !important;
      color: #333 !important;
      cursor: not-allowed !important;
      border-color: #aaa !important;
    }",
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
