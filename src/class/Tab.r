Tab = R6Class("Tab",
  public = list(
    tabList = NULL,
    initialize = function(tabList = NULL) {
      self$tabList = tabList
    },
    disable = function() {
      for (tab in self$tabList) {
        js$disableTab(tab)
      }
    },
    enable = function() {
      for (tab in self$tabList) {
        js$enableTab(tab)
      }
    }
  ),
  active = list(),
  private= list()
)
