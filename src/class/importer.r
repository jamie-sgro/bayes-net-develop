setClass("importer", slots=list(name="character", age="numeric", GPA="numeric"))
setMethod(show,
          "importer",
          function(object) {
            print("it worked")
          }
)
