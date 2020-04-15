# Deploy Docs

*Used for deploying this app to shinyapps.io*

1. Ensure main.r is renamed to app.r

2. Replace `runApp()` with `shinyApp(ui = ui, server = server)`

3. remove check package method i.e.

   1. ```R
      for (package in PACK_LIST) {
        #checkPackage(package)
        #library(package, character.only = TRUE)
      }
      ```

4. manually write out `library()` without character.only i.e.
   
   1. `library(htmlwidgets)`
   
5. compile all `source("some file")` files together into a single app.r. 

6. I also needed to change environment for loading rdata, but that could be an artefact of compiling the source files to a single app.r file

