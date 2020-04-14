server <- function(input, output, session) {
  values <- reactiveValues()

  observeEvent(input$saveNetworkBtn, {
    fileName = ""
    if (input$saveNetworkSelect == "Save as new:") {
      fileName = input$saveNetworkFileName
    } else {
      fileName = input$saveNetworkSelect
    }

    if (fileName == "") {
      print("Nope")
      return()
    }

    print(fileName)
    # tags$script(HTML(
    #   "document.getElementsByClassName('sidebar-toggle')[0].click();"
    # ))
    # shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].click();")
    expandSidebar()
    setActiveTab(session, "Network")
  })

  observeEvent(input$loadNetworkBtn, {
    print("test")
  })

  observeEvent(input$fileTabType, {
    # Files that end with "RData" case sensitive
    files = list.files(SAVE_FOLDER, pattern="\\.RData$")
    if (length(files) == 0) {
      files = c("No saved files")
    }
    parsedFiles = c()
    for (file in files) {
      # Remove .RData from file name
      parsedFiles = c(parsedFiles, gsub(".RData", "", file))
    }
    output$saveNetworkSelect = renderUI({
      selectInput(
        "saveNetworkSelect",
        "Select File",
        c(parsedFiles, "Save as new:"),
        selected = "Save as new:"
      )
    })
    output$loadNetworkSelect = renderUI({
      selectInput(
        "loadNetworkSelect",
        "Select File",
        c(parsedFiles, "Select one:"),
        selected = "Select one:"
      )
    })
  })

  observeEvent(input$newCsv, {
    if (is.null(input$newCsv)) {
      return(NULL)
    }
    mainData <<- read.csv(input$newCsv$datapath)
    init(output)
    expandSidebar()
    setActiveTab(session, "Network")
  })

  #setup network
  output$myNetId <- renderVisNetwork({
    getVisNetwork()
  })



  #update graph changes
  observe({
    if (valid(input$getNodeStruc)) {
      print("Initializing node structure...")
      visNetworkProxy("myNetId") %>%
        visGetNodes()
      #Returns: id, name, label, title, x, y
      nodeStruc <<- input$myNetId_nodes
    }
  })



  #On new tab click
  observeEvent(input$tabset, {
    if (input$tabset == "Graph") {
      output$priorPlot <- renderPlot({
        plotPost(input)
      })
    } else if (input$tabset == "Set CPT") {
      observe({
        getSelectState(input, output)
        getSaveState(input, output)
      })
    }
  })


  #on visNet Click
  observe({
    clickType = getClickType(input)

    if (is.null(clickType)) {
      output$shiny_return <- renderPrint({
        print(getScore(dag, mainData))
      })
      output$hot <- renderRHandsontable({})
      output$savePrior <- renderUI({})
    } else if (clickType == "node") {
      printNodeProb(input, output)
      getSavePrior(input, output)
    } else if (clickType == "edge") {

      output$hot <- renderRHandsontable({})
      output$savePrior <- renderUI({})

      visNetworkProxy("myNetId") %>%
        visGetSelectedEdges()

      edgeIndex = which(edgeDf$id == input$myNetId_selectedEdges)

      #CPT radio selected
      output$shiny_return <- renderPrint({
        print(getArcStrength(dag, mainData, input$netScore)[edgeIndex,])
      })
    }
  })



  #BN Score radio selected
  observeEvent(input$useType == 'BN Score', {
    updateBnScoreTextBox(output, dag, mainData)
  })



  ################## #
  #### MANIPULATE ####
  ################## #

  #Display manipulate data
  observeEvent(input$myNetId_graphChange, {
    cmd = input$myNetId_graphChange$cmd
    if (is.null(cmd)) return();
    print(cmd)

    if (cmd == "addEdge") {
      errMsg = validEdge(input$myNetId_graphChange, edgeDf)
      if (valid(errMsg)) {
        output$shiny_return <- renderPrint({
          print(errMsg)
        })
        return()
      }

      addEdge(input, output, edgeDf)
      updateSidbarUi(input, output, dag, mainData)
      return()
    }

    if (cmd == "deleteElements") {
      deleteEdge(input$myNetId_graphChange, edgeDf)

    #  output$shiny_return <- renderPrint({
    #    print(getScore(dag, mainData))
    #  })
      updateRadioButtons(session, "useType", "Select Output",
        c("CP Table", "BN Score", "Evaluate"),
        selected = "BN Score"
      )
      updateBnScoreTextBox(output, dag, mainData)
    }

    if (cmd == "deleteCanceled") {
      visNetworkProxy("myNetId") %>%
        visOptions(manipulation = TRUE)
    }
  })



  #### HandsOnTable ####

  observeEvent(input$hot, {
    if (valid(input$hot)) {
      df = hot_to_r(input$hot)
    } else {
      if (is.null(values[["df"]])) {
        df <- df
      } else {
        #df <- values[["df"]]
      }
    }

    values[["df"]] <- df
  })


  ############### #
  #### BUTTONS ####
  ############### #

  observeEvent(input$debugButton, {
    #switch Physics
    if (exists("phys")) {
      if (phys) {
        phys <<- FALSE
      } else {
        phys <<- TRUE
      }
    } else {
      phys <<- FALSE
    }

    visNetworkProxy("myNetId") %>%
      visPhysics(enabled = phys)
    #print(getArcStrength(dag, mainData, input$netScore))
    #print(dag)
    #print(edgeDf)
    #print(nodeStruc)
  })

  observeEvent(input$learnNetButton, {
    if (is.null(edgeDf)) {
      return()
    }

    #remove all edges
    if (nrow(edgeDf) > 0) {
      clearChildParent()
      removeAllEdges(edgeDf, dag)
    }

    #add new (ML) edges
    #input$netScore:
      #loglik
      #aic
      #bic
      #bde
      #bds
      #mbde
      #bdla
      #k2

    switch(input$strucAlgo,
           hc={
             dag <<- hc(mainData, score = input$netScore)
           },
           tabu={
             dag <<- tabu(mainData, score = input$netScore)
           },
           mmhc={
             dag <<- mmhc(mainData, maximize = input$netScore)
           },
           #only "hc" and "tabu" allowed for rsmax2
           rsmax2_hc={
             dag <<- rsmax2(mainData, maximize = "hc")
           },
           rsmax2_tabu={
             dag <<- rsmax2(mainData, maximize = "tabu")
           },
           h2pc={
             dag <<- h2pc(mainData, maximize = input$netScore)
           }
    )

    # If we implement gs() and other constraint-based algos in the future
    #if (!directed(dag)) {
    #  dag = cextend(dag)
    #}

    edgeDf <<- getEdgeList(dag, mainData, input$netScore)

    #update NodeStruc
    if (nrow(edgeDf) > 0) {
      for (i in 1:nrow(edgeDf)) {
        updateNodeStruc(edgeDf[i,])
      }
    }

    print(edgeDf)

    visNetworkProxy("myNetId") %>%
      visUpdateEdges(edges = edgeDf)

    updateSidbarUi(input, output, dag, mainData)
  })

  observeEvent(input$useType, {
    updateSidbarUi(input, output, dag, mainData)
  })

  observeEvent(input$savePriorButton, {
    if (is.null(input$current_node_id)) {
      output$shiny_return <- renderPrint({
        print("No node selected")
      })
      return()
    }

    nodeLabel = idToLabel(input)
    beta = c(values[["df"]])

    #Update prior list (memory)
    nodeStruc[[nodeLabel]][["prior"]] <<- c(beta[1], beta[2])

    for (i in 1:2) {
      if (!is.numeric(beta[i])) {
        warning("Please set a number for the alpha/beta priors")
        return()
      }

      if (is.na(beta[i])) {
        warning("Please ensure alpha/beta priors are valid numbers")
        return()
      }

      if (beta[i] < 0) {
        warning("Please set nonnegative alpha/beta priors")
        return()
      }
    }

    #get alpha beta values from handsontable
    freqTable = table(mainData[[nodeLabel]])
    success = freqTable[1]
    failure = freqTable[2]

    prior = getPostMean(success, failure, beta[1], beta[2])
    prior = as.numeric(prior)

    #Write prior mean to nodeStruc (global)
    nodeStruc[[nodeLabel]][["postMean"]] <<- prior

    parent = nodeStruc[[nodeLabel]][["myParent"]]
    nodesList = c(nodeLabel, parent)

    #Try getting parameters from 'Set CPT' else assume 'either'
    if (input$tabset == "Set CPT") {
      responseList = vector()
      for (i in 1:length(nodesList)) {
        inputName = paste("select", as.character(i), sep = "")
        responseList = c(responseList, input[[inputName]])
      }
    } else {
      responseList = generateList(length(nodesList), "Either")
    }

    if (input$tabset == "Graph") {
      output$priorPlot <- renderPlot({
        plotPost(input)
      })
    }

    output$shiny_return <- renderPrint({
      getMultiPosterior(nodesList, responseList, mainData)
    })
  })

  observeEvent(input$saveStateButton, {
    #Shouldn't occur from user input (button hides when no node selected)
    if (is.null(input$current_node_id)) {
      warning("No node selected")
      return()
    }

    nodeLabel = idToLabel(input)
    parent = nodeStruc[[nodeLabel]][["myParent"]]
    nodesList = c(nodeLabel, parent)

    responseList = vector()

    #compile responseList (from parents)
    for (i in 1:length(nodesList)) {
      inputName = paste("select", as.character(i), sep = "")
      responseList = c(responseList, input[[inputName]])
    }

    output$shiny_return <- renderPrint({
      getMultiPosterior(nodesList, responseList, mainData)
    })
  })

  if (STANDALONE) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
