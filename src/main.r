#Set Parameters
subData = F
standalone = F

checkPackage = function(pack) {
  if (!is.element(pack, installed.packages()[,1])) {
    install.packages(pack, dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }
}

#Import Libraries
checkPackage("htmlwidgets")
library(htmlwidgets)
checkPackage("shiny")
library(shiny)
checkPackage("visNetwork")
library(visNetwork)
checkPackage("shinydashboard")
library(shinydashboard)
checkPackage("rhandsontable")
library(rhandsontable)
checkPackage("bnlearn")
library(bnlearn)
checkPackage("LearnBayes")
library(LearnBayes)



#### Import Data ####
setwd(getwd())

if (standalone) {
  source(paste(getwd(),"/fsoPath.dat", sep = ""))
  mainData = read.csv(filePath,
                      fileEncoding="UTF-8-BOM")
} else {
  mainData = read.csv("data/hcp_dataset.csv",
                      fileEncoding="UTF-8-BOM")
}

#mainData = read.csv("HCP Household Heads Prop Sample.V2.csv")
#mainData = read.csv("afsun_dataset.csv")
#mainData = read.csv("pseudodata.csv")
#mainData = coronary
#coronary
#asia
#learning.test


#Force to bnlearn supported data type
for (i in 1:ncol(mainData)) {
  mainData[,i] = as.factor(mainData[,i])
}

#Force complete case analysis
if (length(which(is.na(mainData))) > 0) {
  print(paste("Removed", length(which(is.na(mainData))), "incomplete cases."))
  mainData = mainData[complete.cases(mainData),]
}

#Get Subset
if (subData) {
  subata <- sample.int(n = nrow(mainData),
                       size = floor(0.05*nrow(mainData)),
                       replace = F)
  mainData <- mainData[subata, ]
}



#### Functions ####

valid = function(obj) {
  #a variant to the (!is.null) argument
  if (is.null(obj)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

getClickType = function(input) {
  #current_node_id will catch click drags on nodes, selectNode will not
  if (valid(input$selectNode) || valid(input$current_node_id)) {
    return("node")
  } else if (valid(input$selectEdge)) {
    return("edge")
  } else {
    return(NULL)
  }
}

nameNodes = function(rawData) {
  n = length(rawData)
  nodeName = names(rawData[1])
  nodes <- data.frame(id = nodeName, 
                      name = nodeName, 
                      label = nodeName,
                      title = nodeName)
  
  #coerce data type
  nodes$id = as.character((nodes$id))
  nodes$name = as.character((nodes$name))
  nodes$label = as.character((nodes$label))
  nodes$title = as.character((nodes$title))
  
  for (i in 2:n) {
    nodeName = names(rawData[i])
    nodes = rbind(nodes, c(id = nodeName,
                           name = nodeName,
                           label = nodeName,
                           title = nodeName))
  }
  return(nodes)
}

getModString = function(rawData) {
  n = length(rawData)
  modNet = ""
  for (i in 1:n) {
    varName = names(rawData[i])
    if (nchar(varName) > 50) {
      errMsg = paste("The variable in column", i , "exceeds the maximum number of characters")
      if (standalone) {
        tcltk::tk_messageBox(message = errMsg,
                             type="ok",
                             icon="error")
        stopApp()
        q("no")
      } else {
        stop(errMsg, call. = FALSE)
      }
    }
    modNet = paste(modNet, "[", varName, "]", sep = "")
  }
  return(modNet)
}

getEdgeList = function(tempDag, rawData) {
  rawArcStrength = arc.strength(tempDag, rawData)

  n = nrow(arc.strength(tempDag, rawData))
  if (n == 0) {
    edgeList = data.frame(from = character(),
                          to = character(),
                          arrows = character(),
                          id = integer())
  } else {
    edgeList = data.frame(from = unlist(rawArcStrength[1]),
                          to = unlist(rawArcStrength[2]),
                          arrows = "to",
                          id = c(1:n),
                          row.names = NULL)
  }
  
  edgeList$from = as.character((edgeList$from))
  edgeList$to = as.character((edgeList$to))
  edgeList$arrows = as.character((edgeList$arrows))
  
  return(edgeList)
}

getNewEdge = function(graph, edgeList) {
  fromId = graph$from
  toId = graph$to
  
  
  fromLabel = nodeStruc[[fromId]]["label"]
  toLabel = nodeStruc[[toId]]["label"]
  
  g = data.frame(from = unlist(fromLabel),
                 to = unlist(toLabel),
                 arrows = "to",
                 id = graph$id,
                 row.names = NULL)
  
  g$from = as.character(g$from)
  g$to = as.character(g$to)
  g$arrows = as.character(g$arrows)
  
  return(g)
}

addEdge = function(input, output, edgeDf) {
  #create a new dataframe to append to edgelist
  newEdge = getNewEdge(input$myNetId_graphChange, edgeDf)
  
  #update dag
  dag <<- set.arc(dag,
                  newEdge$from,
                  newEdge$to,
                  debug = F)
  
  edgeDf <<- rbind(edgeDf, newEdge)
  
  #update nodeStruc
  addChildParent(newEdge)
}

validEdge = function(input, edgeDf) {
  newEdge = getNewEdge(input$myNetId_graphChange, edgeDf)
  nParents = length(nodeStruc[[newEdge$to]][["myParent"]])
  
  if (isDuplicate(edgeDf, newEdge)) {
    visNetworkProxy("myNetId") %>%
      visRemoveEdges(id = input$myNetId_graphChange$id)
    return("Cannot create duplicate edge.")
  # } else if (nParents == 5) {
  #   visNetworkProxy("myNetId") %>%
  #     visRemoveEdges(id = input$myNetId_graphChange$id)
  #   return("Maximum number of parents reached for this node")
  } else {
    pdag = NULL
    pdag = model2network(modelstring(dag))
    #check cyclic on dummy var
    arcError = tryCatch({
      pdag <- set.arc(pdag,
                      newEdge$from,
                      newEdge$to,
                      debug = F)
      
      return(NULL)
    }, error = function(e) {
      #Cyclical error
      visNetworkProxy("myNetId") %>%
        visRemoveEdges(id = input$myNetId_graphChange$id)
      return("The resulting graph contains cycles.")
    })
  }
}

deleteEdge = function(input, edgeDf) {
  deleteType = getDeleteType(input$myNetId_graphChange)
  
  if (deleteType == "edge") {
    #Remove edge from db
    deleteId = input$myNetId_graphChange$edges
    deleteIndex = which(edgeDf$id == deleteId)
    
    f = edgeDf[deleteIndex,1]
    t = edgeDf[deleteIndex,2]
    
    dag <<- drop.arc(dag,
                     from = f,
                     to = t)
    
    #remove deleted edges from nodeStruc
    rmvIndex = which(nodeStruc[[f]][["myChild"]] == t)
    nodeStruc[[f]][["myChild"]] <<- nodeStruc[[f]][["myChild"]][-rmvIndex]
    
    rmvIndex = which(nodeStruc[[t]][["myParent"]] == f)
    nodeStruc[[t]][["myParent"]] <<- nodeStruc[[t]][["myParent"]][-rmvIndex]
    
    #Remove from edgeDf
    edgeDf <<- edgeDf[-c(deleteIndex), ]
  } else {
    stop("Invalid deletion detected.")
  }
}

getDeleteType = function(inGraph) {
  if (length(inGraph$nodes) > 0) {
    if (length(inGraph$edges) > 0) {
      warning("Connected Node Deleted")
      return("both")
    } else {
      print("Node Deleted")
      return("node")
    }
  } else if (length(inGraph$edges) > 0) {
    print("Edge Deleted")
    if (length(inGraph$edges) > 1) {
      stop("Error: Multiple edges deleted without node deletion")
    } else {
      return("edge")
    }
  }
}

addChildParent = function(edge) {
  t = edge$to
  f = edge$from
  nodeStruc[[f]][["myChild"]] <<- c(nodeStruc[[f]][["myChild"]], t)
  nodeStruc[[t]][["myParent"]] <<- c(nodeStruc[[t]][["myParent"]], f)
}

clearChildParent = function(){
  for (i in 1:length(nodeStruc)) {
    nodeStruc[[i]][["myChild"]] <<- character()
    nodeStruc[[i]][["myParent"]] <<- character()
  }
}

removeAllEdges = function(edgeList, dag) {
  for (i in 1:nrow(edgeList)) {
    dag <<- drop.arc(dag,
                     from = as.character(edgeList[i,1]),
                     to = as.character(edgeList[i,2]),
                     debug = F)
    
    visNetworkProxy("myNetId") %>%
      visRemoveEdges(id = edgeList[i,4])
  }
}

isDuplicate = function(database, observation) {
  k = nrow(database)
  
  if (k == 0) {
    return(FALSE)
  }
  
  for (i in 1:k) {
    obj = database[i,]
    if (as.character(obj$from) == observation$from) {
      if (as.character(obj$to) == observation$to) {
        return(TRUE)
      }
    }
    #Check inverse
    if (as.character(obj$from) == observation$to) {
      if (as.character(obj$to) == observation$from) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

idToLabel = function(input) {
  tempId = input$current_node_id
  #Uses the global variable "nodeStruc
  label = nodeStruc[[tempId]]["label"]
  label = as.character(label)
  return(label)
}

printNodeProb = function(input, output) {
  #TODO: find out why clicking and dragging whitespace calls this function
  #(only when a node is currently selected)
  if(valid(input$myNetId_nodes)) {
    if (valid(input$current_node_id)) {
      
      # nodeLabel = idToLabel(input)
      # fit <- bn.fit(dag, mainData)
      # #Post node values in sidebar
      # output$shiny_return <- renderPrint({
      #   fit[as.character(nodeLabel)]
      # })
      # updateSpreadsheet(nodeLabel, output)

      nodeLabel = idToLabel(input)
      parent = nodeStruc[[nodeLabel]][["myParent"]]
      nodesList = c(nodeLabel, parent)

      #compile responseList (from parents)
      responseList = generateList(length(nodesList), "Either")
      output$shiny_return <- renderPrint({
        getMultiPosterior(nodesList, responseList, mainData)
      })
      updateSpreadsheet(nodeLabel, output)
    }
  }
}

updateSpreadsheet = function(nodeLabel, output) {
  if (length(nodeLabel) == 0) {
    warning("nodeLabel has a length of zero")
    return()
  }
  
  nodeLabel = as.character(nodeLabel)
  freqTable = prop.table(table(mainData[[nodeLabel]]))
  
  #Catch catagorical variables
  if (length(freqTable) > 2) {
    output$hot <- renderRHandsontable({
      #Blank
    })
    warning("Please ensure each variable has up to two possible responses")
    return()
  }
  
  freqTable[1] = round(freqTable[1], 2)
  freqTable[2] = round(freqTable[2], 2)
  
  #Convert to matrix to preserve structure
  m = matrix(freqTable)
  
  #Convert to matrix for naming
  df = as.data.frame(m)
  df = t(df)
  rownames(df) = nodeLabel
  colnames(df) = names(freqTable)
  
  if (is.null(nodeStruc[[nodeLabel]][["prior"]])) {
    df[1] = 1
    df[2] = 1
  } else {
    df[1] = nodeStruc[[nodeLabel]][["prior"]][1]
    df[2] = nodeStruc[[nodeLabel]][["prior"]][2]
  }
  
  
  values <- reactiveValues()
  values[["df"]] = df
  
  if (valid(df)) {
    output$hot <- renderRHandsontable({
      rhandsontable(df, rowHeaderWidth = 150)
    })
  }
}

getPostMean = function(success, failure, a, b) {
  totalSuccess = success+a-1
  totalFailure = failure+b-1
  return(totalSuccess/(totalSuccess+totalFailure))
}

plotPost = function(input) {
  if(valid(input$current_node_id)) {
    nodeLabel = idToLabel(input)
    
    freqTable = table(mainData[[nodeLabel]])
    success = freqTable[1]
    failure = freqTable[2]
    x = seq(0,1,0.01)
    
    likDist = dbeta(x, success + 1, failure + 1)
    
    if (valid(nodeStruc[[nodeLabel]][["prior"]])) {
      beta = nodeStruc[[nodeLabel]][["prior"]]
      for (i in 1:2) {
        if (beta[i] == 0) {
          beta[i] = 1
        }
      }
      
      priorDist = dbeta(x, beta[1], beta[2])
      postDist = dbeta(x, success + beta[1], failure + beta[2])
      contenders = c(priorDist, likDist, postDist)
      contenders = contenders[is.finite(contenders)]
      top = max(contenders)
      
      #Get ess ratio
      priorPercent = (beta[1] + beta[2]) / (success + failure + beta[1] + beta[2])
      priorPercent = round(priorPercent,2) * 100
      priorPercent = ifelse(priorPercent == 0, "<1", priorPercent)
      iss = as.character(paste(priorPercent, "% of this model is elicited from the prior*", sep = ""))
      
      plot(x, likDist, type = "l", col = "red",
           ylim = c(0, top),
           ylab = "Density",
           xlab = c("Percent",iss),
           main = nodeLabel)
      lines(x, priorDist, col = "blue",
            lty = "dotted")
      lines(x, postDist, col = rgb(0,0.75,0),
            lty = "longdash")
      legend(x = 0, y = top,
             c("Likelihood", "Prior", "Posterior"),
             col = c("red", "blue", rgb(0,0.75,0)),
             lty = c("solid", "dotted", "longdash"))
    } else {
      plot(x, likDist, type = "l", col = "red",
           ylab = "Density",
           xlab = "Percent",
           main = nodeLabel)
    }
  }
}

getMultiPosterior = function(nameList, resp, db) {
  n = length(nameList)
  
  if (n != length(resp)) {
    warning("nameList and response lengths do not match")
    return()
  }
  
  #Loop for Either
  eitherIndex = vector()
  
  v = vector()
  respIndex = vector()
  p = logical()
  for (i in 1:n) {
    #(variable)
    v[i] = list(db[[nameList[i]]])
    
    #Check for catagorical variables
    if (length(levels(v[[i]])) > 2) {
      output$shiny_return <- renderPrint({
        print("Please ensure each variable has up to two possible responses")
      })
      return()
    }
    
    #catch all input = 'either'
    if (resp[i] == "Either") {
      eitherIndex = c(eitherIndex, i)
    } else {
      #(response for variable)
      respIndex[i] = which(levels(v[[i]]) == resp[i])
      p[i] = list(v[[i]] == levels(v[[i]])[respIndex[[i]]])
    }
  }
  
  #Permutate all 'either' responses up to 3
  nEither = length(eitherIndex)
  cpt = vector()
  
  if (nEither > 0) {
    #either = 1 to 3
    cpt = combinate(cpt, p, eitherIndex, v, nameList, resp, respIndex, nEither)
    
    cpt = as.matrix(cpt)
    dim(cpt) = generateList(nEither, 2)
    
    cptName = list()
    for (i in 1:length(eitherIndex)) {
      cptName[[i]] = levels(v[[eitherIndex[i]]])
    }
    
    dimnames(cpt) = cptName
    
    nameDim = list()
    for (i in 1:length(eitherIndex)) {
      nameDim[[i]] = nameList[eitherIndex[i]]
    }
    
    names(dimnames(cpt)) = nameDim
    ##0##
  } else {
    prior = nodeStruc[[nameList[1]]][["postMean"]]
    
    if (respIndex[1] == 2) {
      if (valid(prior)) {
        prior = 1 - prior
      }
    }
    
    cpt = c(cpt, getInteractionProb(p, prior))
    cpt = as.matrix(cpt)
    #Assume the selected node is the one they want
    dimnames(cpt) = list(resp[1], nameList[1])
  }
  
  #Print contingencies
  given = character()
  for (i in 1:n) {
    if (resp[i] != "Either") {
      given = c(given, paste(nameList[i], "=", resp[i], "\n"))
    }
  }
  cat("Conditional Probability Table\n")
  cat("\n")
  print(cpt)
  
  if (nEither < n) {
    cat("\n\ngiven that:\n", given)
  }
  
  prior = nodeStruc[[nameList[1]]][["postMean"]]
  if (valid(prior)) {
    cat("Used a prior for", nameList[1], "of", round(prior, 4), '\n')
  }
}

getInteractionProb = function(p, prior) {
  numberOfVar = length(p)
  if (numberOfVar > 1) {
    if (valid(prior)) {
      
      #With Prior
      pa = length(which(p[[1]])) / length(p[[1]])
      top = length(which(Reduce("&", p))) / length(p[[1]])
      top = top / pa * prior
      
      #attach all logical vectors since Reduce() is finicky
      intersection = list()
      
      #get P(a') Not
      intersection[[1]] = ifelse(p[[1]] == TRUE, FALSE, TRUE)
      for (i in 2:numberOfVar) {
        intersection[i] = p[i]
      }
      
      bottom = length(which(Reduce("&", intersection))) / length(p[[1]])
      bottom = bottom / (1-pa) * (1-prior)
      
      return(top/(top+bottom))
    } else {
      #No Prior
      top = length(which(Reduce("&", p)))
      bottom =length(which(Reduce("&", p[-1])))
      return(top/bottom)
    }
  } else {
    if (valid(prior)) {
      return(prior)
    } else {
      top = length(which(p[[1]]))
      bottom = length(p[[1]])
      return(top/bottom)
    }
  }
}

generateList = function(n, int) {
  myVar = vector()
  for (i in 1:n) {
    myVar = c(myVar, int)
  }
  return(myVar)
}

combinate = function(cpt, p, eitherIndex, v, nameList, resp, respIndex, x, n) {
  if (missing(n)) {
    x = generateList(x, 1)
    n = length(x)
  }
  
  for (i in 1:2) {
    x[n] = i
    p[eitherIndex[n]] = list(v[[eitherIndex[n]]] == levels(v[[eitherIndex[n]]])[i])
    
    if (n != 1) {
      cpt = combinate(cpt, p, eitherIndex, v, nameList, resp, respIndex, x, n-1)
    } else {
      #Main function
      prior = nodeStruc[[nameList[1]]][["postMean"]]
      
      if (valid(prior)) {
        if(resp[1] == "Either") {
          if (i == 2) {
            prior = 1 - prior
          }
          #Can respIndex be deduced from resp?
        } else if (respIndex[1] == 2) {
          prior = 1 - prior
        }
      }
      
      cpt = c(cpt, getInteractionProb(p, prior))
    }
  }
  return(cpt)
}

getSelectState = function(input, output) {
  if (valid(input$current_node_id)) {
    nodeLabel = idToLabel(input)
    
    parent = nodeStruc[[nodeLabel]][["myParent"]]
    
    #Parents and self
    nodesList = c(nodeLabel, parent)
    
    # if (length(nodesList) > 6) {
    #   output$selectState <- renderUI({
    #     print("Too many parents connected to a single node (>5)")
    #   })
    #   warning("Cannot process a node with more than 5 parents")
    #   return()
    # }
    
    output$selectState <- renderUI({
      if(valid(input$current_node_id)) {
        box(height = 375, tags$head(tags$style(HTML("#selectState {
                                       overflow-y:scroll;
                                       max-height: 400px;
                                       max-width: 480px;
                                       }"))),
          lapply(1:length(nodesList), function(i) {
            response = levels(mainData[[nodesList[i]]])
            selectInput(paste("select", as.character(i), sep = ""),
                        nodesList[i],
                        c(response[1], response[2], "Either"),
                        selected = "Either")
          })
        )
      }
    })
  } else {
    output$selectState <- renderUI({
      print("Please select a node to generate conditional probability table (CPT)")
    })
  }
}

getSaveState = function(input, output) {
  output$saveState <- renderUI({
    if(valid(input$current_node_id)) {
      actionButton("saveStateButton", "Apply Changes")
    }
  })
}

getSavePrior = function(input, output) {
  output$savePrior <- renderUI({
    actionButton("savePriorButton", "Save Priors")
  })
}

getScore = function(graph, data, output) {
  output$bnScoreTextBox <- renderPrint({
    print(paste("Bayesian Network Score:", round(score(graph, data), 4)))
    print(graph)
  })
}

updateSidbarUi = function(input, output, dag, mainData) {
  if (input$useType == 'CP Table') {
    clickType = getClickType(input)
    
    if (is.null(clickType)) {
      output$shiny_return <- renderPrint({
        print(paste("Bayesian Network Score:", round(score(dag,  mainData), 4)))
      })
    } else if (clickType == "node") {
      printNodeProb(input, output)
    } else if (clickType == "edge") {
      output$hot <- renderRHandsontable({})
      output$savePrior <- renderUI({})
      edgeIndex = which(edgeDf$id == input$myNetId_selectedEdges)
    
      #CPT radio selected
      output$shiny_return <- renderPrint({
        print(arc.strength(dag, mainData, criterion = "bic")[edgeIndex,])
      })
    }
    
  } else if (input$useType == 'BN Score') {
    getScore(dag, mainData, output)
  }
}



#### CREATE DAG ####

modString = getModString(mainData)

# create and plot the network structure.
dag = model2network(modString)

#Declare Global Variable
edgeDf <- getEdgeList(dag, mainData)


########## #
#### UI ####
########## #

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



############## #
#### SERVER ####
############## #

server <- function(input, output, session) {
  values <- reactiveValues()
  
  #setup network
  output$myNetId <- renderVisNetwork({
    visNetwork(nameNodes(mainData), edgeDf) %>%
      visPhysics(solver = "barnesHut",
                 minVelocity = 0.1,
                 forceAtlas2Based = list(gravitationalConstant = -150)) %>%
      visOptions(manipulation = TRUE, highlightNearest = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visEvents(type = "once", beforeDrawing = "function(init) {
                Shiny.onInputChange('getNodeStruc','init');
  }") %>%
      visEvents(selectNode = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
      }",
                dragging = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
                }",
                deselectNode = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
                }",
                selectEdge = "function(e) {
                Shiny.onInputChange('current_edge_id', e.edges);
                }") %>%
      visEvents(click = "function(click) {
                Shiny.onInputChange('selectNode', click.nodes)
                Shiny.onInputChange('selectEdge', click.edges)
      }")
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
  observeEvent(input$bodyTab, {
    if (input$bodyTab == "Graph") {
      output$priorPlot <- renderPlot({
        plotPost(input)
      })
    } else if (input$bodyTab == "Set CPT") {
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
        print(paste("Bayesian Network Score:", round(score(dag,  mainData), 4)))
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
        print(arc.strength(dag, mainData, criterion = "bic")[edgeIndex,])
      })
    }
  })
  
  
  
  #BN Score radio selected
  observeEvent(input$useType == 'BN Score', {
    getScore(dag, mainData, output)
  })
  
  
  
  ################## #
  #### MANIPULATE ####
  ################## #
  
  #Display manipulate data
  observeEvent (input$myNetId_graphChange, {
    cmd = input$myNetId_graphChange$cmd
    if (valid(cmd)) {
      if (cmd == "addEdge") {
        errMsg = validEdge(input, edgeDf)
        if (is.null(errMsg)) {
          addEdge(input, output, edgeDf)
          updateSidbarUi(input, output, dag, mainData)
        } else {
          output$shiny_return <- renderPrint({
            print(errMsg)
          })
        }
      } else if (cmd == "deleteElements") {
        deleteEdge(input, edgeDf)
        print(getClickType(input))
        
        output$shiny_return <- renderPrint({
          print(paste("Bayesian Network Score:", round(score(dag,  mainData), 4)))
        })
      } else if (cmd == "deleteCanceled") {
        visNetworkProxy("myNetId") %>%
          visOptions(manipulation = TRUE)
      }
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
    #print(arc.strength(dag, mainData, criterion = "bic"))
    #print(dag)
    #print(edgeDf)
    #print(nodeStruc)
  })
  
  observeEvent(input$learnNetButton, {
    #remove all edges
    
    if (nrow(edgeDf) > 0) {
      clearChildParent()
      removeAllEdges(edgeDf, dag)
    }
    
    #add new (ML) edges
    dag <<- hc(mainData, score = "bic")
    
    edgeDf <<- getEdgeList(dag, mainData)
    
    #update NodeStruc
    if (nrow(edgeDf) > 0) {
      for (i in 1:nrow(edgeDf)) {
        addChildParent(edgeDf[i,])
      }
    }
    
    print(edgeDf)
    
    visNetworkProxy("myNetId") %>%
      visUpdateEdges(edges = edgeDf)
    
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
    
    #Try getting perameters from 'Set CPT' else assume 'either'
    if (input$bodyTab == "Set CPT") {
      responseList = vector()
      for (i in 1:length(nodesList)) {
        inputName = paste("select", as.character(i), sep = "")
        responseList = c(responseList, input[[inputName]])
      }
    } else {
      responseList = generateList(length(nodesList), "Either")
    }
    
    if (input$bodyTab == "Graph") {
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
  
  if (standalone) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() { 
      stopApp()
      q("no")
    })
  }
}

shinyApp(ui = ui, server = server)