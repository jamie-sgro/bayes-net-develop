#Set Parameters
SUBDATA = F
STANDALONE = F
LOCALHOST = F

SAVE_FOLDER = "./save/"
DEFAULT_NETSCORE = "loglik"
DEFAULT_STRUCALGO = "hc"

PACK_LIST = c("htmlwidgets", "shiny", "visNetwork", "shinydashboard",
             "rhandsontable", "bnlearn", "LearnBayes", "ROCR", "shinyjs", "R6")

#### Import Package ####

checkPackage = function(pack) {
  if (!is.element(pack, installed.packages()[,1])) {
    install.packages(pack, dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }
}

for (package in PACK_LIST) {
  checkPackage(package)
  library(package, character.only = TRUE)
}

DEFAULT_LOAD_STR1 = "Default: coronary"
DEFAULT_LOAD_DATA1 = coronary
DEFAULT_LOAD_STR2 = "Default: asia"
DEFAULT_LOAD_DATA2 = asia

#### Import Classes ####

source("class/Sidebar.r")
source("class/Tab.r")

#### Functions ####

cleanDataset = function(df) {
  # Force to bnlearn supported data type
  for (i in 1:ncol(df)) {
    df[,i] = as.factor(df[,i])
  }

  # Force complete case analysis
  if (length(which(is.na(df))) > 0) {
    print(paste("Removed", length(which(is.na(df))), "incomplete cases."))
    df = df[complete.cases(df),]
  }

  #Get Subset
  if (SUBDATA) {
    subata <- sample.int(n = nrow(df),
      size = floor(0.05*nrow(df)),
      replace = F)
    df <- df[subata, ]
  }

  return (df)
}


valid = function(obj) {
  return(!is.null(obj))
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
  nodes = NULL
  for (nodeName in names(rawData)) {
    rowDf = data.frame(id = nodeName,
                       name = nodeName,
                       label = nodeName,
                       title = nodeName)
   nodes = rbind(nodes, rowDf)
  }
  return(nodes)
}

"
  * take all column names and return a network datastructure readable by bnlearn
  * @param  {list} rawData unmutated data read in from a function like read.csv
  * @return {[type]} a network structure for a graph with no edges
  * - i.e. data with headers 'a','b','c' would return '[a][b][c]'
"
getModString = function(rawData) {
  n = length(rawData)
  modNet = ""
  for (i in 1:n) {
    varName = names(rawData[i])
    if (nchar(varName) > 50) {
      errMsg = paste("The variable in column", i , "exceeds the maximum number of characters")
      if (STANDALONE) {
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

getArcStrength = function(dag, data, criterion) {
  return (arc.strength(dag, data, criterion = criterion))
}

getEdgeList = function(tempDag, rawData, netScore) {
  if (missing(netScore)) {
    netScore = DEFAULT_NETSCORE
  }
  rawArcStrength = getArcStrength(tempDag, rawData, netScore)

  n = nrow(rawArcStrength)
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
  updateNodeStruc(newEdge)
}

"
  * remove edge from visNet only (not from bnlearn edgeDf)
  * @param {str} id id string of edge(s) to delete
"
undoEdge = function(id) {
  visNetworkProxy("myNetId") %>%
    visRemoveEdges(id = id)
}

"
 * check whether the newly proposed edge to be created in a visNet satisfies
 * the following conditions:
 * - new edge is not a duplicate (i.e. does not have the same 'to' and 'from')
 * - new edge is not a reverse of an old edge
 * - new edge does not create a partially directed acyclic graph (pdag)
 * @param  {list} graphChange data structure regarding the new edge:
 * - $cmd = 'addEdge'
 * - $id ~= '4e7db4f3-3484-4902-8d7c-d52f05da9db9'
 * - $from = 'someNodeName'
 * - $to = 'someOtherNodeName'
 * @param  {list} edgeDf current database of all edges in network with params:
 * - from, to, arrows, id
 * @return {string} NULL if no errors occured, else string with error code
"
validEdge = function(graphChange, edgeDf) {
  newEdge = getNewEdge(graphChange, edgeDf)
  nParents = length(nodeStruc[[newEdge$to]][["myParent"]])

  if (isDuplicate(edgeDf, newEdge)) {
    undoEdge(graphChange$id)
    return("Cannot create duplicate edge.")
  }

  pdag = NULL
  pdag = model2network(modelstring(dag))
  #check cyclic on dummy var
  arcError = tryCatch({
    pdag <- set.arc(pdag, newEdge$from, newEdge$to, debug = F)
    return(NULL)
  }, error = function(e) {
    #Cyclical error
    undoEdge(graphChange$id)

    return("The resulting graph contains cycles.")
  })
}

deleteEdge = function(graphChange, edgeDf) {
  if (getDeleteType(graphChange) != "edge") stop("Invalid deletion detected.")

  #Remove edge from db
  deleteIndex = which(edgeDf$id == graphChange$edges)

  f = edgeDf[deleteIndex,1]
  t = edgeDf[deleteIndex,2]

  dag <<- drop.arc(dag, from = f, to = t)

  #Remove from edgeDf
  edgeDf <<- edgeDf[-c(deleteIndex), ]

  #remove deleted edges from nodeStruc
  rmvIndex = which(nodeStruc[[f]][["myChild"]] == t)
  nodeStruc[[f]][["myChild"]] <<- nodeStruc[[f]][["myChild"]][-rmvIndex]

  rmvIndex = which(nodeStruc[[t]][["myParent"]] == f)
  nodeStruc[[t]][["myParent"]] <<- nodeStruc[[t]][["myParent"]][-rmvIndex]
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

updateBnScoreTextBox = function(output, dag, mainData) {
  output$bnScoreTextBox <- renderPrint({
    print(getScore(dag, mainData))
    print(dag)
  })
}

"
 * add/remove at least one new child to a node and a new parent to another node
 * indicating that an edge has been created or destroyed
 * @param  {[type]} edge [description]
 * @return {[type]}      [description]
"
updateNodeStruc = function(edge) {
  t = edge$to
  f = edge$from
  nodeStruc[[f]][["myChild"]] <<- c(nodeStruc[[f]][["myChild"]], t)
  nodeStruc[[t]][["myParent"]] <<- c(nodeStruc[[t]][["myParent"]], f)
}

clearChildParent = function() {
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

    undoEdge(edgeList[i,4])
  }
}

"
 * check if new edge has the same 'to' and 'from' as another edge
 * (i.e. duplicate)
 * @param  {list} database current database of all edges in network with params:
 * - from, to, arrows, id
 * @param  {list} observation current new edge to compare against
 * @return {boolean} T if duplicate, else F
"
isDuplicate = function(database, observation) {
  k = nrow(database)

  if (k == 0) {
    return(FALSE)
  }

  #check if network already has edge with the same 'from' and 'to' nodes
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
  if(is.null(input$myNetId_nodes)) return()
  if (is.null(input$current_node_id)) return()

  nodeLabel = idToLabel(input)
  if (length(nodeLabel) == 0) return()

  parent = nodeStruc[[nodeLabel]][["myParent"]]
  nodesList = c(nodeLabel, parent)

  #compile responseList (from parents)
  responseList = generateList(length(nodesList), "Either")
  output$cptTextBox = renderPrint({
    getMultiPosterior(input, output, nodesList, responseList, mainData)
  })
  updateSpreadsheet(input, output, nodeLabel)
}

updateSpreadsheet = function(input, output, nodeLabel) {
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
    sidebar = Sidebar$new()
    msg = "Please ensure each variable has up to two possible responses"
    warning(msg)
    sidebar$printActiveTextBox(input, output, msg)
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
  if(is.null(input$current_node_id)) return()

  nodeLabel = idToLabel(input)
  if (length(nodeLabel) == 0) return()

  freqTable = table(mainData[[nodeLabel]])
  success = freqTable[1]
  failure = freqTable[2]
  x = seq(0,1,0.01)

  likDist = dbeta(x, success + 1, failure + 1)

  if (is.null(nodeStruc[[nodeLabel]][["prior"]])) {
    plot(x, likDist, type = "l", col = "red",
      ylab = "Density",
      xlab = "Percent",
      main = nodeLabel)
    return()
  }

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
}

getMultiPosterior = function(input, output, nameList, resp, db) {
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
      sidebar = Sidebar$new()
      msg = "Please ensure each variable has up to two possible responses"
      sidebar$printActiveTextBox(input, output, msg)
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
  if (is.null(input$current_node_id)) {
    output$selectState <- renderUI({
      print("Please select a node to generate conditional probability table (CPT)")
    })
    return()
  }

  nodeLabel = idToLabel(input)
  if (length(nodeLabel) == 0) return()

  parent = nodeStruc[[nodeLabel]][["myParent"]]

  #Parents and self
  nodesList = c(nodeLabel, parent)

  output$selectState <- renderUI({
    if (valid(input$current_node_id)) {
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

getScore = function(graph, data) {
    if (is.null(graph)) {
      return("Please import dataset from 'File' tab.")
    }
    rtn = paste("Bayesian Network Score:", round(score(graph, data), 4))
    return(rtn)
}

updateSidebarUi = function(input, output, dag, mainData) {
  if (input$useType == 'CP Table') {
    clickType = getClickType(input)

    if (is.null(clickType)) {
      output$cptTextBox = renderPrint({
        print(getScore(dag, mainData))
      })
    } else if (clickType == "node") {
      printNodeProb(input, output)
    } else if (clickType == "edge") {
      output$hot <- renderRHandsontable({})
      output$savePrior <- renderUI({})
      edgeIndex = which(edgeDf$id == input$myNetId_selectedEdges)

      #CPT radio selected | print arc strength unless it was just deleted
      output$cptTextBox = renderPrint({
        if (nrow(getArcStrength(dag, mainData, input$netScore)[edgeIndex,]) == 0) {
          print(getScore(dag, mainData))
        } else {
          print(getArcStrength(dag, mainData, input$netScore)[edgeIndex,])
        }
      })
    }

  } else if (input$useType == 'BN Score') {
    updateBnScoreTextBox(output, dag, mainData)
  } else if (input$useType == 'Evaluate') {
    output$evalTextBox <- renderPrint({
      print("Calculating...")
    })
    tryMethod = function() {
      strength = boot.strength(mainData, R = 200, m = 30, algorithm = "hc")
      pred = as.prediction(strength, dag)
      perf = performance(pred, "auc")
      output$evalTextBox <- renderPrint({
        print("Area under the ROC curve:")
        print(unlist(perf@y.values))
      })
    }

    tryCatch(tryMethod(),
    error = function(e) {
      output$evalTextBox <- renderPrint({
        print("Could not get area under the ROC curve.")
      })
    })
  }
}

getIp = function(LOCALHOST) {
  if (LOCALHOST) return("127.0.0.1");

  ipconfig = system("ipconfig", intern=TRUE)
  ipv4 = ipconfig[grep("IPv4", ipconfig)]
  ip = gsub(".*? ([[:digit:]])", "\\1", ipv4)
  return(ip)
}

getVisNetwork = function(df) {
  if (missing(df)) {
    df = data.frame(id = 1, hidden = TRUE)
  }
  return (visNetwork(df, edgeDf) %>%
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
    }"))
}

init = function(output) {
  # Create and plot the network structure.
  mainData <<- cleanDataset(mainData)
  dag <<- model2network(getModString(mainData))
  edgeDf <<- getEdgeList(dag, mainData)

  output$myNetId = renderVisNetwork({
    getVisNetwork(nameNodes(mainData))
  })
}

#### Import Data ####
setwd(getwd())

# if (STANDALONE) {
#   source(paste(getwd(),"/fsoPath.dat", sep = ""))
#   mainData = read.csv(filePath, fileEncoding="UTF-8-BOM")
# } else {
#   mainData = read.csv("data/hcp_dataset.csv", fileEncoding="UTF-8-BOM")
# }

mainData = NULL
dag = NULL
edgeDf = NULL


########## #
#### UI ####
########## #

source("class/ui.r")



############## #
#### SERVER ####
############## #

source("./class/server.r")


#shinyApp(ui = ui, server = server)
arcError = tryCatch({
  runApp(list(ui=ui, server=server), host=getIp(LOCALHOST), port=80)
}, error = function(e) {
  warning("Could not run app over localhost.")
  runApp(list(ui=ui, server=server))
})
