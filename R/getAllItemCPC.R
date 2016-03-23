##' Get All Item CPC
##' 
##' Description
##' 
##' @import igraph
##' 


## Function to obtain all CPC item 
getAllItemCPC = function(){
    itemEdgeList =
        adjacent2edge(
            GetCodeTree(domain = "agriculture",
                        dataset = "aproduction",
                        dimension = itemVar)
        )
    itemEdgeGraph = igraph::graph.data.frame(itemEdgeList)
    itemDist = igraph::shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}