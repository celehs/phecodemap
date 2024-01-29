
# color for ICD9s, ICD10s, highlightNode, Phecode

col_vector <- c("#984EA3", "#FFFF33", "#A65628", "#F781BF",
                "#FFD92F", "#E5C494", "#B3B3B3",
                "#DECBE4",
                "#FED9A6", "#FFFFCC", "#E5D8BD",
                "#BEAED4", "#FDC086", "#FFFF99", "#386CB0",
                "#F0027F", "#BF5B17", "#1B9E77",
                "#7570B3", "#66A61E", "#E6AB02", "#A6761D",
                "#1F78B4", "#33A02C", "#FB9A99",
                "#FDBF6F", "#6A3D9A", "#8DD3C7",
                "#B15928",  "#FDDAEC", "#F2F2F2",
                "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9",
                "#FFF2AE", "#F1E2CC", "#CCCCCC", "#377EB8", "#7FC97F",
                "#4DAF4A",
                "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
                "#A6D854",
                "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
                "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#FFED6F", "#666666")


colorlist <- c("lightblue", "palegreen", "red", "orange", col_vector)


buildPath <- function(rootid, icdmap, dict_icd) {

  rootid <- paste0("Phe:", rootid)
  s_map <- icdmap
  s_map$Phecode <- paste0("Phe:", s_map$Phecode)
  s_map <- s_map[grepl(paste0(rootid, "\\..+"), s_map$Phecode, perl = TRUE) | (s_map$Phecode == rootid), ]

  print(paste("s_map:", nrow(s_map)))
  
  getPath3 <- function(node, all_ids, depth = NULL){
    parents <- sapply(1:nchar(node), function(x){
      substring(node, 1, x)
    })
    parents <- parents[parents %in% all_ids]
    if (!is.null(depth)){
      n <- length(parents)
      parents <- c(parents, rep(parents[n], depth - n))
    }
    paste(parents, collapse = "/")
  }

  node_phe <- s_map[, c("Phecode", "Phenotype")]
  node_phe <- node_phe[!duplicated(node_phe), ]
  
  node_phe$pathString <- sapply(node_phe$Phecode, getPath3, paste0("Phe:", unique(icdmap$Phecode)), 3)
  # 
  # node_icd <- s_map[, c("ICD_version", "ICD_id", "ICD_str")]
  # node_icd <- node_icd[!duplicated(node_icd), ]
  # node_icd$pathString <- sapply(node_icd$ICD_id, getPath3, dict_icd$id)
  
  
  
  removeP <- function(x, p, all_ids){
    x[x == p | p == "" | (!x %in% all_ids)] <- ""
    x
  }
  
  getPath4 <- function(node, all_ids, depth = NULL){
    p0 = gsub("\\..+", "", node)
    p1 <- removeP(gsub("^(.+\\..).*$", "\\1", node, perl = TRUE), p0, all_ids)
    p2 <- removeP(gsub("^(.+\\...).*$", "\\1", node, perl = TRUE), p1, all_ids)
    p3 <- removeP(gsub("^(.+\\....).*$", "\\1", node, perl = TRUE), p2, all_ids)
    p4 <- removeP(node, p3, all_ids)
    parents <- paste(p0, p1, p2, p3, p4, sep = "/")
    parents <- gsub("/+$", "", parents, perl = TRUE)
    parents <- gsub("//+", "/", parents, perl = TRUE)
    parents
  }
  
  node_icd <- s_map[, c("ICD_version", "ICD_id", "ICD_str")]
  node_icd <- node_icd[!duplicated(node_icd), ]
  node_icd$pathString <- getPath4(node_icd$ICD_id, dict_icd$id)

  s_map <- dplyr::left_join(s_map, node_phe[, c(1, 3)], by = "Phecode")
  s_map <- dplyr::left_join(s_map, node_icd[, c(2, 4)], by = "ICD_id")
  s_map$pathString <- paste(s_map$pathString.x, s_map$ICD_version,s_map$pathString.y, sep = "/")
  
  getP <- function(x){
    ids <- x
    while(grepl("/", x, fixed = TRUE)){
      x <- gsub("^(.+)/.+$", "\\1", x)
      ids <- c(x, ids)
    }
    ids
  }
  
  ids <- unique(unlist(lapply(s_map$pathString, getP)))
  node <- data.frame(ids = ids,
                     labels = gsub("^.+/([^/]+)$", "\\1", ids, perl = TRUE),
                     parents = gsub("^(.+)/[^/]+$", "\\1", ids, perl = TRUE))
  node$parents[node$ids == rootid] <- ""
  
  if(!rootid %in% node$ids) {
    node$parents[node$ids == node$parents] <- ""
  }
  
  # node_phe <- s_map[, c("Phecode", "Phenotype")]
  # node_phe <- node_phe[!duplicated(node_phe), ]
  # node_phe$pathString <- sapply(node_phe$Phecode, getPath, dict_icd, 3)
  # 
  # node_icd <- s_map[, c("ICD_version", "ICD_id", "ICD_str")]
  # node_icd <- node_icd[!duplicated(node_icd), ]
  # node_icd$pathString <- sapply(node_icd$ICD_id, getPath, dict_icd)
  # 
  # s_map <- dplyr::left_join(s_map, node_phe[, c(1, 3)], by = "Phecode")
  # s_map <- dplyr::left_join(s_map, node_icd[, c(2, 4)], by = "ICD_id")
  # s_map$pathString <- paste(s_map$pathString.x, s_map$ICD_version,s_map$pathString.y, sep = "/")
  # 
  # node <- data.frame(ids = c(), labels = c(), parents = c())
  # for (id in s_map$pathString) {
  #   while (grepl("/", id, fixed = TRUE)) {
  #     all_n <- strsplit(id, "/", fixed = TRUE)[[1]]
  #     label <- all_n[length(all_n)]
  #     parent <- substring(id, 1, nchar(id) - nchar(label) - 1)
  #     node <- rbind(node, data.frame(ids = id, labels = label, parents = parent))
  #     id <- parent
  #   }
  # }
  # node <- node[!duplicated(node), ]
  # node <- rbind(node, data.frame(ids = rootid, labels = rootid, parents = ""))


  node$ICD_str <- NA
  node$ICD_str[grepl("ICD-9", node$ids)] <- (dict_icd$term[dict_icd$version == "ICD-9"])[match(node$labels[grepl("ICD-9", node$ids)], (dict_icd$id[dict_icd$version == "ICD-9"]))]
  node$ICD_str[grepl("ICD-10", node$ids)] <- (dict_icd$term[dict_icd$version == "ICD-10-cm"])[match(node$labels[grepl("ICD-10", node$ids)], (dict_icd$id[dict_icd$version == "ICD-10-cm"]))]
  node$Phenotype <- icdmap$Phenotype[match(node$labels, paste0("Phe:", icdmap$Phecode))]
  node <- node[!duplicated(node), ]
  node[is.na(node)] <- ""
  node$strs <- paste0(node$ICD_str, node$Phenotype)
  node$strs[node$strs == ""] <- node$labels[node$strs == ""]
  return(node)
}

addClass <- function(rootid, icdmap, dict_icd, df_highlight = df_highlight){
  node <- buildPath(rootid, icdmap, dict_icd)
  nodes_color <- node[order(node$ids), c("ids", "labels")]
  nodes_color$newid <- nodes_color$labels

  node_phe <- unique(node$labels[grepl("Phe", node$labels)])

  nodes1 <- nodes_color[(!duplicated(nodes_color$labels)) | (!nodes_color$labels %in% node_phe),]
  # dup nodes in phecode
  nodes2 <- nodes_color[duplicated(nodes_color$labels) & (nodes_color$labels %in% node_phe),]
  nodes2$newid <- sapply(1:nrow(nodes2), addBlank)

  dup_node <- unique(nodes1$newid[duplicated(nodes1$newid)])
  if (length(dup_node) != 0) {
    for (i in dup_node) {
      len <- 0:(sum(nodes1$newid %in% i)-1)
      nodes1$newid[nodes1$newid %in% i] <- paste0(sapply(len, addBlank), i)
    }
  }
  nodes2$class <- "dupnode"
  nodes1$class <- sapply(nodes1$ids, classifyNode)
  # add highlight class

  df_highlight$Phecode <- paste0("Phe:", df_highlight$Phecode)
  highligh_nodes <- intersect(node_phe, df_highlight$Phecode)

  if (length(highligh_nodes) > 0){
    for (i in highligh_nodes){
      nodes1$class[nodes1$labels == i] <- paste0(i, "(only ", df_highlight$ICD_version[df_highlight$Phecode == i], ")")
    }
  }
  node <- addAsterisk(node)
  return(list(node, nodes1, nodes2))
}



addColor <- function(nodes_list, plot = "tree"){

  node <- nodes_list[[1]]
  nodes1 <- nodes_list[[2]]
  nodes2 <- nodes_list[[3]]

  df_color <- data.frame(class = unique(nodes1$class), color = "")
  df_color$color[df_color$class == "ICD-9"] <- colorlist[1]
  df_color$color[df_color$class == "ICD-10-cm"] <- colorlist[2]
  df_color$color[!df_color$class %in% c("ICD-9", "ICD-10-cm")] <- colorlist[4:(nrow(df_color[!df_color$class %in% c("ICD-9", "ICD-10-cm"),])+3)]

  df_color$color[grepl("only", df_color$class)] <- colorlist[3]
  nodes1 <- dplyr::left_join(nodes1, df_color, by = "class")
  if (plot == "tree"){
    nodes2$color <- "none"
  } else{
    nodes2 <- dplyr::left_join(nodes2, nodes1[, c("labels", "color")], by = "labels", keep = FALSE)
  }
  return(list(node, nodes1, nodes2))
}


dfPlot <- function(nodes_list, plot = "tree"){

  nodes_list <- addColor(nodes_list, plot)

  node <- nodes_list[[1]]
  nodes1 <- nodes_list[[2]]
  nodes2 <- nodes_list[[3]]
  nodes_color <- rbind(nodes1, nodes2)

  df_plot <- dplyr::left_join(node, nodes_color[, -2], by = "ids")
  df_plot <- dplyr::left_join(df_plot, nodes_color[, c("ids", "newid")], by = c("parents" = "ids"))
  df_plot <- df_plot[, c(10, 7, 1, 3, 2, 6, 8, 9)]
  colnames(df_plot) <- c("parents", "ids", "nodepath", "parentpath", "labels", "info", "class","color")
  return(df_plot)
}

dfSunburst <- function(nodes_list){
  df_plot <- dfPlot(nodes_list, plot = "sunb")
  df_plot$linecolor <- df_plot$color
  dup_nodes <- df_plot$labels[grep("Phe", df_plot$labels)]
  dup_nodes <- unique(dup_nodes[duplicated(dup_nodes)])
  df_plot$linecolor[!df_plot$labels %in% dup_nodes] <- "white"
  df_plot$labels[df_plot$class == "dupnode"] <- ""
  return(df_plot)
}

sunburstPlotly <- function(centernode, df_plot, maxd = 10) {
  plotly::plot_ly(df_plot,
          ids =~ ids,
          labels =~ labels,
          parents =~ parents,
          hovertext =~ info,
          maxdepth = maxd,
          marker = list(colors = df_plot$color,
                        line = list(color = df_plot$linecolor #,
                                    # width = df_plot$linewidth
                                    )
                        ),
          type = "sunburst") %>%
  plotly::config(
    toImageButtonOptions = list(
      format = "svg",
      filename = paste0("barplot_", paste(centernode, collapse = "_"))
    ))
}

treePlot <- function(nodes_list, maxd = 4, collapsed = FALSE) {
  df_plot <- dfPlot(nodes_list, plot = "tree")
  df_plot <- df_plot[sapply(df_plot$nodepath, filterNode, maxd), ]
  asterisk_ids1 <- df_plot$ids[grepl("\\d\\*$", df_plot$labels, perl = TRUE)]
  asterisk_ids2 <- df_plot$ids[grepl("\\d\\*{2}$", df_plot$labels, perl = TRUE)]
  
  df_plot$ids[df_plot$ids %in% asterisk_ids1] <- paste0(df_plot$ids[df_plot$ids %in% asterisk_ids1], "*")
  df_plot$ids[df_plot$ids %in% asterisk_ids2] <- paste0(df_plot$ids[df_plot$ids %in% asterisk_ids2], "**")
  df_plot$parents[df_plot$parents %in% asterisk_ids1] <- paste0(df_plot$parents[df_plot$parents %in% asterisk_ids1], "*")
  df_plot$parents[df_plot$parents %in% asterisk_ids2] <- paste0(df_plot$parents[df_plot$parents %in% asterisk_ids2], "**")
  collapsibleTree::collapsibleTreeNetwork(df_plot,
                         attribute = "labels", fill = "color",
                         collapsed = collapsed, tooltip = TRUE,
                         tooltipHtml = "info", width = "100%", height = 1000
  )
}

legends <- function(df_sunb){
  df <- delDupRow(df_sunb[, c("class", "color")])
  df <- df[df$class != "dupnode",]
  df <- df[order(df$class),]
  options(repr.plot.width=6, repr.plot.height=18)
  par(mar=c(1,1,1,1))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  graphics::legend("top", legend =df$class, pch=16, pt.cex=3, cex=1.2, bty='n',
         y.intersp=1.5, ncol=2,
         col = df$color)
}

delDupRow <- function(x, cols = 1:ncol(x)) {
  x <- x[, cols]
  x[!duplicated(x), ]
}

getParents <- function(node, dict_icd){
  parents <- strsplit(node, ".", fixed = TRUE)[[1]][1]
  children <- strsplit(node, ".", fixed = TRUE)[[1]][2]
  if (!is.na(children)){
    for (i in strsplit(children, "")[[1]]){
      p <- parents[length(parents)]
      p <- ifelse(grepl(".", p, fixed = TRUE), p, paste0(p, "."))
      parents <- c(parents, paste0(p, i))
    }
  }
  if(node %in% dict_icd$id){
    parents <- parents[parents %in% dict_icd$id]
  }
  parents
}

getPath <- function(node, dict_icd, depth = NULL){
  parents <- getParents(node, dict_icd)
  path <- paste(parents, collapse = "/")
  if (!is.null(depth)){
    p <- parents[length(parents)]
    dup_node <- rep(p, depth - length(parents))
    path <- purrr::reduce(c(path, dup_node), function(a, b){paste(a, b, sep = "/")})
  }
  path
}

filterNode <- function(x, maxd, pattern = "/") {
  return(length(strsplit(x, pattern, fixed = TRUE)[[1]]) <= maxd)
}

# if ICD in the path, class -> ICD_version
classifyNode <- function(x) {
  all_path <- strsplit(x, "/", fixed = TRUE)[[1]]
  if (grepl("CD", x, fixed = TRUE)) {
    return(all_path[grepl("CD", all_path, fixed = TRUE)])
  } else {
    return(all_path[length(all_path)])
  }
}

addBlank <- function(x){
  paste(rep(" ", times = x), collapse = "")
}





# getUnid <- function(session, dict_uqid){
#   url_vars <- session$clientData$url_search
#   if(grepl("uqid", url_vars)){
#     uqid = gsub(".*\\?uqid=(\\w+)$", "\\1", url_vars, perl = TRUE)
#     dict_uqid <- readr::read_csv(dict_uqid)
#     df_uqid <- NULL
#     for (i in 1:ncol(dict_uqid)){
#       if(sum(!grepl("^[A-Za-z0-9]+$", dict_uqid[[i]])) == 0){
#         df_uqid$uqid <- as.character(dict_uqid[[i]])
#       } else if(sum(!grepl("^[\\w\\:\\.\\-]+$", dict_uqid[[i]], perl = TRUE)) == 0){
#         df_uqid$id <- dict_uqid[[i]]
#         df_uqid$id[grepl("_", df_uqid$id)] <- gsub("Phe_", "PheCode:", df_uqid$id[grepl("_", df_uqid$id)])
#         df_uqid$id[grepl("_", df_uqid$id)] <- gsub("_", ".", df_uqid$id[grepl("_", df_uqid$id)])
#       }
#     }
#     paste0(df_uqid$id[df_uqid$uqid == uqid])
#   } else {
#     id = gsub(".*\\?phecode=([\\w\\.]+)$", "\\1", url_vars, perl = TRUE)
#     paste0("PheCode:", id)
#   }
# }

addAsterisk <- function(node){
  # nodes_list <- readRDS("~/celehs/phecodemap/test_nodes_list.rds")
  # node <- nodes_list[[1]]
  # nodes1 <- nodes_list[[2]]
  # nodes2 <- nodes_list[[3]]
  node$labels_p <- gsub("^.*Phe\\:([\\d\\.]+)/ICD.+$", "\\1", node$parents, perl = TRUE)
  node$labels_p[!grepl("ICD", node$parents)] <- NA
  node$ICD_version <- gsub(".+(ICD\\-\\d+[\\-cm]*).*", "\\1", node$parents, perl = TRUE)
  node$ICD_version[!grepl("ICD", node$parents)] <- NA
  node <- dplyr::left_join(node, icdmap[, c(1,2,4,6)], by = c("ICD_version", "labels" = "ICD_id", "labels_p" = "Phecode"))
  if(sum(grepl(0, node$Rollup)) >0 ){
    node$asterisk <- NA
    node$asterisk[node$Rollup %in% 0] <- "*"
    node$asterisk[node$Rollup %in% 0 & grepl("\\.\\d\\d", node$labels_p, perl = TRUE)] <- "**"
    node$labels[!is.na(node$asterisk)] <- paste0(node$labels[!is.na(node$asterisk)], node$asterisk[!is.na(node$asterisk)])
  }
  node[, 1:6]
}