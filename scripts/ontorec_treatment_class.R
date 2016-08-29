#install.packages("rjson", "plyr", "dplyr", "ggplot2", "gridExtra", "nortest")

require("rjson")
require("plyr")
require("dplyr")
require("ggplot2")
require("gridExtra")
require("nortest")

#######################
## General functions ##
#######################

#fixJsonFormat <- function (data) {
#  data <- gsub(' ', '', data)
#  data <- gsub(',', '","', data)
#  data <- gsub('\\[', '["', data)
#  data <- gsub('\\]', '"]', data)
#  data <- toJSON(sort(fromJSON(data)))
#  
#  return (data)
#}

fixJsonFormat <- function (data) {
  data <- gsub('\\[', '', data)
  data <- gsub('\\]', '', data)
  data <- strsplit(data, split=',')[[1]]
  data <- toJSON(data)
  
  return (data)
}


setDFColFirstLineValue <- function (df, colName, newValue) {
  df <- df[ , !(names(df) %in% c(colName))]
  df[1, colName] <- newValue
  
  return (df)
}

calculatePrecision <- function(recommended, selected) {
  recommended <- fromJSON(toString(recommended))
  selected <- fromJSON(toString(selected))
  
  if (selected[1] == "") {
    selected = c()
  }
  
  return (length(intersect(selected, recommended)) / length(recommended))
}

simplifyPreferencesColumn <- function(dfLine) {
  result <- dfLine[1, "user_profile"]
  result <- paste("[", as.character(result), "]", sep = "")
  result <- as.data.frame(fromJSON(result))
  
  # prevents the loss of names if the result data frame has only one column:
  result <- cbind(data.frame(INVALID.REMOVE.ME = 1), result)
  
  result <- result[, colSums(result != 0) > 0]
  result <- names(result)
  
  # removes the invalid result:
  result <- result[result != 'INVALID.REMOVE.ME']
  
  result <- toJSON(result)
  
  dfLine <- subset(dfLine, select = -c(user_profile))
  dfLine[1, "user_profile"] <- result
  
  return (dfLine)
}

printFeatureUsage <- function (experimentData) {
  groups <- as.vector(unique(experimentData$user_degree))
  featuresCount <- data.frame()
  
  for (group in groups) {
    groupData <- experimentData[experimentData$user_degree == group, ]
    profileCounts <- data.frame(POP = c(0))
    
    for (i in 1:nrow(groupData)) {
      dfLine <- groupData[i, ]
      userProfile <- fromJSON(dfLine[1, "user_profile"])
      for (element in userProfile) {
        if (element %in% names(profileCounts)) {
          currentValue <- profileCounts[1, element]
          currentValue <- currentValue + 1
          profileCounts <- profileCounts[, !(colnames(profileCounts) %in% c(element))]
        } else {
          currentValue <- 1
        }
        
        newCol <- data.frame(
          X = c(currentValue)
        )
        names(newCol) <- c(element)
        if (nrow(profileCounts) == 0) {
          profileCounts <- newCol
        } else {
          profileCounts <- cbind(profileCounts, newCol)
        }
      }
      result <- rbind(rep(1, length(userProfile)), data.frame())
      names(result) <- userProfile
    }
    
    profileCounts <- profileCounts[ , order(names(profileCounts))]
    
    png(filename = paste("classDiagram_featureUsage_", group, ".png", sep = ""), width = 1200, height= 500)
    barplot(
      as.numeric(profileCounts[1, ]),
      names = names(profileCounts),
      xlab = "Features", ylab = "Quantity",
      ylim = c(0, 14),
      main = group
    )
    dev.off()
    
    print(profileCounts)
  }
}



########################
## Preparing the data ##
########################

experimentData <- read.csv('experiment_instance.csv')
experimentData <- experimentData %>%
  filter(complete.cases(.)) %>%
  filter(current_experiment == 'class') %>%
  select(c(
    Id,
    user_degree,
    user_profile,
    has_professional_experience,
    random_approach_topn,
    bag_of_words_approach_topn,
    content_based_approach_topn,
    knowledge_based_approach_topn,
    random_approach_selections,
    bag_of_words_approach_selections,
    content_based_approach_selections,
    knowledge_based_approach_selections
  ))

classDiagrams <- read.csv('item_profile.csv')
classDiagrams <- classDiagrams %>% filter(type == 'class')

classDiagrams <- ddply(classDiagrams, .(id), function(dfLine) {
  dfLine <- simplifyProfile(dfLine)
})

experimentData <- ddply(experimentData, .(Id), function(dfLine) {
  # Converting professional experience column to boolean:
  profExp <- as.logical(dfLine$has_professional_experience)
  dfLine <- dfLine[, !(names(dfLine) %in% c("has_professional_experience"))]
  dfLine[, "has_professional_experience"] <- profExp
  
  dfLine <- simplifyPreferencesColumn(dfLine)
  
  # Fixing JSON format at columns:
  approaches <- c(
    "random",
    "bag_of_words",
    "content_based",
    "knowledge_based"
  )
  
  suffixes <- c(
    "approach_topn",
    "approach_selections"
  )
  
  for (suffix in suffixes) {
    for (approach in approaches) {
      colName <- paste(approach, suffix, sep = "_")
      dfLine <- setDFColFirstLineValue(
        dfLine,
        colName,
        fixJsonFormat(dfLine[1, colName])
      )
    }
  }
  
  # Calculating the precisions:
  for (approach in approaches) {
    dfLine[1, paste('precision', approach, sep = "_")] <- calculatePrecision(
      dfLine[1, paste(approach, suffixes[1], sep = "_")],
      dfLine[1, paste(approach, suffixes[2], sep = "_")]
    )
  }
  
  userProfile <- fromJSON(dfLine[1, "user_profile"])
  mrrRandom <- 0
  mrrBagOfWords <- 0
  mrrContentBased <- 0
  mrrKnowledgeBased <- 0
  
  diagramsRandom <- fromJSON(dfLine[1, "random_approach_topn"])
  diagramsBagOfWords <- fromJSON(dfLine[1, "bag_of_words_approach_topn"])
  diagramsContentBased <- fromJSON(dfLine[1, "content_based_approach_topn"])
  diagramsKnowledgeBased <- fromJSON(dfLine[1, "knowledge_based_approach_topn"])
  
  index <- 0
  for (diagramId in diagramsRandom) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      mrrRandom <- mrrRandom + 1 / index
    }
  }
  
  index <- 0
  for (diagramId in diagramsBagOfWords) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      mrrBagOfWords <- mrrBagOfWords + 1 / index
    }
  }
  
  index <- 0
  for (diagramId in diagramsContentBased) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      mrrContentBased <- mrrContentBased + 1 / index
    }
  }
  
  index <- 0
  for (diagramId in diagramsKnowledgeBased) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      mrrKnowledgeBased <- mrrKnowledgeBased + 1 / index
    }
  }
  mrrRandom <- mrrRandom / 3
  mrrBagOfWords <- mrrBagOfWords / 3
  mrrContentBased <- mrrContentBased / 3
  mrrKnowledgeBased <- mrrKnowledgeBased / 3
  
  dfLine[, "mrr_random"] <- mrrRandom
  dfLine[, "mrr_bag_of_words"] <- mrrBagOfWords
  dfLine[, "mrr_content_based"] <- mrrContentBased
  dfLine[, "mrr_knowledge_based"] <- mrrKnowledgeBased
  
  dfLine[, "profile_length"] <- length(fromJSON(dfLine$user_profile))
  
  return (dfLine)
})

cat("The mean of features selected at experiment is", mean(experimentData$profile_length), ", and the median is", median(experimentData$profile_length), "\n")
ddply(experimentData, .(user_degree), function(subDf) {
  cat("The mean of features selected for the group", as.character(subDf[1, "user_degree"]), "is", mean(subDf$profile_length), ", and the median is", median(subDf$profile_length), "\n")
})

precisionsByApproach <- ddply(experimentData, .(Id), function(dfLine) {
  userDegrees <- rep(as.character(dfLine[1, "user_degree"]), 4)
  hasProfessionalExperience <- rep(as.logical(dfLine[1, "has_professional_experience"]), 4)
  approaches <- c(
    "random",
    "bag_of_words",
    "content_based",
    "knowledge_based"
  )
  precisions <- c(
    as.double(dfLine[1, "precision_random"]),
    as.double(dfLine[1, "precision_bag_of_words"]),
    as.double(dfLine[1, "precision_content_based"]),
    as.double(dfLine[1, "precision_knowledge_based"])
  )
  mrr <- c(
    as.double(dfLine[1, "mrr_random"]),
    as.double(dfLine[1, "mrr_bag_of_words"]),
    as.double(dfLine[1, "mrr_content_based"]),
    as.double(dfLine[1, "mrr_knowledge_based"])
  )
  result <- data.frame(
    user_degree = userDegrees,
    has_professional_experience = hasProfessionalExperience,
    approach = approaches,
    precision = precisions,
    mrr = mrr
  )
  
  return (result)
})

colsToSelect <- c("approach", "has_professional_experience", "precision")
precisionsByApproach.graduateCurrent <- precisionsByApproach[precisionsByApproach$user_degree == "graduate_current", colsToSelect]
precisionsByApproach.graduateFinished <- precisionsByApproach[precisionsByApproach$user_degree == "graduate_finished", colsToSelect]
precisionsByApproach.postgraduateCurrent <- precisionsByApproach[precisionsByApproach$user_degree == "postgraduate_current", colsToSelect]
precisionsByApproach.postgraduateFinished <- precisionsByApproach[precisionsByApproach$user_degree == "postgraduate_finished", colsToSelect]
rm (colsToSelect)


###################
## Printing data ##
###################

precisionPercentTableByApproach <- function (df) {
  approaches = as.character(unique(precisionsByApproach$approach))
  result <- data.frame()
  
  for (app in approaches) {
    subDf <- precisionsByApproach %>% filter(approach == app)
    
    auxDf <- data.frame(
      X = c(
        nrow(subDf %>% filter(precision == 0)) / nrow(subDf),
        nrow(subDf %>% filter(precision > 0.3, precision < 0.4)) / nrow(subDf),
        nrow(subDf %>% filter(precision > 0.6, precision < 0.7)) / nrow(subDf),
        nrow(subDf %>% filter(precision == 1)) / nrow(subDf)
      )
    )
    row.names(auxDf) <- c("0%", "33%", "66%", "100%")
    names(auxDf) <- c(app)
    if (nrow(result) == 0) {
      result <- auxDf
    } else {
      result <- cbind(auxDf, result)
    }
  }
  
  return (result)
}

plotApproachPrecisionMeansPercent <- function (ppt) {
  plotData <- data.frame()
  
  for (rowName in row.names(ppt)) {
    auxPlotData <- data.frame(
      percent = replicate(length(names(ppt)), rowName),
      approach = gsub('_', '-', names(ppt)),
      precisionMean = round(as.numeric(ppt[rowName, ] * 100), digits = 1)
    )
    
    if (nrow(plotData) == 0) {
      plotData <- auxPlotData
    } else {
      plotData <- rbind(plotData, auxPlotData)
    }
  }
  
  ggplot(plotData, aes(x = factor(percent), y = precisionMean, fill = approach)) +
    geom_bar(stat = "identity", position = "dodge") +
    #guides(fill=FALSE) +
    expand_limits(y = c(0, 50)) +
    scale_x_discrete(name = "Approach") +
    scale_y_continuous(name = "Precision frequency (%)")
}

plotApproachPrecisions <- function (pba, topLimit, mainTitle) {
  approaches <- gsub('_', '-', unique(pba$approach))
  
  meanByPrecision <- ddply(pba, .(approach), summarize, mean=mean(precision))
  
  plot <- ggplot(meanByPrecision, aes(x = factor(approach), y = mean, fill = approach)) +
    geom_bar(stat = "identity", position = "dodge") +
    expand_limits(y = c(0, 1)) +
    scale_x_discrete(
      name = "Approach",
      labels = c(
        "bag_of_words" = "Bag-of-Words",
        "content_based" = "Content-based",
        "knowledge_based" = "Knowledge-based",
        "random" = "Random"
      )
    ) +
    scale_y_continuous(name = "Precision means") +
    theme(legend.position = "none")
    ggtitle(mainTitle)
  
  # frequencyByPrecision <- ddply(pba, .(approach, precision), summarize, sum=length(precision))
  #
  # plot <- ggplot(frequencyByPrecision, aes(x = factor(precision), y = sum, fill = approach)) +
  #   geom_bar(stat = "identity", position = "dodge") +
  #   expand_limits(y = c(0, topLimit)) +
  #   scale_x_discrete(
  #     name = "Calculated precision",
  #     labels = c("0%", "33%", "66%", "100%")
  #   ) +
  #   scale_y_continuous(name = "Number of times") +
  #   scale_fill_discrete(
  #     name = "Approach",
  #     labels = c(
  #       "bag_of_words" = "Bag-of-Words",
  #       "content_based" = "Content-based",
  #       "knowledge_based" = "Knowledge-based",
  #       "random" = "Random"
  #     )
  #   ) +
  #   ggtitle(mainTitle)
  
  return (plot)
}

plotApproachPrecisionsBoxplots <- function (pba, mainTitle) {
  plot <- ggplot(pba, aes(approach, precision, fill = approach)) +
    geom_boxplot() +
    scale_x_discrete(
      name = "Approach",
      labels = c(
        "bag_of_words" = "Bag-of-Words",
        "content_based" = "Content-based",
        "knowledge_based" = "Knowledge-based",
        "random" = "Random"
      )
    ) +
    scale_y_continuous(name = "Precision") +
    guides(fill = FALSE) +
    ggtitle(mainTitle)
  
  return (plot)
}

plot <- ggplot(pba, aes(approach, mrr, fill = approach)) +
  geom_boxplot() +
  scale_x_discrete(
    name = "Approach",
    labels = c(
      "bag_of_words" = "Bag-of-Words",
      "content_based" = "Content-based",
      "knowledge_based" = "Knowledge-based",
      "random" = "Random"
    )
  ) +
  scale_y_continuous(name = "Mean Reciprocal Rank (MRR)") +
  guides(fill = FALSE) +
  ggtitle(mainTitle)



plotApproachPrecisions(precisionsByApproach, 25, "")
ggsave("classDiagrams-precisionsByApproach.pdf", plot = last_plot(), width = 5, height = 2)

grid.arrange(
  plotApproachPrecisions(precisionsByApproach.graduateCurrent, 10, "Number of precisions by approach\nfor undergraduate students"),
  plotApproachPrecisions(precisionsByApproach.graduateFinished, 10, "Number of precisions by approach\nfor graduated subjects"),
  plotApproachPrecisions(precisionsByApproach.postgraduateCurrent, 10, "Number of precisions by approach\nfor postgraduate students"),
  plotApproachPrecisions(precisionsByApproach.postgraduateFinished, 10, "Number of precisions by approach\nfor postgraduated subjects"),
  
  layout_matrix = cbind(
    c(1, 3),
    c(2, 4)
  )
)
ggsave("classDiagrams-precisionsByApproach.pdf", plot = last_plot(), width = 40, height = 16)

png("classDiagrams-precisionsByApproachBoxplot.png", width = 350, height = 350)
plotApproachPrecisionsBoxplots(precisionsByApproach, "Precisions by approach for all groups")
dev.off()

png("classDiagrams-precisionsByApproachBoxplotByEA.png", width = 740, height = 680)
grid.arrange(
  plotApproachPrecisionsBoxplots(precisionsByApproach.graduateCurrent, "Precisions by approach\nfor undergraduate students"),
  plotApproachPrecisionsBoxplots(precisionsByApproach.graduateFinished, "Precisions by approach\nfor graduated subjects"),
  plotApproachPrecisionsBoxplots(precisionsByApproach.postgraduateCurrent, "Precisions by approach\nfor postgraduate students"),
  plotApproachPrecisionsBoxplots(precisionsByApproach.postgraduateFinished, "Precisions by approach\nfor postgraduated subjects"),
  
  layout_matrix = cbind(
    c(1, 3),
    c(2, 4)
  )
)
dev.off()


#qplot(
#  fill = user_degree,
#  x = user_degree,
#  y = user_degree,
#  data = precisionsByApproach,
#  geom = "bar",
#  main="Precision for undergraduate students",
#  xlab="Frequency",
#  ylab="Precision"
#)

#qplot(x=group, y=mean, fill=variable,
#      data=means, geom="bar", stat="identity",
#      position="dodge")

############################
## Calculating Statistics ##
############################

printFeatureUsage(experimentData)

#  PABC PADV PAO PASC PCOA PDA PEN PGS PNAA POD POG POI POP POQ PRI PSA PSHA PSO PTC
#   11    7   7    6    6   5   8   2    3  11   9  11   3   5   5   9    2   7   7
#   6     2   4    4    2   2   5   2    3   5   6   8   0   1   5   5    2   2   3
#   12    7   6    8    8   3   8   1    6   9  13  11   3   3   7   7    5   3   5
#   7     3   5    7    6   3   7   6    2   8   7   8   3   2   7   8    3   3   3
featureUsage <- c(36, 19, 22, 25, 22, 13, 28, 11, 14, 33, 35, 38, 9, 11, 24, 29, 12, 15, 18)
names(featureUsage) <- c("PABC", "PADV", "PAO", "PASC", "PCOA", "PDA", "PEN", "PGS", "PNAA", "POD", "POG", "POI", "POP", "POQ", "PRI", "PSA", "PSHA", "PSO", "PTC")
sort(featureUsage, decreasing = TRUE)[1:8]
sort(featureUsage, decreasing = TRUE)[1:10]
#barplot(featureUsage)
ggplot(
    data.frame(names(featureUsage), as.numeric(featureUsage)),
    aes(names(featureUsage), as.numeric(featureUsage))
  ) +
  geom_bar(stat = "identity") +
  labs(
    x = "Features",
    y = "Number of occurrences"
  ) +
  scale_y_continuous(breaks = round(seq(0, max(featureUsage), by = 5), 1))

ggsave("classDiagram_featureUsage_ALL.pdf", plot = last_plot(), width = 10, height = 4)



# Normality testing
calculateNormalityTests <- function () {
  shapiroResultAll <- shapiro.test(as.numeric(precisionsByApproach$precision))
  shapiroResultGC <- shapiro.test(as.numeric(precisionsByApproach.graduateCurrent$precision))
  shapiroResultGF <- shapiro.test(as.numeric(precisionsByApproach.graduateFinished$precision))
  shapiroResultPC <- shapiro.test(as.numeric(precisionsByApproach.postgraduateCurrent$precision))
  shapiroResultPF <- shapiro.test(as.numeric(precisionsByApproach.postgraduateFinished$precision))
  
  adResultAll <- ad.test(as.numeric(precisionsByApproach$precision))
  adResultGC <- ad.test(as.numeric(precisionsByApproach.graduateCurrent$precision))
  adResultGF <- ad.test(as.numeric(precisionsByApproach.graduateFinished$precision))
  adResultPC <- ad.test(as.numeric(precisionsByApproach.postgraduateCurrent$precision))
  adResultPF <- ad.test(as.numeric(precisionsByApproach.postgraduateFinished$precision))

  resultDF <- data.frame(
    all = c(
      paste("W = ", as.numeric(shapiroResultAll$statistic), ", p-value = ", shapiroResultAll$p.value, sep = ""),
      paste("A = ", as.numeric(adResultAll$statistic), ", p-value = ", adResultAll$p.value, sep = "")
    ),
    gc = c(
      paste("W = ", as.numeric(shapiroResultGC$statistic), ", p-value = ", shapiroResultGC$p.value, sep = ""),
      paste("A = ", as.numeric(adResultGC$statistic), ", p-value = ", adResultGC$p.value, sep = "")
    ),
    gf = c(
      paste("W = ", as.numeric(shapiroResultGF$statistic), ", p-value = ", shapiroResultGF$p.value, sep = ""),
      paste("A = ", as.numeric(adResultGF$statistic), ", p-value = ", adResultGF$p.value, sep = "")
    ),
    pc = c(
      paste("W = ", as.numeric(shapiroResultPC$statistic), ", p-value = ", shapiroResultPC$p.value, sep = ""),
      paste("A = ", as.numeric(adResultPC$statistic), ", p-value = ", adResultPC$p.value, sep = "")
    ),
    pf = c(
      paste("W = ", as.numeric(shapiroResultPF$statistic), ", p-value = ", shapiroResultPF$p.value, sep = ""),
      paste("A = ", as.numeric(adResultPF$statistic), ", p-value = ", adResultPF$p.value, sep = "")
    )
  )
  
  names(resultDF) <- c("All", "Undergraduate students", "Graduated subjects", "Postgraduate students", "Postgraduated subjects")
  row.names(resultDF) <- c("Shapiro-Wilk", "Anderson-Darling")
  
  return (resultDF)
}

calculateNormalityTests()

# Boxplots
#par(mfrow=c(2,2))
#boxplot(precision ~ approach, data = precisionsByApproach.graduateCurrent, main="graduate current")
#boxplot(precision ~ approach, data = precisionsByApproach.graduateFinished, main="graduate finished")
#boxplot(precision ~ approach, data = precisionsByApproach.postgraduateCurrent, main="post graduate current")
#boxplot(precision ~ approach, data = precisionsByApproach.postgraduateFinished, main="post graduate finished")

# Kruskal-Wallis
kruskal.test(precision ~ approach, data = precisionsByApproach)
kruskal.test(precision ~ approach, data = precisionsByApproach.graduateCurrent)
kruskal.test(precision ~ approach, data = precisionsByApproach.graduateFinished)
kruskal.test(precision ~ approach, data = precisionsByApproach.postgraduateCurrent)
kruskal.test(precision ~ approach, data = precisionsByApproach.postgraduateFinished)

# Bonferroni
pairwise.t.test(
  precisionsByApproach$precision,
  precisionsByApproach$approach,
  p.adj = "bonf"
)
pairwise.t.test(
  precisionsByApproach.graduateCurrent$precision,
  precisionsByApproach.graduateCurrent$approach,
  p.adj = "bonf"
)
pairwise.t.test(
  precisionsByApproach.graduateFinished$precision,
  precisionsByApproach.graduateFinished$approach,
  p.adj = "bonf"
)
pairwise.t.test(
  precisionsByApproach.postgraduateCurrent$precision,
  precisionsByApproach.postgraduateCurrent$approach,
  p.adj = "bonf"
)
pairwise.t.test(
  precisionsByApproach.postgraduateFinished$precision,
  precisionsByApproach.postgraduateFinished$approach,
  p.adj = "bonf"
)

source('nonparametric_effect_sizes.r')

combinationTestSets <- list(
  allGroups = list(
    random = precisionsByApproach[precisionsByApproach$approach == 'random', ],
    bagOfWords = precisionsByApproach[precisionsByApproach$approach == 'bag_of_words', ],
    contentBased = precisionsByApproach[precisionsByApproach$approach == 'content_based', ],
    knowledgeBased = precisionsByApproach[precisionsByApproach$approach == 'knowledge_based', ]
  ),
  graduateCurrent = list(
    random = precisionsByApproach.graduateCurrent[precisionsByApproach.graduateCurrent$approach == 'random', ],
    bagOfWords = precisionsByApproach.graduateCurrent[precisionsByApproach.graduateCurrent$approach == 'bag_of_words', ],
    contentBased = precisionsByApproach.graduateCurrent[precisionsByApproach.graduateCurrent$approach == 'content_based', ],
    knowledgeBased = precisionsByApproach.graduateCurrent[precisionsByApproach.graduateCurrent$approach == 'knowledge_based', ]
  ),
  graduateFinished = list(
    random = precisionsByApproach.graduateFinished[precisionsByApproach.graduateFinished$approach == 'random', ],
    bagOfWords = precisionsByApproach.graduateFinished[precisionsByApproach.graduateFinished$approach == 'bag_of_words', ],
    contentBased = precisionsByApproach.graduateFinished[precisionsByApproach.graduateFinished$approach == 'content_based', ],
    knowledgeBased = precisionsByApproach.graduateFinished[precisionsByApproach.graduateFinished$approach == 'knowledge_based', ]
  ),
  postgraduateCurrent = list(
    random = precisionsByApproach.postgraduateCurrent[precisionsByApproach.postgraduateCurrent$approach == 'random', ],
    bagOfWords = precisionsByApproach.postgraduateCurrent[precisionsByApproach.postgraduateCurrent$approach == 'bag_of_words', ],
    contentBased = precisionsByApproach.postgraduateCurrent[precisionsByApproach.postgraduateCurrent$approach == 'content_based', ],
    knowledgeBased = precisionsByApproach.postgraduateCurrent[precisionsByApproach.postgraduateCurrent$approach == 'knowledge_based', ]
  ),
  postgraduateFinished = list(
    random = precisionsByApproach.postgraduateFinished[precisionsByApproach.postgraduateFinished$approach == 'random', ],
    bagOfWords = precisionsByApproach.postgraduateFinished[precisionsByApproach.postgraduateFinished$approach == 'bag_of_words', ],
    contentBased = precisionsByApproach.postgraduateFinished[precisionsByApproach.postgraduateFinished$approach == 'content_based', ],
    knowledgeBased = precisionsByApproach.postgraduateFinished[precisionsByApproach.postgraduateFinished$approach == 'knowledge_based', ]
  )
)

# Vargha Delaney and Wilcoxon
doCombinationTests <- function (testSets, outputPrefix) {
  options(warn=1)
  testSeparator <- paste(paste(rep("-", 80), collapse = ""), "\n\n", sep = "")
  groupSeparator <- paste(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
  cat(groupSeparator)
  
  k <- 1
  testSetsNames <- names(testSets)
  for (dfsToCombine in testSets) {
    cat("Running the test set", testSetsNames[k], "\n")
    
    aStatResultTable <- data.frame()
    wilcoxonResultTable <- data.frame()
    
    i <- 1
    approaches <- names(dfsToCombine)
    for (approachResult1 in dfsToCombine) {
      j <- 1
      for (approachResult2 in dfsToCombine) {
        if (i != j) {
          cat("Approach 1:", approaches[i], "- Approach 2:", approaches[j], "\n")
          
          aStat <- a.statistic(
            approachResult1$precision,
            approachResult2$precision,
            approaches[i],
            approaches[j]
          )
          
          aStatResultTable <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              A = aStat[['a']],
              interpretation = aStat[['interpretation']],
              superior = aStat[['superior']],
              confidence.0.95 = paste('[', paste(aStat[['confidence.0.95']], collapse = ", "), ']'),
              confidence.0.99 = paste('[', paste(aStat[['confidence.0.99']], collapse = ", "), ']'),
              different.at.0.05 = aStat[['different.at.0.05']],
              different.at.0.01 = aStat[['different.at.0.01']]
            ),
            aStatResultTable
          )
          
          wilcResult <- wilcox.test(approachResult1$precision, approachResult2$precision, paired=FALSE)
          
          wilcoxonResultTable <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              W = as.double(wilcResult[['statistic']]),
              p.value = as.double(wilcResult[['p.value']]),
              alternative = wilcResult[['alternative']],
              method = wilcResult[['method']]
            ),
            wilcoxonResultTable
          )
          
          #print.a.statistic(aStat)
          cat(testSeparator)
        }
        
        j <- j + 1
      }
      
      write.csv(aStatResultTable, file = paste(outputPrefix, '_varghaDelaney_', testSetsNames[k], ".csv", sep = ""))
      write.csv(wilcoxonResultTable, file = paste(outputPrefix, '_wilcoxon_', testSetsNames[k], ".csv", sep = ""))
      i <- i + 1
    }
    
    k <- k + 1
    cat(groupSeparator)
  }
  options(warn=0)
}

#sink("classDiagrams_combinationTestSetsOutput.txt")
doCombinationTests(combinationTestSets, "classDiagrams")
#sink()

