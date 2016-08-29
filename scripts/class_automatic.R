#install.packages("rjson", "plyr", "dplyr", "ggplot2", "gridExtra", "nortest")

require("rjson")
require("plyr")
require("dplyr")
require("ggplot2")
require("gridExtra")

source('nonparametric_effect_sizes.r')

fixJsonFormat <- function (data, maxlength) {
  data <- gsub('\\[', '', data)
  data <- gsub('\\]', '', data)
  data <- strsplit(data, split=',')[[1]]
  if (length(data) > maxlength) {
    data <- data[1:maxlength]
  }
  data <- toJSON(data)

  return (data)
}

cosSimilarity <- function(v1, v2) {
  if (length(v1) != length(v2)) {
    return ("v1 length != v2 length")
  }
  
  sum1 <- 0
  sum2 <- 0
  sum3 <- 0
  
  for (i in 1:length(v1)) {
    sum1 <- sum1 + v1[i] * v2[i]
    sum2 <- sum2 + (v1[i])^2
    sum3 <- sum3 + (v2[i])^2
  }
  
  if (sum2 == 0 || sum3 == 0) {
    return (0)
  } else {
    return (sum1 / (sqrt(sum2) * sqrt(sum3)))
  }
}


simplifyProfile <- function(dfLine) {
  result <- dfLine[1, "profile"]
  result <- paste("[", as.character(result), "]", sep = "")
  result <- as.data.frame(fromJSON(result))
  
  # prevents the loss of names if the result data frame has only one column:
  result <- cbind(data.frame(INVALID.REMOVE.ME = 1), result)
  
  result <- result[, colSums(result != 0) > 0]
  result <- names(result)
  
  # removes the invalid result:
  result <- result[result != 'INVALID.REMOVE.ME']
  
  result <- toJSON(result)
  if (result == 'null') {
    result = "[]"
  }
  
  dfLine <- subset(dfLine, select = -c(profile))
  dfLine[1, "profile"] <- result
  
  return (dfLine)
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


setDFColFirstLineValue <- function (df, colName, newValue) {
  df <- df[ , !(names(df) %in% c(colName))]
  df[1, colName] <- newValue
  
  return (df)
}


experiment2Data <- read.csv('class_nbyn.csv')
experiment2Data <- experiment2Data %>%
  select(c(
    Id,
    user_profile,
    random_approach_topn,
    bag_of_words_approach_topn,
    content_based_approach_topn,
    knowledge_based_approach_topn,
    
    random_approach_full_list,
    bag_of_words_approach_full_list,
    content_based_approach_full_list,
    knowledge_based_approach_full_list
  ))

classDiagrams <- read.csv('item_profile.csv')
classDiagrams <- classDiagrams %>% filter(type == 'class')

classDiagrams <- ddply(classDiagrams, .(id), function(dfLine) {
  dfLine <- simplifyProfile(dfLine)
})

experiment2Data <- ddply(experiment2Data, .(Id), function(dfLine) {
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
    "approach_full_list"
  )
  
  for (suffix in suffixes) {
    for (approach in approaches) {
      colName <- paste(approach, suffix, sep = "_")
      dfLine <- setDFColFirstLineValue(
        dfLine,
        colName,
        fixJsonFormat(dfLine[1, colName], 5)
      )
    }
  }
  
  dfLine[1, "profileLength"] <- length(fromJSON(dfLine[1, "user_profile"]))
  
  dfResult <- data.frame() 
  
  for (approach in approaches) {
    newLineDf <- dfLine[, c("Id", "user_profile", "profileLength")]
    newLineDf[1, "type"] <- approach
    newLineDf[1, "top3"] <- dfLine[1, c(paste(approach, suffixes[1], sep = "_"))]
    top5 <- fromJSON(dfLine[1, c(paste(approach, suffixes[2], sep = "_"))])
    top5 <- top5[1:5]
    newLineDf[1, "top5"] <- toJSON(top5)
    
    if (nrow(dfResult) == 0) {
      dfResult <- newLineDf
    } else {
      dfResult <- rbind(dfResult, newLineDf)
    }
  }
  
  
  
  return (dfResult)
})

experiment2Data <- ddply(experiment2Data, .(Id, type), function(dfLine) {
  
  userProfile <- fromJSON(dfLine[1, "user_profile"])
  diagramsTop3 <- fromJSON(dfLine[1, "top3"])
  diagramsTop5 <- fromJSON(dfLine[1, "top5"])
  
  numPositiveInterceptsTop3 <- 0
  numPositiveInterceptsTop5 <- 0
  
  mrrTop3 <- 0
  index <- 0
  for (diagramId in diagramsTop3) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      numPositiveInterceptsTop3 <- numPositiveInterceptsTop3 + 1
      mrrTop3 <- mrrTop3 + 1 / index
    }
  }
  mrrTop3 <- mrrTop3 / 3
  
  mrrTop5 <- 0
  index <- 0
  
  for (diagramId in diagramsTop5) {
    index <- index + 1
    diagramProfile <- fromJSON(classDiagrams[classDiagrams$id == diagramId, "profile"])
    if (length(intersect(userProfile, diagramProfile)) > 0) {
      numPositiveInterceptsTop5 <- numPositiveInterceptsTop5 + 1
      mrrTop5 <- mrrTop5 + 1 / index
    }
  }
  mrrTop5 <- mrrTop5 / 5
  
  precisionTop3 <- numPositiveInterceptsTop3 / length(diagramsTop3)
  precisionTop5 <- numPositiveInterceptsTop5 / length(diagramsTop5)
  
  dfLine[1, "precisionTop3"] <- precisionTop3
  dfLine[1, "precisionTop5"] <- precisionTop5
  dfLine[1, "mrrTop3"] <- mrrTop3
  dfLine[1, "mrrTop5"] <- mrrTop5
  
  return (dfLine)
})

top3Boxplots <- list()
mrrTop3Boxplots <- list()
ddply(experiment2Data, .(profileLength), function(subDf) {
  currentProfileLength <- subDf[1, "profileLength"]
  
  if (nrow(subDf) > 4) {
    #boxplot(
    #  subDf$precisionTop3 ~ subDf$type,
    #  main = paste(
    #    "Rec. by approach (Top 3, n = ",
    #    (nrow(subDf) / 4),
    #    ", profile length = ",
    #    currentProfileLength,
    #    ")",
    #    sep = ""
    #  )
    #)
    
    plotResult <- ggplot(subDf, aes(type, precisionTop3, fill = type)) +
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
      ylim(0, 1) +
      guides(fill = FALSE) +
      ggtitle(paste(
        "Precisions by approach for\nprofile length = ",
        currentProfileLength,
        "\n(built with ",
        (nrow(subDf) / 4),
        " elements)",
        sep = ""
      ))
    
    top3Boxplots[[length(top3Boxplots) + 1]] <<- plotResult
    
    
    plotResult <- ggplot(subDf, aes(type, mrrTop3, fill = type)) +
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
      ylim(0, 1) +
      guides(fill = FALSE) +
      ggtitle(paste(
        "Precisions by approach for\nprofile length = ",
        currentProfileLength,
        "\n(built with ",
        (nrow(subDf) / 4),
        " elements)",
        sep = ""
      ))
    
    
    mrrTop3Boxplots[[length(mrrTop3Boxplots) + 1]] <<- plotResult
  }
  
  return (subDf)
})

top5Boxplots <- list()
mrrTop5Boxplots <- list()
ddply(experiment2Data, .(profileLength), function(subDf) {
  currentProfileLength <- subDf[1, "profileLength"]
  
  if (nrow(subDf) > 4) {
    #boxplot(
    #  subDf$precisionTop5 ~ subDf$type,
    #  main = paste(
    #    "Rec. by approach (Top 5, n = ",
    #    (nrow(subDf) / 4),
    #    ", profile length = ",
    #    currentProfileLength,
    #    ")",
    #    sep = ""
    #  )
    #)
    
    plotResult <- ggplot(subDf, aes(type, precisionTop5, fill = type)) +
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
      ylim(0, 1) +
      guides(fill = FALSE) +
      ggtitle(paste(
        "Precisions by approach for\nprofile length = ",
        currentProfileLength,
        "\n(built with ",
        (nrow(subDf) / 4),
        " elements)",
        sep = ""
      ))
    
    top5Boxplots[[length(top5Boxplots) + 1]] <<- plotResult
    
    
    plotResult <- ggplot(subDf, aes(type, mrrTop5, fill = type)) +
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
      ylim(0, 1) +
      guides(fill = FALSE) +
      ggtitle(paste(
        "Precisions by approach for\nprofile length = ",
        currentProfileLength,
        "\n(built with ",
        (nrow(subDf) / 4),
        " elements)",
        sep = ""
      ))
    
    mrrTop5Boxplots[[length(mrrTop5Boxplots) + 1]] <<- plotResult
  }
  
  return (data.frame())
})

arrangedPots <- do.call(grid.arrange, c(top3Boxplots, ncol = 2))
ggsave("classDiagrams-2nd_exp-exp2PrecisionsByApproach-top3.pdf", plot = arrangedPots, width = 10, height = 12)

arrangedPots <- do.call(grid.arrange, c(top5Boxplots, ncol = 2))
ggsave("classDiagrams-2nd_exp-exp2PrecisionsByApproach-top5.pdf", plot = arrangedPots, width = 10, height = 12)


arrangedPots <- do.call(grid.arrange, c(mrrTop3Boxplots, ncol = 2))
ggsave("classDiagrams-2nd_exp-exp2MrrByApproach-top3.pdf", plot = arrangedPots, width = 10, height = 12)

arrangedPots <- do.call(grid.arrange, c(mrrTop5Boxplots, ncol = 2))
ggsave("classDiagrams-2nd_exp-exp2MrrByApproach-top5.pdf", plot = arrangedPots, width = 10, height = 12)

par(mfrow = c(1, 1))

boxplot(
  experiment2Data$precisionTop3 ~ experiment2Data$type,
  main = paste(
    "Rec. by approach (All data, Top 5, n = ",
    (nrow(experiment2Data) / 4),
    ")",
    sep = ""
  )
)

experiment2Data$type <- as.factor(experiment2Data$type)
kruskal.test(precisionTop3 ~ type, data = experiment2Data)
kruskal.test(precisionTop5 ~ type, data = experiment2Data)

experiment2Data.pl1 <- experiment2Data %>% filter(profileLength == 1)
experiment2Data.pl2 <- experiment2Data %>% filter(profileLength == 2)
experiment2Data.pl3 <- experiment2Data %>% filter(profileLength == 3)
experiment2Data.pl4 <- experiment2Data %>% filter(profileLength == 4)
experiment2Data.pl5 <- experiment2Data %>% filter(profileLength == 5)
experiment2Data.pl6 <- experiment2Data %>% filter(profileLength == 6)
experiment2Data.pl7 <- experiment2Data %>% filter(profileLength == 7)

kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl1)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl2)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl3)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl4)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl5)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl6)
kruskal.test(precisionTop3 ~ type, data = experiment2Data.pl7)

kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl1)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl2)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl3)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl4)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl5)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl6)
kruskal.test(precisionTop5 ~ type, data = experiment2Data.pl7)

exp2CombinationTestSets <- list(
  length1 = list(
    random = experiment2Data.pl1 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl1 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl1 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl1 %>% filter(type == 'knowledge_based')
  ),
  length2 = list(
    random = experiment2Data.pl2 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl2 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl2 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl2 %>% filter(type == 'knowledge_based')
  ),
  length3 = list(
    random = experiment2Data.pl3 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl3 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl3 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl3 %>% filter(type == 'knowledge_based')
  ),
  length4 = list(
    random = experiment2Data.pl4 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl4 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl4 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl4 %>% filter(type == 'knowledge_based')
  ),
  length5 = list(
    random = experiment2Data.pl5 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl5 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl5 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl5 %>% filter(type == 'knowledge_based')
  ),
  length6 = list(
    random = experiment2Data.pl6 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl6 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl6 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl6 %>% filter(type == 'knowledge_based')
  ),
  length7 = list(
    random = experiment2Data.pl7 %>% filter(type == 'random'),
    bagOfWords = experiment2Data.pl7 %>% filter(type == 'bag_of_words'),
    contentBased = experiment2Data.pl7 %>% filter(type == 'content_based'),
    knowledgeBased = experiment2Data.pl7 %>% filter(type == 'knowledge_based')
  )
)

# Vargha Delaney and Wilcoxon
do2ExpCombinationTests <- function (testSets, outputPrefix) {
  options(warn=1)
  testSeparator <- paste(paste(rep("-", 80), collapse = ""), "\n\n", sep = "")
  groupSeparator <- paste(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
  cat(groupSeparator)
  
  k <- 1
  testSetsNames <- names(testSets)
  for (dfsToCombine in testSets) {
    cat("Running the test set", testSetsNames[k], "\n")
    
    aStatResultTableTop3 <- data.frame()
    aStatResultTableTop5 <- data.frame()
    wilcoxonResultTableTop3 <- data.frame()
    wilcoxonResultTableTop5 <- data.frame()
    
    i <- 1
    approaches <- names(dfsToCombine)
    for (approachResult1 in dfsToCombine) {
      j <- 1
      for (approachResult2 in dfsToCombine) {
        if (i != j) {
          cat("Approach 1:", approaches[i], "- Approach 2:", approaches[j], "\n")
          
          aStatTop3 <- a.statistic(
            approachResult1$precisionTop3,
            approachResult2$precisionTop3,
            approaches[i],
            approaches[j]
          )
          
          aStatTop5 <- a.statistic(
            approachResult1$precisionTop5,
            approachResult2$precisionTop5,
            approaches[i],
            approaches[j]
          )
          
          aStatResultTableTop3 <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              A = aStatTop3[['a']],
              interpretation = aStatTop3[['interpretation']],
              superior = aStatTop3[['superior']],
              confidence.0.95 = paste('[', paste(aStatTop3[['confidence.0.95']], collapse = ", "), ']'),
              confidence.0.99 = paste('[', paste(aStatTop3[['confidence.0.99']], collapse = ", "), ']'),
              different.at.0.05 = aStatTop3[['different.at.0.05']],
              different.at.0.01 = aStatTop3[['different.at.0.01']]
            ),
            aStatResultTableTop3
          )
          
          aStatResultTableTop5 <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              A = aStatTop5[['a']],
              interpretation = aStatTop5[['interpretation']],
              superior = aStatTop5[['superior']],
              confidence.0.95 = paste('[', paste(aStatTop5[['confidence.0.95']], collapse = ", "), ']'),
              confidence.0.99 = paste('[', paste(aStatTop5[['confidence.0.99']], collapse = ", "), ']'),
              different.at.0.05 = aStatTop5[['different.at.0.05']],
              different.at.0.01 = aStatTop5[['different.at.0.01']]
            ),
            aStatResultTableTop5
          )
          
          wilcResultTop3 <- wilcox.test(approachResult1$precisionTop3, approachResult2$precisionTop3, paired=FALSE)
          wilcResultTop5 <- wilcox.test(approachResult1$precisionTop5, approachResult2$precisionTop5, paired=FALSE)
          
          wilcoxonResultTableTop3 <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              W = as.double(wilcResultTop3[['statistic']]),
              p.value = as.double(wilcResultTop3[['p.value']]),
              alternative = wilcResultTop3[['alternative']],
              method = wilcResultTop3[['method']]
            ),
            wilcoxonResultTableTop3
          )
          
          wilcoxonResultTableTop5 <- rbind(
            data.frame(
              approach1 = approaches[i],
              approach2 = approaches[j],
              W = as.double(wilcResultTop5[['statistic']]),
              p.value = as.double(wilcResultTop5[['p.value']]),
              alternative = wilcResultTop5[['alternative']],
              method = wilcResultTop5[['method']]
            ),
            wilcoxonResultTableTop5
          )
          
          cat(testSeparator)
        }
        
        j <- j + 1
      }
      
      write.csv(aStatResultTableTop3, file = paste(outputPrefix, '_varghaDelaney_top3_', testSetsNames[k], ".csv", sep = ""))
      write.csv(wilcoxonResultTableTop3, file = paste(outputPrefix, '_wilcoxon_top3_', testSetsNames[k], ".csv", sep = ""))
      
      write.csv(aStatResultTableTop5, file = paste(outputPrefix, '_varghaDelaney_top5_', testSetsNames[k], ".csv", sep = ""))
      write.csv(wilcoxonResultTableTop5, file = paste(outputPrefix, '_wilcoxon_top5_', testSetsNames[k], ".csv", sep = ""))
      i <- i + 1
    }
    
    k <- k + 1
    cat(groupSeparator)
  }
  options(warn=0)
}

#sink("classDiagrams_exp2CombinationTestSetsOutput.txt")
do2ExpCombinationTests(exp2CombinationTestSets, "classDiagrams")
#sink()


experiment2Data.pl1 <- experiment2Data %>% filter(profileLength == 1)
experiment2Data.pl2 <- experiment2Data %>% filter(profileLength == 2)
experiment2Data.pl3 <- experiment2Data %>% filter(profileLength == 3)
experiment2Data.pl4 <- experiment2Data %>% filter(profileLength == 4)
experiment2Data.pl5 <- experiment2Data %>% filter(profileLength == 5)
experiment2Data.pl6 <- experiment2Data %>% filter(profileLength == 6)
experiment2Data.pl7 


# Normality testing
exp2NormalityTests <- function () {
  shapiroResult.pl1.t3 <- shapiro.test(as.numeric(experiment2Data.pl1$precisionTop3))
  shapiroResult.pl2.t3 <- shapiro.test(as.numeric(experiment2Data.pl2$precisionTop3))
  shapiroResult.pl3.t3 <- shapiro.test(as.numeric(experiment2Data.pl3$precisionTop3))
  shapiroResult.pl4.t3 <- shapiro.test(as.numeric(experiment2Data.pl4$precisionTop3))
  shapiroResult.pl5.t3 <- shapiro.test(as.numeric(experiment2Data.pl5$precisionTop3))
  shapiroResult.pl6.t3 <- shapiro.test(as.numeric(experiment2Data.pl6$precisionTop3))
  shapiroResult.pl7.t3 <- shapiro.test(as.numeric(experiment2Data.pl7$precisionTop3))
  
  shapiroResult.pl1.t5 <- shapiro.test(as.numeric(experiment2Data.pl1$precisionTop5))
  shapiroResult.pl2.t5 <- shapiro.test(as.numeric(experiment2Data.pl2$precisionTop5))
  shapiroResult.pl3.t5 <- shapiro.test(as.numeric(experiment2Data.pl3$precisionTop5))
  shapiroResult.pl4.t5 <- shapiro.test(as.numeric(experiment2Data.pl4$precisionTop5))
  shapiroResult.pl5.t5 <- shapiro.test(as.numeric(experiment2Data.pl5$precisionTop5))
  shapiroResult.pl6.t5 <- shapiro.test(as.numeric(experiment2Data.pl6$precisionTop5))
  shapiroResult.pl7.t5 <- shapiro.test(as.numeric(experiment2Data.pl7$precisionTop5))
  
  
  adResult.pl1.t3 <- ad.test(as.numeric(experiment2Data.pl1$precisionTop3))
  adResult.pl2.t3 <- ad.test(as.numeric(experiment2Data.pl2$precisionTop3))
  adResult.pl3.t3 <- ad.test(as.numeric(experiment2Data.pl3$precisionTop3))
  adResult.pl4.t3 <- ad.test(as.numeric(experiment2Data.pl4$precisionTop3))
  adResult.pl5.t3 <- ad.test(as.numeric(experiment2Data.pl5$precisionTop3))
  adResult.pl6.t3 <- ad.test(as.numeric(experiment2Data.pl6$precisionTop3))
  adResult.pl7.t3 <- ad.test(as.numeric(experiment2Data.pl7$precisionTop3))
  
  adResult.pl1.t5 <- ad.test(as.numeric(experiment2Data.pl1$precisionTop5))
  adResult.pl2.t5 <- ad.test(as.numeric(experiment2Data.pl2$precisionTop5))
  adResult.pl3.t5 <- ad.test(as.numeric(experiment2Data.pl3$precisionTop5))
  adResult.pl4.t5 <- ad.test(as.numeric(experiment2Data.pl4$precisionTop5))
  adResult.pl5.t5 <- ad.test(as.numeric(experiment2Data.pl5$precisionTop5))
  adResult.pl6.t5 <- ad.test(as.numeric(experiment2Data.pl6$precisionTop5))
  adResult.pl7.t5 <- ad.test(as.numeric(experiment2Data.pl7$precisionTop5))
  
  resultDF <- data.frame(
    pl1.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl1.t3$statistic), ", p-value = ", shapiroResult.pl1.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl1.t3$statistic), ", p-value = ", adResult.pl1.t3$p.value, sep = "")
    ),
    pl2.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl2.t3$statistic), ", p-value = ", shapiroResult.pl2.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl2.t3$statistic), ", p-value = ", adResult.pl2.t3$p.value, sep = "")
    ),
    pl3.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl3.t3$statistic), ", p-value = ", shapiroResult.pl3.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl3.t3$statistic), ", p-value = ", adResult.pl3.t3$p.value, sep = "")
    ),
    pl4.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl4.t3$statistic), ", p-value = ", shapiroResult.pl4.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl4.t3$statistic), ", p-value = ", adResult.pl4.t3$p.value, sep = "")
    ),
    pl5.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl5.t3$statistic), ", p-value = ", shapiroResult.pl5.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl5.t3$statistic), ", p-value = ", adResult.pl5.t3$p.value, sep = "")
    ),
    pl6.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl6.t3$statistic), ", p-value = ", shapiroResult.pl6.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl6.t3$statistic), ", p-value = ", adResult.pl6.t3$p.value, sep = "")
    ),
    pl7.t3 = c(
      paste("W = ", as.numeric(shapiroResult.pl7.t3$statistic), ", p-value = ", shapiroResult.pl7.t3$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl7.t3$statistic), ", p-value = ", adResult.pl7.t3$p.value, sep = "")
    ),
    
    
    pl1.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl1.t5$statistic), ", p-value = ", shapiroResult.pl1.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl1.t5$statistic), ", p-value = ", adResult.pl1.t5$p.value, sep = "")
    ),
    pl2.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl2.t5$statistic), ", p-value = ", shapiroResult.pl2.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl2.t5$statistic), ", p-value = ", adResult.pl2.t5$p.value, sep = "")
    ),
    pl3.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl3.t5$statistic), ", p-value = ", shapiroResult.pl3.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl3.t5$statistic), ", p-value = ", adResult.pl3.t5$p.value, sep = "")
    ),
    pl4.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl4.t5$statistic), ", p-value = ", shapiroResult.pl4.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl4.t5$statistic), ", p-value = ", adResult.pl4.t5$p.value, sep = "")
    ),
    pl5.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl5.t5$statistic), ", p-value = ", shapiroResult.pl5.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl5.t5$statistic), ", p-value = ", adResult.pl5.t5$p.value, sep = "")
    ),
    pl6.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl6.t5$statistic), ", p-value = ", shapiroResult.pl6.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl6.t5$statistic), ", p-value = ", adResult.pl6.t5$p.value, sep = "")
    ),
    pl7.t5 = c(
      paste("W = ", as.numeric(shapiroResult.pl7.t5$statistic), ", p-value = ", shapiroResult.pl7.t5$p.value, sep = ""),
      paste("A = ", as.numeric(adResult.pl7.t5$statistic), ", p-value = ", adResult.pl7.t5$p.value, sep = "")
    )
  )
  
  #names(resultDF) <- c("Undergraduate students", "Graduated subjects", "Postgraduate students", "Postgraduated subjects")
  row.names(resultDF) <- c("Shapiro-Wilk", "Anderson-Darling")
  
  return (resultDF)
}

