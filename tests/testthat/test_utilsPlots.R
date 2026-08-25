test_that("plotDiagnosticSystematicBias returns a ggplot object", {
    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), paste0("S", 1:6))
    )

    p <- plotDiagnosticSystematicBias(rawData)

    expect_s3_class(p, "ggplot")
})


test_that("plotDiagnosticPCV returns a ggplot object", {
    groupData <- data.frame(
        Samples = paste0("S", 1:6),
        Groups = rep(c("A", "B"), each = 3)
    )

    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), groupData$Samples)
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticPCV(
        normalizedDataList = normalizedDataList,
        groupData = groupData
    )

    expect_s3_class(p, "ggplot")
})


test_that("plotDiagnosticWithinGroupCor returns a ggplot object", {
    groupData <- data.frame(
        Samples = paste0("S", 1:6),
        Groups = rep(c("A", "B"), each = 3)
    )

    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), groupData$Samples)
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticWithinGroupCor(
        normalizedDataList = normalizedDataList,
        groupData = groupData
    )

    expect_s3_class(p, "ggplot")
})


test_that("plotDiagnosticMA returns an arranged ggplot object", {
    groupData <- data.frame(
        Samples = paste0("S", 1:6),
        Groups = rep(c("A", "B"), each = 3)
    )

    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), groupData$Samples)
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticMA(
        normalizedDataList = normalizedDataList,
        groupData = groupData,
        refGroup = "B",
        altGroup = "A"
    )

    expect_s3_class(p, "gg")
})


test_that("plotDiagnosticMeanSD returns an arranged ggplot object", {
    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), paste0("S", 1:6))
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticMeanSD(normalizedDataList)

    expect_s3_class(p, "gg")
})


test_that("plotDiagnosticRLE returns an arranged ggplot object", {
    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), paste0("S", 1:6))
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticRLE(normalizedDataList)

    expect_s3_class(p, "gg")
})


test_that("plotDiagnosticTotalIntensity returns an arranged ggplot object", {
    rawData <- matrix(
        runif(120 * 6, 100, 1000),
        nrow = 120,
        ncol = 6,
        dimnames = list(paste0("P", 1:120), paste0("S", 1:6))
    )

    normalizedDataList <- list(
        Norm1 = log2(rawData),
        Norm2 = log2(rawData + 1)
    )

    p <- plotDiagnosticTotalIntensity(normalizedDataList)

    expect_s3_class(p, "gg")
})
