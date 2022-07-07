library(debrowser)
library(testthat)

load(system.file("extdata", "demo", "demodata.Rda",
    package = "debrowser"))
metadata <- metadatatable 
columns <- c("exper_rep1", "exper_rep2", "exper_rep3",
    "control_rep1", "control_rep2", "control_rep3")
conds <- factor( c("Control", "Control", "Control",
    "Treat", "Treat", "Treat") )
data <- data.frame(demodata[, columns])

#Run DESeq2 with the following parameters
params <- c("DESeq2", "NoCovariate", "parametric", F, "Wald", "None") 
non_expressed_cutoff <- 10
data <- subset(data, rowSums(data) > 10)
test_that("Able to run DESeq2", {
    deseqrun <- runDE(data, metadata, columns, conds, params)
    expect_true(exists("deseqrun"))
})

##################################################
deseqrun <- runDE(data, metadata, columns, conds, params)

de_res <- data.frame(deseqrun)
norm_data <- getNormalizedMatrix(data[, columns])
rdata <- cbind(rownames(de_res), norm_data[rownames(de_res), columns],
                log10(rowMeans(norm_data[rownames(de_res),
                paste(c("exper_rep1", "exper_rep2", "exper_rep3"))])
                + 0.1), log10( rowMeans( norm_data[ rownames( de_res ),
                paste(c("control_rep1", "control_rep2", "control_rep3"))])
                + 0.1), de_res[rownames(de_res),
                c("padj", "log2FoldChange")], 2 ^ de_res[rownames(de_res),
                "log2FoldChange"], -1 *
                log10(de_res[rownames(de_res), "padj"]))
colnames(rdata) <- c("ID", columns, "Cond1", "Cond2", "padj",
                "log2FoldChange", "foldChange", "log10padj")
rdata <- as.data.frame(rdata)
rdata$padj[is.na(rdata$padj)] <- 1

padj_cutoff <- 0.01
foldChange_cutoff <- 2

rdata$Legend <- character(nrow(rdata))
rdata$Legend[rdata$log2FoldChange > log2(foldChange_cutoff) &
        rdata$padj < padj_cutoff] <- "Up"
rdata$Legend[rdata$log2FoldChange < log2(1 / foldChange_cutoff) &
        rdata$padj < padj_cutoff] <- "Down"
rdata$Legend[abs(rdata$log2FoldChange) <= 
        log2(foldChange_cutoff)] <- "NS"
rdata$Legend[is.null(rdata$log10padj)] <- "NA"
rdata$Size <- character(nrow(rdata))
rdata[, "Size"] <- "40"

dat <- rdata
dat$M <- rdata$Cond1 - rdata$Cond2
dat$A <- (rdata$Cond1 + rdata$Cond2) / 2

updown <- rdata[rdata$Legend=="Up" | rdata$Legend=="Down",columns]
##################################################


