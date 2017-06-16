# DEBrowser:
Interactive Differential Expression Analysis Tool

# Introduction

Differential expression (DE) analysis has become an increasingly popular tool
in determining and viewing up and/or down experssed genes between two sets of
samples.  The goal of differential gene expression analysis is to find genes
or transcripts whose difference in expression, when accounting for the
variance within condition, is higher than expected by chance.
[DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html) is
an R package available via Bioconductor and is designed to normalize count
data from high-throughput sequencing assays such as RNA-Seq and test for
differential expression (Love et al. 2014).  With multiple parameters such as
padjust values, log fold changes, plot styles, and so on, altering plots
created with your DE data can be a hassle as well as time consuming. The
Differential Expression Browser uses DESeq2 (Love et al., 2014),
[EdgeR](https://bioconductor.org/packages/release/bioc/html/edgeR.html)
(Robinson et al., 2010), and
[Limma](https://bioconductor.org/packages/release/bioc/html/limma.html)
(Ritchie et al., 2015) coupled with
shiny (Chang, W. et al., 2016)  to produce real-time changes within your
plot queries and allows for interactive browsing of your DE results.
In addition to DE analysis, DEBrowser also offers a variety of other plots
and analysis tools to help visualize your data even further.

## DEBrowser

DEBrowser utilizes Shiny, a R based application development tool that creates
a wonderful interactive user interface (UI) combined with all of the
computing prowess of R. After the user has selected the data to analyze and
has used the shiny UI to run DESeq2, the results are then input to DEBrowser.
DEBrowser manipulates your results in a way that allows for interactive
plotting by which changing padj or fold change limits also changes the
displayed graph(s). For more details about these plots and tables, please
visit our quick start guide for some helpful tutorials.

For comparisons against other popular data visualization tools, see the
comparison table below (Figure 40).

# Quick start

Before you start;

First, you will have to install R and/or RStudio.
(On Fedora/Red Hat/CentOS, these packages have to be installed;
openssl-devel, libxml2-devel, libcurl-devel, libpng-devel)
Running these simple commands will launch the DEBrowser within your local
machine:

```
# Installation instructions:
# 1. Install DEBrowser and its dependencies by running the lines below
# in R or RStudio.

source(“https://www.bioconductor.org/biocLite.R”)

biocLite("debrowser")

# 2. Load the library

library(debrowser)

# 3. Start DEBrowser

startDEBrowser()
```

# Browsing your Data

## Data via TSV file

Once you have the DEBrowser running, a page will load asking to choose a TSV
file or to load the demo data.  In order to run DESeq2, we are going to need
gene quantifications for genes contained in a tab-separated values (TSV)
format. Gene quantifications table can be obtained running standard software
like HTSeq (Anders,S. et al, 2014) or RSEM (Li and Dewey, 2011). The file
values must contain the gene, transcript(s), and the sample raw count values
you wish to enter into DEBrowser.

It's important to note that if your rows contain duplicate gene names,
DEBrowser will reject your TSV file.  Please try to keep unique gene names.  A
sample file looks like:

| gene     | transcript | exper_rep1 | exper_rep2 | control_rep1 | control_rep2 |
|----------|------------|------------|------------|--------------|--------------|
| DQ714826 | uc007tfl.1 | 0.00       | 0.00       | 0.00         | 0.00         |
| DQ551521 | uc008bml.1 | 0.00       | 0.00       | 0.00         | 0.00         |
| AK028549 | uc011wpi.1 | 2.00       | 1.29       | 0.00         | 0.00         |

You can also view/use the demo data by clicking the 'Load Demo!' text as an
example.  For the case study demo data, feel free to download our case study
demo files at https://bioinfo.umassmed.edu/pub/debrowser/advanced_demo.tsv or
a simplified version https://bioinfo.umassmed.edu/pub/debrowser/simple_demo.tsv.
Please also note that, DEBrowser skips second column and starts reading the quantification values from the 3rd column.

## Data via JSON objects

DEBrowser also accepts JSON objects via hyperlink by following a few conversion steps.  
First, using the API provided by Dolphin, we will convert a TSV file into a JSON
object using this web api:

```
https://dolphin.umassmed.edu/public/api/
```

The Two parameters it accepts (and examples) are:

	1. source=https://bioinfo.umassmed.edu/pub/debrowser/advanced_demo.tsv
	2. format=JSON

Leaving you with a hyperlink for:

```
https://dolphin.umassmed.edu/public/api/?source=https://bioinfo.umassmed.edu/pub/debrowser/
advanced_demo.tsv&format=JSON
```

Next you will need to encode the URL so you can pass it to the DEBrowser website.
You can find multiple URL encoders online, such as the one located at this
web address: https://www.url-encode-decode.com/.

Encoding our URL will turn it into this:

```
http%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttp%3A%2F%2Fbioinfo
.umassmed.edu%2Fpub%2Fdebrowser%2Fadvanced_demo.tsv%26format%3DJSON
```

Now this link can be be used in debrowser as:

```
https://debrowser.umassmed.edu:444/debrowser/R/
```

It accepts two parameters:

	1. jsonobject=http%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttp%3A%2F%2F
	   bioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fadvanced_demo.tsv%26format%3DJSON
	2. title=no

The finished product of the link will look like this:

```
https://debrowser.umassmed.edu:444/debrowser/R/?jsonobject=https://dolphin.umassmed.edu/public/
api/?source=https://bioinfo.umassmed.edu/pub/debrowser/advanced_demo.tsv&format=JSON&title=no
```

Entering this URL into your web browser will automatically load in your data as a JSON
object, allowing you to start browsing your data right away.

## Batch Effect Corrections

In addition to the sample TSV file you will provide; you can also correct for batch effects or any other normalizing conditions you might want to address
that might be within your results.  To handle for these corrections, simply create a TSV file such as the one located below:


| sample   | batch      | condition  |
|----------|------------|------------|
| s1_b1_cA | 1          | A          |
| s2_b1_cA | 1          | A          |
| s3_b2_cB | 2          | B          |
| s4_b2_cB | 2          | B          |
| s5_b1_cB | 1          | B          |

This meta data file is custom made TSV created by the user and is used in order to establish different batch effects for multiple conditions.
You can have as many conditions as you may require, as long as all of the samples are present.  Once the TSV file has been loaded in along with your
data TSV file, DEBrowser uses ComBat (part of the SVA bioconductor package) to adjust for possible batch effect or conditional biases.  For more information
about ComBat within the SVA package you can visit here: https://bioconductor.org/packages/release/bioc/vignettes/sva/inst/doc/sva.pdf.

To load in the specific file that contains the batch meta data, at the start of the DEBrowser there will be a
"Choose Meta Data File (Optional)" which you can then select the batch meta data file to use for this analysis.
Upon meta-data loading, you will then be able to select from a drop down box that will specify which condition
column you want to use for analysis.

After obtaining and loading in the gene quantifications file, and if specified the
meta data file containing your batch correction fields, you
then have the option to view QC information of your quantifications or you can
continue on to running DESeq2 (Figure 1).

![*Figure 1. (A) The initial data selection menu.  Intial TSV data is loaded in the 'Choose TSV File' while the optional meta data file is loaded in under 'Choose Mera Data File (Optional)'.  (B) Options list once data/meta data have been loaded in.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_1.png "Figure 1. Initial option selection")

## Metadata Upload

If you prefer to select conditions beforehand, and save them as a TSV file to upload, you have this option
as of February 2017. You can split up conditions into two groups in a TSV file, and have as many selections as
you want for different groupings.

To load in the specific file that contains the meta data, at the start of the DEBrowser there will be a
"Choose Meta Data File (Optional)" which you can then select the meta data file to use for this analysis.
In the metadata file, you will need to have a sample column as the first column and from then on exactly 2
groups in each column([cond1, cond2], [1, 2], etc) to be matched to the sample column. Sample TSV:

| sample        | select1      | selection2  |
|---------------|--------------|-------------|
| exper_rep1    | cond1	       | 1           |
| exper_rep2	  | cond1	       | 2           |
| exper_rep3	  | cond2        | 1           |
| control_rep1	| cond2	       | 2           |
| control_rep2	| cond2	       | 1           |
| control_rep3	| cond2	       | 2           |

The example above would result in 'select1' having the first set of conditions as {exper_rep1, exper_rep2}
from 'cond1' and second set of conditions as {exper_rep3, control_rep1, control_rep2, control_rep3} from
'cond2' as they correspond to those conditions in the 'sample' column.

In the same way, 'selection2' would have the first set as {exper_rep1, exper_rep3, control_rep2} from '1'
and second set as {exper_rep2, control_rep1, control_rep3} from '2'  as they correspond to those conditions
in the 'sample' column.

## Quality Control Information:

Upon selection of QC information, you will be shown an all-to-all plot of your
samples (Figure 2). This sample-by-sample comparison will help you visualize possible
discrepancies between replicate samples, in case you may want to omit them
for further analysis.  This graph includes sample-to-sample dotplot correlations
as well as sample histograms. To the left of this plot are various plot-shaping
options you can alter to more easily view the all-to-all plot.

Additionally, two more QC plots are available for you to use: heatmap and
PCA plots.  The heatmap (Figure 3) will display genes for each sample within your dataset
in the form of a heatmap based on your dataset selection and PCA (Figure 4)
will display Principal component analysis of your dataset.
Additionally, you can view the IRQ (Interquartile  Range) for both your raw data and your
data after normalization (Figure 5).  You can also view a density plot for your
sample data for your raw data and the data after normalization (Figure 6).
IQR and Density plots are another great visualization too to help you spot
outliers within your sample data incase you want to remove or look into
any possible discrepancies.

All of these plots will aid in viewing your preliminary data to see if
there are any potential errors between replicates or batch effects
(Reese et. al, 2013; Risso et al., 2014). You have the option of viewing an
interactive heatmap by selecting the 'Interactive' checkbox in the left side
panel when you have selected the Heatmap option.  This Interactive heatmap will
display genes as you hover over them for a more in-depth understanding.  
You can select these various plot options by selecting the type of plot you
wish to view on the left panel.

![*Figure 2. Display of the all-to-all plot in the initial QC plots page.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_2.png "Figure 2. Display of the all-to-all plot in the initial QC plots page.")

You can also view the genes within your quantification file in various
ways.  The 'Tables' tab will bring you to a table setup based on the dataset
you have selected on the left options panel. The 'All detected' option
lists all of the genes present within your file.  The 'Selected' option
lets you browse your gene selection based on your interactive heatmap
selection.  The Last option, 'Most Varied' (Figure 7), will display your
top N varied genes.  You can alter the value of N by selecting 'most-varied'
from the dropdown menu on the left.

![*Figure 3. Display of the most varied genes heatmap in the initial QC plots page.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_3.png "Figure 3. Display of the most varied genes heatmap in the initial QC plots page.")

![*Figure 4. Display of the PCA plot in the initial QC plots page.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_4.png "Figure 4. Display of the PCA plot in the initial QC plots page.")

![*Figure 5. Display of the IQR in the initial QC plots page.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_5.png "Figure 5. Display of the IQR in the initial QC plots page.")

![*Figure 6. Display of the density plot in the initial QC plots page.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_6.png "Figure 6. Display of the density plot in the initial QC plots page.")

![*Figure 7. Displayed table of most varied genes.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_7.png "Figure 7. Displayed table of most varied genes.")

## Starting DESeq:

Upon selecting to run DESeq, you are then able to select
which samples will be used within your differential expression analysis
By clicking the 'Add New Comparison' button, you can add as many different
sample comparisons as you want.  To alter the samples within each comparison,
you can click on a sample and press delete to remove it or you can click the empty
whitespace within the tab to bring a dropdown of samples to select from.
Sample names are created based on the column headers within your data file.
Once you've selected your comparisons, you are then ready to run DESeq2 to
calculate differential expression by clicking on the 'Submit!' button (Figure 8).

![*Figure 8. Menus after loading in a sample.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_8.png "Figure 8. Menus after loading in a sample.")

# Differential Expression Calculations

## DESeq2

For the details please check the user guide at this location:
<https://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.pdf>

DESeq2 performs multiple steps in order to analyze the data you’ve provided
for it. The first step is to indicate the condition that each column
(experiment) in the table represent. You can group multiple samples into
one condition column. DESeq2 will compute the probability that a gene is
differentially expressed (DE) for ALL genes in the table. It outputs both a
nominal and a multiple hypothesis corrected p-value (padj) using a negative
binomial distribution.

## Un-normalized counts
DESeq2 requires count data as input obtained from RNA-Seq or another
high-throughput sequencing experiment in the form of matrix values. Here we
convert un-integer values to integer to be able to run DESeq2. The matrix
values should be un-normalized, since DESeq2 model internally corrects for
library size. So, transformed or normalized values such as counts scaled by
library size should not be used as input. Please use edgeR or limma for
normalized counts.

## Used parameters for DESeq2

* **fitType:**
	Either “parametric”, “local”, or “mean” for the type of fitting of
	dispersions to the mean intensity. See estimate Dispersions for description.

* **betaPrior:**
	Whether or not to put a zero-mean normal prior on the non-intercept
	coefficients See nbinomWaldTest for description of the calculation of the
	beta prior. By default, the beta prior is used only for the Wald test, but
	can also be specified for the likelihood ratio test.

* **testType:**
	Either “Wald” or “LRT”, which will then use either Wald significance tests
	(defined by nbinomWaldTest), or the likelihood ratio test on the difference
	in deviance between a full and reduced model formula (defined by nbinomLRT)

* **rowsum.filter:**
	Regions/Genes/Isoforms with total count (across all samples) below this
	value will be filtered out

## EdgeR

For the details please check the user guide at this location:
<https://www.bioconductor.org/packages/release/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf>

## Used parameters for EdgeR

* **Normalization:**
	Calculate normalization factors to scale the raw library sizes. Values
	can be “TMM”,”RLE”,”upperquartile”, or ”none”.

* **Dispersion:**
	Either a numeric vector of dispersions or a character string indicating
	that dispersions should be taken from the data object.

* **testType:**
	exactTest or glmLRT. exactTest: Computes p-values for differential
	abundance for each gene between two samples, conditioning on the total
	count for each gene. The counts in each group are assumed to follow a
	binomial distribution.  glmLRT: Fits a negative binomial generalized
	log-linear model to the read counts for each gene and conducts
	genewise statistical tests.

* **rowsum.filter:**
	Regions/Genes/Isoforms with total count (across all samples) below this
	value will be filtered out

## Limma

For the details please check the user guide at this location:
<https://bioconductor.org/packages/release/bioc/vignettes/limma/inst/doc/usersguide.pdf>

Limma is a package to analyze  of microarray or RNA-Seq data. If data is
normalized with spike-in or any other scaling, transformation  or
normalization method, Limma can be ideal. In that case, prefer limma rather
than DESeq2 or EdgeR.

## Used parameters for Limma

* **Normalization:**
	Calculate normalization factors to scale the raw library sizes. Values
	can be “TMM”,”RLE”,”upperquartile”, or ”none”.

* **Fit Type:**
	Fitting method: “ls” for least squares or “robust” for robust regression

* **Norm. Bet. Arrays:**
	Normalization Between Arrays; Normalizes expression intensities so that
	the intensities or log-ratios have similar distributions across a set of
	arrays.

* **rowsum.filter:**
	Regions/Genes/Isoforms with total count (across all samples) below this
	value will be filtered out

# Saving the State

After the file upload is complete and a pair of conditions are selected, "Save Selection!"
button should appear on the sidebar on the left. If you click this button, you will be able
to name your save and access it later with the name you choose. There are certain limitations
on the naming, but you will be given an error message to make the necessary correction as it is
based on bookmarking functionality of Shiny.

Your new save will appear as a clickable link under "New Save:" and as you make more saves, those
will be available under "History:" after refreshing the page. Only the last 20 saves will appear
for better user interface, so it is advisable to delete the unused saves by clicking "X" icon.

## Google Login

If you start up the shiny server using startDEBrowser(), you will automatically be logged in as 'local'.
However, if you use the runApp() command to start the server, you'll be asked to log in using a
Google account. This is to ensure the past saves correspond to the right person. You can log in using
any Google account, and then give permission to the DEBrowser to log in for the first time.

Once you are done using DEBrowser, you can either choose to stay logged in for your next use or sign out
to stop access to your account. In order to sign out, click on the gear icon on the top right corner and
then click on "Sign Out". If you want to start over from the beginning while staying logged in, you can
click on "Refresh" to go back to the beginning. You will still be able to access your save history when
you sign out or refresh.

#Analyzing the Results

After clicking on the 'Submit!' button, DESeq2 will analyze your comparisons
and store the results into separate data tables.  Shiny will then allow you
to access this data, with multiple interactive features, and at the click of a
button.  It is important to note that the resulting data produced from DESeq
is normalized. Upon finishing the DESeq analysis, a tab-based menu will appear
with multiple options (Figure 9) at the top-center of the page.

![*Figure 9. List of the tabbed menus in DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_9.png "Figure 9. List of the tabbed menus in DEBrowser.")

The first tab, the 'Main Plots' section, is where you will be able to view
the interactive results plots.  On the left hand side of the screen will be
the options panel used to alter the padj and fold change
cutoff values, what specific data set to use such as up or down regulated
genes, what comparison dataset you would like to use to plot,
and what type of plot you would like to view your results in (Figure 10).  
Comparisons are ordered based on how they were entered by the user and
for information about samples within each comparison are displayed right under
the option tabs.  Plot choices include:

* Scatter plot (Figure 11)

* Volcano plot (Figure 12)

* MA plot (Figure 13)

Once you have selected your values, you can hit the 'Submit!' button to create
your interactive plots!  Depending on whether you are using the local install
or the web-based application, rendering times my vary.

![*Figure 10. The Left parameter options panel*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_10.png "Figure 10. The Left parameter options panel")

The top left plot is the main plot of whichever plot you have
selected to use to analyze your results.  Up-regulated genes are displayed
in green while down-regulated genes are displayed in red (Figure 14).
Hovering over a gene on this plot will display the bottom two bar graphs: the
genes normalized variation and colored by condition in the left graph,
and the normalized variation between conditions within the right graph.
Hovering over a gene will also display information about that gene in
regards to both conditions you have selected.
By clicking and dragging your mouse to create a selection over the main graph,
you will create the top right plot, or the zoomed in version of your
selection.  If you are going to change any of the parameters on the left,
please make sure to re-click the 'Submit!' button to update the graphs.
You can also change which type of dataset to use within the main plots by
selecting from the drop down dataset box such as looking at the N most varied genes
which are displayed in yellow(Figure 15).  Additionally, you can further
filter these datasets by typing in the genes of interest, or regex for
specific genes, to search for those specific genes within the dataset (Figrue 14).
Specific gene searches are displayed in blue.
All of these filtration options can be located on the left side panel which will
also change based on the plot or dataset you wish to view/manipulate.
It's also worth noting that the plots are resizable as well as downloable.

![*Figure 11. Main scatter plot.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_11.png 'Figure 11. Main scatter plot.')

![*Figure 12. Main volcano plot*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_12.png 'Figure 12. Main volcano plot')

![*Figure 13. Main MA plot.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_13.png 'Figure 13.  Main MA plot')

![*Figure 14. The main plots page within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_14.png "Figure 14. Main plots")

![*Figure 15. isplay of the most varied genes as a scatter plot.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_15.png "Figure 15. varied plots")

![*Figure 16. Display of the geneset list as a scatter plot.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_16.png "Figure 16. geneset plots")

Selecting the 'QC Plots' tab will take you to the quality control plots
section.  These QC plots are very similar to the QC plots shown before
running DESeq, however the dataset being used here depends on the one
you select on the left menu.  In addition to the all-to-all plot shown
within the previous QC analysis, users can also view a heatmap and PCA
plot of their analyzed data by selecting the proper plot on the left
menu.

![*Figure 17. Display of the heatmap within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_17.png "Figure 17. Heatmap")

The heatmap is a great way to analyze replicate results of genes all in
one simple plot (Figure 19).  Users have the option to change the clustering method used
as well as the distance method used to display their heatmap.  In addition,
you can also change the size of the heatmap produced and adjust the p-adjust
and fold change cut off for this plot as well.  Once all of the parameters
have been set, click the 'Submit!' button at the bottom of the left menu to
generate your heatmap.

## Used clustering and linkage methods in heatmap

* **complete:**
	Complete-linkage clustering is one of the linkage method used in hierarchical clustering.
	In each step of clustering, closest cluster pairs are always merged up to a specified distance
	threshold. Distance between clusters for complete link clustering is the maximum of
	the distances between the members of the clusters.

* **ward D2:**
	Ward method aims to find compact and spherical clusters. The distance between two clusters
	is calculated by the sum of squared deviations from points to centroids. "ward.D2" method uses
	criterion (Murtagh and Legendre 2014) to minimize ward clustering method. The only difference
	ward.D2 and ward is the dissimilarities from ward method squared before cluster updating. This
	method tends to be sensitive to the outliers.

* **single:**
	Distance between clusters for single linkage is the minimum of	the distances between
	the members of the clusters.

* **average:**
	Distance between clusters for average linkage is the average of the distances between
	the members of the clusters.

* **mcquitty:**
	mcquitty linkage is when two clusters are joined, the distance of the new cluster
	to any other cluster is calculated by the average of the distances of the soon to be
	joined clusters to that other cluster.

* **median:**
	This is a different averaging method that uses the median instead of the mean.
	It is used to reduce the effect of outliers.

* **centroid:**
	The distance between cluster pairs is defined as the Euclidean distance
	between their centroids or means.

## Used distance methods in heatmap

* **cor:**
	1 - cor(x) are used to define the dissimilarity between samples. It is less
	sensitive to the outliers and scaling.

* **euclidean:**
	It is the most common use of distance. It is sensitive to the outliers and scaling.
	It is defined as the square root of the sum of the square differences between gene counts.

* **maximum:**
	The maximum distance between two samples is the sum of the maximum expression value of the
	corresponding genes.

* **manhattan:**
	The Manhattan distance between two samples is the sum of the differences of their
	corresponding genes.

* **canberra:**
	Canberra distance is similar to the Manhattan distance and it is a special form of
	the Minkowski distance. The difference is that the absolute difference between the
	gene counts of the two genes is divided by the sum of the absolute counts
	prior to summing.

* **minkowsky:**
	It is generalized form of euclidean distance.

You can also select to view an interactive version of the heatmap by clicking
on the 'Interactive' checkbox on the left panel under the height and width
options.  Selecting this feature changes the heatmap into an interactive
version with two colors, allowing you to select specific genes to be compared
within the GO term plots (Figure 18).  In order to use the interactive heatmap selection
within your GO term query, you must use either the up+down dataset or the
most varied dataset for the heatmap display.
This will allow you to compare interesting clusters
found within the heatmap within our GO Term analysis section which will
be discussed later within the materials.

![*Figure 18. View of the interactive Heatmap.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_18.png "Figure 18. Interactive Heatmap")

![*Figure 19. Display of the PCA plot within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_19.png "Figure 19. PCA")

Principal Component Analysis (PCA) is another excellent method of checking
replicates (Figure 19).  PCA calculates the variance between all of the samples genes
within your current comparison set and creates a two-dimensional
graph to represent the proportion of variance explained in different
components.  Within the PCA plot section you can select the p-adjust
value, fold change cut off value, which comparison set to use, which dataset
to use, the height and width of the corresponding plots, as well as which
principal components to analyze by changing the appropriate values on the
left menu.  If loaded with a batch meta data file, you can select from a
specific dropdown within the PCA QC plots in order to group specific points on
color or shape based on specified groups within your meta data file.

The next tab, 'GO Term', takes you to the ontology comparison portion of
DEBrowser.  From here you can select the standard dataset options such as
p-adjust value, fold change cut off value, which comparison set to use, and
which dataset to use on the left menu.  In addition to these parameters, you
also can choose from the 4 different ontology plot options: 'enrichGO' (Figure 20-21),
'Disease' (Figure 22-23), 'enrichKEGG' (Figure 24), and 'compareCluster'.  
Selecting one of these plot options queries their specific databases with your
current DESeq results. By selecting the 'selection' dataset on the left panel after
selecting specific genes from the interactive heatmap, you will be able to compare
your specific gene selection within the various GO Term databases.

In order to use your selected genes from the interactive heatmap, you must
first make your selection within the interactive heatmap.  Next you will want
to switch to the GO Terms tab and use the 'selected' dataset (Figure 25).  Once all your
other parameters have been selected, hit submit and you will use your selected
genes from the interactive heatmap in your GO Term analysis.

![*Figure 20. Display of the GO Plot section within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_20.png "Figure 20. GO")

![*Figure 22. Display of the GO dotplot section within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_21.png "Figure 21. dotplot GO")

![*Display of the DO plot section within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_22.png "Figure 22. DO")

![*Figure 23. Display of the DO dotplot section within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_23.png "Figure 23. DO dotplot")

![*Figure 24. Display of the KEGG dotplot section within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_24.png "Figure 24. KEGG")

![*Figure 25. Display of Heatmap selected enriched GO Term search.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_25.png "Figure 25. heatmap enrich")

The last tab, 'Tables', contains various result information in table formats.
The 'All Detected' option contains the list of all the genes within the
TSV/CSV provided with the corresponding DESeq analyses.
Up-regulated values are shown in green while down-regulated values
are displayed in red.  To view any particular dataset's custom options,
the dataset type must be selected.

The 'Up+Down' option contains a list of all the up and down-regulated genes based on the
options selected on the left panel (Figure 26).
The 'Up' option contains a list of all the up-regulated genes based on the
options selected on the left panel.  The 'Down' option contains a list of all
the down-regulated genes based on the options selected (Figure 27).
The 'Selected' option contains the list of genes selected from the main plots
section.  By clicking and dragging your mouse on the main plot within the
'Main Plots' tab, you will then be able to see that selection in list form
within the 'Selected' option.
The 'Gene Set' option allows you to filter out gene data based on
genes selected via a text box.  To create a gene set, simply type the names
of the genes you wish to view in the text box on the left panel in a comma-
seperated format.  You can also use regular expressions in order to search
for specific gene sets (Figure 28-29).
The 'Most Varied' option, much like the original QC 'Most Varied' tab,
allows you to view the list of most varied genes based on user input
parameters on the left panel.
The 'Comparisons' option allows you to view the
differences between your different condition comparisons (Figure 30).
Comparisons between datasets are shown if at least one of the conditional
comparisons has passed the padj value or fold change cut off.

It is also important to note that comparisons with only
one sample cannot create statistically significant p-adjust values.  The
more replicates you have within a condition, the greater the statistical
significance of your comparisons.

![*Figure 26. Display of the up+down-regulated genes table.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_26.png "Figure 26. Up+Down Regulated")

![*Figure 27. Display of the down-regulated genes table.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_27.png "Figure 27. Down Regulated")

![*Figure 28. Display of the geneset input box.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_28.png "Figure 28. geneset")

![*Figure 29. Display of the gene set search of the term '^al'.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_29.png "Figure 29. geneset table")

![*Figure 30. Condition comparisons table within DEBrowser.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_30.png "Figure 30. Comparisons")

Lastly, the tables have a bunch of features that allow you to view your DESeq
results more conveniently.  By clicking on a column header, you can sort the
data within the table based either alphabetical or numeric sorting.
You can also enter a term, or regex query, within the search box on the left
panel to filter for a specific gene within the table.

With that, you've now successfully navigated the DEBrowser and are ready to
start inserting your own data files and browsing your own experiments.  Enjoy
the DEBrowser!

#       Case Study

Taking a look at the case study (Vernia S. et al 2014), Multiple heatmaps were
created to display findings within the research.  The heatmaps generated
for the study were customized to a high level of specificity.  However,
using a sample dataset generated from this study, it is possible to
recreate similar heatmaps (Figure 31-32) displayed within the studies findings.

![*Figure 31. All detected genes heatmap using case study data.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_31.png "Figure 31. All detected genes heatmap using case study data")

![*Figure 32. Most varied genes heatmap using case study data.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_32.png "Figure 32. Most varied genes heatmap using case study data")

The main difference between the heatmaps created within DEBrowser
and the heatmaps created within the research paper is that the clustering
method used within the paper was a k-means method with k equaling 6.

**The JNK2 Knock-out versus JNK1 Knock-out:**

High fat diet JNK1 knock-out and JNK2 knock-out samples compared against high fat
diet wild type samples showed a stronger effect from JNK2 KO.  
From the figures below, JNK2 KO has a
stronger effect than JNK1 KO samples. There are 177 genes (Figure 33) that have
padj < 0.01 and |log2 foldchange| > 1 in the JNK2 KO comparison while there are
only 17 genes (Figure 34) detected in the JNK1 KO comparison with the same cutoffs.

![*Figure 33. High fat diet JNK2 vs. High fat diet wild type.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_33.png "Figure 33. High fat diet JNK2 vs. High fat diet wild type.")

![*Figure 34. High fat diet JNK1 vs. High fat diet wild type.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_34.png "Figure 34. High fat diet JNK1 vs. High fat diet wild type.")

**JNK1 and JNK2 serve partially redundant functions:**

High fat diet JNK1 and High fat diet JNK2 double KO has 1018 significantly different genes.
When we compare HFD JNK1 KO only (177 Genes) and HFD JNK2 KO only (17 genes)  with
HFD wild type side-by-side, most of the up and down regulated genes are not
overlapping.  Up regulated genes (Figure 35.) and down regulated (Figure 36.) in
JNK1 KO was easy to analyze for these gene comparisons.
There is only 1 gene overlapping out of the 17 that were
significantly different in JNK1 KO comparisons with padj < 0.01 and
|log2foldchange| > 1 cutoffs.  It shows that both individual KO might have
individual functions in addition to their redundant functions.
When we looked at the genes in JNK1 KO in the KEGG database,  they are enriched
in "Fatty acid elongation”. JNK2 KO are enriched in "PPAR signaling pathway”
and "Biosynthesis of unsaturated fatty acids”. DEBrowser’s powerful comparison
function makes different condition comparisons and running GO Term analysis
on selected genes much easier.

![*Figure 35. Upregulated genes in hfd JNK1 KO (C1) vs. hfd wt (C2) DE comparison shows 4 upregulated genes (padj <0.01 and |log2foldchange| > 1).*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_35.png "Figure 35. Upregulated genes in hfd JNK1 KO (C1) vs. hfd wt (C2) DE comparison shows 4 upregulated genes (padj <0.01 and |log2foldchange| > 1).")

![*Figure 36. Downregulated genes in hfd JNK1 KO (C1) vs. hfd wt (C2) DE comparison shows 13 downregulated genes (padj <0.01 and |log2foldchange| > 1). Only one of them is in JNK2 KO (C3) vs. hfd wt (C4) DE comparison.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_36.png "Figure 36. Downregulated genes in hfd JNK1 KO (C1) vs. hfd wt (C2) DE comparison shows 13 downregulated genes (padj <0.01 and |log2foldchange| > 1). Only one of them is in JNK2 KO (C3) vs. hfd wt (C4) DE comparison.")

Comparing the HFD wild type and the normal chow wild type also shows significant differences between regulated genes (Figure 37).
Expanding on the analysis further, the upregulated genes analyzed are then compared to KEGG and Disease ontologies
to show a variety of metabolism related correlations (Figures 38-39).

Using the 'advanced demo' dataset we mentioned earlier, you too can
recreate these tables using the same data.  Browsing, changing parameters,
and creating unique plots to view and analyze data can be a creative way
to recreate the same analytical results produced.  DEBrowser can be used in
multiple ways to check the reproducibility of research results in a highly
interactive format!

**DEBrowser vs other Differential Expression analysis software:**

The comparison table (Figure 40) displays multiple comparisons between debrowser and other various methods of viewing Differential Expression Analysis results.

Some of the comparisons can be viewed either within the tool itself or within some of the figures provided. A multiple tool comparisons
can be observed within figure 34, an interactive visualization of gene highlighting can be observed for figures 12-14, and an interactive
visualization of biological variation or condition comparisons can also be observed for figure 34.

For more information on MeV (Howe et al., 2011) please visit this link: [MeV](http://www.tm4.org/mev.html)

For more information about Chipster (Kallio et al., 2011), please visit this link: [Chipster](http://chipster.csc.fi/manual/)

For more information about Galaxy (Giardine et al., 2005), please visit this link: [Galaxy](https://usegalaxy.org/)

For more information about CummeRBund (Trapnell et al., 2012), please visit this link: [CummeRbund](http://compbio.mit.edu/cummeRbund/manual_2_0.html)

#       Batch Effect Correction Example

Batch effects can have negative effects on your overall data as a whole.  If samples were not handled at the same time, or just do not
have the sample level of technical variance as the rest of your samples, this can lead to specific batch effects that might skew test results.  In
Figures 41-44, we show both before batch and after batch correction IQR plots and how it can having skewing effects on the results of QC analysis.

#       Future Plan:

Future plans will include the following:

	* Venn Diagrams to compare overlapping differentially expressed genes in different
	  condition comparison results.
	* Increase in the number of used clustering methods.
	* GO term analysis gene lists will be added for found GO categories.

#       References

1. Anders,S. et al. (2014) HTSeq - A Python framework to work with high-throughput sequencing data.

2. Chang,W. et al. (2016) shiny: Web Application Framework for R.

3. Chang,W. and Wickham,H. (2015) ggvis: Interactive Grammar of Graphics.

4. Giardine,B. et al. (2005) Galaxy: a platform for interactive large-scale genome analysis. Genome Res., 15, 1451–1455.

5. Howe,E.A. et al. (2011) RNA-Seq analysis in MeV. Bioinformatics, 27, 3209–3210.

6. Kallio,M.A. et al. (2011) Chipster: user-friendly analysis software for microarray and other high-throughput data. BMC Genomics, 12, 507.

7. Li,B. and Dewey,C.N. (2011) RSEM: accurate transcript quantification from RNA-Seq data with or without a reference genome. BMC Bioinformatics, 12, 323.

8. Love,M.I. et al. (2014) Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2. Genome Biol., 15, 550.

9. Reese,S.E. et al. (2013) A new statistic for identifying batch effects in high-throughput genomic data that uses guided principal component analysis. Bioinformatics, 29, 2877–2883.

10. Reich,M. et al. (2006) GenePattern 2.0. Nat. Genet., 38, 500–501.

11. Risso,D. et al. (2014) Normalization of RNA-seq data using factor analysis of control genes or samples. Nat. Biotechnol., 32, 896–902.

12. Ritchie,M.E. et al. (2015) limma powers differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Res., 43, e47–e47.

13. Trapnell,C. et al. (2012) Differential gene and transcript expression analysis of RNA-seq experiments with TopHat and Cufflinks. Nat. Protoc., 7, 562–578.

14. Vernia,S. et al. (2014) The PPAR$\alpha$-FGF21 hormone axis contributes to metabolic regulation by the hepatic JNK signaling pathway. Cell Metab., 20, 512–525.

15. Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification 31 (forthcoming).

16. Johnson et al. (2007) Adjusting batch effects in microarray expression data using empirical Bayes methods.  Biostatistics, 8, 118-127.

![*Figure 37. Up and Down regulated genes volcano plot of HFD WT vs Chow WT.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_37.png "Figure 37. Up and Down regulated genes volcano plot of HFD WT vs Chow WT.")

![*Figure 38. HFD upregulated gene list used for DO enrichment*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_38.png "Figure 38. HFD upregulated gene list used for DO enrichment")

![*Figure 39. HFD upregulated gene list used for KEGG enrichment*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_39.png "Figure 39. HFD upregulated gene list used for KEGG enrichment")

![*Figure 40. Comparison table of DEBrowser, MeV, Chipster, Galaxy, and CummeRBund*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_40.png "Figure 40. Comparison table of DEBrowser, MeV, Chipster, Galaxy, and CummeRBund*")

![*Figure 41. IQR of samples before batch correction*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_41.png "Figure 41. IQR of samples before batch correction")

![*Figure 42. IQR of samples after batch correction*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_42.png "Figure 42. IQR of samples after batch correction")

![*Figure 43. All-2-All scatter plots showing QC without batch correction.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_43.png "Figure 43. All-2-All scatter plots showing QC without batch correction.")

![*Figure 44. All-2-All scatter plots showing QC with batch correction.*](http://bioinfo.umassmed.edu/pub/debrowser/imgs/figure_44.png "Figure 44. All-2-All scatter plots showing QC with batch correction.")
