*******************
Local Install Guide
*******************

Quick Local Install
===================

Running these simple command will launch the DEBrowser within your local
machine.

Before you start;
First, you will have to install R and/or RStudio.
(On Fedora/Red Hat/CentOS, these packages have to be installed;
openssl-devel, libxml2-devel, libcurl-devel, libpng-devel)

You can download the source code or the tar file for DEBrowser `here. <https://github.com/UMMS-Biocore/debrowser/releases>`_

**Installation instructions from source:**

1. Install the required dependencies by running the following commands in R or RStudio. 

		source("https://www.bioconductor.org/biocLite.R")

		biocLite("debrowser")

2. Start R and load the library

        library(debrowser)

3. Start DE browser

        startDEBrowser()

Once you run 'startDEBrowser()' shiny will launch a web browser with your local version of DEBrowser running and ready to use!

For more information about DEBrowser, please visit our Quick-start Guide or our DESeq/DEBrowser section within readthedocs.
