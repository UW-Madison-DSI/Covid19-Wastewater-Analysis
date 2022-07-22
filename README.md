<p align="center">
  <div align="center">
    <img src="./images/covid-droplet.svg" alt="Logo" style="width:33%">
  </div>
</p>

# DSI Covid Wastewater Analysis (Development)
 
This is a collaborative project between the University of Wiscosin's Data Science Institute (DSI) and the Wisconsin Department of Health Services (DHS) on data analsysis for statewide covid wastewater surveillance. 

DSI:
- <https://datascience.wisc.edu>

DHS:
- <https://www.dhs.wisconsin.gov>
- <https://www.dhs.wisconsin.gov/covid-19/wastewater.htm>
- <http://www.slh.wisc.edu/environmental/covid-19-wastewater/>

## Sample results

- [Downsampling (Two Days)](results/DownsamplingTwoDays.pdf)

- [Downsampling Interceptor (Two Days)](results/DownsamplingInterceptor.pdf)


## Requirements

1. The R Language

The wastewater analysis code is written in R so you will need an R interpreter / environment to run it.  We recommend the following two systems: 

- R Studio

R Studio is a complete integrated development environment for R:
https://www.rstudio.com 

- R Console

This is a simple console app which provides a command line interface to the R interpreter:
https://cran.r-project.org/bin 

2. The AFIDSI Wastewater Package

The AFIDSI Wastewater package is an R package that provides the foundation for analysis of wastewater data. 

## Installation
There are two ways to install the AFIDSI wastewater package:

### 1. Using Devtools

Download the AFIDSI wastewater package using the R command below
```
devtools::install_github(
    "AFIDSI/DSIWastewater",
    auth_token = "REPLACEWITHYOURAUTHTOKEN"
)
```
 
To get a AUTHTOKEN you need to go to https://github.com/settings/tokens and create one with at least [repo level access](./docs/github/repo-level-access.md).

### 2. Using a TAR (.tgz) file

1. Download the tar file

You can download the tar from the following location:
https://github.com/AFIDSI/DSIWastewater/blob/main/DSIWastewater_0.2.01.tar.gz

Note: When you download it, make sure that it remains zipped.  It should be a .tar.gz file rather than a .tar file.   If the file has been unzipped, then you can re-zip the file using the following command:

```
gzip DSIWastewater_0.2.01.tar
```

2. Install the tar file

To install the tar file, you can use the following command:

```
install.packages("DSIWastewater_0.2.01.tar.gz", repos = NULL, type="source") 
```

Alternatively, if you are using RStudio, you can install the tar file [using the user interface](./docs/r-studio/installing-packages.md).

## Viewing
 
- View the package vignettes with:

```
vignette(package = "DSIWastewater")
```

- Look at all package functionality with:

```
help(package = "DSIWastewater")
```
 
- Release notes are stored here:
 
https://docs.google.com/document/d/1-Rbd0YTyPZ2slbW9ksvF36n_nhhEBNi0vJZAmhwNHsg/edit

## Code style
 
- Camel case for functions and file names
 
- Snake case for variables
 
- _data for data objects

- _Plot if the function creates a plot

<!-- LICENSE -->
## License

Distributed under the Sustainable Use License. See `LICENSE.md` for more information.

<!-- CONTACT -->
## Contact

Steve Goldstein - (mailto:sgoldstein@wisc.edu) - email

Project Link: [https://github.com/AFIDSI/DSIWastewater](https://github.com/AFIDSI/DSIWastewater)
