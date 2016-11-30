


# R package to create manhattan plots using ggplot 

## Installation

```
library(devtools)
install_github("veera-dr/ggman")
```

##Usage 

### example data 

```
head(gwas)
```

### Create a basic Manhattan plot 

```
ggman(gwas, snp = "SNP", bp = "BP", chr = "CHR", pvalue = "P")
```