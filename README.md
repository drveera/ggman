


# R package to create manhattan plots using ggplot 

##Note: 
The package is currently under development.  Please raise issues for any bugs you identify.

## Installation

```
library(devtools)
install_github("veera-dr/ggman")
```

##Quick Reference 

Please refer to package vignette and the wiki page for detailed information on the usage. 

### Create a basic Manhattan plot 

```
ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue")
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p1.png)


### Use relative positioning 

```
ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue", relative.positions = TRUE)
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p2.png)

### Add labels 

```
#subset only the SNPs with -log10(pvalue) > 8
toy.gwas.sig <- toy.gwas[-log10(toy.gwas$pvalue)>8,]

## save the main layer in a variable
p1 <- ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue", relative.positions = TRUE)

##add label
ggmanLabel(p1, labelDfm = toy.gwas.sig, snp = "snp", label = "snp")
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p3.png)
### Add text

```
ggmanLabel(p1, labelDfm = toy.gwas.sig, snp = "snp", label = "snp", type = "text")
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p4.png)



### Highlight a single group of points

```
ggmanHighlight(p1, highlight = toy.highlights)
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p5.png)

### Multiple Highlights

```
ggmanHighlightGroup(p1, highlightDfm = toy.highlights.group, snp = "snp", group = "group", 
                    size = 0.5, legend.title = "Significant groups")
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p6.png)

### Add clumps

```
gwas.clump <- read.table("plink.clumped", header = TRUE)

toy.clumps <- ggmanClumps(toy.clumped, index.snp.column = "SNP", clumps.column = "SP2") 

ggman(toy.gwas,clumps = toy.clumps, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue", relative.positions = TRUE, pointSize = 0.5)
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p7.png)

### highlight and label the clumps

```
toy.clumps <- ggmanClumps(toy.clumped, index.snp.column = "SNP", clumps.column = "SP2", group.column = "group", label.column = "label") 

ggman(toy.gwas,clumps = toy.clumps, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue", relative.positions = TRUE, pointSize = 0.5)
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p8.png)

### Zoom in to a specific chromosome
```
ggmanZoom(p1, chromosome = 1)
```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p9.png)

### Zoom in to a specific region of a chromosome
```
ggmanZoom(p1, chromosome = 1, start.position = 215388741, end.position = 238580695)

```

![enter image description here](https://github.com/veera-dr/storage_ggman/blob/master/plots/p10.png)

### Highlight points in the zoomed region

```
ggmanZoom(p1, chromosome = 1, start.position = 215388741, end.position = 238580695, highlight.group = "gene", legend.title = "Genes")
```

### Inverted Manhattan Plot

```
ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue", invert = TRUE, invert.method = 'or', invert.var = "or")
```

### Plot Odds Ratio
```
ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "or", logTransform = FALSE, ymax = 3)
```

### Plot Beta Values

```
ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "beta", logTransform = FALSE, ymin = -2, ymax = 2)
```