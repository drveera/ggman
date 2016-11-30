


# R package to create manhattan plots using ggplot 

## Installation

```
library(devtools)
install_github("veera-dr/ggman")
```

##Usage 

### Example data 

```
head(gwas)

  CHR     SNP BP      P     OR
1   1   rs1_0  1 0.2954 1.1050
2   1 rs1_0_M  2 0.9296 0.9922
3   1   rs1_1  3 0.8162 1.0160
4   1 rs1_1_M  4 0.6669 0.9719
5   1   rs1_2  5 0.4310 0.9426
6   1 rs1_2_M  6 0.3743 1.0580
>
```

### Create a basic Manhattan plot 

```
ggman(gwas, snp = "SNP", bp = "BP", chrom = "CHR", pvalue = "P")
```

![enter image description here](https://github.com/veera-dr/ggman/blob/master/data/manhattan.basic.png)

### Add labels to Manhattan plot 

```
p1 <- ggman(gwas, snp = "SNP", bp = "BP", chrom = "CHR", pvalue = "P")

gwas.sig <- gwas[-log10(gwas$P) > 8,]

ggmanLabel(p1,labelDfm = gwas.sig, snp = "SNP", label = "SNP")
```

![enter image description here](https://github.com/veera-dr/ggman/blob/master/data/manhattan.labelled.png)