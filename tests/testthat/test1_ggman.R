context("Main ggman function")

test_that("class of argument inputs", {
    expect_error(ggman(toy.gwas, snp = toy.gwas),"The snp input is not a character")
    expect_error(ggman(toy.gwas,bp = toy.gwas), "The bp input is not a character")
    expect_error(ggman(toy.gwas, chrom = toy.gwas), "The chrom input is not a character")
    expect_error(ggman(toy.gwas, pvalue = toy.gwas), "The pvalue input is not a character")
    expect_error(ggman(toy.gwas, sigLine = "8"), "The sigLine input is not numeric")
    expect_error(ggman(toy.gwas, lineColour = "hello"), "'hello' is not a valid color")
    expect_error(ggman(toy.gwas, pointSize = "1"), "The pointSize input is not numeric")
    expect_error(ggman(toy.gwas, ymin = "0"), "The ymin input is not numeric")
    expect_error(ggman(toy.gwas, ymax = "10"), "The ymax input is not numeric")
    expect_error(ggman(toy.gwas, logTransform = "TRUE"), "The logTransform input is not logical")
    expect_error(ggman(toy.gwas, invert = "TRUE"),"The invert input is not logical")
    expect_error(ggman(toy.gwas, invert = TRUE, invert.method = "something"),"The invert.method argument can take values: 'or' or 'beta'")
})

test_that("column names are specified correctly",{
    expect_error(ggman(toy.gwas,snp="something"))
    expect_error(ggman(toy.gwas,bp = "something"))
    expect_error(ggman(toy.gwas,chrom="something"))
    expect_error(ggman(toy.gwas,pvalue="something"))
}

)
