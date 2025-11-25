`light`
#' Object defining the app's light mode ui layout
#'
#' @name light
#' @usage toggle switch in the ui
#'
#' @format ## `R object`
#' An object describing the ui light mode's colour patterns
#' \describe{
#'  \item{light}{a bstheme object containing information on theme and colours of the application's light mode}
#'  }
#'
#'

`dark`
#' Object defining the app's dark mode ui layout
#'
#' @name dark
#' @usage toggle switch in the ui
#'
#' @format ## `R object`
#' An object describing the ui dark mode's colour patterns
#' \describe{
#'  \item {dark}{a bstheme object containing information on theme and colours of the application's dark mode}
#'  }
#'
#'

`refgenome_model`
#' Dataset containing exome references for gene matching
#'
#' @name refgenome_model
#' @usage used in the affected transcripts tab of the app
#'
#' @format ## `tibble`
#' A tibble with 10 columns
#' \describe{
#'  \item{chromosome}{the chromosome containing the feature}
#'  \item{start}{start position of the feature}
#'  \item{end}{end position of the feature}
#'  \item{width}{number of nucleotides of the feature}
#'  \item{strand}{strand on which the feature is positioned}
#'  \item{feature}{type of data regarding the gene, either transcript, exon or gene}
#'  \item{gene}{Ensembl's standard gene reference}
#'  \item{exon}{Ensembl's standard exon reference}
#'  \item{transcript}{Ensembl's standard transcript reference}
#'  \item{symbol}{gene name}
#'  }
#' @source created with TxDb.Hsapiens.UCSC.hg19.knownGene R package
#'

`express_data.sqlite`
#' Sql containing the expression data reference
#'
#'
#' @name express_data
#' @usage used in the tissues expression scores tab
#'
#' @format ## `dataset sqlite`
#' An SQL database containing one table with 3 columns:
#' \describe{
#'  \item{expression_data}{the table containing the expression data values}
#'  \item{gene}{genes' names column}
#'  \item{tissue}{tissue name}
#'  \item{nTPM}{number of gene trascripts per million in the specified tissue}
#'  }
#'
#'  @source generated using GTEx informations


system.file("extdata", "example.vcf.gz", package = "variantsinvestigator")
system.file("extdata", "express_data.sqlite", package = "RSQLite")
