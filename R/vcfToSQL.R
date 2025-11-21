library(shiny)
library(shinyFiles)

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vcfToSQL <- function(...){

  # Define UI for the application
  ui <- fluidPage(
    titlePanel("Convert a VCF file to SQLite database"),
    verticalLayout(
      shinyFilesButton("vcf_file", "Select VCF input file", "Please select a VCF file", multiple = FALSE),
      textOutput("vcf_path"),
      shinySaveButton("sqlite_file", "Select SQLite output file", "Please select output database file"),
      textOutput("sqlite_path"),
      actionButton("run_convert", "Convert")
    )
  )

  # Define server logic
  server <- function(input, output, session) {

    source_python("inst/convert_vcf_to_sql.py")

    volumes <- shinyFiles::getVolumes()()

    shinyFiles::shinyFileChoose(input, "vcf_file",
                                roots = volumes,
                                session = session,
                                filetypes = c("vcf", "gz"))

    shinyFiles::shinyFileSave(input, "sqlite_file",
                              roots = volumes,
                              session = session)

    output$vcf_path <- renderText({
      req(input$vcf_file)
      vcf_info <- shinyFiles::parseFilePaths(volumes, input$vcf_file)
      if (nrow(vcf_info) == 0) return("")
      as.character(vcf_info$datapath)
    })

    output$sqlite_path <- renderText({
      req(input$sqlite_file)
      db_info <- shinyFiles::parseSavePath(volumes, input$sqlite_file)
      if (nrow(db_info) == 0) return("")
      as.character(db_info$datapath)
    })

    observeEvent(input$run_convert, {
      req(input$vcf_file, input$sqlite_file)

      vcf_info <- shinyFiles::parseFilePaths(volumes, input$vcf_file)
      db_info  <- shinyFiles::parseSavePath(volumes, input$sqlite_file)

      req(nrow(vcf_info) > 0, nrow(db_info) > 0)

      vcf_path <- as.character(vcf_info$datapath)
      db_path  <- as.character(db_info$datapath)

      convertVCFtoSQL(vcf_path, db_path)
      showNotification("Conversion completed!", type = "message")
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
