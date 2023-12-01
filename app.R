# Shiny app for RYB Chapter 1 Exercise 1-1 from my book 'Reimagining Your Business, A Personalized Guidebook to Beat Your Competition'

# Deployed to shinyapps.io Nov 30, 2023


# PUSH to GitHub any time the app is updated
# REPUBLISH via RStudio any time the app is updated


library(shiny)
library(rmarkdown)

ui <- fluidPage(
    titlePanel("Chapter 1 – Exercise 1-1"),
    
    # Save to PDF Button (Top)
    fluidRow(
        column(12, downloadButton("downloadPDF_top", "Save to PDF", class = "btn-danger"))
    ),
    
    # Specific Questions and Input Fields with Reset Buttons
    fluidRow(
        column(12, h3("Ideas take form when you commit them to writing. Write your answers to the following questions:"))
    ),
    lapply(1:2, function(i) {
        questionText <- c(
            "1. List your personal motivations.  They don't have to be politically correct, socially conscious, or profound...just honest.",
            "2. Draft a brief statement of why YOU are the right (or only) person to bring this project to the world."
        )
        fluidRow(
            column(12,
                   h4(questionText[i]),
                   textAreaInput(paste("input", i, sep = ""), label = NULL, placeholder = "Type your answer here..."),
                   actionButton(paste("reset", i, sep = ""), "Reset")
            )
        )
    }),
    
    # Reset All Button
    fluidRow(
        column(12, actionButton("reset_all", "Reset All"))
    ),
    
    # Save to PDF Button (Bottom)
    fluidRow(
        column(12, downloadButton("downloadPDF", "Save to PDF", class = "btn-danger"))
    ),
    
    # Warning Message
    tags$script(HTML("
        $(window).on('beforeunload', function(){
            return 'Are you sure you want to leave before saving your input? All your data will be lost.';
        });
    "))
)

server <- function(input, output, session) {
    # Reset Individual Fields
    lapply(1:2, function(i) {
        observeEvent(input[[paste("reset", i, sep = "")]], {
            updateTextAreaInput(session, paste("input", i, sep = ""), value = "")
        })
    })
    
    # Reset All Fields
    observeEvent(input$reset_all, {
        lapply(1:2, function(i) {
            updateTextAreaInput(session, paste("input", i, sep = ""), value = "")
        })
    })
    
    # Function for PDF Generation
    generatePDF <- function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.create(tempReport)
        dateTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        answers <- sapply(1:2, function(i) input[[paste("input", i, sep = "")]], USE.NAMES = FALSE)
        writeLines(c(
            "---",
            "title: 'Chapter 1 – Exercise 1-1 Answers'",
            "author: 'Generated on", dateTime, "'",
            "output: pdf_document",
            "geometry: margin=1in",
            "---",
            "",
            paste("1. **", answers[1], "**"),
            "",
            paste("2. **", answers[2], "**")
        ), tempReport)
        rmarkdown::render(tempReport, output_file = file)
    }
    
    # Save to PDF (Top and Bottom Buttons)
    output$downloadPDF_top <- downloadHandler(
        filename = function() { paste("exercise-1-1-", Sys.Date(), ".pdf", sep = "") },
        content = generatePDF,
        contentType = "application/pdf"
    )
    output$downloadPDF <- downloadHandler(
        filename = function() { paste("exercise-1-1-", Sys.Date(), ".pdf", sep = "") },
        content = generatePDF,
        contentType = "application/pdf"
    )
}

# Run the application
shinyApp(ui = ui, server = server)
