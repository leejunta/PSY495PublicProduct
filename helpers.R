library(shinyjs)

# All the code in this file needs to be copied to your Shiny app, and you need
# to call `withBusyIndicatorUI()` and `withBusyIndicatorServer()` in your app.
# You can also include the `appCSS` in your UI, as the example app shows.

# =============================================

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
    id <- button[['attribs']][['id']]
    div(
        `data-for-btn` = id,
        button,
        span(
            class = "btn-loading-container",
            hidden(
                img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
                icon("check", class = "btn-done-indicator")
            )
        ),
        hidden(
            div(class = "btn-err",
                div(icon("exclamation-circle"),
                    tags$b("Error: "),
                    span(class = "btn-err-msg")
                )
            )
        )
    )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })
    
    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
        value <- expr
        shinyjs::show(selector = doneEl)
        shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                           time = 0.5))
        value
    }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
    errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

#appCSS <- "
#.btn-loading-container {
#margin-left: 10px;
#font-size: 1.2em;
#}
#.btn-done-indicator {
#color: green;
#}
#.btn-err {
#margin-top: 10px;
#color: red;
#}
#"

################################################################3
#helpers for scoring

#iat scoring
iatScoringGuidelines <- function(iat_responses) {
    iat_scores <- list()
    iat_scores$total <- sum(iat_responses)
    iat_scores$salience <- sum(iat_responses[c(10,12,13,15,19)])
    iat_scores$excessiveuse <- sum(iat_responses[c(1,2,14,18,20)])
    iat_scores$neglect <- sum(iat_responses[c(6,8,9)])
    iat_scores$anticipation <- sum(iat_responses[c(7,11)])
    iat_scores$lackofcontrol <- sum(iat_responses[c(5,16,17)])
    iat_scores$neglectsocial <- sum(iat_responses[c(3,4)])
    return(iat_scores)
}

#psq scoring
psqScoringGuidelines <- function(psq_responses) {
    psq_scores <- list()
    psq_scores$authoritative <- sum(psq_responses[c(1:13)])/13
    psq_scores$authoritarian <- sum(psq_responses[c(14:26)])/13
    psq_scores$permissive <- sum(psq_responses[c(27:30)])/4
    return(psq_scores)
}

#erq scoring
erqScoringGuidelines <- function(ewq_responses) {
    erq_scores <- list()
    erq_scores$cognitivereappraisal <- sum(ewq_reponses[c(1,3,5,7,8,10)])/4.2
    erq_scores$expressivesuppression <- sum(ewq_responses[c(2,4,6,9)])/2.8
    return(erq_scores)
}

prqcScoringGuidelines <- function(prqc_responses) {
    prqc_scores <- list()
    prqc_scores$bully <- sum(prqc_responses[c(4,9,11,14,16,17)])/2.4
    prqc_scores$victim <- sum(prqc_responses[c(3,8,12,18,19)])/2
    prqc_scores$prosocial <- sum(prqc_responses[c(5,10,15,20)])/1.6
    return(prqc_scores)
}
