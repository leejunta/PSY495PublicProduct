library(shiny)
source("helpers.R")
source("global.R")

server <- function(input, output, session) {
    
    ################################
    # IAT
    iat_responses <- reactive({
        c(input$iat_1,
          input$iat_2,
          input$iat_3,
          input$iat_4,
          input$iat_5,
          input$iat_6,
          input$iat_7,
          input$iat_8,
          input$iat_9,
          input$iat_10,
          input$iat_11,
          input$iat_12,
          input$iat_13,
          input$iat_14,
          input$iat_15,
          input$iat_16,
          input$iat_17,
          input$iat_18,
          input$iat_19,
          input$iat_20)
    })
    
    observeEvent(input$iat_submit, {
        # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
        withBusyIndicatorServer("iat_submit", {
            Sys.sleep(1)
            if (length(iat_responses()) != 20) {
                stop("select a response for all questions")
            }
        })
    })
    
    output$iat_scores <- renderText({
        if (length(iat_responses()) != 20) {
            NA
        } else {
            iat_scores <- iatScoringGuidelines(as.integer(iat_responses()))
            format(iat_scores$total,digits = 2)
        }
    })
    
    ################################
    # PCIAT
    pciat_responses <- reactive({
        c(input$pciat_1,
          input$pciat_2,
          input$pciat_3,
          input$pciat_4,
          input$pciat_5,
          input$pciat_6,
          input$pciat_7,
          input$pciat_8,
          input$pciat_9,
          input$pciat_10,
          input$pciat_11,
          input$pciat_12,
          input$pciat_13,
          input$pciat_14,
          input$pciat_15,
          input$pciat_16,
          input$pciat_17,
          input$pciat_18,
          input$pciat_19,
          input$pciat_20)
    })
    
    observeEvent(input$pciat_submit, {
        # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
        withBusyIndicatorServer("pciat_submit", {
            Sys.sleep(1)
            if (length(pciat_responses()) != 20) {
                stop("select a response for all questions")
            }
        })
    })
    
    output$pciat_scores <- renderText({
        if (length(pciat_responses()) != 20) {
            NA
        } else {
            pciat_scores <- iatScoringGuidelines(as.integer(pciat_responses()))
            format(pciat_scores$total,digits = 2)
        }
    })
    
    ################################
    # PSQ
    psq_responses <- reactive({
        c(input$psq_1,
          input$psq_2,
          input$psq_3,
          input$psq_4,
          input$psq_5,
          input$psq_6,
          input$psq_7,
          input$psq_8,
          input$psq_9,
          input$psq_10,
          input$psq_11,
          input$psq_12,
          input$psq_13,
          input$psq_14,
          input$psq_15,
          input$psq_16,
          input$psq_17,
          input$psq_18,
          input$psq_19,
          input$psq_20,
          input$psq_21,
          input$psq_22,
          input$psq_23,
          input$psq_24,
          input$psq_25,
          input$psq_26,
          input$psq_27,
          input$psq_28,
          input$psq_29,
          input$psq_30)
    })
    
    observeEvent(input$psq_submit, {
        # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
        withBusyIndicatorServer("psq_submit", {
            Sys.sleep(1)
            if (length(psq_responses()) != 30) {
                stop("select a response for all questions")
            }
        })
    })
    
    output$psq_score1 <- renderText({
        if (length(psq_responses()) != 30) {
            NA
        } else {
            psq_score1 <- psqScoringGuidelines(as.integer(psq_responses()))
            format(psq_score1$authoritative,digits = 2)
        }
    })
    output$psq_score2 <- renderText({
        if (length(psq_responses()) != 30) {
            NA
        } else {
            psq_score2 <- psqScoringGuidelines(as.integer(psq_responses()))
            format(psq_score2$authoritarian,digits = 2)
        }
    })
    output$psq_score3 <- renderText({
        if (length(psq_responses()) != 30) {
            NA
        } else {
            psq_score3 <- psqScoringGuidelines(as.integer(psq_responses()))
            format(psq_score3$permissive,digits = 2)
        }
    })

    ################################
    # ERQ
    erq_responses <- reactive({
        c(input$erq_1,
          input$erq_2,
          input$erq_3,
          input$erq_4,
          input$erq_5,
          input$erq_6,
          input$erq_7,
          input$erq_8,
          input$erq_9,
          input$erq_10)
    })
    
    observeEvent(input$erq_submit, {
        # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
        withBusyIndicatorServer("erq_submit", {
            Sys.sleep(1)
            if (length(erq_responses()) != 10) {
                stop("select a response for all questions")
            }
        })
    })
    
    output$erq_score1 <- renderText({
        if (length(erq_responses()) != 10) {
            NA
        } else {
            erq_score1 <- erqScoringGuidelines(as.integer(erq_responses()))
            format(erq_score1$cognitivereappraisal,digits = 2)
        }
    })
    output$erq_score2 <- renderText({
        if (length(erq_responses()) != 10) {
            NA
        } else {
            erq_score2 <- erqScoringGuidelines(as.integer(erq_responses()))
            format(erq_score2$expressivesuppression,digits = 2)
        }
    })
    
    ################################
    # PRQC
    prqc_responses <- reactive({
        c(input$prqc_1,
          input$prqc_2,
          input$prqc_3,
          input$prqc_4,
          input$prqc_5,
          input$prqc_6,
          input$prqc_7,
          input$prqc_8,
          input$prqc_9,
          input$prqc_10,
          input$prqc_11,
          input$prqc_12,
          input$prqc_13,
          input$prqc_14,
          input$prqc_15,
          input$prqc_16,
          input$prqc_17,
          input$prqc_18,
          input$prqc_19,
          input$prqc_20)
    })
    
    observeEvent(input$prqc_submit, {
        # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
        withBusyIndicatorServer("prqc_submit", {
            Sys.sleep(1)
            if (length(prqc_responses()) != 20) {
                stop("select a response for all questions")
            }
        })
    })
    
    output$prqc_score1 <- renderText({
        if (length(prqc_responses()) != 20) {
            NA
        } else {
            prqc_score1 <- prqcScoringGuidelines(as.integer(prqc_responses()))
            format(prqc_score1$bully,digits = 2)
        }
    })
    output$prqc_score2 <- renderText({
        if (length(prqc_responses()) != 20) {
            NA
        } else {
            prqc_score2 <- prqcScoringGuidelines(as.integer(prqc_responses()))
            format(prqc_score2$victim,digits = 2)
        }
    })
    output$prqc_score3 <- renderText({
        if (length(prqc_responses()) != 20) {
            NA
        } else {
            prqc_score3 <- prqcScoringGuidelines(as.integer(prqc_responses()))
            format(prqc_score3$prosocial,digits = 2)
        }
    })
}