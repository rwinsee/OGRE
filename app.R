packages <- c("dplyr", "DT")

for (pkg in packages) {
  
  cat("\n-----------------------------\n")
  cat("Traitement du package :", pkg, "\n")
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Le package", pkg, "n'est pas installé. Installation en cours...\n")
    install.packages(pkg)
    cat("Installation terminée pour", pkg, "\n")
  } else {
    cat("Le package", pkg, "est déjà installé.\n")
  }
  
  cat("Chargement du package", pkg, "...\n")
  library(pkg, character.only = TRUE)
  
  print(paste("Le package", pkg, "est maintenant chargé."))
}

source(file.path("edition", "global_edition.R"), local = TRUE)
source(file.path("edition", "ui_edition.R"), local = TRUE)
source(file.path("edition", "server_edition.R"), local = TRUE)
source(file.path("supervision", "global_supervision.R"), local = TRUE)
source(file.path("supervision", "ui_supervision.R"), local = TRUE)
source(file.path("supervision", "server_supervision.R"), local = TRUE)
source(file.path("validation", "global_validation.R"), local = TRUE)
source(file.path("validation", "ui_validation.R"), local = TRUE)
source(file.path("validation", "server_validation.R"), local = TRUE)


ui <- fluidPage(
  title = "OGRE - Outil de Gestion du ROME pour EDEP",
  tags$head(
    tags$style(HTML("
      .workspace-shell {
        max-width: 1480px;
        margin: 0 auto;
        padding-top: 18px;
        padding-bottom: 26px;
      }
      .workspace-intro {
        background: linear-gradient(140deg, #16324a 0%, #27567b 52%, #2f7db6 100%);
        border-radius: 24px;
        color: #ffffff;
        padding: 26px 28px;
        margin-bottom: 18px;
        box-shadow: 0 18px 32px rgba(15, 37, 58, 0.22);
      }
      .workspace-kicker {
        text-transform: uppercase;
        letter-spacing: 0.14em;
        font-size: 12px;
        opacity: 0.82;
        margin-bottom: 8px;
      }
      .workspace-title {
        font-size: 34px;
        font-weight: 800;
        line-height: 1.05;
        margin-bottom: 10px;
      }
      .workspace-copy {
        font-size: 15px;
        max-width: 920px;
        line-height: 1.6;
        opacity: 0.92;
      }
      .mode-tabs > .nav {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 12px;
        border-bottom: none;
        margin-bottom: 18px;
      }
      .mode-tabs > .nav > li {
        float: none;
      }
      .mode-tabs > .nav > li > a {
        border: 1px solid #d9e2ee;
        border-radius: 18px;
        background: #ffffff;
        color: #17324d;
        font-size: 18px;
        font-weight: 700;
        min-height: 84px;
        padding: 22px 20px;
        box-shadow: 0 12px 24px rgba(23, 43, 77, 0.08);
        transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
      }
      .mode-tabs > .nav > li > a:hover {
        transform: translateY(-1px);
        border-color: #b8cae3;
        box-shadow: 0 16px 28px rgba(23, 43, 77, 0.12);
      }
      .mode-tabs > .nav > li.active > a,
      .mode-tabs > .nav > li.active > a:hover,
      .mode-tabs > .nav > li.active > a:focus {
        background: linear-gradient(140deg, #16324a 0%, #295f8f 100%);
        color: #ffffff;
        border-color: #16324a;
        box-shadow: 0 18px 30px rgba(22, 50, 74, 0.20);
      }
      .mode-tabs > .tab-content {
        background: transparent;
      }
    "))
  ),
  div(
    class = "workspace-shell",
    div(
      class = "workspace-intro",
      div(class = "workspace-kicker", "OGRE"),
      div(class = "workspace-title", "Outil de Gestion du ROME pour EDEP"),
      div(
        class = "workspace-copy",
        "Edition, supervision et validation sont separees pour piloter le cycle de vie complet des propositions de familles."
      ),
      div(
        style = "margin-top: 14px;",
        actionButton("refresh_workflow", "Rafraichir les files", class = "btn-default")
      )
    ),
    div(
      class = "mode-tabs",
      tabsetPanel(
        id = "workflow_mode",
        type = "tabs",
        selected = "Edition",
        tabPanel("Edition", edition_ui()),
        tabPanel("Supervision", supervision_ui()),
        tabPanel("Validation", validation_ui())
      )
    )
  )
)

server <- function(input, output, session) {
  session$userData$workflow_refresh <- reactiveVal(0)
  
  observeEvent(input$refresh_workflow, {
    token <- session$userData$workflow_refresh
    token(token() + 1)
    showNotification("Files de workflow rafraichies.", type = "message")
  })
  
  edition_server(input, output, session)
  supervision_server(input, output, session)
  validation_server(input, output, session)
}

shinyApp(ui, server)
