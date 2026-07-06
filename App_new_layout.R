args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
setwd(dirname(normalizePath(script_path)))

cat("Working directory impostata a:", getwd(), "\n")

user_lib <- file.path(Sys.getenv('USERPROFILE'), 'Rlibs')
.libPaths(c(user_lib, .libPaths()))
 
options(shiny.maxRequestSize = 1024^3)  # 1 GB

#caricamento pacchetti
library("shiny")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("shinyalert")
library("shinyFeedback")
library("plotly")
library("ggplot2")
library("dplyr")
library("readr")
library("readxl")
library("DT")
library("parsec")
library("tidyverse")
library("shinycssloaders")
library("htmltools")
library("shinybusy")
library("shinyjs")
library("highcharter")
library("rmarkdown")
library("gridExtra")
library("grid")
library("later")
library("png")

options(shiny.maxRequestSize = 50*1024^2)

addResourcePath("prefix", "www")

logo <- readPNG("www/logo_pdf.png")  # metti il file nella cartella www della shiny app
footer <- readPNG("www/footer.png")

logo_grob <- rasterGrob(logo, width = unit(0.9, "npc"))
footer_grob <- rasterGrob(footer, width = unit(1, "npc"))


ui <- fluidPage(
  
  useShinyjs(),
  
#impostazioni pagine e layout
  tags$head(
    tags$style(HTML("
      body {
        margin: 0; 
        padding: 0; 
        width: 100%;
        overflow: hidden !important;
      }

      .container-fluid {
        padding-left: 0; /* Rimuove il padding a sinistra */
        padding-right: 0; /* Rimuove il padding a destra */
      }
      
      .header {
        background-color: white;
        color: #006699;
        padding: 10px;
        height: 100px;
        font-size: 20px;
        font-weight: bold;
        display: flex;
        justify-content: flex-start; /* Mantiene le immagini a sinistra */
        align-items: center;
        border-bottom: 1px solid #ccc;
      }
      .header img {
        height: 100%;
        max-height: 60px; /* Per adattarsi alla dimensione del header */
        max-width: 200px; /* Imposta un limite di larghezza per il logo */
        object-fit: contain; /* Mantiene le proporzioni del logo */
      }
      .header-left img {
        height: 100%;
        max-height: 72px; /* Altezza personalizzata per l'immagine a sinistra */
        margin-right: 2px; /* Distanza tra l'immagine e il testo */
      }
      
     .header-text {
        position: absolute;
        left: 50%; /* Centra orizzontalmente */
        transform: translateX(-50%); /* Compensa il margine per centrare esattamente */
        text-align: center;
        font-size: 20px; /* Puoi personalizzare la dimensione del testo */
        color: #006699;
      }
      
      .header-small {
        position: relative;
        max-height: 72px;
        margin-right: 2px; /* Distanza tra l'immagine a sinistra e l'immagine piccola */
        max-width: 50px;  /* Imposta una larghezza massima per l'immagine piccola */
        object-fit: contain; /* Mantiene le proporzioni dell'immagine */
      }
      
      .header-logo {
        margin-left: auto; /* Spinge l'immagine verso destra */
      }
      
      
      /* Styling for the Tabset Panel */
      .nav-tabs {
        background-color: #f5f5f5; /* Colore di sfondo della striscia delle tab */
        border-bottom: 2px solid #ccc; /* Linea sottile sotto la striscia delle tab */
        width: 100%; /* Estende la larghezza a tutta la pagina */
      }

      .nav-tabs .nav-item .nav-link {
        color: #006699; /* Colore del testo delle tab */
        background-color: #e9e9e9; /* Colore di sfondo delle tab non selezionate */
        border: 1px solid #ccc; /* Bordo delle tab */
        border-radius: 5px;
        margin-right: 5px;
      }

      .nav-tabs .nav-item .nav-link:hover {
        background-color: #d1d1d1; /* Colore di sfondo al passaggio del mouse */
      }

      .nav-tabs .nav-item .nav-link.active {
        background-color: #006699; /* Colore di sfondo della tab selezionata */
        color: white; /* Colore del testo della tab selezionata */
      }

      .tab-content {
        width: 100%; /* Estende la larghezza del contenuto del tab */
      }
      
      .footer {
        background-color: white;
        color: #555; /* Grigio scuro */
        padding: 0;
        position: fixed;
        bottom: 0;
        width: 100%;
        text-align: center;
        font-size: 10px;
        border-top: 1px solid #ccc;
      }
      .footer img {
        width: 100%; /* L'immagine del footer occupa tutta la larghezza */
        height: auto; /* Mantieni le proporzioni */
        display: block; /* Rimuovi spazi bianchi sotto l'immagine */
        margin: 0; /* Rimuovi margini */
      }
      .footer-text {
        display: block;
        padding: 5px;
        color: #555;
        font-size: 11px;
        text-align: center;
      }
      
      .sidebar {
        margin-left: 10px; /* Aggiunge spazio a sinistra */
      }
      
		  
		  .shiny-file-input-progress {
		    margin-bottom: 0px !important; /* Riduce lo spazio sotto la barra di progresso del file input */
		  }
		  .form-group.shiny-input-container {
		    margin-bottom: 5px !important; /* Riduce il margine del file input */
		  }
      
   
    "))
  ),
  
	
	tags$script(HTML("
  Shiny.addCustomMessageHandler('bindDownload', function(message) {
    document.getElementById(message.id).addEventListener('click', function() {
      document.getElementById(message.button).click();
    });
  });
")),

	

  tags$div(class = "header",
           tags$div(class = "header-left", 
                    tags$img(src = "prefix/main.png") 
           ),
           tags$div(class = "header-small", 
                    tags$img(src = "prefix/ageit_spoke4.png") 
           ),
           tags$div(class = "header-text", 
                    "Costruzione Indice di Fragilità Sanitaria" 
           ),
           tags$div(class = "header-logo", 
                    tags$img(src = "prefix/LogoUnipdDip.png") 
           ) 
  ),
  
#imposto layout con tabpanel
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("Home",div(style = "display: flex; justify-content: space-between; align-items: center; padding: 10px;",
                          h2("Benvenuto!", style = "font-size: 16px; color: #006699; margin-left: 10px; margin-top: 10px; margin-bottom: 10px;"),
                          downloadButton("download_manual", "Scarica Manuale Utente", class = "btn btn-primary")
             ),
             p(HTML("Questa applicazione consente di calcolare un Indicatore di Fragilità Sanitaria sulla popolazione di 65 anni o più assistita da un'Azienda Sanitaria Locale.<br><strong>(<a href='https://arxiv.org/abs/2506.23158' target='_blank'>https://arxiv.org/abs/2506.23158</a>)</strong><br>
							Seguire attentamente le indicazioni del <a id='download_link' href='#' style='color: blue; text-decoration: underline;'>Manuale Utente</a>.<br>"), 
							  style = "margin-left: 20px; font-size: 14px; color: #006699;"),
             	
tags$link(rel = "stylesheet",
            href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
#card animate per la home
  fluidRow(
    column(3,
      div(class = "card shadow-sm animate__animated animate__fadeInUp",
          style = "width: 350px; min-height: 350px; padding: 15px; margin: auto;",
          tags$img(src = "prefix/passo1.png",
                   style = "max-height:160px; width:auto; display:block; margin:0 auto 10px;"),
          h4("1° Passo:",
             class = "text-center", style = "color:#006699; font-size:16px;"),
          p("Preparare il dataset seguendo le indicazioni del Manuale Utente",
            class = "text-center", style = "font-size:13px;")
      )
    ),
    column(3,
      div(class = "card shadow-sm animate__animated animate__fadeInUp animate__delay-1s",
          style = "width: 350px; min-height: 350px; padding: 15px; margin: auto;",
          tags$img(src = "prefix/passo2.png",
                   style = "max-height:160px; width:auto; display:block; margin:0 auto 10px;"),
          h4("2° Passo:",
             class = "text-center", style = "color:#006699; font-size:16px;"),
          p("Caricare il file .csv e avviare il calcolo dell'Indicatore",
            class = "text-center", style = "font-size:13px;")
      )
    ),
    column(3,
      div(class = "card shadow-sm animate__animated animate__fadeInUp animate__delay-2s",
          style = "width: 350px; min-height: 350px; padding: 15px; margin: auto;",
          tags$img(src = "prefix/passo3.png",
                   style = "max-height:160px; width:auto; display:block; margin:0 auto 10px;"),
          h4("3° Passo:",
             class = "text-center", style = "color:#006699; font-size:16px;"),
          p("Visualizzare i risultati. A questo punto sarà possibile scaricare il nuovo file con l'indicatore di Fragilità sanitaria della popolazione",
            class = "text-center", style = "font-size:13px;")
      )
    ),
  	column(3,
      div(class = "card shadow-sm animate__animated animate__fadeInUp animate__delay-3s",
          style = "width: 350px; min-height: 350px; padding: 15px; margin: auto;",
          tags$img(src = "prefix/passo4.png",
                   style = "max-height:160px; width:auto; display:block; margin:0 auto 10px;"),
          h4("4° Passo",
             class = "text-center", style = "color:#006699; font-size:16px;"),
          p("Analisi aggiuntive in base alle variabili disponibili o caricamento di un nuovo soggetto.",
            class = "text-center", style = "font-size:13px;")
      )
    ),
  ),

  tags$style(HTML("
    .card:hover {
      box-shadow: 0 0.5rem 1rem rgba(0,0,0,.25)!important;
      transform: translateY(-4px);
      transition: all .2s ease-in-out;
    }
  "))
             	),
             tabPanel("Calcolo Indicatore Popolazione",
             p("Leggere attentamente le linee guida prima di procedere al calcolo. L'applicazione non memorizza alcun dato; chiudere la finestra comporterà la perdita del lavoro fatto.",
               style = "margin-left: 20px; padding: 10px; font-size: 12px; color: #006699; margin-bottom: 0px;"),
             sidebarLayout(
               sidebarPanel(
               	width=4,
                 div(style = "color: #006699; font-weight: bold; font-size: 12px; margin-bottom: 2px;",
										  "Caricare il file CSV contenente tutta la popolazione di interesse"
										),
												  fileInput(
												    "file_csv",
												    label = NULL, 
												    accept = c(".csv"),
												    buttonLabel = "Seleziona file",
												    placeholder = "Nessun file caricato",
												    width = "100%"
												  )
												,
                 div(style = "margin-bottom: 5px;", uiOutput("upload_message")),
						   div(
						  style = "display: flex; align-items: center; gap: 5px; flex-wrap: wrap;",
						
						  actionButton("start_calculation", "Avviare Calcolo Indicatore", 
						               class = "btn btn-primary btn-sm", 
						               style = "font-size: 12px; line-height: 1.5; padding: 6px 10px;"),
						
						  div(
						    style = "display: inline-block; line-height: 1.5; font-size: 12px;", 
						    uiOutput("download_button_ui")
						  )
						),
               	p(HTML("Caricare un <strong>file CSV</strong> con le seguenti variabili necessarie:<br>
             	<i>(Clicca sulle singole variabili per ulteriori dettagli; il file può contenere variabili aggiuntive utili per le analisi successive)</i><br>"),
               style = "margin-top: 10px; font-size: 12px; color: #006699;"),
               	uiOutput("elenco_variabili"),
               	class = "sidebar"
               ),
               mainPanel(
               	width=8,
               	div(style = "max-height: 600px; overflow-y: auto; overflow-x: hidden; padding-right: 5px;",
               	      fluidRow(
								        column(6, uiOutput("summary_text"), 
																			        	uiOutput("box_summary_table")
								        	),
               	      	column(6, div(style = "text-align: right; margin-right: 20px;",
                  uiOutput("download_pdf_ui")))
								      ),
               		div(style = "height: 30px;"),
								      fluidRow(
								        column(6, plotOutput("histogram", height = "230px")),        
								        column(6, plotOutput("variability_plot", height = "230px")) 
    									 )
               ))
             )),
										tabPanel("Analisi",
											p("In questa pagina è possibile visualizzare analisi aggiuntive ma solo dopo aver calcolato l'indicatore per la popolazione.",
										    style = "font-style: italic; color: #006699; margin-bottom: 20px; margin-left: 15px; margin-top: 5px;"
										  ),
										  uiOutput("contenuto_analisi")
										),
           	tabPanel("Calcolo Indicatore Soggetto Aggiuntivo",uiOutput("contenuto_dinamico")  
							)
										),
           	
           )
    )
  ,
  
  # Footer
  tags$footer(class = "footer",
              tags$img(src = "prefix/footer.png"), 
              tags$span(class = "footer-text", 
                        "Progetto finanziato dal Piano Nazionale di Ripresa e Resilienza - Missione 4, \"Istruzione e Ricerca\" - Componente 2, \"Dalla ricerca all'impresa\" - Linea di investimento 1.3 - Avviso Pubblico D.D. n.341/2022 Ministero dell'Università e della Ricerca"
              ),
  
  actionButton(
    "close_app", "Termina Applicazione",
    class = "btn-danger",
    style = "position: absolute; right: 20px; top: 50%; transform: translateY(-50%);"
  )
  	
  )
)



server <- function(input, output, session) {

observeEvent(input$close_app, {
  showModal(modalDialog(
    title = NULL,  # nessun titolo per un look pulito
    easyClose = FALSE,
    footer = NULL,
    size = "l",  # larghezza grande
    tags$div(
      style = "font-size: 28px; text-align: center; padding: 100px 20px; height: 80vh;",
      "Grazie! E' ora possibile chiudere tutte le finestre."
    )
  ))
  
  # Ferma l'app dopo 2 secondi
  later::later(function() { stopApp() }, 5)
})	

observe({
  session$sendCustomMessage("bindDownload", list(id = "download_link", button = "download_manual"))
	})
  
  output$download_manual <- downloadHandler(
    filename = function() {
      "Manuale_Utente.pdf"  # Nome del file scaricato
    },
    content = function(file) {
      # Copia il file PDF dalla cartella 'www' alla destinazione di download
      file.copy("www/Manuale_utente.pdf", file)
    },
    contentType = "application/pdf"
  )
  
  
voci <- c(
  "ID",
  "Età",
  "Disabilità",
  "Numero Ricoveri",
  "Malattie del sistema nervoso",
  "Disturbi Psichici",
  "Insufficienza Renale",
  "Insufficienza Cardiaca",
  "Cancro"
)

  descrizioni <- list(
  HTML("ID (la variabile nel file deve essere denominata <b>ID</b>): Codice identificativo del soggetto."),
  
  HTML("Età (la variabile nel file deve essere denominata <b>Eta</b>): Età del soggetto aggiornata al 31 dicembre dell’ultimo anno. Nel file indicare con “1” se il soggetto ha un’età compresa tra 65 e 69 anni, “2” tra 70 e 74 anni, “3” tra 75 e 79 anni, “4” tra 80 e 84 anni, “5” tra 85 e 89 anni, “6” 90 anni o più."),
  
  HTML("Disabilità (la variabile nel file deve essere denominata <b>Disabilita1_2</b>): La disabilità del soggetto viene osservata tramite il registro delle esenzioni e il flusso relativo ai servizi di assistenza domiciliare. Nel file indicare con “1” se il soggetto presenta una disabilità, “0” altrimenti."),
  
  HTML("Numero Ricoveri (la variabile nel file deve essere denominata <b>Ricoveri1_2</b>): Numero di ricoveri del soggetto negli ultimi due anni, osservabile tramite il flusso delle schede di dimissione ospedaliera. Nel file indicare con “0” se il soggetto non ha avuto ricoveri negli ultimi due anni, “1” se il soggetto ha avuto uno o due ricoveri, “2” se ha avuto tre o più ricoveri."),
  
  HTML("Malattie del sistema nervoso (la variabile nel file deve essere denominata <b>Nervoso1_2</b>): Le malattie del sistema nervoso vengono osservate tramite il registro delle esenzioni e i flussi delle schede di dimissione ospedaliera, del pronto soccorso, della psichiatria territoriale e della farmaceutica territoriale. Nel file indicare con “1” se il soggetto ha avuto una malattia del sistema nervoso negli ultimi due anni, “0” altrimenti."),
  
  HTML("Disturbi Psichici (la variabile nel file deve essere denominata <b>Psichici1_2</b>): I disturbi psichici e comportamentali vengono osservati tramite il registro delle esenzioni e i flussi delle schede di dimissione ospedaliera, del pronto soccorso, della psichiatria territoriale e della farmaceutica territoriale. Nel file indicare con “1” se il soggetto ha avuto un disturbo psichico negli ultimi due anni, “0” altrimenti."),
  
  HTML("Insufficienza Renale (la variabile nel file deve essere denominata <b>InsRenale1_2</b>): L’insufficienza renale viene osservata tramite il registro delle esenzioni e i flussi delle schede di dimissione ospedaliera e del pronto soccorso. Nel file indicare con “1” se il soggetto ha avuto un'insufficienza renale negli ultimi due anni, “0” altrimenti."),
  
  HTML("Insufficienza Cardiaca (la variabile nel file deve essere denominata <b>InsCardiaca1_2</b>): L’insufficienza cardiaca viene osservata tramite il registro delle esenzioni e i flussi delle schede di dimissione ospedaliera e del pronto soccorso. Nel file indicare con “1” se il soggetto ha avuto un'insufficienza cardiaca negli ultimi due anni, “0” altrimenti."),
  
  HTML("Cancro (la variabile nel file deve essere denominata <b>Cancro1_2</b>): Il cancro viene osservato tramite il registro delle esenzioni e i flussi delle schede di dimissione ospedaliera, del pronto soccorso e della farmaceutica territoriale. Nel file indicare con “1” se il soggetto ha avuto un cancro negli ultimi due anni, “0” altrimenti.")
)
  
  
  label_variabili <- c(
    "Eta" = "Età",
    "Disabilita1_2" = "Disabilità",
    "Ricoveri1_2" = "Numero Ricoveri",
    "Nervoso1_2" = "Malattie del sistema nervoso",
    "Psichici1_2" = "Disturbi Psichici",
    "InsRenale1_2" = "Insufficienza Renale",
    "InsCardiaca1_2" = "Insufficienza Cardiaca",
    "Cancro1_2" = "Cancro"
  )
  
  output$download_pdf_ui <- renderUI({
  req(dati_modificati())  
  downloadButton("scarica_report", "Scarica PDF", class = "btn-primary")
})


  
output$elenco_variabili <- renderUI({
  n <- length(voci)
  ids <- paste0("voce_", seq_len(n))
  voci_split <- split(seq_len(n), rep(1:2, length.out = n))

  fluidRow(
    lapply(1:2, function(col) {
      column(
        width = 6,
        lapply(voci_split[[col]], function(idx) {
          tagList(
            actionLink(
              inputId = ids[idx],
              label = tags$div(
                style = "
                  background-color: #e6f2f8;
                  color: #16668c;
                  padding: 6px 10px;
                  margin-bottom: 6px;
                  font-size: 12px;
                  border-radius: 6px;
                  display: inline-block;
                  width: 100%;
                ",
               voci[idx]
              )
            )
          )
        })
      )
    })
  )
})

  # Generazione dinamica delle finestre modali
		lapply(seq_along(voci), function(i) {
		  observeEvent(input[[paste0("voce_", i)]], {
		    showModal(modalDialog(
		      title = paste("Dettaglio Variabile:", voci[i]),
		      descrizioni[i],
		      easyClose = TRUE,
		      footer = modalButton("Chiudi")
		    ))
		  }, ignoreInit = TRUE)
		})
  
  # Disabilita i bottoni all'avvio
  shinyjs::disable("start_calculation")
  shinyjs::disable("scarica_report")
  
  observe({
    req(dati_caricati())  
    shinyjs::enable("start_calculation")  # Abilita il bottone se c'è un file       
  })
  
  output$download_button_ui <- renderUI({
    req(dati_modificati())  
    downloadButton("download_csv", "Scaricare file con indicatore", class = "btn btn-success", style = "font-size: 12px; line-height: 1.5; padding: 6px 10px;")  
  })
  
  dati_caricati <- reactiveVal(NULL) 
  dati_analisi <- reactiveVal(NULL)
  dati_analisi_final <- reactiveVal(NULL)
  dati_modificati <- reactiveVal(NULL)  
  stato_pagina <- reactiveVal("inizio")
  scelta_effettuata <- reactiveVal(FALSE)
  variabile_scelta <- reactiveVal("")
  
  
deloof=function(data){
  obs_strings<-apply(X = data,MARGIN = 1,FUN = toString)
  labels=gsub(", ", "", obs_strings)
  ranks_list=rep(NA,length(labels))
  set_profiles=pop2prof(data,sep="")
  zeta=getzeta(set_profiles)
  list_prof=names(set_profiles$freq)
  num_prof=length(list_prof)
  avrank=rep(NA,num_prof)
  incomparables=incomp(zeta)
  mutprob1=matrix(0,num_prof,num_prof)
  colnames(mutprob1)=list_prof
  rownames(mutprob1)=list_prof  
  downsets= apply(zeta, 2, function(x) sum(x)-1)
  upsets= apply(zeta, 1, function(x) sum(x)-1)
  for (rig1 in 1:num_prof){
    for (col1 in 1:rig1){
      if (incomparables[rig1,col1]==TRUE){
        prob=((downsets[rig1]+1)*(upsets[col1]+1))/((downsets[rig1]+1)*(upsets[col1]+1)+(downsets[col1]+1)*(upsets[rig1]+1))
        mutprob1[rig1,col1]=prob
        mutprob1[col1,rig1]=1-prob
      }
    }
  }
  down2=downsets+apply(mutprob1,1,sum)
  up2=upsets+apply(mutprob1,2,sum)
  print("first-level mutual rank probabilities computed")
  mutprob2=matrix(0,num_prof,num_prof)
  colnames(mutprob2)=list_prof
  rownames(mutprob2)=list_prof  
  
  for (rig1 in 1:num_prof){
    for (col1 in 1:rig1){
      if (incomparables[rig1,col1]==TRUE){
        prob=((down2[rig1]+1)*(up2[col1]+1))/((down2[rig1]+1)*(up2[col1]+1)+(down2[col1]+1)*(up2[rig1]+1))
        mutprob2[rig1,col1]=prob
        mutprob2[col1,rig1]=1-prob
      }
    }
  }
  print("second-level mutual rank probabilities computed")
  result= downsets+1+ apply(mutprob2,1,sum)
  
  for (i in 1: num_prof){
    ranks_list[which(labels==list_prof[i])]=result[i]
  }
  names(ranks_list)=labels
  return((ranks_list))  
}
  
  
  observeEvent(input$file_csv, {
    req(input$file_csv)
    
    result <- tryCatch({
      
      # Caricamento CSV
      dati <- read.csv(input$file_csv$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
      
      
	   clean_names <- function(x) {
  x |> trimws() |> tolower()
}

colonne_necessarie <- c(
  "Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", 
  "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2"
)

# SOLO confronto, senza modificare dati
colonne_presenti_clean <- clean_names(names(dati))
colonne_necessarie_clean <- clean_names(colonne_necessarie)

colonne_mancanti <- setdiff(colonne_necessarie_clean, colonne_presenti_clean)

if (length(colonne_mancanti) > 0) {
  stop(
    paste0(
      "❌ Mancano le seguenti variabili:\n- ",
      paste(colonne_mancanti, collapse = "\n- ")
    )
  )
}
      
      # Aggiunta ID fake se mancante
      if (!"ID" %in% colnames(dati)) {
        dati$id_fake <- seq_len(nrow(dati))
      }
      id_col <- if ("ID" %in% colnames(dati)) "ID" else "id_fake"
      
      # Salva i dati grezzi per eventuale uso
      dati_analisi(dati)
      
      # Subset delle colonne usate nell’analisi
      dati <- dati[, c(id_col, colonne_necessarie)]
      
      # Gestione valori NA
      righe_na <- which(apply(dati, 1, function(x) any(is.na(x))))
      num_righe_na <- length(righe_na)
      
      if (num_righe_na > 0) {
        dati <- dati[-righe_na, ]
      }
      
      # Validazione dei valori
      check_variable_values <- function(df) {
        errors <- c()
        
        if (!all(df$Eta %in% 1:6)) {
          errors <- c(errors, "⚠️ Eta deve contenere solo valori interi tra 1 e 6.")
        }
        
        if (!all(df$Ricoveri1_2 %in% 0:2)) {
          errors <- c(errors, "⚠️ Ricoveri1_2 deve contenere solo 0, 1 o 2.")
        }
        
        binary_vars <- c("Disabilita1_2", "Nervoso1_2", "Psichici1_2",
                         "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2")
        for (var in binary_vars) {
          if (!all(df[[var]] %in% c(0, 1))) {
            errors <- c(errors, paste0("⚠️ ", var, " deve contenere solo 0 o 1."))
          }
        }
        
        if (length(errors) > 0) {
          stop(paste(errors, collapse = "\n"))
        }
      }
      
      check_variable_values(dati)  # <-- Esegui controllo valori
      
      # Tutto ok, restituisco
      list(dati = dati, num_righe_na = num_righe_na)
      
    }, error = function(e) {
      # Gestione errore
      list(error = paste("❌ Errore durante il caricamento o la pulizia dei dati:\n", e$message))
    })
    
    # Output messaggi
    if (!is.null(result$error)) {
      output$upload_message <- renderUI({
        tags$p(result$error, style = "color: red; font-size: 14px; font-weight: bold; white-space: pre-wrap;")
      })
    } else {
      dati <- result$dati
      num_righe_na <- result$num_righe_na
      
      output$upload_message <- renderUI({
        if (num_righe_na > 0) {
          tags$p(paste0("✅ File caricato con successo!\n",
                        "⚠️ Sono state eliminate ", num_righe_na, " righe contenenti valori mancanti.\n",
                        "Il nuovo dataset ha ", nrow(dati), " righe."),
                 style = "color: green; font-size: 12px; font-weight: bold; white-space: pre-wrap;")
        } else {
          tags$p("✅ File caricato con successo! Nessun valore mancante rilevato.",
                 style = "color: green; font-size: 12px; font-weight: bold;")
        }
      })
      
      # Salva i dati
      dati_caricati(dati)
      dati_modificati(NULL)
    }
  })
  
  
  
  
  # Calcolo dell'indicatore, avviato dal bottone
  observeEvent(input$start_calculation, {
      	show_modal_spinner(
  text = HTML(
    "Calcolo in corso... Attendere prego ⏳<br>
    <small>L'operazione potrebbe richiedere circa 10/15 minuti, non chiudere la finestra.</small>"
  )
)
    req(dati_caricati()) 

    dati <- dati_caricati()
    #print("aaa")
    # Esegui calcolo indicatore
    indicatore_poi <- deloof(dati[, c("Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2")])
    indicatore_poi <- round(indicatore_poi, 10)
    indicatore_poi <- (indicatore_poi - min(indicatore_poi)) / (max(indicatore_poi) - min(indicatore_poi))
    dati$Indicatore <- indicatore_poi
    
    dati <- dati %>% select(Indicatore, everything())
    
    dati <- dati %>%
	 mutate(Eta = factor(Eta, levels = c(1, 2, 3, 4, 5, 6), 
                      labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")))
    
    remove_modal_spinner() 
    dati_modificati(dati)  # Salva i dati modificati
    
			dm <- dati_modificati()
			da <- dati_analisi()
			
			# Identifica il nome della colonna ID in ciascun dataset
			id_dm <- if ("ID" %in% names(dm)) "ID" else "id_fake"
			id_da <- if ("ID" %in% names(da)) "ID" else "id_fake"
			
			# Rinominare temporaneamente per la join
			dm_join <- dm %>% dplyr::rename(ID_JOIN = all_of(id_dm))
			da_join <- da %>% dplyr::rename(ID_JOIN = all_of(id_da))
			
			# Prende solo le colonne aggiuntive di 'da'
			da_extra <- da_join %>% select(-ID_JOIN, -any_of(names(dm)))
			
			# Join
			risultato_join <- dm_join %>%
			  dplyr::left_join(bind_cols(da_join["ID_JOIN"], da_extra), by = "ID_JOIN")
			
			# Rinomina finale condizionato
			final_id_name <- id_dm  # cioè "ID" o "id_fake"
			risultato_join <- risultato_join %>%
			  dplyr::rename(!!final_id_name := ID_JOIN)
			
			# Assegna al reactiveVal
			dati_analisi_final(risultato_join)
    
  })
  
  # Output messaggio di calcolo
  output$loading_message <- renderUI({
    if (is.null(dati_caricati())) {  # Nessun file caricato
      h4("Carica un file CSV per iniziare")
    } else if (is.null(dati_modificati())) {  # Nessun calcolo avviato
      h4("Premi il bottone 'Inizia Calcolo Indicatore'")
    } else {  # Calcolo completato
      h4("Calcolo completato! Visualizza l'anteprima o scarica il file.")
    }
  })
  

  
  # Permette di scaricare il file modificato
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("file_modificato_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dati_analisi_final(), file, row.names = FALSE)
    }
  )
 

  
  output$summary_text <- renderUI({
  req(dati_modificati())
  num_soggetti <- nrow(dati_modificati())

  tags$p(
    paste("Sono stati analizzati", num_soggetti, "soggetti. Di seguito alcune statistiche relative all'indicatore di fragilità:"),
    style = "color: #006699; font-size: 13px; font-weight: bold; margin-bottom: 10px;"
  )
})
  
  output$box_summary_table <- renderUI({
  req(dati_modificati())

  div(
    style = "display: flex; justify-content: center; background-color:#f0f8ff; padding:0px; border-radius:8px;
             border:1px solid #f0f8ff; color:#16668c; font-size: 12px;",
     tableOutput("summary_table")
  )
})
  
    # Creazione della tabella riassuntiva
  output$summary_table <- renderTable({
  	req(dati_modificati())  # Aspetta che ci siano dati validi
    dati_analisi <- dati_modificati()
		stats <- dati_analisi %>%
		    summarise(
		      Media = round(mean(Indicatore, na.rm = TRUE), 3),
		      Deviazione_Standard = round(sd(Indicatore, na.rm = TRUE), 3),
		      Q1 = round(quantile(Indicatore, 0.25, na.rm = TRUE), 3),
		      Mediana = round(quantile(Indicatore, 0.5, na.rm = TRUE), 3),
		      Q3 = round(quantile(Indicatore, 0.75, na.rm = TRUE), 3)
		    )
		  
		  # Riordini come tabella orizzontale
		  colnames(stats) <- c("Media", "Deviazione Std", "1° Quartile", "Mediana", "3° Quartile")
		  stats
		}, 
		striped = TRUE, 
		bordered = FALSE, 
		hover = TRUE, 
		spacing = "xs",
  	digits=3)
 
  
  # Creazione dell'istogramma
output$histogram <- renderPlot({
  req(dati_modificati())  
  dati_hist <- dati_modificati()

  ggplot(
    dati_hist,
    aes(
      x = Indicatore,
      y = after_stat(count / sum(count) * 100)
    )
  ) +
    geom_histogram(
      binwidth = 0.05,
      fill = "#16668c",
      color = "white"
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Distribuzione dell'Indicatore",
      x = "Indicatore di Fragilità",
      y = "Frequenza (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, color = "#044463"),
      axis.text.x = element_text(size = 12, color = "#044463"),
      axis.title = element_text(size = 14, color = "#044463"),
      plot.title = element_text(size = 14, color = "#044463", hjust = 0.5)
    )
})
  
  # Creazione del grafico di variabilità per classi di età
  output$variability_plot <- renderPlot({
  	req(dati_modificati())
  	shinyjs::enable("scarica_report")
  	dati_plot <- dati_modificati()
    dati_eta_stats <- dati_plot %>%
      group_by(Eta) %>%
      summarise(
        Quantile_25 = quantile(Indicatore, 0.25, na.rm = TRUE),
        Quantile_50 = quantile(Indicatore, 0.5, na.rm = TRUE),
        Quantile_75 = quantile(Indicatore, 0.75, na.rm = TRUE)
      )
    
    ggplot(dati_eta_stats, aes(y = Eta)) +
      geom_segment(aes(x = Quantile_25, xend = Quantile_75, yend = Eta), 
                   color = "#16668c", size = 2) +
      geom_point(aes(x = Quantile_50), color = "red", size = 3) +
    geom_text(aes(x = Quantile_25, label = round(Quantile_25, 3)),
              hjust = 1.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    geom_text(aes(x = Quantile_50, label = round(Quantile_50, 3)),
              vjust = -1, size = 4, color = "#e60000") +
    geom_text(aes(x = Quantile_75, label = round(Quantile_75, 3)),
              hjust = -0.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    	scale_x_continuous(
      limits = c(0, 1),
      #expand = c(0, 0)  # elimina margine ai bordi
    ) +
      labs(title = "Mediana e Scarto Interquartile dell'indicatore per classi di Età",
           x = "Indicatore di Fragilità",
           y = "Classe d'età") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12, color = "#16668c"),
        axis.text.x = element_text(size = 12, color = "#16668c"),
        axis.title = element_text(size = 14, color = "#16668c"),
        plot.title = element_text(size = 14, color = "#16668c", hjust = 0.5)
      )
  })
  
output$scarica_report <- downloadHandler(
  filename = function() paste0("analisi-fragilita-", Sys.Date(), ".pdf"),
  contentType = "application/pdf",  # ✅ specifica il tipo corretto
  content = function(file) {
    req(dati_modificati())

    dati <- dati_modificati()

    # 1. genera tabella statistiche
    stats <- dati %>% summarise(
      Media   = round(mean(Indicatore, na.rm = TRUE), 3),
      Dev.Std = round(sd(Indicatore, na.rm = TRUE), 3),
      Q1      = round(quantile(Indicatore, .25, na.rm = TRUE), 3),
      Mediana = round(quantile(Indicatore, .50, na.rm = TRUE), 3),
      Q3      = round(quantile(Indicatore, .75, na.rm = TRUE), 3)
    )

    tbl <- tableGrob(stats, rows = NULL,
                     theme = ttheme_minimal(base_size = 11))

    # 2. istogramma
    p1 <- ggplot(dati, aes(Indicatore)) +
      geom_histogram(binwidth = .05, fill = "#16668c", color = "white") +
    	scale_x_continuous(
      limits = c(0, 1),
      #expand = c(0, 0)  # elimina margine ai bordi
    ) +
      labs(title = "Distribuzione dell'Indicatore", x = NULL, y = "Frequenza") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5))

    # 3. variabilità per età
    dati_eta <- dati %>%
      group_by(Eta) %>%
      summarise(q25 = quantile(Indicatore, .25, na.rm = TRUE),
                q50 = quantile(Indicatore, .50, na.rm = TRUE),
                q75 = quantile(Indicatore, .75, na.rm = TRUE),
                .groups = "drop")

    p2 <- ggplot(dati_eta, aes(y = factor(Eta), x = q50)) +
      geom_errorbar(aes(xmin = q25, xmax = q75), width = 0.3, color = "#16668c") +
      geom_point(color = "red", size = 2.5) +
    geom_text(aes(x = q25, label = round(q25, 3)),
              hjust = 1.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    geom_text(aes(x = q50, label = round(q50, 3)),
              vjust = -1, size = 4, color = "#e60000") +
    geom_text(aes(x = q75, label = round(q75, 3)),
              hjust = -0.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    	scale_x_continuous(
      limits = c(0, 1),
      #expand = c(0, 0)  # elimina margine ai bordi
    ) +
      labs(title = "Mediana e Scarto Interquartile per classi d’età", x = NULL, y = "Classe d’età") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5))

    # HEADER
header <- arrangeGrob(
  logo_grob,
  textGrob(
    "Analisi Indicatore di fragilità",
    gp = gpar(fontsize = 14, fontface = "bold"),
    hjust = 0
  ),
  ncol = 2,
  widths = c(2, 5)
)

  # FOOTER
footer_band <- arrangeGrob(
  footer_grob
)

# 📝 DIDASCALIA sotto il footer
caption_text <- "Progetto finanziato dal Piano Nazionale di Ripresa e Resilienza - Missione 4, \"Istruzione e Ricerca\" - Componente 2, \"Dalla ricerca all'impresa\" - Linea di investimento 1.3 - Avviso Pubblico D.D. n.341/2022 Ministero dell'Università e della Ricerca"

caption <- textGrob(
  paste(strwrap(caption_text, width = 130), collapse = "\n"),
  gp = gpar(fontsize = 8.5, col = "grey30", lineheight = 1.05),
  just = "center"
)

 content <- arrangeGrob(
  tbl,
  textGrob("", gp = gpar(fontsize = 2)),
  p1,
  textGrob("", gp = gpar(fontsize = 2)),
  p2,
  ncol = 1,
  heights = c(1, 0.1, 3, 0.1, 3)
)

 footer_block <- arrangeGrob(
  footer_band,
  caption,
  ncol = 1,
  heights = unit.c(
    unit(1.2, "cm"),
    unit(1, "null")   # fondamentale: evita spazio fisso inutile
  )
)
 
 
# 📄 COSTRUZIONE PAGINA COMPLETA
final_page <- arrangeGrob(
  header,
  content,
  footer_block,
  ncol = 1,
  heights = unit.c(
    unit(2.5, "cm"),
    unit(1, "null"),
    unit(2.2, "cm")
  )
)

# 📄 PDF
pdf(file, width = 8.27, height = 11.69, onefile = FALSE)
grid.newpage()
grid.draw(final_page)
dev.off()
})

 
  output$contenuto_dinamico <- renderUI({
  stato <- stato_pagina()

    if (stato == "risultato") {
    tagList(
    	tags$div(style = "max-height: 600px; overflow-y: auto; padding-right: 15px; margin-left: 20px;",
      tags$h4("Calcolo completato!", style = "color: #006699; font-size: 13px; font-weight: bold; margin-bottom: 10px;"),
    							fluidRow(
								        column(6, uiOutput("summary_text_agg"), uiOutput("box_summary_table_agg")),
               	      	column(6, div(
									            style = "display: flex; justify-content: flex-end; gap: 10px; margin-right: 20px;",
									            downloadButton("scarica_report_agg", "Scarica PDF", class = "btn btn-primary"),
									            actionButton("reset_ui", "Inserisci un nuovo soggetto", class = "btn btn-primary")
									          ))# Tabella in alto su tutta la larghezza
								      ), 	div(style = "height: 40px;"),
								      fluidRow(
								        column(6, plotOutput("histogram_agg", height = "230px")),       
								        column(6, plotOutput("variability_plot_agg", height = "230px"))  
    									 )
    ))
  } else {
    tagList(
      h2("Caricare il file scaricato in precedenza dalla sezione 'Calcolo Indicatore Popolazione', contenente i dati relativi alla popolazione comprensivi dell'indicatore di fragilità:", style = "font-size: 14px; color: #006699; margin-left: 20px; margin-top: 10px; margin-bottom: 10px;"),
      fluidRow(
        column(1),
												column(3, {
												  output_messaggio <- NULL
												  dati_temp <- NULL
												  
												  if (!is.null(input$file_csv_2)) {
												    dati_temp <- tryCatch({
												      read.csv(input$file_csv_2$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
												    }, error = function(e) NULL)
												
												    variabili_richieste <- c("Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", 
												                             "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2", "Indicatore")
												
												    variabili_mancanti <- if (!is.null(dati_temp)) {
												      setdiff(variabili_richieste, names(dati_temp))
												    } else {
												      variabili_richieste
												    }
												
												    if (length(variabili_mancanti) > 0) {
												      output_messaggio <- div(style = "color: red; margin-top: 8px;",
												                              tags$span(icon("times-circle"), style = "color: red;"),
												                              " File caricato, ma mancano le seguenti variabili:",
												                              tags$ul(lapply(variabili_mancanti, tags$li))
												                              )
												      dati_temp <- NULL  # Reset se mancano variabili
												    } else {
												      output_messaggio <- div(style = "color: #006699; margin-top: 8px;",
												                              tags$span(icon("check-circle"), style = "color: green;"),
												                              " File CSV caricato correttamente.")
												    }
												  }
												
												  tagList(
												    fileInput(
												      "file_csv_2",
												      label = NULL,
												      accept = c(".csv"),
												      buttonLabel = "Seleziona file",
												      placeholder = "Nessun file caricato",
												      width = "100%"
												    ),
												    output_messaggio
												  )
												})
												      ),
    	if (!is.null(dati_temp) && all(variabili_richieste %in% names(dati_temp))) {

  tagList(
    h2("Inserire i dati relativi al nuovo soggetto da analizzare:", style = "font-size: 14px; color: #006699; margin-left: 20px; margin-top: 10px; margin-bottom: 10px;"),
    fluidRow(
      column(1),
      column(2, selectInput("var1", label = span("Età", style = "color:#006699;"), choices = setNames(1:6, c('1 (65-69)', '2 (70-74)', '3 (75-79)', '4 (80-84)', '5 (85-89)', '6 (90+)')))),
      column(2, selectInput("var2", span("Disabilità", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)')))),
      column(2, selectInput("var3", span("Numero Ricoveri", style = "color:#006699;"), setNames(c(0, 1, 2),c('0 (nessun ricovero)','1 (1 o 2 ricoveri)','2 (3 o più ricoveri)')))),
      column(2, selectInput("var4", span("Malattie del sistema nervoso", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)'))))
    ),
    fluidRow(
      column(1),
      column(2, selectInput("var5", span("Disturbi Psichici", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)')))),
      column(2, selectInput("var6", span("Insufficienza Renale", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)')))),
      column(2, selectInput("var7", span("Insufficienza Cardiaca", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)')))),
      column(2, selectInput("var8", span("Cancro", style = "color:#006699;"), choices = setNames(c(0, 1),c('0 (assente)','1 (presente)'))))
    ),
    fluidRow(
      column(1),
      column(3, actionButton("controlla_soggetto", "Avvia controllo nuovo soggetto", class = "btn btn-primary")),
      column(3, textOutput("profilo_check_msg"))
    )
  )
}
    	)
  }
})  
  
  
  
    
new_subject <- eventReactive(input$controlla_soggetto, {
    data.frame(
      id = "x",
      Eta = as.numeric(input$var1),
      Disabilita1_2 = as.numeric(input$var2),
      Ricoveri1_2 = as.numeric(input$var3),
      Nervoso1_2 = as.numeric(input$var4),
      Psichici1_2 = as.numeric(input$var5),
      InsRenale1_2 = as.numeric(input$var6),
      InsCardiaca1_2 = as.numeric(input$var7),
      Cancro1_2 = as.numeric(input$var8),
      stringsAsFactors = FALSE
    )
  })
  

observeEvent(input$controlla_soggetto, {
    req(input$file_csv_2)  # Aspetta che il file venga caricato
		dati_popolazione <- read.csv(input$file_csv_2$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
		eta_labels <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")
		dati_popolazione$Eta <- match(dati_popolazione$Eta, eta_labels)
		dati_popolazione$profilo <- dati_popolazione$Eta*10^7+
																	dati_popolazione$Disabilita1_2*10^6+
																	dati_popolazione$Ricoveri1_2*10^5+
																	dati_popolazione$Nervoso1_2*10^4 +
  																dati_popolazione$Psichici1_2*10^3+
																	dati_popolazione$InsRenale1_2*100+
																	dati_popolazione$InsCardiaca1_2*10+
																	dati_popolazione$Cancro1_2
		
		dati_caricati(dati_popolazione[, c("Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2")])
		
		profili_distinti<-unique(dati_popolazione$profilo)
		
		dati_new_subject <- new_subject()
		
		profilo_new_subject <- dati_new_subject$Eta*10^7+
																	dati_new_subject$Disabilita1_2*10^6+
																	dati_new_subject$Ricoveri1_2*10^5+
																	dati_new_subject$Nervoso1_2*10^4 +
  																dati_new_subject$Psichici1_2*10^3+
																	dati_new_subject$InsRenale1_2*100+
																	dati_new_subject$InsCardiaca1_2*10+
																	dati_new_subject$Cancro1_2
		
messaggio <- if (profilo_new_subject %in% profili_distinti) {
  "Profilo già presente"
} else {
  "Nuovo profilo"
}
	 
	 
	 # Se profilo già presente, prepara il grafico
  if (messaggio == "Profilo già presente") {
    valore_indicatore_soggetto <- dati_popolazione$Indicatore[
      which(dati_popolazione$profilo == profilo_new_subject)[1]
    ]
    
    
output$istogramma_indicatore <- renderPlot({
  
  x <- dati_popolazione$Indicatore
  
  # Calcolo istogramma senza disegnarlo
  h <- hist(
    x,
    breaks = seq(0, 1, by = 0.05),
    plot = FALSE
  )
  
  # Percentuale per classe
  h$counts <- h$counts / sum(h$counts) * 100
  
  # Disegno istogramma mantenendo grafica originale
  plot(
    h,
    freq = TRUE,  # ora counts sono percentuali
    col = "lightblue",
    border = "white",
    xlim = c(0, 1),
    ylim = c(0, max(h$counts) * 1.1),
    main = "Distribuzione dell'indicatore",
    xlab = "Valori dell'indicatore",
    ylab = "Frequenza (%)",
    axes = FALSE
  )
  
  # Asse X normale
  axis(1)
  
  # Asse Y formattato con %
  yticks <- pretty(h$counts)
  axis(2, at = yticks, labels = paste0(yticks, "%"), las = 1)
  
  box()
  
  # Linea verticale
  abline(v = valore_indicatore_soggetto, col = "red", lwd = 2)
  
  legend(
    "topright",
    legend = paste("Valore soggetto:", round(valore_indicatore_soggetto, 3)),
    col = "red",
    lwd = 2,
    bty = "n"
  )
})

}

  # Mostra popup
  showModal(
    modalDialog(
      title = "Esito del controllo",
      if (messaggio == "Profilo già presente") {
        tagList(
          tags$p("Il profilo inserito corrisponde a uno già esistente nella popolazione."),
          tags$p("Ecco la posizione del soggetto nella distribuzione dell’indicatore:"),
          plotOutput("istogramma_indicatore", height = "300px"),
          tags$p(style = "color: red; font-weight: bold;", messaggio)
        )
      } else {
        tagList(
      tags$p("Questo è un nuovo profilo non ancora presente nei dati caricati."),
      tags$p("E' possibile procedere con un nuovo calcolo dell'indicatore. Premere il bottone per avviare il calcolo."),
      actionButton("avvia_calcolo", "Avviare nuovo calcolo",   
      class = "btn btn-primary", 
			style = "font-size: 12px; padding: 5px;"),
      br(),
      uiOutput("esito_calcolo")  
    	)
      },
      easyClose = TRUE,
      footer = modalButton("Chiudi")
    )
  )
  
  
})


# Secondo blocco: separato, per il bottone dentro il modal
observeEvent(input$avvia_calcolo, {
	req(new_subject())
  show_modal_spinner(
  text = HTML(
    "Calcolo in corso... Attendere prego ⏳<br>
    <small>L'operazione potrebbe richiedere circa 10/15 minuti, non chiudere la finestra.</small>"
  )
)

  dati_uniti <- isolate({
    dati_new <- new_subject()
    dati_new <- dati_new[, c("Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2")]
    dati_pop <- dati_caricati()
    rbind(dati_new, dati_pop)
  })

  # Esegui calcolo indicatore
    indicatore_poi <- deloof(dati_uniti[, c("Eta", "Disabilita1_2", "Ricoveri1_2", "Nervoso1_2", "Psichici1_2", "InsRenale1_2", "InsCardiaca1_2", "Cancro1_2")])
    indicatore_poi <- round(indicatore_poi, 10)
    indicatore_poi <- (indicatore_poi - min(indicatore_poi)) / (max(indicatore_poi) - min(indicatore_poi))
    dati_uniti$Indicatore <- indicatore_poi
    
    dati_uniti <- dati_uniti %>% select(Indicatore, everything())
    
    dati_uniti <- dati_uniti %>%
	 mutate(Eta = factor(Eta, levels = c(1, 2, 3, 4, 5, 6), 
                      labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")))


  dati_modificati(dati_uniti)
  dati_analisi_final(dati_uniti)

  remove_modal_spinner()


  
  output$summary_text_agg <- renderUI({
  	req(dati_modificati())
    num_soggetti <- nrow(dati_modificati())  
    tags$p(
    paste("Sono stati analizzati", num_soggetti, "soggetti. Di seguito alcune statistiche relative all'indicatore di fragilità:"),
    style = "color: #006699; font-size: 13px; font-weight: bold; margin-bottom: 10px;"
  )
  })
  
 output$box_summary_table_agg <- renderUI({
  req(dati_modificati())

  div(
    style = "display: flex; justify-content: center; background-color:#f0f8ff; padding:0px; margin: 0px; border-radius:8px;
             border:1px solid #f0f8ff; color:#16668c; font-size: 12px; line-height: 1.2;",
     tableOutput("summary_table_agg")
  )
})
  
  
    # Creazione della tabella riassuntiva
  output$summary_table_agg <- renderTable({
  	req(dati_modificati())  
    dati_analisi <- dati_modificati()
		stats <- dati_analisi %>%
		    summarise(
		      Media = round(mean(Indicatore, na.rm = TRUE), 3),
		      Deviazione_Standard = round(sd(Indicatore, na.rm = TRUE), 3),
		      Q1 = round(quantile(Indicatore, 0.25, na.rm = TRUE), 3),
		      Mediana = round(quantile(Indicatore, 0.5, na.rm = TRUE), 3),
		      Q3 = round(quantile(Indicatore, 0.75, na.rm = TRUE), 3)
		    )
		  
	
		  colnames(stats) <- c("Media", "Deviazione Std", "1° Quartile", "Mediana", "3° Quartile")
		  stats
		}, 
		striped = TRUE, 
		bordered = FALSE, 
		hover = TRUE, 
		spacing = "xs",
  	digits = 3)
  
  # Creazione dell'istogramma
output$histogram_agg <- renderPlot({
  req(dati_modificati())	
  dati_hist <- dati_modificati()

  valore_indicatore_soggetto <- dati_hist$Indicatore[1]
  valtxt <- sprintf("%.3f", valore_indicatore_soggetto)

  ggplot(
    dati_hist,
    aes(
      x = Indicatore,
      y = after_stat(count / sum(count) * 100)
    )
  ) +
    geom_histogram(
      binwidth = 0.05,
      fill = "#16668c",
      color = "white"
    ) +
    
    # Linea rossa
    geom_vline(
      xintercept = valore_indicatore_soggetto,
      color = "red",
      linetype = "solid",
      linewidth = 1.2
    ) +
    
    annotate(
      "text",
      x = valore_indicatore_soggetto + 0.05,
      y = Inf,
      label = "Posizione nuovo soggetto",
      color = "red",
      size = 4,
      hjust = 0,
      vjust = 2
    ) +
    
    annotate(
      "label",
      x = valore_indicatore_soggetto,
      y = Inf,
      label = valtxt,
      color = "red",
      fill = "white",
      size = 4,
      hjust = 0.5,
      vjust = 1.5
    ) +

    scale_x_continuous(
		  limits = c(0, 1),
		  expand = expansion(mult = c(0, 0.05))
		) +
    
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    
    labs(
      title = "Distribuzione dell'Indicatore",
      x = "Indicatore di Fragilità",
      y = "Frequenza (%)"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, color = "#044463"),
      axis.text.x = element_text(size = 12, color = "#044463"),
      axis.title = element_text(size = 14, color = "#044463"),
      plot.title = element_text(size = 14, color = "#044463", hjust = 0.5)
    )
})
  
  # Creazione del grafico di variabilità per classi di età
  output$variability_plot_agg <- renderPlot({
  	req(dati_modificati())
  	shinyjs::enable("scarica_report")
  	dati_plot <- dati_modificati()
    dati_eta_stats <- dati_plot %>%
      group_by(Eta) %>%
      summarise(
        Quantile_25 = quantile(Indicatore, 0.25, na.rm = TRUE),
        Quantile_50 = quantile(Indicatore, 0.5, na.rm = TRUE),
        Quantile_75 = quantile(Indicatore, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(dati_eta_stats, aes(y = Eta)) +
      geom_segment(aes(x = Quantile_25, xend = Quantile_75, yend = Eta), 
                   color = "#16668c", size = 2) +
      geom_point(aes(x = Quantile_50), color = "red", size = 3) +
   geom_text(aes(x = Quantile_25, label = round(Quantile_25, 3)),
              hjust = 1.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    geom_text(aes(x = Quantile_50, label = round(Quantile_50, 3)),
              vjust = -1, size = 4, color = "#e60000") +
    geom_text(aes(x = Quantile_75, label = round(Quantile_75, 3)),
              hjust = -0.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    	scale_x_continuous(
      limits = c(0, 1),
      #expand = c(0, 0)  # elimina margine ai bordi
    ) +
      labs(title = "Mediana e Scarto Interquartile dell'Indicatore per classi di Età",
           x = "Indicatore di Fragilità",
           y = "Classe d'età") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12, color = "#16668c"),
        axis.text.x = element_text(size = 12, color = "#16668c"),
        axis.title = element_text(size = 14, color = "#16668c"),
        plot.title = element_text(size = 14, color = "#16668c", hjust = 0.5)
      )
  })
  
output$scarica_report_agg <- downloadHandler(
  filename = function() paste0("analisi-fragilita-", Sys.Date(), ".pdf"),
  contentType = "application/pdf",
  content = function(file) {

    req(dati_modificati())

    dati <- dati_modificati()
    valore_indicatore_soggetto <- dati$Indicatore[1]

    ## ---------- 1. TABELLA (INVARIATA) ----------
    stats <- dati %>% summarise(
      Media   = round(mean(Indicatore, na.rm = TRUE), 3),
      DevStd  = round(sd(Indicatore,   na.rm = TRUE), 3),
      Q1      = round(quantile(Indicatore, .25, na.rm = TRUE), 3),
      Mediana = round(quantile(Indicatore, .50, na.rm = TRUE), 3),
      Q3      = round(quantile(Indicatore, .75, na.rm = TRUE), 3)
    )

    tbl <- tableGrob(stats, rows = NULL,
                     theme = ttheme_minimal(base_size = 11))

    ## ---------- 2. ISTOGRAMMA (INVARIATO) ----------
    p1 <- ggplot(dati, aes(Indicatore)) +
      geom_histogram(binwidth = .05, fill = "#16668c", colour = "white") +
      geom_vline(xintercept = valore_indicatore_soggetto,
                 colour = "red", size = 1.2) +
      annotate("text",
               x = valore_indicatore_soggetto + .05,
               y = Inf,
               label = "Posizione nuovo soggetto",
               colour = "red", size = 4,
               hjust = 0, vjust = 2) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      labs(title = "Distribuzione Indicatore", x = NULL, y = "Frequenza") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = .5))

    ## ---------- 3. ETA (INVARIATO) ----------
    dati_eta <- dati %>%
      group_by(Eta) %>%
      summarise(
        q25 = quantile(Indicatore, .25, na.rm = TRUE),
        q50 = quantile(Indicatore, .50, na.rm = TRUE),
        q75 = quantile(Indicatore, .75, na.rm = TRUE),
        .groups = "drop"
      )

    p2 <- ggplot(dati_eta, aes(y = factor(Eta), x = q50)) +
      geom_errorbar(aes(xmin = q25, xmax = q75),
                    width = .3, colour = "#16668c") +
      geom_point(colour = "red", size = 2.5) +
      geom_text(aes(x = q25, label = round(q25, 3)),
                hjust = 1.2, size = 3.5, color = "#16668c") +
      geom_text(aes(x = q50, label = round(q50, 3)),
                vjust = -1, size = 4, color = "#e60000") +
      geom_text(aes(x = q75, label = round(q75, 3)),
                hjust = -0.2, size = 3.5, color = "#16668c") +
      scale_x_continuous(limits = c(0, 1)) +
      labs(title = "Mediana e Scarto Interquartile dell'indicatore per classi di Età",
           x = NULL, y = "Classe d’età") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = .5))

    ## ---------- HEADER (COPIATO DAL SECONDO REPORT) ----------
    header <- arrangeGrob(
      logo_grob,
      textGrob(
        "Analisi Indicatore di fragilità",
        gp = gpar(fontsize = 14, fontface = "bold"),
        hjust = 0
      ),
      ncol = 2,
      widths = c(2, 5)
    )

    ## ---------- CONTENT (INVARIATO) ----------
    content <- arrangeGrob(
      tbl,
      textGrob("", gp = gpar(fontsize = 2)),
      p1,
      textGrob("", gp = gpar(fontsize = 2)),
      p2,
      ncol = 1,
      heights = c(1, 0.1, 2, 0.1, 2)
    )

    ## ---------- FOOTER (COPIATO DAL SECONDO REPORT) ----------
    footer_band <- arrangeGrob(footer_grob)

    caption_text <- "Progetto finanziato dal Piano Nazionale di Ripresa e Resilienza - Missione 4, \"Istruzione e Ricerca\" - Componente 2, \"Dalla ricerca all'impresa\" - Linea di investimento 1.3 - Avviso Pubblico D.D. n.341/2022 Ministero dell'Università e della Ricerca"

    caption <- textGrob(
      paste(strwrap(caption_text, width = 130), collapse = "\n"),
      gp = gpar(fontsize = 8.5, col = "grey30", lineheight = 1.05),
      just = "center"
    )

    footer_block <- arrangeGrob(
      footer_band,
      caption,
      ncol = 1,
      heights = unit.c(
        unit(1.2, "cm"),
        unit(1, "null")
      )
    )

    ## ---------- PAGINA FINALE ----------
    final_page <- arrangeGrob(
      header,
      content,
      footer_block,
      ncol = 1,
      heights = unit.c(
        unit(2.5, "cm"),
        unit(1, "null"),
        unit(2.2, "cm")
      )
    )

    ## ---------- EXPORT ----------
    pdf(file, width = 8.27, height = 11.69, onefile = FALSE)
    grid.newpage()
    grid.draw(final_page)
    dev.off()
  }
)
  

  
  stato_pagina("risultato")  # Cambia lo stato della pagina
	removeModal()  
})

observeEvent(input$reset_ui, {
  stato_pagina("inizio")
})


# flag per mostrare/nascondere il fileInput
show_upload <- reactiveVal(NULL)

output$contenuto_analisi <- renderUI({
  req(dati_analisi_final())          # serve che i dati di base esistano

  # blocco SINISTRA (statico)
  col_sx <- column(
    3,
    div(
      style = "margin-left:20px; background-color:#dadfe3;
               padding:15px; border-radius:8px; border:1px solid #dadfe3;",
      tags$small(
        "Se non lo si è fatto prima è possibile caricare un file aggiuntivo ",
        "contenente le variabili di interesse. ",
        "Assicurarsi che sia presente la variabile ", tags$b("ID"), ".",
      	"         ",
        style = "color:#16668c;"
      ),
      br(),
      div(
        style = "display:flex; gap:8px;",
        actionButton("btn_carica", "Carica file integrativo",
                     class = "btn btn-primary btn-sm"),
        actionButton("btn_usa_precedente", "Usa il file precedente",
                     class = "btn btn-primary btn-sm")
      ),
      br(), br(),
      uiOutput("upload_ui"),
      uiOutput("selezione_variabile_ui")
    )
  )


  # ---- se la variabile è selezionata, aggiungi tabella e grafico ----
  col_tab <- column(
    4,
    div(
      style = "background-color:#f0f8ff; padding:15px; border-radius:8px;
               border:1px solid #f0f8ff; color:#16668c; font-size: 14px;",
      h5("Distribuzione della variabile selezionata",
         style = "color:#16668c; margin-bottom:0px; margin-top:0px;"),
      tableOutput("tabella_analisi")
    )
  )

  col_plot <- column(
    5,
    div(
      style = "background-color:#f0f8ff; padding:15px; border-radius:8px;
               border:1px solid #f0f8ff;",
      plotOutput("grafico_analisi", height = "320px")
    )
  )

  
   # se la variabile NON è ancora selezionata, mostra solo colonna sinistra
 if (!nzchar(variabile_scelta())) {   # ← usa variabile_scelta(), non input
  fluidRow(col_sx)
		} else {
		  fluidRow(col_sx, col_tab, col_plot)
		}
  
})


observeEvent(input$btn_usa_precedente, {
  show_upload(FALSE)
  scelta_effettuata(TRUE)
})

observeEvent(input$btn_carica, {
  show_upload(TRUE)
  scelta_effettuata(TRUE)
})

observeEvent(input$variabile_analisi, {
  if (!is.null(input$variabile_analisi) && input$variabile_analisi != "") {
    variabile_scelta(input$variabile_analisi)
  }
})


# UI del fileInput condizionato
output$upload_ui <- renderUI({
  if (isTRUE(show_upload())) {
    fileInput(
      "csv_variabili", NULL,
      buttonLabel = "Seleziona file", placeholder = "Nessun file caricato",
      accept = ".csv", width = "100%"
    )
  }
})


dati_nuovi <- reactive({
  req(show_upload())
  req(input$csv_variabili)

  df <- tryCatch(
    read.csv(input$csv_variabili$datapath, stringsAsFactors = FALSE),
    error = function(e) {
      showModal(modalDialog(
        title = "Errore",
        "Errore nella lettura del file CSV.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
  )

  if (is.null(df) || !"ID" %in% names(df)) {
    showModal(modalDialog(
      title = "Errore",
      "Il file integrativo deve contenere la variabile ID.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)}
  	
  	
 sesso_col <- names(df)[tolower(names(df)) == "sesso"]

  if (length(sesso_col) > 0) {
    valori_validi <- c("M", "F")

    valori_presenti <- unique(df[[sesso_col]])

    if (any(!valori_presenti %in% valori_validi)) {
      showModal(modalDialog(
        title = "Errore",
        "La variabile 'sesso' deve contenere solo valori 'M' o 'F'.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
  }
  df
})


observeEvent(dati_nuovi(), ignoreNULL = TRUE, {
  extra <- dati_nuovi()
  base  <- dati_analisi_final()

  if (!"ID" %in% names(base)) {
    showModal(modalDialog(
      title = "Errore",
      "I dati di base non contengono la variabile ID.",
      easyClose = TRUE,
      footer = NULL
    ))
    return()
  }

  extra_vars <- setdiff(names(extra), names(base))
  extra_keep <- c("ID", extra_vars)

  if (length(extra_vars) == 0) {
    showModal(modalDialog(
      title = "Attenzione",
      "Il file non contiene variabili aggiuntive rispetto ai dati esistenti.",
      easyClose = TRUE,
      footer = NULL
    ))
    return()
  }

  # join e sovrascrittura
  nuovo_dataset <- dplyr::inner_join(base, extra[extra_keep], by = "ID")
  
   # calcolo righe unite
  righe_merge <- nrow(nuovo_dataset)
  righe_base  <- nrow(base)
  
  dati_analisi_final(nuovo_dataset)      #  <<—  sovrascrive
  
  showModal(modalDialog(
  title = "Operazione completata",
      div(
      style = "font-size: 16px; color: #16668c; margin-top: 10px;",
      paste0("Il file integrativo è stato unito correttamente ai dati esistenti."),
      tags$br(),
      paste0("Righe unite: ", righe_merge, " su ", righe_base, " righe iniziali.")
    ),
  easyClose = TRUE,
  footer = modalButton("Chiudi"),
  size = "m"
))
})



output$tabella_analisi <- renderTable({
	req(dati_analisi_final())
	req(nzchar(variabile_scelta()))
	var <- variabile_scelta()

  df <- dati_analisi_final()
  
n_livelli <- df %>% filter(!is.na(.data[[var]])) %>% pull(var) %>% unique() %>% length()

tabella <- df %>%
  filter(!is.na(.data[[var]])) %>%
  group_by(.data[[var]]) %>%
  summarise(
    Frequenza = n(),
    Media_Indicatore = mean(Indicatore, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Trasforma in carattere
    Modalità = as.character(.data[[var]]),

    # Rinomina in base al tipo di variabile
			Modalità = case_when(
			  var == "Ricoveri1_2" & Modalità == "0" ~ "0",
			  var == "Ricoveri1_2" & Modalità == "1" ~ "1 o 2",
			  var == "Ricoveri1_2" & Modalità == "2" ~ "3 o più",
			  
			  var == "Sesso" & Modalità == "M" ~ "Maschio",
			  var == "Sesso" & Modalità == "F" ~ "Femmina",
			  
			  var != "Ricoveri1_2" & Modalità %in% c("0","1") & n_livelli == 2 ~ 
			    if_else(Modalità == "0", "assente", "presente"),
			  
			  TRUE ~ Modalità
			),

    Percentuale = paste0(round(100 * Frequenza / sum(Frequenza), 3), "%"),

    # Mantieni sempre 3 decimali
    Media_Indicatore = sprintf("%.3f", Media_Indicatore)
  ) %>%
  select(Modalità, Frequenza, Percentuale, Media_Indicatore)

 tabella
})




output$grafico_analisi <- renderPlot({
  req(dati_analisi_final())
  req(nzchar(variabile_scelta()))
  var_sel <- variabile_scelta()
  
  # Trova il nome leggibile, se esiste
  var_label <- if (var_sel %in% names(label_variabili)) label_variabili[[var_sel]] else var_sel
  
  n_livelli <- dati_analisi_final() %>%
  filter(!is.na(.data[[var_sel]])) %>%
  pull(var_sel) %>%
  unique() %>%
  length()

  dati_plot <- dati_analisi_final() %>%
    filter(!is.na(.data[[var_sel]])) %>%
    mutate(VarRinominata = as.character(.data[[var_sel]]),

      # applicazione della rinominazione su tre livelli solo se è 'Ricoveri1_2'
			VarRinominata = case_when(
			  var_sel == "Ricoveri1_2" & VarRinominata == "0" ~ "0",
			  var_sel == "Ricoveri1_2" & VarRinominata == "1" ~ "1 o 2",
			  var_sel == "Ricoveri1_2" & VarRinominata == "2" ~ "3 o più",
			  
			  var_sel == "Sesso" & VarRinominata == "M" ~ "Maschio",
			  var_sel == "Sesso" & VarRinominata == "F" ~ "Femmina",
			  
			  # variabili binarie 0/1 (escluso 'Ricoveri1_2')
			  var_sel != "Ricoveri1_2" & VarRinominata %in% c("0", "1") & n_livelli == 2 ~ 
			    if_else(VarRinominata == "0", "assente", "presente"),
			  
			  TRUE ~ VarRinominata
			)
    )

  dati_stats <- dati_plot %>%
    group_by(VarRinominata) %>%
    summarise(
      Quantile_25 = quantile(Indicatore, 0.25, na.rm = TRUE),
      Quantile_50 = quantile(Indicatore, 0.5,  na.rm = TRUE),
      Quantile_75 = quantile(Indicatore, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(Variabile = VarRinominata)

  p<-ggplot(dati_stats, aes(y = Variabile)) +
    geom_segment(aes(x = Quantile_25, xend = Quantile_75, yend = Variabile),
                 color = "#16668c", size = 2) +
    geom_point(aes(x = Quantile_50), color = "#e60000", size = 3) +
    geom_text(aes(x = Quantile_25, label = round(Quantile_25, 3)),
              hjust = 1.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    geom_text(aes(x = Quantile_50, label = round(Quantile_50, 3)),
              vjust = -1, size = 4, color = "#e60000") +
    geom_text(aes(x = Quantile_75, label = round(Quantile_75, 3)),
              hjust = -0.2, vjust = 0.5, size = 3.5, color = "#16668c") +
    scale_x_continuous(limits = c(0, 1)) +
    labs(
      title = paste("Mediana e Scarto Interquartile dell'Indicatore per", var_label),
      x = "Indicatore",
      y = var_label,
      caption = "Barra: Q1–Q3 | Punto rosso: Mediana"
    ) +
    theme_minimal(base_family = "Helvetica") +
    theme(
      panel.background = element_rect(fill = "#f0f8ff", color = NA),
      plot.background  = element_rect(fill = "#f0f8ff", color = NA),
      axis.text.y      = element_text(size = 12, color = "#16668c"),
      axis.text.x      = element_text(size = 12, color = "#16668c"),
      axis.title       = element_text(size = 14, color = "#16668c", face = "bold"),
      plot.title       = element_text(size = 14, color = "#16668c", hjust = 0.5, face = "bold"),
      plot.caption     = element_text(size = 12, color = "#16668c", hjust = 0.5),
      axis.line.x      = element_line(color = "#8cbbd1", size = 1)
    )
  
 p
})

output$selezione_variabile_ui <- renderUI({

  ## parte solo dopo che l’utente ha cliccato uno dei bottoni
  req(scelta_effettuata())

  df <- dati_analisi_final()
  req(!is.null(df) && nrow(df) > 0)
  
  exclude_vars <- c(
  "ID", "Indicatore"
  )

  candidate_vars <- setdiff(names(df), exclude_vars)

  filtered_vars <- candidate_vars[
    sapply(df[candidate_vars], function(col)
      length(unique(na.omit(col))) <= 6)
  ]
  
  choices <- setNames(filtered_vars, sapply(filtered_vars, function(var) {
    if (var %in% names(label_variabili)) {
      label_variabili[[var]]
    } else {
      var
    }
  }))

selected_var <- isolate(variabile_scelta())

selectInput(
  inputId  = "variabile_analisi",
  label    = "Selezionare la variabile da analizzare",
  choices  = c("— seleziona una variabile —" = "", choices),
  selected = variabile_scelta(),
  width    = "100%"
)
})
  
}

#avvio app
 
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)
