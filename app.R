# load in packages
library(shiny)
library(dplyr)
library(shinycustomloader)

# load in ds packages
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(lfltmagic)
library(hotr)
library(homodatum)
library(hgchmagic)

frtypes_doc_viz <- suppressWarnings(yaml::read_yaml("conf/frtypes_viz.yaml"))
frtypes_doc_map <- suppressWarnings(yaml::read_yaml("conf/frtypes_map.yaml"))

# load data
df <- readRDS("data/covid_mobility_actions.RDS") %>%
  mutate(Country = Gnm(Country))


# Define UI for data download app ----
ui <- panelsPage(panel(title = "Choose data",
                       width = 200,
                       color = "chardonnay",
                       body = div(
                         div(
                           uiOutput("choose_data"),
                           uiOutput("select_var")
                         )
                       )),
                 panel(title = "Visualise data",
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = withLoader(uiOutput("viz"),
                                         type = "image",
                                         loader = "img/loading_fucsia.gif"),
                       footer = uiOutput("viz_icons")))


server <- function(input, output, session) {

  output$viz <- renderUI({
    if(input$dataset == "dat_viz"){
      highchartOutput("view_hgch_viz", height = 500)
    } else {
      leafletOutput("map_lflt")
    }
  })

  output$choose_data <- renderUI({
    selectInput("dataset", "Choose dataset to visualize:",
                selected = "dat_viz",
                c("COVID mobility actions" = "dat_viz",
                  "Map - COVID mobility actions by country" = "dat_map"))
  })

  inputData <- reactive({
    req(input$dataset)
    if(input$dataset == "dat_viz"){
      df %>% select(-Country)
    } else {
      df %>% rename(`Actions total` = Country)
    }
  })

  # Vista de datos ----------------------------------------------------------

  data_fringe <- reactive({
    fringe(inputData())
  })

  data_load <- reactive({
    data_fringe()$data
  })

  dic_load <- reactive({
    data_fringe()$dic
  })


  output$select_var <- renderUI({
    available_fTypes <- names(frtypes_doc_viz)
    data_ftype <- data_fringe()$frtype
    if(is.null(dic_load)) return()

    if (data_ftype %in% available_fTypes) {
      data_order <- dic_load()$id
    } else if (grepl("Cat|Yea|Dat",data_ftype)&grepl("Num",data_ftype)){
      data_order <- c(dic_load()$id[grep("Cat|Yea|Dat", dic_load()$hdType)[1]],
                      dic_load()$id[grep("Num", dic_load()$hdType)[1]])
    } else {
      data_order <- dic_load()$id[grep("Cat|Num", dic_load()$hdType)[1]]
    }

    list_var <- dic_load()$id
    if (is.null(list_var)) return()
    names(list_var) <- dic_load()$label[match(list_var, dic_load()$id)]

    if(input$dataset == "dat_viz"){
      selectizeInput("var_order",
                     div(class="title-data-select","Select up to two variables:"),
                     choices = list_var,
                     multiple = TRUE,
                     options = list(maxItems = 2,
                                    plugins= list('remove_button', 'drag_drop')))
    } else {
      selectInput("var_order",
                     div(class="title-data-select","Select a variable to show on the map:"),
                     choices = list_var)
    }
  })

  # Preparación data para graficar ------------------------------------------

  data_draw <- reactive({
    var_select <- input$var_order
    if (is.null(var_select)) return()
    d <- data_load()[var_select]
    names(d) <- dic_draw()$label
    d
  })

  dic_draw <- reactive({
    var_select <- input$var_order
    if (is.null(var_select)) return()
    dic_load() %>% filter(id %in% var_select)
  })

  ftype_draw <- reactive({
    req(input$dataset)
    if(input$dataset == "dat_viz"){
      if (is.null(dic_draw())) return()
      paste0(dic_draw()$hdType, collapse = "-")
    } else {
      if (is.null(dic_draw())) {
        x <- "Gnm"
      } else if (dic_draw()$label == "Actions total"){
        x <- "Gnm"
      } else {
        x <- paste0(dic_draw()$hdType, collapse = "-")
        if (x == "Num") {
          x <- "GnmNum"
        } else if (x == "Cat"){
          x <- "GnmCat"
        }
      }
      x
    }

  })

  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    if(input$dataset == "dat_viz"){
      frtypes_doc_viz[[ftype_draw()]]
    } else {
      frtypes_doc_map[[ftype_draw()]]
    }
  })

  actual_but <- reactiveValues(active_viz = 'bar', active_map = 'choropleth')

  observe({
    viz_rec <- possible_viz()
    if (is.null(viz_rec)) return()
    if (is.null(input$viz_selection)) return()
    if (!( input$viz_selection %in% viz_rec)) {
      if(input$dataset == "dat_viz"){
        actual_but$active_viz <- viz_rec[1]
      } else {
        actual_but$active_map <- viz_rec[1]
      }
    } else {
      if(input$dataset == "dat_viz"){
        actual_but$active_viz <- input$viz_selection
      } else {
        actual_but$active_map <- input$viz_selection
      }
    }
  })

  output$viz_icons <- renderUI({
    if(input$dataset == "dat_viz"){
      path <- 'img/svg/viz/'
      active <- actual_but$active_viz
    } else {
      path <- 'img/svg/map/'
      active <- actual_but$active_map
    }
    buttonImageInput('viz_selection',
                     "Viz type",
                     images = possible_viz(),
                     path = path,
                     format = 'svg',
                     active = active)
  })

  # Renderizar highchart plot -----------------------------------------------

  viz_name <- reactive({
    if (is.null(ftype_draw())) return()
    if (ftype_draw() == "") return()
    ctype <- gsub("-", "", ftype_draw())
    gtype <- actual_but$active_viz
    if (is.null(gtype)) return()
    typeV <- paste0('hgch_', gtype, '_', ctype)
    typeV
  })

  hgch_viz <- reactive({
    if (is.null(viz_name())) return()
    # viz <- do.call(viz_name(), c(list(data = data_draw(),
    #                                   opts_viz(), theme = theme_draw()
    #
    # )))
    viz <- do.call(viz_name(), c(list(data = data_draw()

    )))
    viz
  })

  output$view_hgch_viz <- renderHighchart({
    viz <- hgch_viz()
    if (is.null(viz)) return()
    suppressWarnings(
      viz
    )
  })

  lftl_viz <- reactive({
    # browser()
    geotype <- gsub("-", "", ftype_draw())
    print(geotype)
    viz <- paste0("lflt_", actual_but$active_map, "_", geotype)
    # opts <- c(opts_viz(), theme_draw())
    data <- data_draw()
    if(is.null(data) | dic_draw()$label == "Actions total"){
      data <- df %>% select(Country)
    } else {
      data <- cbind(df %>% select(Country), data)
    }
    do.call(viz, c(list(data = data
    ))
    )
  })

  output$map_lflt <- renderLeaflet({
    lftl_viz()
  })

  output$download <- renderUI({
    downloadImageUI("download_plot", dropdownLabel = "Download plot", formats = c("html","jpeg", "pdf", "png", "link"), display = "dropdown")
  })


  callModule(downloadImage, "download_plot", graph = hgch_viz(),
             lib = "highcharter", formats = c("html","jpeg", "pdf", "png", "link"))

}


shinyApp(ui, server)
