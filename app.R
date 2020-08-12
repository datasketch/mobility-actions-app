# load in packages
library(shiny)
library(dplyr)
library(shinycustomloader)
library(rgdal)

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

# custom css styles
styles <- "
#panel_data {
    border-top: 2px solid #276151 !important;
}

#panel_data .panel-header-title.text-malibu
{
 color: #276151;
}

#panel_data .panel-header-dismiss {
 color: #276151;
}

#panel_viz {
  border-top: 2px solid #293845  !important;
}

#panel_viz .panel-header-title.text-malibu
{
 color: #293845;
}

.control-label {
 color: #df5c33;
}

.shiny-html-output.shiny-bound-output {
 color: #df5c33;
}

.buttons-group .button-style.active-btn {
 background-color: #df5c33 !important;
}

.buttons-group .button-style.active-btn:hover {
 background-color: #c14c2d !important;
}

.buttons-group .button-style {
 background-color: #bdcad1 !important;
}

.buttons-group .button-style:hover {
 background-color: #8097a4 !important;
}

.dropdown-action-trigger {
 background: #df5c33;
}

.dropdown-action-trigger:hover {
 background-color:  #c14c2d
}
"
# #view_hgch_viz {
# height: 600px !important;
# }
#
# .highcharts-plot-background {
#   height: 400px !important;
# }

# load data
df <- readRDS("data/covid_mobility_actions.RDS")
sources <- "Sources: Mobility Actions Database. Combs, T. Streetplans. NUMO Mobility Works. Full citation details at bit.ly/mobility-actions"
caption <- paste0("<p style='font-family:Ubuntu;color:#293845;font-size:12px;'>",sources,"</p>")

# Define UI for data download app ----
ui <- panelsPage(styles = styles,
                 panel(id = "panel_data",
                       title = "Choose data",
                       width = 200,
                       body = div(
                         div(
                           uiOutput("choose_data"),
                           uiOutput("select_var"),
                           uiOutput("select_category")
                         )
                       )),
                 panel(id = "panel_viz",
                       title = "Visualize data",
                       title_plugin = uiOutput("download"),
                       can_collapse = FALSE,
                       # body = withLoader(uiOutput("viz"),
                       #                   type = "image",
                       #                   loader = "img/loading_fucsia.gif"),
                       body = uiOutput("viz"),
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
    selectInput("dataset", "Choose visualization type:",
                selected = "dat_map",
                c("Explorative visualization" = "dat_viz",
                  "Actions by country" = "dat_map",
                  "Actions by US state" = "dat_map_us"))
  })

  inputData <- reactive({
    req(input$dataset)
    if(input$dataset == "dat_viz"){
      df %>% select(-Country, -Country.code, -Country.region)
    } else if (input$dataset == "dat_map"){
      df %>% rename(`Actions total` = Country.code) %>%
        select(-Country, -Country.region, -`Date started`, -`Date announced`, -`Week started`, -`Week announced`, -`World region`)
    } else {
      df %>% filter(Country.code == "USA") %>% rename(`Actions total` = Country.region) %>%
        select(-Country, -Country.code, -`Date started`, -`Date announced`, -`Week started`, -`Week announced`, -`World region`)
    }
  })

  output$select_category <- renderUI({
    if (input$dataset == "dat_viz" | dic_draw()$label == "Actions total") return ()
    selectInput("selected_cat",
                div(class="title-data-select","Select category:"),
                choices = data_draw() %>% pull() %>% unique())
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
                     selected = "action_types",
                     multiple = TRUE,
                     options = list(maxItems = 2,
                                    plugins= list('remove_button', 'drag_drop')))
    } else {
      selectInput("var_order",
                     div(class="title-data-select","Select variable:"),
                     choices = list_var)
    }
  })

  # Preparación data para graficar ------------------------------------------

  data_draw <- reactive({
    var_select <- input$var_order
    dic <- dic_draw()
    if (is.null(var_select)) return()
    d <- data_load()[var_select]
    names(d) <- dic$label
    d
  })

  dic_draw <- reactive({
    var_select <- input$var_order
    if (is.null(var_select)) return()
    dic_load()[match(input$var_order, dic_load()$id),]
  })

  ftype_draw <- reactive({
    req(input$dataset)
    if(input$dataset == "dat_viz"){
      if (is.null(dic_draw())) return()
      paste0(dic_draw()$hdType, collapse = "-")
    } else {
      if (input$dataset == "dat_map"){
        x <- "GcdNum"
      } else {
        x <- "GnmNum"
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
                     "Visualization type",
                     images = possible_viz(),
                     checkmarkColor = "orange",
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
    # browser()
    data <- data_draw()
    viz_name <- viz_name()
    dataLabels_show <- FALSE
    if(actual_but$active_viz %in% c("treemap", "bubbles")){
      dataLabels_show <- TRUE
    }
    opts <- dsvizopts::merge_dsviz_options(palette_colors = c("#3A3766", "#5964C6", "#B956A6", "#DF5C33", "#FCBB1C", "#F8DEAC", "#9DE2C5", "276151"),
                                           na_color = "#EAEAEA",
                                           caption = caption,
                                           legend_position = "top",
                                           dataLabels_show = dataLabels_show)
    viz <- do.call(viz_name, c(list(data = data, opts = opts
    )))
    if(nrow(dic_draw()) == 2){
      viz <- viz %>% hc_legend(verticalAlign = "top")
    }
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
    geotype <- gsub("-", "", ftype_draw())
    print(geotype)
    viz <- paste0("lflt_", actual_but$active_map, "_", geotype)
    # opts <- c(opts_viz(), theme_draw())
    data <- data_draw()

    palette_colors <- c("#FFDD65", "#F9BE58", "#F29F4B", "#E97F3F", "#DF5C33")
    if(actual_but$active_map == "bubbles"){
      palette_colors <- "#df5c33"
    }

    if(is.null(data) | dic_draw()$label == "Actions total"){
      if (input$dataset == "dat_map_us"){
        data <- df %>% filter(Country.code == "USA") %>% group_by(Country.region) %>% summarise(Actions = n()) %>% select(Country.region, Actions)
        tooltip <- "<b>State:</b> {Country.region}<br/><b>Actions:</b> {Actions}"
        map_name <- "usa_states"
      } else if (input$dataset == "dat_map"){
        data <- df %>% group_by(Country.code, Country) %>% summarise(Actions = n()) %>% select(Country.code, Actions, Country)
        tooltip <- "<b>Country:</b> {Country}<br/><b>Actions:</b> {Actions}"
        map_name <- "world_countries"
      }
    } else {
      req(input$selected_cat)
      if (input$dataset == "dat_map_us"){
        data <- cbind(df %>% filter(Country.code == "USA") %>% select(Country.region), data) %>%
          filter(.data[[dic_draw()$label]] == input$selected_cat) %>%
          group_by(Country.region) %>% summarise(Count = n()) %>% select(Country.region, Count)
        tooltip <- "<b>State:</b> {Country.region}<br/><b>Count:</b> {Count}"
        map_name <- "usa_states"
      } else if (input$dataset == "dat_map"){
        data <- cbind(df %>% select(Country.code, Country), data) %>% filter(.data[[dic_draw()$label]] == input$selected_cat) %>%
          group_by(Country.code, Country) %>% summarise(Count = n()) %>% select(Country.code, Count, Country)
        tooltip <- "<b>Country:</b> {Country}<br/><b>Count:</b> {Count}"
        map_name <- "world_countries"
      }
    }
    opts <- dsvizopts::merge_dsviz_options(map_name = map_name,
                                           tooltip = tooltip,
                                           palette_colors = palette_colors,
                                           na_color = "#EAEAEA",
                                           caption = caption,
                                           border_weight = 0.75)
    do.call(viz, c(list(data = data, opts = opts
    ))
    )
  })

  output$map_lflt <- renderLeaflet({
    lftl_viz()
  })

  output$download <- renderUI({
      downloadImageUI("download_plot", dropdownLabel = "Download", formats = c("html","jpeg", "pdf", "png"), display = "dropdown")
  })

  download_opts <- reactive({
    if(input$dataset == "dat_viz"){
      hgch_viz()
    } else {
      lftl_viz()
    }
  })

  callModule(downloadImage, "download_plot", graph = download_opts(),
             lib = "highcharter", formats = c("html","jpeg", "pdf", "png"))

}


shinyApp(ui, server)
