# load in packages
library(shiny)
library(dplyr)
library(shinycustomloader)
library(rgdal)
library(googlesheets4)

# load in ds packages
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(lfltmagic)
library(hotr)
library(homodatum)
library(hgchmagic)

webshot::install_phantomjs()
gs4_deauth()

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

h4 {
    color: #df5c33;
}

.leaflet-top, .leaflet-bottom {
    z-index: 1 !important;
}

.control-label {
 color: #df5c33;
 font-weight: 700;
}

.title-data-select {
 color: #df5c33 !important;
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

.button-checkmark {
 display: none;
}

.buttons-group .button-style {
 background-color: #bdcad1 !important;
}

.buttons-group .button-style:hover {
 background-color: #8097a4 !important;
}

.buttons-group .button-container {
 height: 45px;
}

.dropdown-action-trigger {
 background: #df5c33;
}

.dropdown-action-trigger:hover {
 background-color:  #c14c2d;
}

#viewAllData.btn {
 background: #df5c33;
 margin-bottom: 10px;
}

#viewAllData.btn:hover {
 background: #c14c2d;
}

#downloadCSV.btn {
 background: #df5c33;
}

#downloadCSV.btn:hover {
 background: #c14c2d;
}

"


### load data -----------------------------------------------------------------------------------

# link to googlesheet
# data_link <- "https://docs.google.com/spreadsheets/d/1_rJoqel3UkhQ2j7H_CwDTo7-8Uom1Vv26lKVWo_1O6k/edit?ts=5f47b671#gid=789320453"
data_link <- "https://docs.google.com/spreadsheets/d/1_rJoqel3UkhQ2j7H_CwDTo7-8Uom1Vv26lKVWo_1O6k/edit?ts=5f47b671#gid=1972370104"

# load dataset
df_read <- read_sheet(data_link, sheet = "All_data_Jan2021")

# load data dictionary
df_read_dic <- read_sheet(data_link, sheet = "variable-dictionary")

## load lookup tables
# iso2 iso3 lookup
lookup <- read.csv("data/lookup_iso2_iso3.csv")
# us states lookup
lookup_us_states <- read.csv("data/usa-states.csv") %>%
  select(Country.region = name, us_state_id = id)
# cities lookup
df_cities_complete <- readRDS("data/clean_data_with_cities_long_lat.RDS")


### format data -----------------------------------------------------------------------------------

plotting_vars <- c("country", "state", "type", "region",
                   # "statusml",
                   "space", "time", "intensity", "scale", "trigger",
                   "selection", "infrastructure", "implementation",
                   "policy", "strategy", "perception",
                   "purpose", "longevity", "sector", "modetype", "mode", "approach",
                   "dateann", "dateimp", "city")

extra_dic <- data.frame(Varlabel = c("countrycode", "weekann", "weekimp", "worldcountry", "lon", "lat"),
                        Varname = c("Country.code", "Week.announced", "Week.started", "World country", "lon", "lat"),
                        Description = c("", "Week action was announced or first mentioned in media", "Week action began",
                                        "Country in which the action occurred", "", ""))

# format dictionary
dic <- df_read_dic %>% select(Varlabel, Varname, Description = `Variable Description`) %>%
  filter(Varlabel %in% plotting_vars) %>%
  bind_rows(extra_dic) %>%
  mutate(Description = replace_na(Description, ""))

## format dataset
df_clean <- df_read %>%
  left_join(lookup %>% select(Country.code.repl = iso2, Country = name), by = "Country") %>%
  mutate(Country.code = recode(Country.code,
                               "GB-NIR" = "GB",
                               "GB-SCT" = "GB",
                               "GB-WLS" = "GB",
                               "GB-ENG" = "GB",
                               "UK" = "GB"),
         Country.code = ifelse(Country == "Malta" & Country.code == "US", NA, Country.code),
         Country.code = ifelse(Country.code == "US" & Country != "United States", NA, Country.code),
         Country.code = ifelse(is.na(Country.code) & complete.cases(Country), Country.code.repl, Country.code),
         Country.code = tidyr::replace_na(Country.code, "unknown"),
         Country = recode(Country,
                          "CANADA" = "Canada"),
         Country = ifelse(Country.code == "GB", "United Kingdom", Country),
         Country.region = recode(Country.region, "Kentucky, Ohio" = "Kentucky"),
         Country.region = ifelse(City == "Florida", "Florida", Country.region),
         Country.region = ifelse(City == "Connecticut", "Connecticut", Country.region),
         Country.region = ifelse(City == "Colorado", "Colorado", Country.region),
         Country.region = ifelse(City == "Washington", "Washington", Country.region),
         Country.region = ifelse(City == "Massachusetts", "Massachusetts", Country.region),
         Country.region = ifelse(City == "New York City", "New York", Country.region),
         Country.region = ifelse(City == "Nassau County", "New York", Country.region),
         Country.region = ifelse(City == "Wisconsin", "Wisconsin", Country.region),
         Country.region = ifelse(City == "Milwaukee", "Wisconsin", Country.region),
         Country.region = ifelse(City == "Metro Boston", "Massachusetts", Country.region),
         Country.region = ifelse(City == "Washington State", "Washington", Country.region),
         Country.region = ifelse(City == "New Jersey", "New Jersey", Country.region),
         Country.region = ifelse(City == "Washington, D.C.", "District of Columbia", Country.region),
         Country.region = ifelse(City == "Washington DC", "District of Columbia", Country.region),
         Country.region = ifelse(City == "Las Vegas", "Nevada", Country.region),
         Country.region = ifelse(City == "Los Angeles County", "California", Country.region),
         Country.region = ifelse(City == "Detroit", "Michigan", Country.region)) %>%
  left_join(lookup %>% select(Country.code = iso2, Country.code.iso3 = iso3), by = "Country.code") %>%
  left_join(lookup_us_states, by = "Country.region") %>%
  mutate(Country = Gnm(Country),
         Country.code.iso3 = Gcd(Country.code.iso3),
         Country.region = Gnm(Country.region)) %>%
  select(-us_state_id)

# format dates
df_time <- df_clean %>%
  mutate(Date.started.wrong.format = map_lgl(Date.started, is.character)) %>%
  filter(!Date.started.wrong.format) %>%
  select(UID, Date.announced, Date.started)

df_time <- df_time %>% unnest(Date.started) %>%
  mutate(Week.announced = as.character(cut(as.Date(Date.announced), "week")),
         Week.started = as.character(cut(as.Date(Date.started), "week")),
         Date.announced = as.character(as.Date(Date.announced)),
         Date.started = as.character(as.Date(Date.started))) %>%
  select(UID, Date.announced, Date.started, Week.announced, Week.started)

# select variables
df <- df_clean %>% select(-Date.announced, -Date.started) %>%
  left_join(df_time, by = "UID") %>%
  left_join(df_cities_complete, by = "City") %>%
  mutate(Country = Gnm(Country),
         `World country` = Cat(Country),
         Country.code = Gcd(Country.code.iso3),
         Country.region = Gnm(Country.region),
         World.region = Cat(World.region)) %>%
  select(all_of(dic$Varname))

## create final dictionary
f_without_descriptions <- homodatum::fringe(df)$dic
dic_prep <- dic %>% select(label = Varname, description = Description)
df_dic <- f_without_descriptions %>%
  left_join(dic_prep, by = "label") %>%
  select(label, id, description, hdType)

# create country lookup
country_lookup <- df %>% distinct(Country, Country.code)

# create caption with sources for graphs
sources <- "Sources of data: 'Shifting Streets' Covid-19 mobility reponses around the world. PBIC, NUMO, Mobility Works, Streetplans, EpiAndes, Datasketch (2020). Full citation details at bit.ly/mobility-actions"
caption <- paste0("<p style='font-family:Ubuntu;color:#293845;font-size:12px;'>",sources,"</p>")


### shiny app -----------------------------------------------------------------------------------

# Define UI
ui <- panelsPage(styles = styles,
                 panel(id = "panel_data",
                       title = "Choose data",
                       width = 270,
                       body = div(
                         div(
                           uiOutput("choose_data"),
                           uiOutput("select_var"),
                           uiOutput("select_category"),
                           actionButton(inputId='viewAllData', label="View complete dataset",
                                        icon = icon("th"),
                                        onclick = paste0("window.open('",data_link,"', '_blank')")),
                           downloadButton("downloadCSV", "Download current selection"),
                           br(),
                           uiOutput("dataDict")
                         )
                       )),
                 panel(id = "panel_viz",
                       title = "Visualize data",
                       title_plugin = uiOutput("download"),
                       can_collapse = FALSE,
                       body = withLoader(uiOutput("viz"), type = "image",loader = "img/loading_gris.gif"),
                       footer = uiOutput("viz_icons")))


server <- function(input, output, session) {

  output$viz <- renderUI({
    if(input$dataset == "dat_viz"){
      highchartOutput("view_hgch_viz", height = 400)
    } else {
      leafletOutput("map_lflt")
    }
  })

  dataDictText <- reactive({
    if (is.null(input$var_order)) return ("")
    if(input$var_order == "actions_total"){
      tags$div(tags$h4("Actions total"),
               tags$p("Total number of actions")
               )
    } else if (length(input$var_order) == 1){
      tags$div(tags$h4(df_dic %>% filter(id == input$var_order) %>% pull(label)),
               tags$p(df_dic %>% filter(id == input$var_order) %>% pull(description))
               )
    } else if (length(input$var_order) == 2) {
      tags$div(tags$h4(df_dic %>% filter(id == input$var_order[1]) %>% pull(label)),
               tags$p(df_dic %>% filter(id == input$var_order[1]) %>% pull(description)),
               tags$br(),
               tags$h4(df_dic %>% filter(id == input$var_order[2]) %>% pull(label)),
               tags$p(df_dic %>% filter(id == input$var_order[2]) %>% pull(description))
               )
    }
  })

  output$dataDict <- renderUI({
    if (is.null(input$var_order)) return ()
    dataDictText()
    # tags$div(tags$h5(infoTooltip("Data dictionary", dataDictHover())))
  })

  output$choose_data <- renderUI({
    selectInput("dataset", "CHOOSE VISUALIZATION TYPE:",
                selected = "dat_map",
                c("Graphs" = "dat_viz",
                  "Maps by Country" = "dat_map",
                  "Maps by US State" = "dat_map_us",
                  "Maps by City" = "dat_map_city"))
  })

  inputData <- reactive({
    req(input$dataset)
    if(input$dataset == "dat_viz"){
      excluded_vars_dic <- c("country", "country_code", "country_region", "city", "lon", "lat")
      excluded_vars <- df_dic %>% filter(id %in% excluded_vars_dic) %>% pull(label)
      df %>% select(-one_of(excluded_vars))
    } else if (input$dataset == "dat_map"){
      excluded_vars_dic <- c("country", "country_region", "date_started", "date_announced",
                         "week_started", "week_announced", "world_region",
                         "world_country", "city", "lon", "lat")
      excluded_vars <- df_dic %>% filter(id %in% excluded_vars_dic) %>% pull(label)
      df %>% rename(`Actions total` = Country.code) %>%
        select(-one_of(excluded_vars))
    } else if (input$dataset == "dat_map_us") {
      excluded_vars_dic <- c("country", "country_code", "date_started", "date_announced",
                             "week_started", "week_announced", "world_region",
                             "world_country", "city", "lon", "lat")
      excluded_vars <- df_dic %>% filter(id %in% excluded_vars_dic) %>% pull(label)
      df %>% filter(Country.code == "USA") %>%
        rename(`Actions total` = Country.region) %>%
        select(-one_of(excluded_vars))
    } else {
      excluded_vars_dic <- c("country", "country_region", "country_code", "date_started", "date_announced",
                             "week_started", "week_announced", "world_region",
                             "world_country", "city", "lon", "lat")
      excluded_vars <- df_dic %>% filter(id %in% excluded_vars_dic) %>% pull(label)
      df %>% rename(`Actions total` = City) %>%
        select(-one_of(excluded_vars))
    }
  })

  output$select_category <- renderUI({
    req(input$dataset)
    req(dic_draw())
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
                     div(class="title-data-select", "Select up to two variables:"),
                     choices = list_var,
                     selected = "primary_type",
                     multiple = TRUE,
                     options = list(maxItems = 2,
                                    plugins= list('remove_button', 'drag_drop')))
    } else {
      selectInput("var_order",
                     div(class="title-data-select","Select variable:"),
                     choices = list_var,
                  selected = "actions_total")
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

  data_draw_map <- reactive({
    data <- data_draw()
    if(is.null(data) | dic_draw()$label == "Actions total"){
      if (input$dataset == "dat_map_us"){
        data <- df %>% filter(Country.code == "USA") %>% group_by(Country.region) %>% summarise(Actions = n()) %>% select(Country.region, Actions)
      } else if (input$dataset == "dat_map"){
        data <- df %>% group_by(Country.code, Country) %>% summarise(Actions = n()) %>% select(Country.code, Actions, Country)
      } else if (input$dataset == "dat_map_city"){
        data <- df %>% group_by(lon, lat, City) %>% summarise(Actions = n()) %>% select(lon, lat, Actions, City)
      }
    } else {
      if(!is.null(input$selected_cat)){
        if (input$dataset == "dat_map_us"){
          data <- cbind(df %>% filter(Country.code == "USA") %>% select(Country.region), data) %>%
            filter(.data[[dic_draw()$label]] == input$selected_cat) %>%
            group_by(Country.region) %>% summarise(Count = n()) %>% select(Country.region, Count)
        } else if (input$dataset == "dat_map"){
          data <- cbind(df %>% select(Country.code, Country), data) %>% filter(.data[[dic_draw()$label]] == input$selected_cat) %>%
            group_by(Country.code, Country) %>% summarise(Count = n()) %>% select(Country.code, Count, Country)
        } else if (input$dataset == "dat_map_city"){
          data <- cbind(df %>% select(City, lon, lat), data) %>% filter(.data[[dic_draw()$label]] == input$selected_cat) %>%
            group_by(lon, lat, City) %>% summarise(Count = n()) %>% select(lon, lat, Count, City)
          }
        }
      }
    data
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
      } else if (input$dataset == "dat_map_us"){
        x <- "GnmNum"
      } else {
        x <- "GlnGltNum"
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
    req(possible_viz())
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
                     #checkmarkColor = "#df5c33",
                     path = path,
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
    data <- data_draw()
    if(ncol(data) == 1 & names(data) == "type"){names(data) <- "Type"}
    print(names(data))
    viz_name <- viz_name()
    dataLabels_show <- FALSE
    if(actual_but$active_viz %in% c("treemap", "bubbles")){
      dataLabels_show <- TRUE
    }
    opts <- dsvizopts::merge_dsviz_options(palette_colors = c("#3A3766", "#5964C6", "#B956A6", "#DF5C33", "#FCBB1C", "#F8DEAC", "#9DE2C5", "276151"),
                                           na_color = "#EAEAEA",
                                           caption = caption,
                                           label_wrap = 30,
                                           #plot_margin_bottom = 150,
                                           legend_y_position = 10,
                                           legend_verticalAlign = 'top',
                                           dataLabels_show = dataLabels_show)
    if ("Status" %in% names(data)) {
      opts <- c(opts, plot_margin_bottom = 230)
    }
    viz <- do.call(viz_name, c(list(data = data, opts = opts
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
    req(input$dataset)
    if(input$dataset == "dat_viz") return()
    geotype <- gsub("-", "", ftype_draw())
    print(geotype)
    viz <- paste0("lflt_", actual_but$active_map, "_", geotype)
    # opts <- c(opts_viz(), theme_draw())

    palette_colors <- c("#FFDD65", "#F9BE58", "#F29F4B", "#E97F3F", "#DF5C33")
    if(actual_but$active_map == "bubbles"){
      palette_colors <- "#df5c33"
    }

    legend_show = TRUE

    if(is.null(data) | dic_draw()$label == "Actions total"){
      if (input$dataset == "dat_map_us"){
        tooltip <- "<b>State:</b> {Country.region}<br/><b>Actions:</b> {Actions}"
        map_name <- "usa_states"
      } else if (input$dataset == "dat_map"){
        tooltip <- "<b>Country:</b> {Country}<br/><b>Actions:</b> {Actions}"
        map_name <- "world_countries"
      } else {
        tooltip <- "<b>City:</b> {City}<br/><b>Actions:</b> {Actions}"
        map_name <- "world_countries"
        legend_show <- FALSE
      }
    } else {
      req(input$selected_cat)
      if (input$dataset == "dat_map_us"){
        tooltip <- "<b>State:</b> {Country.region}<br/><b>Count:</b> {Count}"
        map_name <- "usa_states"
      } else if (input$dataset == "dat_map"){
        tooltip <- "<b>Country:</b> {Country}<br/><b>Count:</b> {Count}"
        map_name <- "world_countries"
      } else {
        tooltip <- "<b>City:</b> {City}<br/><b>Count:</b> {Count}"
        map_name <- "world_countries"
        legend_show <- FALSE
      }
    }
    opts <- dsvizopts::merge_dsviz_options(map_name = map_name,
                                           tooltip = tooltip,
                                           palette_colors = palette_colors,
                                           legend_show = legend_show,
                                           na_color = "#EAEAEA",
                                           caption = caption,
                                           border_weight = 0.75)
    if(viz == "lflt_choropleth_GlnGltNum"){viz <- "lflt_bubbles_GlnGltNum"}
    do.call(viz, c(list(data = data_draw_map(), opts = opts
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

  downloadImageServer("download_plot", element = reactive(download_opts()),
                      lib = "highcharter", formats = c("html","jpeg", "pdf", "png"))


  data_download <- reactive({
    dd <- data_draw() %>% group_by_all() %>% summarise(Count = n())
    if(input$dataset == "dat_map"){
      if(is.null(data) | dic_draw()$label == "Actions total"){
        dd <- data_draw_map() %>% ungroup() %>% select(Country, Actions)
      } else {
        req(input$selected_cat)
        dd <- data_draw_map() %>% ungroup() %>% select(Country, Count)
        names(dd) <- c("Country", input$selected_cat)
      }
    }
    if(input$dataset == "dat_map_us"){
      if(is.null(data) | dic_draw()$label == "Actions total"){
        dd <- data_draw_map() %>% ungroup() %>% select(State = Country.region, Actions)
      } else {
        req(input$selected_cat)
        dd <- data_draw_map() %>% ungroup() %>% select(State = Country.region, Count)
        names(dd) <- c("State", input$selected_cat)
      }
    }
    if(input$dataset == "dat_map_city"){
      if(is.null(data) | dic_draw()$label == "Actions total"){
        dd <- data_draw_map() %>% ungroup() %>% select(City, Actions)
      } else {
        req(input$selected_cat)
        dd <- data_draw_map() %>% ungroup() %>% select(City, Count)
        names(dd) <- c("City", input$selected_cat)
      }
    }
    dd
  })

  #download csv of selected data displayed in viz
  output$downloadCSV <- downloadHandler(

    filename = function() {
      paste("covid_mobility_actions_", input$var_order, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_download(), file, row.names = FALSE)
    }
  )

}


shinyApp(ui, server)
