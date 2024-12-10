library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)
library(bslib)
library(openxlsx)


Protein_Digestibility_Data <-
    read_csv("Bioavailability - full data.csv") %>%
    mutate(`Protein Form` = ifelse(`Protein Form` == "cystine", "cysteine", `Protein Form`)) %>%
    mutate(`Protein Form` = ifelse(`Protein Form` == "arganine", "arginine", `Protein Form`)) %>%
    select(!Notes) %>%
    select(!Diet) %>%
    select(!`Food group`) %>%
    filter(Calculation != "biological value")
EAA_composition <- read_csv("EAA_composition.csv") %>%
    pivot_longer(histidine:valine, names_to = "AA", values_to = "value") %>%
    mutate(`food identifier` = str_trim(`food identifier`)) %>%
    mutate(fdcId = as.character(fdcId)) %>%
    filter(!(is.na(`food identifier`) & is.na(fdcId))) %>%
    replace_na(list(NI_ID = "Not Available", fdcId = "Not Applicable"))

scoring_pattern <- read_csv("scoring_pattern.csv")
portion_sizes <- read_csv("portion_sizes.csv")




# shiny app
ui <- fluidPage(
    tags$head(tags$style(
        HTML(
            "
            .my-wrapper {
      width: 100%;
      margin: 0 auto;
    }
    .my-inner1 {
      margin: 0 20px;
      display: inline-block;
      vertical-align: left;
    }
    .my-inner2 {
      margin: 0 50px;
      display: inline-block;
      vertical-align: top;
    }
        .float-right-button {
            float: right;
            margin-right: 10px;
            margin-top: 5px;
            background-color: transparent;
            border: none;
        }

        .accordion-toggle {
            cursor: pointer;
            background-color: #dedede;
            border: 0.5px solid #bdbdbd;
            margin-top: 0px;
            font-weight: bold;
            padding-left: 20px;
            padding-bottom: 50px;
        }

        .accordion-content {
            display: none;
            padding: 20px;
            background-color: #F6F6F6;
            border-left: 0.5px solid #bdbdbd;
            border-right: 0.5px solid #bdbdbd;
            border-bottom: 0.5px solid #bdbdbd;
        }

        .nav-tabs {
            border-bottom: 1px solid black;
            margin-bottom: 10px;
            width: 100%;
            padding: 0px;
            margin-left: -10px;
        }

        .nav-tabs li {
            font-size: 25px;
            background-color: white;
            color: black;
            padding: 0;
        }

        .nav > li > a:hover, .nav > li > a:focus {
            outline: rgb(0, 0, 0) none 1px;
        }

        .nav-tabs li a {
            font-size: 25px;
            background-color: #F6F6F6;
            color: #808080;
            border-bottom: solid 1px #000;
        }

        .nav-tabs li a:hover {
            background-color: #e0e0e0; /* Change background color on hover */
            color: #000; /* Change text color on hover */
            transition: background-color 0.3s ease, color 0.3s ease; /* Smooth transition */
        }

        .nav-tabs > li:not(.active) > a {
            border: 1px solid black; /* Adds a border around inactive tabs */
            border-radius: 4px 4px 0 0; /* Rounded top corners, square bottom corners */
            color: #555; /* Optional: Text color for inactive tabs */
            background-color: #f9f9f9; /* Optional: Background color */
        }

        .tab-content {
            width: 100%;
        }

        .tabbable {
            width: 100%;
        }

        .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
            border-color: rgb(0, 0, 0) rgb(0, 0, 0) transparent;
            border-radius: 4px 4px 0 0; /* Rounded top corners, square bottom corners */
        }
        "
        )
    )),
    fluidPage(
        fluidRow(br()),
        withMathJax(),
        fluidRow(
            column(5, h1("Protein Quality Hub")),
            actionButton(
                "gitbutton",
                label = tags$h5(
                    "View data or docummentation on github  ",
                    fa(
                        "github",
                        fill = "black",
                        height = "20px",
                        vertical_align = "-0.35em"
                    ),
                    onclick = "window.open('https://github.com/NutrientInstitute/protein-quality-hub', '_blank')"
                ),
                style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px;float:right;"
            ),
            actionButton(
                "gitbutton",
                label = tags$h5(
                    "Provide feedback",
                    fa(
                        "comment",
                        fill = "black",
                        height = "20px",
                        vertical_align = "-0.35em"
                    ),
                    onclick = "window.open('https://www.nutrientinstitute.org/protein-digestibility-feedback', '_blank')"
                ),
                style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px; margin-right: 20px; float:right;"
            )
        ),
        fluidRow(br()),
        useShinyjs(),
        navset_tab(
            nav_panel(
                "Protein Quality Scoring",
                fluidRow(
                    div(
                        id = "info-toggle_2",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_2_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top: 10px;")
                        ),
                        column(4, uiOutput("toggleButton_2"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent_2",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p(
                            "In the",
                            tags$em(" Protein Quality Scoring"),
                            " tab of the ",
                            tags$em("Protein Quality Hub"),
                            "you will find protein quality scores generated using digestibility values from the ",
                            tags$em("Bioavailability Data"),
                            " tab. Bioavailability values can be identified across tables using the variable ",
                            tags$em("NI_ID"),
                            ".",
                            br(),
                            "For more information on EAA recommendations and scoring patterns used in this app, consult the ",
                            a(href = "https://github.com/NutrientInstitute/protein-quality-hub", "project GitHub page"),
                            ". Note that this tool is still in development, please contact",
                            a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
                            "to report problems or provide feedback."
                        ),
                        br(),
                        h4(
                            "Protein Digestibility Corrected Amino Acid Score (PDCAAS):",
                            tags$sup("1")
                        ),
                        p(
                            "Intended application: PDCAAS values range from 0 to 1 and represent the quality of 1 gram of protein from a food source compared to a reference gram of protein. In practice, the score can be used to compare the protein quality between food sources on a per gram basis."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        p(
                            "The PDCAAS calculation is based on the ratio of limiting amino acid in a gram of protein compared to the same amino acid in a reference protein capped at 1 (i.e. 100%), adjusted for digestibility. PDCASS was designed to be calculated using fecal digestibility values.*"
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{PDCAAS}=min(\\frac{\\text{mg of amino acid in 1 g test protein}}{\\text{mg of amino acid in reference pattern}},1)\\times\\text{digestibility}\\)',
                        br(),
                        p(br()),
                        h4(
                            "Digestible Indispensable Amino Acid Score (DIAAS):",
                            tags$sup("2")
                        ),
                        p(
                            "Intended application: DIAAS values represent the ratio between the quality of 1 gram of protein from a food source compared to a reference gram of protein. In practice, the score can be used to compare the protein quality between food sources on a per gram basis."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        p(
                            "The DIAAS calculation is based on the ratio of limiting amino acid in a gram of protein compared to the same amino acid in a reference protein, adjusted for digestibility. Unlike PDCAAS, DIAAS is not truncated at 1 (i.e. 100%) and is  calculated using ileal digestibility.*"
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{DIAAS}=100\\times\\frac{\\text{mg of digestible dietary indispensable amino acid in 1 g of the dietary protein}}{\\text{mg of the same dietary indispensable amino acid in 1g of the reference protein}}\\times\\text{digestibility}\\)',
                        p(br()),
                        p(
                            "*While specific digestibility samples (i.e. ileal, fecal) are preferred in the calculation of PDCAAS and DIAAS, protein quality scores are provided for all available bioavailability values."
                        ),
                        p(br()),
                        h4("EAA-9:", tags$sup("3")),
                        p(
                          "Intended application: EAA-9 scores are percentages, representing the ability of a food to meet daily essential amino acid (EAA) recommendations, by default the RDAs. In practice, the score can be used to compare protein quality between food sources, and as a dietary quality tool to track progress toward meeting EAA recommendations."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        # " Calculation is based on the minimum percentage of the RDA met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid.",
                        p(
                          "The EAA-9 calculation is based on the minimum percentage of the RDA (or personalized EAA recommendations) met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid adjusted for bioavailability (if bioavailability data is available). EAA RDAs are satisfied when the EAA-9 score for foods consumed in a day reaches 100%."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{EAA-9}=min(\\frac{\\text{His Present}}{\\text{His RDA}},\\frac{\\text{Ile Present}}{\\text{Ile RDA}},\\frac{\\text{Leu Present}}{\\text{Leu RDA}},\\frac{\\text{Lys Present}}{\\text{Lys RDA}},\\frac{\\text{Met Present}}{\\text{Met RDA}},\\frac{\\text{Phe Present}}{\\text{Phe RDA}},\\frac{\\text{Thr Present}}{\\text{Thr RDA}},\\frac{\\text{Trp Present}}{\\text{Trp RDA}},\\frac{\\text{Val Present}}{\\text{Val RDA}})\\times100\\times\\text{bioavailability}\\)',
                        br(),


                        br(),
                        tags$small(h5("References:"),
                                   tags$ol(
                                   tags$li(
                                       p(
                                           "Food and Agriculture Organization of the United Nations, World Health Organization & United Nations University. Protein and amino acid requirements in human nutrition : report of a joint FAO/WHO/UNU expert consultation [Internet]. World Health Organization; 2007 [cited 2022 Dec 1]. Available from: ",
                                           a(href = "https://apps.who.int/iris/handle/10665/43411", "https://apps.who.int/iris/handle/10665/43411"),
                                           "."
                                       )
                                   ),
                                   tags$li(
                                       p(
                                           "FAO. Dietary protein quality evaluation in human nutrition: report of an FAO Expert Consultation. Food and nutrition paper; 92. FAO: Rome [Internet]. FAO (Food and Agriculture Organization); 2013. Available from:",
                                           a(
                                               href = "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf",
                                               "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf"
                                           ),
                                           "."
                                       )
                                   ),
                                   tags$li(
                                     p(
                                       "Forester SM, Jennings-Dobbs EM, Sathar SA, Layman DK. Perspective: Developing a Nutrient-Based Framework for Protein Quality. J Nutr. 2023 Aug;153(8):2137-2146. doi: ",
                                       a(href = "10.1016/j.tjnut.2023.06.004", "https://doi.org/10.1016/j.tjnut.2023.06.004"),
                                       ". Epub 2023 Jun 8. PMID: 37301285."
                                     )
                                   )))
                    )
                )),
                fluidRow(br()),
                # Create a new Row in the UI for search options
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 10px; padding-top: 10px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                         column(
                             2,
                             textInput(
                                 inputId = "NI_ID_tab1",
                                 label = "NI_ID:",
                                 width = '100%'
                             )
                         ),
                         column(
                             2,
                             textInput(
                                 inputId = "food_tab1",
                                 label = "Food:",
                                 width = '100%'
                             )
                         )),
                fluidRow(br()),
                # Create a new Row in the UI for selectInputs
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             4,
                             p("Choose scoring method(s)", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    fluidRow(
                        column(
                            3,
                            checkboxGroupInput(
                                inputId = "score",
                                label = "Score(s)",
                                choices = c("PDCAAS", "DIAAS", "EAA-9"),
                                selected = c("PDCAAS", "DIAAS", "EAA-9")
                            ),
                            radioButtons(
                                inputId = "show_calc",
                                label = "Score calculation",
                                choiceNames = c("Show calculation and score", "Show score only"),
                                choiceValues = c("show_calc", "score_only")
                            )
                        ),
                        column(
                            3,
                            conditionalPanel(
                                condition = "input.score.includes('PDCAAS')",
                                p("PDCAAS Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
                                fluidRow(
                                    style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
                                    pickerInput(
                                        inputId = "EAA_rec_PDCAAS",
                                        label = "EAA Recommendations",
                                        choices = unique(unlist(
                                            scoring_pattern %>% filter(Unit == "mg/g protein") %>% select(`Pattern Name`)
                                        )),
                                        selected = "FAO Scoring Pattern"
                                    ),
                                    pickerInput(
                                        inputId = "rec_age_PDCAAS",
                                        label = "Age",
                                        choices = unique(scoring_pattern$Age)
                                    ),
                                    br()
                                )
                            )
                        ),
                        column(
                            3,
                            conditionalPanel(
                                condition = "input.score.includes('DIAAS')",
                                p("DIAAS Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
                                fluidRow(
                                    style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
                                    pickerInput(
                                        inputId = "EAA_rec_DIAAS",
                                        label = "EAA Recommendations",
                                        choices = unique(unlist(
                                            scoring_pattern %>% filter(Unit == "mg/g protein") %>% select(`Pattern Name`)
                                        )),
                                        selected = "FAO Scoring Pattern"
                                    ),
                                    pickerInput(
                                        inputId = "rec_age_DIAAS",
                                        label = "Age",
                                        choices = unique(scoring_pattern$Age)
                                    ),
                                    br()
                                )
                            )
                        ),
                        column(
                          3,
                          conditionalPanel(
                            condition = "input.score.includes('EAA-9')",
                            p("EAA-9 Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
                            fluidRow(
                              style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
                              pickerInput(
                                inputId = "EAA_rec",
                                label = "EAA Recommendations",
                                choices = unique(c(
                                  unlist(
                                    scoring_pattern %>% filter(Unit == "mg/kg/d") %>% select(`Pattern Name`)
                                  ),
                                  "Choose custom recommendations"
                                )),
                                selected = "RDA for Adults"
                              ),
                              conditionalPanel(condition = "input.EAA_rec == 'Choose custom recommendations'",
                                               fluidRow(
                                                 column(
                                                   4,
                                                   numericInput(
                                                     inputId = "his",
                                                     label = "His (mg/kg/d)",
                                                     value = "14",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "leu",
                                                     label = "Leu (mg/kg/d)",
                                                     value = "19",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "ile",
                                                     label = "Ile (mg/kg/d)",
                                                     value = "42",
                                                     width = '90px'
                                                   )
                                                 ),
                                                 column(
                                                   4,
                                                   numericInput(
                                                     inputId = "lys",
                                                     label = "Lys (mg/kg/d)",
                                                     value = "38",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "met_cys",
                                                     label = "Met+Cys (mg/kg/d)",
                                                     value = "19",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "phe_tyr",
                                                     label = "Phe+Tyr (mg/kg/d)",
                                                     value = "33",
                                                     width = '90px'
                                                   )
                                                 ),
                                                 column(
                                                   4,
                                                   numericInput(
                                                     inputId = "thr",
                                                     label = "Thr (mg/kg/d)",
                                                     value = "20",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "trp",
                                                     label = "Trp (mg/kg/d)",
                                                     value = "5",
                                                     width = '90px'
                                                   ),
                                                   numericInput(
                                                     inputId = "val",
                                                     label = "Val (mg/kg/d)",
                                                     value = "24",
                                                     width = '90px'
                                                   )
                                                 )
                                               )),
                              conditionalPanel(
                                condition = "input.EAA_rec!='Choose custom recommendations'",
                                pickerInput(
                                  inputId = "rec_age",
                                  label = "Age",
                                  choices = unique(scoring_pattern$Age)
                                )
                              ),
                              numericInput(
                                inputId = "weight",
                                label = "Weight (kg)",
                                value = "70"
                              ),
                              radioButtons(
                                inputId = "serving_size",
                                label = "Serving size of food",
                                choices = c("Use standard serving sizes", "Choose your own serving"),
                                selected = "Use standard serving sizes"
                              ),
                              conditionalPanel(
                                condition = "input.serving_size == 'Choose your own serving'",
                                numericInput(
                                  inputId = "serving_weight",
                                  label = "Serving size (g)",
                                  value = "100"
                                )
                              ),
                              checkboxInput(inputId = "require_bioavail", label = "require bioavailability for calculation", value = FALSE)
                            )
                          )
                        )
                    ),
                    fluidRow(
                        p("Bioavalability Coefficients", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;margin-top:20px;"),
                        fluidRow(
                            style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 10px;",
                            column(
                              2,
                              checkboxGroupInput(
                                inputId = "pq_species",
                                label = "Species",
                                choices = c("human", "human (predicted from swine)", "swine", "rat"),
                                selected = c("human", "human (predicted from swine)", "swine", "rat")
                              )
                            ),
                            column(
                                2,
                                checkboxGroupInput(
                                    inputId = "pq_sample",
                                    label = "Sample Location",
                                    choices = c(unique(
                                        as.character(Protein_Digestibility_Data$`Sample Location`)
                                    )),
                                    c(unique(
                                        as.character(Protein_Digestibility_Data$`Sample Location`)
                                    ))
                                )
                            ),
                            column(
                                2,
                                checkboxGroupInput(
                                    inputId = "pq_analyte",
                                    label = "Protein Form",
                                    choices = c("crude protein", "individual amino acids"),
                                    selected = c("crude protein", "individual amino acids")
                                )
                            ),
                            column(
                              2,
                              checkboxGroupInput(
                                inputId = "pq_measure",
                                label = "Calculation",
                                choices = c(unique(
                                  as.character(Protein_Digestibility_Data$Calculation)
                                )),
                                selected = c(unique(
                                  as.character(Protein_Digestibility_Data$Calculation)
                                ))
                              )
                            )
                        )

                    )
                ),
                fluidRow(br()),
                # Create download button
                fluidRow(style = "float: right;",
                    dropdownButton(
                        label = "Download",
                        circle = FALSE,
                        status = "secondary",
                        icon = icon("download"),
                        tags$div(
                            downloadButton("download_csv_PQ", "CSV", class = "btn btn-secondary", icon = icon("file-csv")),
                            downloadButton("download_excel_PQ", "Excel", class = "btn btn-secondary", icon = icon("file-excel"))
                        )
                    )),
                # Create a new row for the table.
                fluidRow(style = "padding-top: -20px;",
                         DT::dataTableOutput("table_2"))
            ),
            nav_panel(
                "Bioavailability Data",
                fluidRow(
                    div(
                        id = "info-toggle",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                        ),
                        column(4, uiOutput("toggleButton"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p(
                            "The Protein Quality Hub Project serves as a dedicated repository for protein digestibility information, addressing a critical gap in accessible data. For more information see the Protein Quality Hub ",
                            a(href = "https://github.com/NutrientInstitute/protein-quality-hub", "github page."),
                            " Please contact ",
                            a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
                            "to report problems or provide feedback."
                        ),
                        br(),
                        p(
                            "Currently, the Protein Quality Hub contains bioavailability data from the following sources:"
                        ),
                        tags$ul(
                            tags$li(
                                a(
                                    href = "https://www.ars.usda.gov/arsuserfiles/80400535/data/classics/usda%20handbook%2074.pdf",
                                    "USDA ENERGY VALUE OF FOODS (Agricultural Handbook No. 74, 1955)"
                                )
                            ),
                            tags$li(
                                "AMINO-ACID CONTENT OF FOODS AND BIOLOGICAL DATA ON PROTEINS (FAO 1970) ",
                                br(),
                                tags$small(
                                    tags$i(
                                        "(",
                                        tags$b("Note"),
                                        ": The original publication has been removed from the FAO website, but can still be accessed via the ",
                                        tags$a(href = "https://web.archive.org/web/20231125115519/https://www.fao.org/3/ac854t/AC854T00.htm", "Wayback Machine"),
                                        ")"
                                    )
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://www.fao.org/ag/humannutrition/36216-04a2f02ec02eafd4f457dd2c9851b4c45.pdf",
                                    "Report of a Sub-Committee of the 2011 FAO Consultation on 'Protein Quality Evaluation in Human Nutrition'"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.17226/13298",
                                    "Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)",
                                    tags$b("**")
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://books.google.com/books?id=ieEEPqffcxEC&lpg=PP1&pg=PP1#v=onepage&q&f=false ISBN: 92-5-103097-9",
                                    "Protein quality Evaluation: Report of Joint FAO/WHO Expert Consultation (FAO 1991)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/137.8.1874",
                                    "Application of the indicator amino acid oxidation technique for the determination of metabolic availability of sulfur amino acids from casein versus soy protein isolate in adult men (Humayun et al. 2007)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.3945/jn.112.166728",
                                    "Lysine from cooked white rice consumed by healthy young men is highly metabolically available when assessed using the indicator amino acid oxidation technique (Prolla et al. 2013)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxaa086",
                                    "Bioavailable Methionine Assessed Using the Indicator Amino Acid Oxidation Method Is Greater When Cooked Chickpeas and Steamed Rice Are Combined in Healthy Young Men (Rafii et al. 2020)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxaa227",
                                    "Bioavailable Lysine, Assessed in Healthy Young Men Using Indicator Amino Acid Oxidation, is Greater when Cooked Millet and Stewed Canadian Lentils are Combined (Fakiha et al. 2020)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxab410",
                                    "Bioavailable Lysine Assessed Using the Indicator Amino Acid Oxidation Method in Healthy Young Males is High when Sorghum is Cooked by a Moist Cooking Method (Paoletti et al. 2022)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxy039",
                                    "Metabolic Availability of the Limiting Amino Acids Lysine and Tryptophan in Cooked White African Cornmeal Assessed in Healthy Young Men Using the Indicator Amino Acid Oxidation Technique (Rafii et al. 2018)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxy039",
                                    "Lysine Bioavailability in School-Age Children Consuming Rice Is Reduced by Starch Retrogradation (Caballero et al. 2020)"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.1093/jn/nxy039",
                                    "Metabolic Availability of Methionine Assessed Using Indicator Amino Acid Oxidation Method, Is Greater when Cooked Lentils and Steamed Rice Are Combined in the Diet of Healthy Young Men (Rafii et al. 2022)"
                                )
                            )
                        ),
                        br(),
                        tags$small(
                            p(
                                tags$b("**"),
                                "Digestibility and protein data from ",
                                tags$i(
                                    "Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)"
                                ),
                                "  was collected from the following sources:"
                            ),
                            tags$ol(
                                tags$li(
                                    "AAFCO (Association of American Feed Control Officials). 2010. Official Publication. Oxford, IN: AAFCO."
                                ),
                                tags$li("AminoDat 4.0. 2010. Evonik Industries, Hanau, Germany."),
                                tags$li(
                                    "Cera, K. R., D. C. Mahan, and G. A. Reinhart. 1989. Apparent fat digestibilities and performance responses of postweaning swine fed diets supplemented with coconut oil, corn oil or tallow. Journal of Animal Science 67:2040-2047."
                                ),
                                tags$li(
                                    "CVB (Dutch PDV [Product Board Animal Feed]). 2008. CVB Feedstuff Database. Available online at http://www.pdv.nl/english/Voederwaardering/about_cvb/index.php. Accessed on June 9, 2011."
                                ),
                                tags$li(
                                    "NRC (National Research Council). 1998. Nutrient Requirements of Swine, 10th Rev. Ed. Washington, DC: National Academy Press."
                                ),
                                tags$li(
                                    "NRC. 2007. Nutrient Requirements of Horses, 6th Rev. Ed. Washington, DC: The National Academies Press."
                                ),
                                tags$li(
                                    "Powles, J., J. Wiseman, D. J. A. Cole, and S. Jagger. 1995. Prediction of the apparent digestible energy value of fats given to pigs. Animal Science 61:149-154."
                                ),
                                tags$li(
                                    "Sauvant, D., J. M. Perez, and G. Tran. 2004. Tables of Composition and Nutritional Value of Feed Materials: Pigs, Poultry, Sheep, Goats, Rabbits, Horses, Fish, INRA, Paris, France, ed. Wageningen, the Netherlands: Wageningen Academic."
                                ),
                                tags$li(
                                    "USDA (U.S. Department of Agriculture), Agricultural Research Service. 2010. USDA National Nutrient Database for Standard Reference, Release 23. Nutrient Data Laboratory Home Page. Available online at http://www.ars.usda.gov/ba/bhnrc/ndl. Accessed on August 10, 2011."
                                ),
                                tags$li(
                                    "van Milgen, J., J. Noblet, and S. Dubois. 2001. Energetic efficiency of starch, protein, and lipid utilization in growing pigs. Journal of Nutrition 131:1309-1318."
                                )
                            )
                        )
                    )
                )),
                fluidRow(br()),
                # Create a new Row in the UI for search options
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 10px; padding-top: 10px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                        2,
                        textInput(
                            inputId = "NI_ID_tab2",
                            label = "NI_ID:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "food_tab2",
                            label = "Food:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "analysisMethod_tab2",
                            label = "Analysis method(s):",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "source_tab2",
                            label = "Data Source(s):",
                            width = '100%'
                        )
                    )
                ),
                fluidRow(br()),
                # Create a new Row in the UI for selectInputs
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Filters", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                      2,
                      virtualSelectInput(
                        inputId = "model",
                        label = "Model:",
                        choices = c("in vivo", "in vitro"),
                        selected = c(unique(
                          as.character(Protein_Digestibility_Data$Model)
                        )),
                        multiple = TRUE,
                        showValueAsTags = TRUE,
                        width = '100%'
                      )
                    ),
                    column(
                      2,
                      virtualSelectInput(
                        inputId = "species",
                        label = "Species:",
                        choices = c("human", "human (predicted from swine)", "swine", "rat"),
                        selected = c(unique(
                          as.character(Protein_Digestibility_Data$Species)
                        )),
                        multiple = TRUE,
                        showValueAsTags = TRUE,
                        width = '100%'
                      )
                    ),
                    column(
                        2,
                        virtualSelectInput(
                            inputId = "sample",
                            label = "Sample Location:",
                            choices = c(unique(
                                as.character(Protein_Digestibility_Data$`Sample Location`)
                            )),
                            selected = c(unique(
                                as.character(Protein_Digestibility_Data$`Sample Location`)
                            )),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                        4,
                        virtualSelectInput(
                            inputId = "analyte",
                            label = "Protein Form:",
                            choices = list(
                                "crude protein" = "crude protein",
                                "Essential amino acid" = c(
                                    "histidine",
                                    "isoleucine",
                                    "leucine",
                                    "lysine",
                                    "reactive lysine",
                                    "methionine",
                                    "phenylalanine",
                                    "threonine",
                                    "tryptophan",
                                    "valine"
                                ),
                                "Conditionally essential amino acid" = c("arginine", "cysteine", "glycine", "proline", "tyrosine"),
                                "Non-essential amino acid" = c("alanine", "aspartic acid", "glutamic acid",  "serine")
                            ),
                            selected = unique(as.character(Protein_Digestibility_Data$`Protein Form`)),
                            showValueAsTags = TRUE,
                            multiple = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                      2,
                      virtualSelectInput(
                        inputId = "measure",
                        label = "Calculation:",
                        choices = c(unique(
                          as.character(Protein_Digestibility_Data$Calculation)
                        )),
                        selected = c(unique(
                          as.character(Protein_Digestibility_Data$Calculation)
                        )),
                        multiple = TRUE,
                        showValueAsTags = TRUE,
                        width = '100%'
                      )
                    )
                ),
                fluidRow(br()),
                # Create download button
                fluidRow(style = "float: right;",
                    dropdownButton(
                        label = "Download",
                        circle = FALSE,
                        status = "secondary",
                        icon = icon("download"),
                        tags$div(
                            downloadButton("download_csv_bv", "CSV", class = "btn btn-secondary", icon = icon("file-csv")),
                            downloadButton("download_excel_bv", "Excel", class = "btn btn-secondary", icon = icon("file-excel"))
                        )
                    )),
                # Create a new row for the table.
                fluidRow(style = "padding-top: 20px;",
                         DT::dataTableOutput("table"))
            ),
            nav_panel(
                "EAA Composition Data",
                fluidRow(
                    div(
                        id = "info-toggle_3",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_3_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                        ),
                        column(4, uiOutput("toggleButton_3"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent_3",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p(
                            "In this tab, you will find the food composition data collected for use in protein quality scoring."
                        ),
                        br()
                    )
                )),
                fluidRow(br()),
                # Create a new Row in the UI for search options
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 10px; padding-top: 10px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                        2,
                        textInput(
                            inputId = "NI_ID_tab3",
                            label = "NI_ID:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "food_tab3",
                            label = "Food:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "source_tab3",
                            label = "Data Source(s):",
                            width = '100%'
                        )
                    )
                ),
                fluidRow(br()),
                # Create download button
                fluidRow(style = "float: right;",
                        dropdownButton(
                        label = "Download",
                        circle = FALSE,
                        status = "secondary",
                        icon = icon("download"),
                        tags$div(
                            downloadButton("download_csv_eaa", "CSV", class = "btn btn-secondary", icon = icon("file-csv")),
                            downloadButton("download_excel_eaa", "Excel", class = "btn btn-secondary", icon = icon("file-excel"))
                        )
                    )),
                fluidRow(style = "float: right;",
                    div(
                        style = "font-size: 1.2em;",
                        prettyToggle(
                            inputId = "show_NI_ID",
                            label_on = "Hide NI_ID",
                            icon_on = icon("square-minus"),
                            status_on = "default",
                            status_off = "default",
                            label_off = "Show NI_ID",
                            icon_off = icon("square-plus"),
                            bigger = TRUE,
                            shape = "curve"
                        )
                    )),
                # Create a new row for the table.
                fluidRow(style = "padding-top: 20px;",
                         DT::dataTableOutput("table_3"))
            )
        )
    )
)


server <- function(input, output, session) {
    visibility <- reactiveVal(TRUE)

    output$toggleButton <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    output$toggleButton_2 <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide_2",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide_2",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    output$toggleButton_3 <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide_3",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide_3",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    observeEvent(input$show_hide, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    })

    observeEvent(input$show_hide_2, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_2", anim = TRUE)
    })

    observeEvent(input$show_hide_3, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_3", anim = TRUE)
    })


    # Observe clicks on the info-toggle div
    observeEvent(input$info_toggle_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    observeEvent(input$info_toggle_2_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_2", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    observeEvent(input$info_toggle_3_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_3", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    # Choice of EAA recommendations
    observeEvent(input$EAA_rec, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec)

        updatePickerInput(
            session = session,
            inputId = "rec_age",
            choices = unique(age_options$Age)
        )
    })

    observeEvent(input$EAA_rec_PDCAAS, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec_PDCAAS)

        updatePickerInput(
            session = session,
            inputId = "rec_age_PDCAAS",
            choices = rev(unique(age_options$Age))
        )
    })

    observeEvent(input$EAA_rec_DIAAS, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec_DIAAS)

        updatePickerInput(
            session = session,
            inputId = "rec_age_DIAAS",
            choices = rev(unique(age_options$Age))
        )
    })



    # render table for protein digestibility data
    output$table <- DT::renderDataTable(server = TRUE, {
        DT::datatable(
            Protein_Digestibility_Data %>%
                mutate(n = as.character(n)) %>%
                mutate(`Bio-avail SD` = as.character(`Bio-avail SD`)) %>%
                mutate(`Protein (g)` = as.character(`Protein (g)`)) %>%
                replace_na(
                    list(
                        Food = "not reported",
                        "Protein (g)" = "not reported",
                        n = "not reported",
                        "Bio-avail SD" = "not reported",
                        "Analysis method(s)" = "not reported",
                        "Original Data Source(s)" = "not reported"
                    )
                ) %>%
                filter(`Species` %in% input$species) %>%
                filter(`Model` %in% input$model) %>%
                filter(`Sample Location` %in% input$sample) %>%
                filter(`Protein Form` %in% input$analyte) %>%
                filter(Calculation %in% input$measure) %>%
                filter(str_detect(
                    NI_ID, ifelse(
                        input$NI_ID_tab2 == "",
                        "(.*?)",
                        paste0("(?i)", input$NI_ID_tab2)
                    )
                )) %>%
                filter(str_detect(
                    Food, ifelse(
                        input$food_tab2 == "",
                        "(.*?)",
                        paste0("(?i)", input$food_tab2)
                    )
                )) %>%
                filter(str_detect(
                    `Analysis method(s)`,
                    ifelse(
                        input$analysisMethod_tab2 == "",
                        "(.*?)",
                        paste0("(?i)", input$analysisMethod_tab2)
                    )
                )) %>%
                filter(
                    str_detect(
                        `Data Collection Source`,
                        ifelse(
                            input$source_tab2 == "",
                            "(.*?)",
                            paste0("(?i)", input$source_tab2)
                        )
                    ) |
                        str_detect(
                            `Original Data Source(s)`,
                            ifelse(
                                input$source_tab2 == "",
                                "(.*?)",
                                paste0("(?i)", input$source_tab2)
                            )
                        )
                ) %>%
                mutate(across(
                    where(is.character), ~ gsub("\n", "<br>", .)
                )),
            extensions = c('FixedHeader'),
            class = "display cell-border compact",
            rownames = FALSE,
            options = list(
                fixedHeader = TRUE,
                dom = 'ltip',
                pageLength = 25,
                lengthMenu = c(25, 50, 75, 100),
                columnDefs = list(
                    list(targets = "_all", className = "dt-head-nowrap"),
                    list(
                        targets = c(12, 13),
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 100 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                            "}"
                        )
                    )
                )
            ),
            escape = FALSE  # Allow HTML rendering
        )  %>%
            formatStyle(1:14,
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle(12:13,
                        'overflow-wrap' = 'break-word',
                        'word-break' = 'break-word')
    })

    # render table for protein quality scores
    output$table_2 <- DT::renderDataTable(server = TRUE, {
        PQ_df <- data.frame(NI_ID = character())

        if ("EAA-9" %in% input$score) {
            if (input$EAA_rec == "Choose custom recommendations") {
                scoring_pattern[199, ] <-
                    list(
                        "Choose custom recommendations",
                        "histidine",
                        ifelse(is.numeric(input$hist), input$hist, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[200, ] <-
                    list(
                        "Choose custom recommendations",
                        "leucine",
                        ifelse(is.numeric(input$leu), input$leu, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[201, ] <-
                    list(
                        "Choose custom recommendations",
                        "isoleucine",
                        ifelse(is.numeric(input$ile), input$ile, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[202, ] <-
                    list(
                        "Choose custom recommendations",
                        "lysine",
                        ifelse(is.numeric(input$lys), input$lys, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[203, ] <-
                    list(
                        "Choose custom recommendations",
                        "methionine+cysteine",
                        ifelse(is.numeric(input$met_cys), input$met_cys, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[204, ] <-
                    list(
                        "Choose custom recommendations",
                        "phenylalanine+tyrosine",
                        ifelse(is.numeric(input$phe_tyr), input$phe_tyr, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[205, ] <-
                    list(
                        "Choose custom recommendations",
                        "threonine",
                        ifelse(is.numeric(input$thr), input$thr, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[206, ] <-
                    list(
                        "Choose custom recommendations",
                        "tryptophan",
                        ifelse(is.numeric(input$trp), input$trp, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[207, ] <-
                    list(
                        "Choose custom recommendations",
                        "valine",
                        ifelse(is.numeric(input$val), input$val, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
            }
            if(input$require_bioavail == TRUE){
                EAA_composition <- EAA_composition %>%
                    drop_na(NI_ID)
            }

            if (input$serving_size == "Use standard serving sizes") {
                EAA_9 <- EAA_composition %>%
                    left_join(
                        portion_sizes %>%
                            select(fdc_id, g_weight , portion) %>%
                            rename("fdcId" = "fdc_id") %>%
                            mutate(fdcId = as.character(fdcId)) %>%
                            mutate(portion = paste0(
                                portion, " (", g_weight, " g)"
                            ))
                    ) %>%
                    mutate(portion = ifelse(is.na(portion), "not provided (100 g)", portion)) %>%
                    mutate(g_weight = ifelse(is.na(g_weight), 100, g_weight)) %>%
                    mutate(value = value * 1000) %>%
                    mutate(value = value * (g_weight / 100))
            }else{
                EAA_9 <- EAA_composition %>%
                    mutate(value = value * 1000) %>%
                    mutate(value = value * (input$serving_weight / 100)) %>%
                    mutate(portion = paste0(input$serving_weight, " g"))
            }

            EAA_9 <- EAA_9 %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec) %>%
                        filter(
                            ifelse(
                                is.character(input$rec_age),
                                Age == input$rec_age,
                                !is.na(Age)
                            )
                        ) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(
                    round(value, 2),
                    "/",
                    round(Amount * input$weight, 2)
                )) %>%
                mutate(value = value / (Amount * input$weight)) %>%
                select(
                    fdcId,
                    NI_ID,
                    `food identifier`,
                    `food description`,
                    Protein,
                    portion,
                    value,
                    AA,
                    calculation,
                    `Food Composition Ref`
                )

            temp <- EAA_9 %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ") x 100 x bioavailability \n = ",
                        paste0(min(round(
                            value, 2
                        ))),
                        " x 100 x bioavailability"
                    )
                ) %>%
                ungroup()

            EAA_9 <- EAA_9 %>%
                select(!calculation) %>%
                left_join(temp) %>%
                group_by(fdcId,
                         `food identifier`,
                         `food description`,
                         Protein,
                         portion,
                         `Food Composition Ref`) %>%
                mutate(`EAA-9` = min(value)) %>%
                filter(value == `EAA-9`) %>%
                ungroup() %>%
                select(
                    fdcId,
                    NI_ID,
                    `food identifier`,
                    `food description`,
                    Protein,
                    AA,
                    portion,
                    `EAA-9`,
                    calculation,
                    `Food Composition Ref`
                ) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                distinct() %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(`EAA-9` = `EAA-9` * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                mutate(calculation = ifelse(is.na(`Bio-avail  (%)`), gsub(" x bioavailability", "", calculation), calculation)) %>%
                mutate(calculation = paste0(calculation, " \n = ", `EAA-9`, "%")) %>%
                filter(ifelse(!is.na(`Protein Form`),
                    `Protein Form` == "crude protein" |
                        `Limiting AA` == `Protein Form` |
                        (
                            `Limiting AA` == "methionine+cysteine" &
                                `Protein Form` == "methionine"
                        ) |
                        (
                            `Limiting AA` == "phenylalanine+tyrosine" &
                                `Protein Form` == "phenylalanine"
                        ) | (
                            `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                        ), !is.na(`EAA-9`))
                ) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    portion,
                    `EAA-9`,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                rename("serving size" = "portion") %>%
                rename("EAA-9 (%)" = "EAA-9") %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()

            if(input$show_calc == "score_only"){
                EAA_9 <- EAA_9 %>%
                    select(!calculation)
            }else{
                EAA_9 <- EAA_9 %>%
                    select(!`EAA-9 (%)`) %>%
                    rename("EAA-9 (%)" = "calculation")
            }

            if(length(input$score) != 1){
                PQ_df <- full_join(PQ_df, EAA_9)
            }else{
                PQ_df <- EAA_9
            }

        }

        if (ifelse(length(input$score) != 1,
                   "PDCAAS" %in% input$score,
                   "PDCAAS" == input$score)) {

            PDCAAS <- EAA_composition %>%
                drop_na(Protein) %>%
                drop_na(NI_ID) %>%
                mutate(value = (value) / Protein) %>%
                mutate(value = value * 1000) %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec_PDCAAS) %>%
                        filter(Age == input$rec_age_PDCAAS) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                mutate(value = value / Amount) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       calculation,
                       `Food Composition Ref`)

            temp_2 <- PDCAAS %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ", 1) x bioavailability \n = ",
                        paste0(ifelse(
                            min(round(value, 2)) >= 1, 1, min(round(value, 2))
                        )),
                        " x bioavailability"
                    )
                ) %>%
                ungroup()

            PDCAAS <- PDCAAS %>%
                select(!calculation) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       `Food Composition Ref`) %>%
                group_by(fdcId, NI_ID,  `food description`, Protein, `Food Composition Ref`) %>%
                mutate(PDCAAS = min(value)) %>%
                filter(value == PDCAAS) %>%
                ungroup() %>%
                left_join(temp_2) %>%
                mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       AA,
                       PDCAAS,
                       calculation,
                       `Food Composition Ref`) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(PDCAAS = PDCAAS * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                filter(
                    `Protein Form` == "crude protein" |
                        `Limiting AA` == `Protein Form` |
                        (
                            `Limiting AA` == "methionine+cysteine" &
                                `Protein Form` == "methionine"
                        ) |
                        (
                            `Limiting AA` == "phenylalanine+tyrosine" &
                                `Protein Form` == "phenylalanine"
                        ) | (
                            `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                        )
                ) %>%
                mutate(PDCAAS = round(PDCAAS, 4)) %>%
                mutate(calculation = paste0(calculation, " \n = ", PDCAAS)) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    PDCAAS,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                distinct() %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()


            rm(temp_2)

            if(input$show_calc == "score_only"){
                PDCAAS <- PDCAAS %>%
                    select(!calculation)
            }else{
                PDCAAS <- PDCAAS %>%
                    select(!PDCAAS) %>%
                    rename("PDCAAS" = "calculation")
            }

            if(length(input$score) != 1){
                PQ_df <- full_join(PQ_df, PDCAAS)
            }else{
                PQ_df <- PDCAAS
            }


        }


        if (ifelse(length(input$score) != 1,
                   "DIAAS" %in% input$score,
                   "DIAAS" == input$score)) {

            DIAAS <- EAA_composition %>%
                drop_na(Protein) %>%
                drop_na(NI_ID) %>%
                mutate(value = (value) / Protein) %>%
                mutate(value = value * 1000) %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec_DIAAS) %>%
                        filter(Age == input$rec_age_DIAAS) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                mutate(value = value / Amount) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       calculation,
                       `Food Composition Ref`)

            temp_2 <- DIAAS %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ", 1) x bioavailability \n = ",
                        paste0(ifelse(
                            min(round(value, 2)) >= 1, 1, min(round(value, 2))
                        )),
                        " x bioavailability"
                    )
                ) %>%
                ungroup()

            DIAAS <- DIAAS %>%
                select(!calculation) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       `Food Composition Ref`) %>%
                group_by(fdcId, NI_ID,`food identifier`,  `food description`, Protein, `Food Composition Ref`) %>%
                mutate(DIAAS = min(value)) %>%
                filter(value == DIAAS) %>%
                ungroup() %>%
                left_join(temp_2) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       AA,
                       DIAAS,
                       calculation,
                       `Food Composition Ref`) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(DIAAS = DIAAS * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                filter(
                    `Protein Form` == "crude protein" |
                        `Limiting AA` == `Protein Form` |
                        (
                            `Limiting AA` == "methionine+cysteine" &
                                `Protein Form` == "methionine"
                        ) |
                        (
                            `Limiting AA` == "phenylalanine+tyrosine" &
                                `Protein Form` == "phenylalanine"
                        ) | (
                            `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                        )
                ) %>%
                mutate(DIAAS = round(DIAAS, 4)) %>%
                mutate(calculation = paste0(calculation, " \n = ", DIAAS)) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    DIAAS,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                distinct() %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()


            rm(temp_2)

            if(input$show_calc == "score_only"){
                DIAAS <- DIAAS %>%
                    select(!calculation)
            }else{
                DIAAS <- DIAAS %>%
                    select(!DIAAS) %>%
                    rename("DIAAS" = "calculation")
            }

            if(length(input$score) != 1){
                PQ_df <- full_join(PQ_df, DIAAS)
            }else{
                PQ_df <- DIAAS
            }


        }

        if ("crude protein" %in% input$pq_analyte) {
            if (!("individual amino acids" %in% input$pq_analyte)) {
                PQ_df <- PQ_df %>%
                    filter(`Protein Form` == "crude protein")
            }
        }
        if ("individual amino acids" %in% input$pq_analyte) {
            if (!("crude protein" %in% input$pq_analyte)) {
                PQ_df <- PQ_df %>%
                    filter(`Protein Form` != "crude protein")
            }
        }
        PQ_df <- PQ_df %>%
            arrange(Species, Calculation, `Sample Location`, `Protein Form`, Food) %>%
            filter((`Sample Location` %in% input$pq_sample) | (`Sample Location` == "Not Available" & input$require_bioavail == FALSE)) %>%
            filter(Calculation %in% input$pq_measure| (Calculation == "Not Available" & input$require_bioavail == FALSE)) %>%
            filter(Species %in% input$pq_species| (Species == "Not Available" & input$require_bioavail == FALSE)) %>%
            rename("Bio-avail Species" = "Species") %>%
            rename("Bio-avail Protein Form" = "Protein Form") %>%
            rename("Bio-avail Sample Location" = "Sample Location") %>%
            rename("Bio-avail Calculation" = "Calculation") %>%
            rename("Bioavailability Ref" = "Data Collection Source") %>%
            filter(str_detect(
                NI_ID, ifelse(
                    input$NI_ID_tab1 == "",
                    "(.*?)",
                    paste0("(?i)", input$NI_ID_tab1)
                )
            )) %>%
            filter(str_detect(
                Food, ifelse(
                    input$food_tab1 == "",
                    "(.*?)",
                    paste0("(?i)", input$food_tab1)
                )
            )) %>%
            mutate(across(
                where(is.character), ~ gsub("\n", "<br>", .)
            )) %>%
            relocate(`Bioavailability Ref`, .after = last_col()) %>%
            relocate(`Food Composition Ref`, .after = last_col()) %>%
          arrange(`Bio-avail Species`)

        DT::datatable(
            PQ_df,
            class = "display cell-border compact",
            rownames = FALSE,
            extensions = c('FixedHeader'),
            options = list(
                fixedHeader = TRUE,
                dom = 'ltip',
                pageLength = 25,
                lengthMenu = c(15, 25, 50, 75, 100),
                order = list(list(2, 'asc'), list(3, 'asc'), list(4, 'asc'), list(5, 'asc')),
                columnDefs = list(
                    # list(targets = c(1, 2, 9), className = "dt-head-nowrap"),
                    list(
                        targets = c('Bioavailability Ref','Food Composition Ref'),
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 100 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                            "}"
                        )
                    )
                )
            ),
            escape = FALSE  # Allow HTML rendering
        ) %>%
            formatStyle(1:ncol(PQ_df),
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle(2:7, width = '90px')
    })

    # render table for food mappings
    output$table_3 <- DT::renderDataTable(server = TRUE, {
        fdcmp_df <- EAA_composition %>%
            filter(str_detect(NI_ID, ifelse(
                input$NI_ID_tab3 == "",
                "(.*?)",
                paste0("(?i)", input$NI_ID_tab3)
            ))) %>%
            filter(str_detect(`food description`, ifelse(
                input$food_tab3 == "",
                "(.*?)",
                paste0("(?i)", input$food_tab3)
            ))) %>%
            filter(str_detect(
                `Food Composition Ref`,
                ifelse(
                    input$source_tab3 == "",
                    "(.*?)",
                    paste0("(?i)", input$source_tab3)
                )
            ))


            fdcmp_df <- fdcmp_df %>%
                select(NI_ID,
                       fdcId,
                       `food description`,
                       Protein,
                       AA,
                       value,
                       `Food Composition Ref`) %>%
                mutate("FDC_ID" = fdcId) %>%
                select(NI_ID,
                       FDC_ID,
                       `food description`,
                       Protein,
                       AA,
                       value,
                       `Food Composition Ref`) %>%
                distinct() %>%
                mutate(AA = factor(
                    AA,
                    levels = c(
                        "histidine",
                        "isoleucine",
                        "leucine",
                        "lysine",
                        "methionine+cysteine",
                        "phenylalanine+tyrosine",
                        "threonine",
                        "tryptophan",
                        "valine"
                    ),
                    labels = c(
                        "His (g/100g)",
                        "Ile (g/100g)",
                        "Leu (g/100g)",
                        "Lys (g/100g)",
                        "Met+Cys (g/100g)",
                        "Phe+Tyr (g/100g)",
                        "Thr (g/100g)",
                        "Trp (g/100g)",
                        "Val (g/100g)"
                    )
                )) %>%
                pivot_wider(names_from = "AA", values_from = "value") %>%
                rename("Protein (g/100g)" = "Protein") %>%
                rename("Food description" = "food description") %>%
                distinct() %>%
                relocate(`Food Composition Ref`, .after = last_col())
            if (input$show_NI_ID == FALSE) {
                fdcmp_df <- fdcmp_df %>%
                    select(!NI_ID)
            }

        DT::datatable(
            fdcmp_df ,
            extensions = c('Buttons', 'FixedHeader'),
            class = "display cell-border compact",
            rownames = FALSE,
            options = list(
                fixedHeader = TRUE,
                scrollCollapse = TRUE,
                dom = 'ltip',
                pageLength = 25,
                lengthMenu = c(15, 25, 50, 75, 100),
                columnDefs = list(list(
                    targets = c(ncol(fdcmp_df) - 1),
                    render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 100 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                        "}"
                    )
                ))
            ),
            escape = FALSE  # Allow HTML rendering
        ) %>%
            formatStyle(1:16,
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle((ncol(fdcmp_df) - 10):(ncol(fdcmp_df) - 1), width = '5%') %>%
            formatStyle(1, width = '5%')
    })

    # Content for downloads
    data_PQ <- reactive({
        PQ_df <- data.frame(NI_ID = character())

        if ("EAA-9" %in% input$score) {
            if (input$EAA_rec == "Choose custom recommendations") {
                scoring_pattern[199, ] <-
                    list(
                        "Choose custom recommendations",
                        "histidine",
                        ifelse(is.numeric(input$hist), input$hist, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[200, ] <-
                    list(
                        "Choose custom recommendations",
                        "leucine",
                        ifelse(is.numeric(input$leu), input$leu, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[201, ] <-
                    list(
                        "Choose custom recommendations",
                        "isoleucine",
                        ifelse(is.numeric(input$ile), input$ile, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[202, ] <-
                    list(
                        "Choose custom recommendations",
                        "lysine",
                        ifelse(is.numeric(input$lys), input$lys, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[203, ] <-
                    list(
                        "Choose custom recommendations",
                        "methionine+cysteine",
                        ifelse(is.numeric(input$met_cys), input$met_cys, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[204, ] <-
                    list(
                        "Choose custom recommendations",
                        "phenylalanine+tyrosine",
                        ifelse(is.numeric(input$phe_tyr), input$phe_tyr, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[205, ] <-
                    list(
                        "Choose custom recommendations",
                        "threonine",
                        ifelse(is.numeric(input$thr), input$thr, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[206, ] <-
                    list(
                        "Choose custom recommendations",
                        "tryptophan",
                        ifelse(is.numeric(input$trp), input$trp, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
                scoring_pattern[207, ] <-
                    list(
                        "Choose custom recommendations",
                        "valine",
                        ifelse(is.numeric(input$val), input$val, 1),
                        "mg/kg/d",
                        "custom",
                        NA,
                        NA,
                        NA
                    )
            }
            if(input$require_bioavail == TRUE){
                EAA_composition <- EAA_composition %>%
                    drop_na(NI_ID)
            }

            if (input$serving_size == "Use standard serving sizes") {
                EAA_9 <- EAA_composition %>%
                    left_join(
                        portion_sizes %>%
                            select(fdc_id, g_weight , portion) %>%
                            rename("fdcId" = "fdc_id") %>%
                            mutate(fdcId = as.character(fdcId)) %>%
                            mutate(portion = paste0(
                                portion, " (", g_weight, " g)"
                            ))
                    ) %>%
                    mutate(portion = ifelse(is.na(portion), "not provided (100 g)", portion)) %>%
                    mutate(g_weight = ifelse(is.na(g_weight), 100, g_weight)) %>%
                    mutate(value = value * 1000) %>%
                    mutate(value = value * (g_weight / 100))
            }else{
                EAA_9 <- EAA_composition %>%
                    mutate(value = value * 1000) %>%
                    mutate(value = value * (input$serving_weight / 100)) %>%
                    mutate(portion = paste0(input$serving_weight, " g"))
            }

            EAA_9 <- EAA_9 %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec) %>%
                        filter(
                            ifelse(
                                is.character(input$rec_age),
                                Age == input$rec_age,
                                !is.na(Age)
                            )
                        ) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(
                    round(value, 2),
                    "/",
                    round(Amount * input$weight, 2)
                )) %>%
                mutate(value = value / (Amount * input$weight)) %>%
                select(
                    fdcId,
                    NI_ID,
                    `food identifier`,
                    `food description`,
                    Protein,
                    portion,
                    value,
                    AA,
                    calculation,
                    `Food Composition Ref`
                )

            temp <- EAA_9 %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ") x 100 x bioavailability \n = ",
                        paste0(min(round(
                            value, 2
                        ))),
                        " x 100 x bioavailability"
                    )
                ) %>%
                ungroup()

            EAA_9 <- EAA_9 %>%
                select(!calculation) %>%
                left_join(temp) %>%
                group_by(fdcId,
                         `food identifier`,
                         `food description`,
                         Protein,
                         portion,
                         `Food Composition Ref`) %>%
                mutate(`EAA-9` = min(value)) %>%
                filter(value == `EAA-9`) %>%
                ungroup() %>%
                select(
                    fdcId,
                    NI_ID,
                    `food identifier`,
                    `food description`,
                    Protein,
                    AA,
                    portion,
                    `EAA-9`,
                    calculation,
                    `Food Composition Ref`
                ) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                distinct() %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(`EAA-9` = `EAA-9` * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                mutate(calculation = ifelse(is.na(`Bio-avail  (%)`), gsub(" x bioavailability", "", calculation), calculation)) %>%
                mutate(calculation = paste0(calculation, " \n = ", `EAA-9`, "%")) %>%
                filter(ifelse(!is.na(`Protein Form`),
                              `Protein Form` == "crude protein" |
                                  `Limiting AA` == `Protein Form` |
                                  (
                                      `Limiting AA` == "methionine+cysteine" &
                                          `Protein Form` == "methionine"
                                  ) |
                                  (
                                      `Limiting AA` == "phenylalanine+tyrosine" &
                                          `Protein Form` == "phenylalanine"
                                  ) | (
                                      `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                                  ), !is.na(`EAA-9`))
                ) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    portion,
                    `EAA-9`,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                rename("serving size" = "portion") %>%
                rename("EAA-9 (%)" = "EAA-9") %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()

            if(input$show_calc == "score_only"){
                EAA_9 <- EAA_9 %>%
                    select(!calculation)
            }else{
                EAA_9 <- EAA_9 %>%
                    select(!`EAA-9 (%)`) %>%
                    rename("EAA-9 (%)" = "calculation")
            }
            PQ_df <- full_join(PQ_df, EAA_9)

        }

        if (ifelse(length(input$score != 1),
                   "PDCAAS" %in% input$score,
                   "PDCAAS" == input$score)) {

            PDCAAS <- EAA_composition %>%
                drop_na(Protein) %>%
                drop_na(NI_ID) %>%
                mutate(value = (value) / Protein) %>%
                mutate(value = value * 1000) %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec_PDCAAS) %>%
                        filter(Age == input$rec_age_PDCAAS) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                mutate(value = value / Amount) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       calculation,
                       `Food Composition Ref`)

            temp_2 <- PDCAAS %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ", 1) x bioavailability \n = ",
                        paste0(ifelse(
                            min(round(value, 2)) >= 1, 1, min(round(value, 2))
                        )),
                        " x bioavailability"
                    )
                ) %>%
                ungroup()

            PDCAAS <- PDCAAS %>%
                select(!calculation) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       `Food Composition Ref`) %>%
                group_by(fdcId, NI_ID,  `food description`, Protein, `Food Composition Ref`) %>%
                mutate(PDCAAS = min(value)) %>%
                filter(value == PDCAAS) %>%
                ungroup() %>%
                left_join(temp_2) %>%
                mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       AA,
                       PDCAAS,
                       calculation,
                       `Food Composition Ref`) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(PDCAAS = PDCAAS * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                filter(
                    `Protein Form` == "crude protein" |
                        `Limiting AA` == `Protein Form` |
                        (
                            `Limiting AA` == "methionine+cysteine" &
                                `Protein Form` == "methionine"
                        ) |
                        (
                            `Limiting AA` == "phenylalanine+tyrosine" &
                                `Protein Form` == "phenylalanine"
                        ) | (
                            `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                        )
                ) %>%
                mutate(PDCAAS = round(PDCAAS, 4)) %>%
                mutate(calculation = paste0(calculation, " \n = ", PDCAAS)) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    PDCAAS,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                distinct() %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()


            rm(temp_2)

            if(input$show_calc == "score_only"){
                PDCAAS <- PDCAAS %>%
                    select(!calculation)
            }else{
                PDCAAS <- PDCAAS %>%
                    select(!PDCAAS) %>%
                    rename("PDCAAS" = "calculation")
            }



            PQ_df <- full_join(PQ_df, PDCAAS)
        }


        if (ifelse(length(input$score != 1),
                   "DIAAS" %in% input$score,
                   "DIAAS" == input$score)) {

            DIAAS <- EAA_composition %>%
                drop_na(Protein) %>%
                drop_na(NI_ID) %>%
                mutate(value = (value) / Protein) %>%
                mutate(value = value * 1000) %>%
                left_join(
                    scoring_pattern %>%
                        rename("AA" = "Analyte") %>%
                        filter(`Pattern Name` == input$EAA_rec_DIAAS) %>%
                        filter(Age == input$rec_age_DIAAS) %>%
                        select(AA, Amount)
                ) %>%
                mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                mutate(value = value / Amount) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       calculation,
                       `Food Composition Ref`)

            temp_2 <- DIAAS %>%
                select(fdcId, NI_ID,`food description`, value, calculation) %>%
                group_by(fdcId, NI_ID, `food description`) %>%
                summarise(
                    calculation = paste0(
                        "min(",
                        str_c(calculation, collapse = ", "),
                        ") x bioavailability \n = ",
                        paste0(min(round(value, 2))),
                        " x bioavailability"
                    )
                ) %>%
                ungroup()

            DIAAS <- DIAAS %>%
                select(!calculation) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       value,
                       AA,
                       `Food Composition Ref`) %>%
                group_by(fdcId, NI_ID,  `food description`, Protein, `Food Composition Ref`) %>%
                mutate(DIAAS = min(value)) %>%
                filter(value == DIAAS) %>%
                ungroup() %>%
                left_join(temp_2) %>%
                select(fdcId,
                       NI_ID,
                       `food identifier`,
                       `food description`,
                       Protein,
                       AA,
                       DIAAS,
                       calculation,
                       `Food Composition Ref`) %>%
                rename("Limiting AA" = "AA") %>%
                separate_longer_delim(NI_ID, delim = ";") %>%
                mutate(NI_ID = str_trim(NI_ID)) %>%
                left_join(Protein_Digestibility_Data %>%
                              select(NI_ID, Food, `Bio-avail  (%)`, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              mutate(Food = str_remove(Food, ", Average")) %>%
                              group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                              summarise(`Bio-avail  (%)` = round(sum(`Bio-avail  (%)`, na.rm = TRUE)/n(),2), NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                              separate_longer_delim(NI_ID, delim = "; ") %>%
                              distinct()) %>%
                replace_na(list("Data Collection Source" = "Not Available")) %>%
                mutate(indicator = ifelse(`Data Collection Source` == `Food Composition Ref`, 1, ifelse(str_detect(`Food Composition Ref`, "Standard Reference"), 2, 3))) %>%
                filter(!(Calculation == "metabolic availability" & !str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
                group_by(`food identifier`) %>%
                mutate(min_indicator = ifelse(`Data Collection Source` == "Not Available", 2, min(indicator, na.rm = TRUE))) %>%
                ungroup() %>%
                filter(indicator == min_indicator ) %>%
                select(!indicator) %>%
                select(!min_indicator) %>%
                mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
                group_by(Food, Species, `Protein Form`, `Sample Location`, Calculation, `Data Collection Source`) %>%
                mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
                distinct() %>%
                mutate(DIAAS = DIAAS * ifelse(!is.na(`Bio-avail  (%)`), (as.numeric(`Bio-avail  (%)`) / 100), 1)) %>%
                filter(
                    `Protein Form` == "crude protein" |
                        `Limiting AA` == `Protein Form` |
                        (
                            `Limiting AA` == "methionine+cysteine" &
                                `Protein Form` == "methionine"
                        ) |
                        (
                            `Limiting AA` == "phenylalanine+tyrosine" &
                                `Protein Form` == "phenylalanine"
                        ) | (
                            `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
                        )
                ) %>%
                mutate(DIAAS = round(DIAAS, 4)) %>%
                mutate(calculation = paste0(calculation, " \n = ", DIAAS)) %>%
                select(
                    NI_ID,
                    Food,
                    Species,
                    `Sample Location`,
                    `Protein Form`,
                    Calculation,
                    `Bio-avail  (%)`,
                    `Limiting AA`,
                    fdcId,
                    DIAAS,
                    calculation,
                    `Food Composition Ref`,
                    `Data Collection Source`
                ) %>%
                distinct() %>%
                mutate(`Bio-avail  (%)` = as.character(`Bio-avail  (%)`)) %>%
                replace_na(list(Calculation = "Not Available", `Sample Location` = "Not Available", `Protein Form` = "Not Available", Species = "Not Available", `Bio-avail  (%)` = "Not Available", `Data Collection Source` = "Not Available")) %>%
                distinct()


            rm(temp_2)

            if(input$show_calc == "score_only"){
                DIAAS <- DIAAS %>%
                    select(!calculation)
            }else{
                DIAAS <- DIAAS %>%
                    select(!DIAAS) %>%
                    rename("DIAAS" = "calculation")
            }



            PQ_df <- full_join(PQ_df, DIAAS)
        }

        if ("crude protein" %in% input$pq_analyte) {
            if (!("individual amino acids" %in% input$pq_analyte)) {
                PQ_df <- PQ_df %>%
                    filter(`Protein Form` == "crude protein")
            }
        }
        if ("individual amino acids" %in% input$pq_analyte) {
            if (!("crude protein" %in% input$pq_analyte)) {
                PQ_df <- PQ_df %>%
                    filter(`Protein Form` != "crude protein")
            }
        }
        PQ_df %>%
            arrange(Species, Calculation, `Sample Location`, `Protein Form`, Food) %>%
            filter((`Sample Location` %in% input$pq_sample) | (`Sample Location` == "Not Available" & input$require_bioavail == FALSE)) %>%
            filter(Calculation %in% input$pq_measure| (Calculation == "Not Available" & input$require_bioavail == FALSE)) %>%
            filter(Species %in% input$pq_species| (Species == "Not Available" & input$require_bioavail == FALSE)) %>%
            rename("Bio-avail Species" = "Species") %>%
            rename("Bio-avail Protein Form" = "Protein Form") %>%
            rename("Bio-avail Sample Location" = "Sample Location") %>%
            rename("Bio-avail Calculation" = "Calculation") %>%
            rename("Bioavailability Ref" = "Data Collection Source") %>%
            filter(str_detect(
                NI_ID, ifelse(
                    input$NI_ID_tab1 == "",
                    "(.*?)",
                    paste0("(?i)", input$NI_ID_tab1)
                )
            )) %>%
            filter(str_detect(
                Food, ifelse(
                    input$food_tab1 == "",
                    "(.*?)",
                    paste0("(?i)", input$food_tab1)
                )
            )) %>%
            relocate(`Bioavailability Ref`, .after = last_col()) %>%
            relocate(`Food Composition Ref`, .after = last_col())
    })
    data_bv <- reactive({
        Protein_Digestibility_Data %>%
            mutate(n = as.character(n)) %>%
            mutate(`Bio-avail SD` = as.character(`Bio-avail SD`)) %>%
            mutate(`Protein (g)` = as.character(`Protein (g)`)) %>%
            replace_na(
                list(
                    Food = "not reported",
                    "Protein (g)" = "not reported",
                    n = "not reported",
                    "Bio-avail SD" = "not reported",
                    "Analysis method(s)" = "not reported",
                    "Original Data Source(s)" = "not reported"
                )
            ) %>%
            filter(`Species` %in% input$species) %>%
            filter(`Model` %in% input$model) %>%
            filter(`Sample Location` %in% input$sample) %>%
            filter(`Protein Form` %in% input$analyte) %>%
            filter(Calculation %in% input$measure) %>%
            filter(str_detect(
                NI_ID, ifelse(
                    input$NI_ID_tab2 == "",
                    "(.*?)",
                    paste0("(?i)", input$NI_ID_tab2)
                )
            )) %>%
            filter(str_detect(
                Food, ifelse(
                    input$food_tab2 == "",
                    "(.*?)",
                    paste0("(?i)", input$food_tab2)
                )
            )) %>%
            filter(str_detect(
                `Analysis method(s)`,
                ifelse(
                    input$analysisMethod_tab2 == "",
                    "(.*?)",
                    paste0("(?i)", input$analysisMethod_tab2)
                )
            )) %>%
            filter(
                str_detect(
                    `Data Collection Source`,
                    ifelse(
                        input$source_tab2 == "",
                        "(.*?)",
                        paste0("(?i)", input$source_tab2)
                    )
                ) |
                    str_detect(
                        `Original Data Source(s)`,
                        ifelse(
                            input$source_tab2 == "",
                            "(.*?)",
                            paste0("(?i)", input$source_tab2)
                        )
                    )
            )
    })
    data_eaa <- reactive({fdcmp_df <- EAA_composition %>%
        filter(str_detect(NI_ID, ifelse(
            input$NI_ID_tab3 == "",
            "(.*?)",
            paste0("(?i)", input$NI_ID_tab3)
        ))) %>%
        filter(str_detect(`food description`, ifelse(
            input$food_tab3 == "",
            "(.*?)",
            paste0("(?i)", input$food_tab3)
        ))) %>%
        filter(str_detect(
            `Food Composition Ref`,
            ifelse(
                input$source_tab3 == "",
                "(.*?)",
                paste0("(?i)", input$source_tab3)
            )
        ))

    if (input$show_NI_ID == FALSE) {
        fdcmp_df <- fdcmp_df %>%
            select(fdcId,
                   `food description`,
                   Protein,
                   AA,
                   value,
                   `Food Composition Ref`) %>%
            mutate("FDC_ID" = fdcId) %>%
            select(FDC_ID,
                   `food description`,
                   Protein,
                   AA,
                   value,
                   `Food Composition Ref`) %>%
            distinct() %>%
            mutate(AA = factor(
                AA,
                levels = c(
                    "histidine",
                    "isoleucine",
                    "leucine",
                    "lysine",
                    "methionine+cysteine",
                    "phenylalanine+tyrosine",
                    "threonine",
                    "tryptophan",
                    "valine"
                ),
                labels = c(
                    "His (g/100g)",
                    "Ile (g/100g)",
                    "Leu (g/100g)",
                    "Lys (g/100g)",
                    "Met+Cys (g/100g)",
                    "Phe+Tyr (g/100g)",
                    "Thr (g/100g)",
                    "Trp (g/100g)",
                    "Val (g/100g)"
                )
            )) %>%
            pivot_wider(names_from = "AA", values_from = "value") %>%
            rename("Protein (g/100g)" = "Protein") %>%
            rename("Food description" = "food description") %>%
            distinct() %>%
            relocate(`Food Composition Ref`, .after = last_col())
    } else{
        fdcmp_df <- EAA_composition %>%
            select(NI_ID, `food description`, fdcId, Protein, AA, value) %>%
            mutate("FDC_ID" = fdcId) %>%
            mutate(NI_ID = str_replace_all(NI_ID, ";", "; ")) %>%
            select(NI_ID, FDC_ID, `food description`, Protein, AA, value) %>%
            distinct() %>%
            mutate(AA = factor(
                AA,
                levels = c(
                    "histidine",
                    "isoleucine",
                    "leucine",
                    "lysine",
                    "methionine+cysteine",
                    "phenylalanine+tyrosine",
                    "threonine",
                    "tryptophan",
                    "valine"
                ),
                labels = c(
                    "His (g/100g)",
                    "Ile (g/100g)",
                    "Leu (g/100g)",
                    "Lys (g/100g)",
                    "Met+Cys (g/100g)",
                    "Phe+Tyr (g/100g)",
                    "Thr (g/100g)",
                    "Trp (g/100g)",
                    "Val (g/100g)"
                )
            )) %>%
            pivot_wider(names_from = "AA", values_from = "value") %>%
            rename("Protein (g/100g)" = "Protein") %>%
            rename("Food description" = "food description") %>%
            distinct() %>%
            relocate(`Food Composition Ref`, .after = last_col())
    }
    fdcmp_df
    })

    # CSV Download Handler
    output$download_csv_PQ <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - Protein Quality Scoring -", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data_PQ(), file, row.names = FALSE)
        }
    )

    output$download_csv_eaa <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - EAA Composition -", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data_eaa(), file, row.names = FALSE)
        }
    )

    output$download_csv_bv <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - Bioavailability -", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data_bv(), file, row.names = FALSE)
        }
    )

    # Excel Download Handler
    output$download_excel_PQ <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - Protein Quality Scoring -", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(data_PQ(), file)
        }
    )

    output$download_excel_bv <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - Bioavailability -", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(data_bv(), file)
        }
    )

    output$download_excel_eaa <- downloadHandler(
        filename = function() {
            paste("Protein Quality Hub - EAA Composition -", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(data_eaa(), file)
        }
    )

}

shinyApp( ui, server)
