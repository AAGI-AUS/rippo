#' A Shiny web application for building, editing and saving IPPO registries
#'
#' @author Braden Thorne, \email{braden.thorne@@curtin.edu.au}

library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(openxlsx)

gen_BIP_incorporated <- function(OUT, USED_IN, BIP){
  out_string <- rep("", nrow(OUT))
  full_df <- OUT |> 
    left_join(USED_IN, by = join_by(OUT_Name)) |> 
    left_join(BIP, by = join_by(BIP_Name))
  for (i in 1:nrow(OUT)){
    t1_nums <- which(BIP[BIP$Table1, "BIP_Name"] %in% USED_IN[USED_IN$OUT_Name == OUT[i,"OUT_Name"], "BIP_Name"])
    t2_nums <- which(BIP[BIP$Table2, "BIP_Name"] %in% USED_IN[USED_IN$OUT_Name == OUT[i,"OUT_Name"], "BIP_Name"])
    t3_nums <- which(BIP[BIP$Table3, "BIP_Name"] %in% USED_IN[USED_IN$OUT_Name == OUT[i,"OUT_Name"], "BIP_Name"])
    out_string[i] <- paste(
      if(length(t1_nums)>0){paste("Table 1, Lines", paste(t1_nums, collapse=","))}else{""},
      if(length(t2_nums)>0){paste("Table 2, Lines", paste(t2_nums, collapse=","))}else{""},
      if(length(t3_nums)>0){paste("Table 3, Lines", paste(t3_nums, collapse=","))}else{""},
      sep="\n"
    )
  }
  return(out_string)
}

send_to_excel <- function(BIP, OUT, USED_IN, SEN, save_path){
  xl_file <- loadWorkbook("App_backend/template.xlsx", isUnzipped = FALSE)
  
  if (nrow(BIP |> filter(Table1))>0){
    writeData(xl_file, 2, 
              BIP |> 
                filter(Table1) %>%
                mutate(No=c(1:nrow(.))) |> 
                select(`No`, `BIP_Code`, `BIP_Description`, `BIP_Date`, `BIP_Licence`),
              startRow = 2,
              colNames=F
    )
  }
  
  if (nrow(BIP |> filter(Table2))>0){
    writeData(xl_file, 3, 
              BIP |> 
                filter(Table2) %>%
                mutate(No=c(1:nrow(.))) |> 
                select(`No`, `BIP_Code`, `BIP_Description`, `BIP_Date`, `BIP_Licence`),
              startRow = 2,
              colNames=F
    )
  }
  
  if (nrow(BIP |> filter(Table3))>0){
    writeData(xl_file, 4, 
              BIP |> 
                filter(Table3) %>%
                mutate(No=c(1:nrow(.))) |> 
                select(`No`, `BIP_Code`, `BIP_Owner`, `BIP_Description`, `BIP_Date`, `BIP_Provider`, `BIP_Licence`, `BIP_Restrictions`),
              startRow = 2,
              colNames=F
    )
  }
  
  if (nrow(OUT)>0){
    adjusted_OUT <-  OUT %>% 
      mutate(No=c(1:nrow(.)), BIP_inc = gen_BIP_incorporated(OUT, USED_IN, BIP))
    writeData(xl_file, 5,
              adjusted_OUT |> 
                select(`No`, `OUT_Code`, `OUT_Description`, `OUT_Date`, `BIP_inc`, `OUT_Licence`),
              startRow = 2,
              colNames=F
    )
  }
  
  if (nrow(SEN)>0){
    writeData(xl_file, 6,
              SEN |> 
                left_join(adjusted_OUT, by = join_by(OUT_Name)) |> 
                select(`No`, `SEN_Name`, `OUT_Description`, `OUT_Date`, `BIP_inc`, `OUT_Licence`),
              startRow = 2,
              colNames=F
    )
  }
  
  saveWorkbook(xl_file, save_path, overwrite = FALSE)
}

get_bip_list <- function(curr_data){
  output_list <- list()
  output_list[curr_data$Name] <- curr_data$Name
  return(output_list)
}

get_output_list <- function(out_data){
  output_list <- list()
  output_list[out_data$Name] <- out_data$No
  return(output_list)
}

gen_BIP_entry <- function(i1, i2, i3){
  return(paste(
    if(length(i1)>0){paste("Table 1, Lines", paste(i1, collapse=","))},
    if(length(i2)>0){paste("Table 2, Lines", paste(i2, collapse=","))},
    if(length(i3)>0){paste("Table 3, Lines", paste(i3, collapse=","))},
    sep="\n"
  ))
}

update_BIP_entry <- function(i1, i2, i3, t1, t2, t3, string){
  t1_numbers <- numeric()
  t2_numbers <- numeric()
  t3_numbers <- numeric()
  for (i in strsplit(string, "\n")[[1]]){
    if (strsplit(i, " ")[[1]][2]=="1,"){
      t1_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t1_numbers <- t1$No[t1$Description %in% i1[t1_init_numbers]]
    } else if (strsplit(i, " ")[[1]][2]=="2,"){
      t2_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t2_numbers <- t2$No[t2$Description %in% i2[t2_init_numbers]]
    } else if (strsplit(i, " ")[[1]][2]=="3,"){
      t3_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t3_numbers <- t3$No[t3$Description %in% i3[t3_init_numbers]]
    }
  }
  
  return(paste(
    if(length(t1_numbers)>0){paste("Table 1, Lines", paste(t1_numbers, collapse=","))},
    if(length(t2_numbers)>0){paste("Table 2, Lines", paste(t2_numbers, collapse=","))},
    if(length(t3_numbers)>0){paste("Table 3, Lines", paste(t3_numbers, collapse=","))},
    sep="\n"
  ))
}

update_BIP_links <- function(i1, i2, i3, t1, t2, t3, string, tab_out){
  t1_numbers <- numeric()
  t2_numbers <- numeric()
  t3_numbers <- numeric()
  for (i in strsplit(string, "\n")[[1]]){
    if (strsplit(i, " ")[[1]][2]=="1," & tab_out==1){
      t1_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t1_numbers <- t1$No[t1$Description %in% i1[t1_init_numbers]]
      return(paste(t1_numbers, collapse=","))
    } else if (strsplit(i, " ")[[1]][2]=="2," & tab_out==2){
      t2_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t2_numbers <- t2$No[t2$Description %in% i2[t2_init_numbers]]
      return(paste(t2_numbers, collapse=","))
    } else if (strsplit(i, " ")[[1]][2]=="3," & tab_out==3){
      t3_init_numbers <- as.numeric(strsplit(
        strsplit(i, " ")[[1]][4], ","
      )[[1]])
      t3_numbers <- t3$No[t3$Description %in% i3[t3_init_numbers]]
      return(paste(t3_numbers, collapse=","))
    }
  }
}

### Base user interface -----
ui <- page_sidebar(
  title = "IPPO Registry Builder",
  sidebar = sidebar(
    helpText(
      "Create IPPO registries by entering information into this app."
    ),
    textInput(
      "load_path",
      label="Enter a path to an existing IPPO to import its entries.",
      value="App_backend/example.xlsx"
    ),
    actionButton(
      "load_data",
      label="Load IPPO"
    ),
    textInput(
      "code",
      label = "Enter project code:",
      value = "XXXX-XXXX"
    ),
    dateInput(
      "date",
      label = "Select project commencement date:",
      value = "2025-01-01"
    ),
    selectInput(
      "sec",
      label = "Choose a section to work on:",
      choices = c(
        "1-3 Background IP",
        "4 Project Output",
        "5 Project Outputs to 3rd Party",
        "Edit existing entries"
      ),
      selected = "1-3 Background IP"
    ),
    actionButton(
      "save_data",
      label="Save Excel"
    )#,
    # actionButton(
    #   "debug",
    #   label="View databases"
    # )
  ),
  uiOutput("input_ui")
)

###-----
### Server logic -----
server <- function(input, output, session) {
  
  ### Load and setup database -----
  db_REF <- read.xlsx("App_backend/Existing_records.xlsx") |> 
    arrange(tolower(Description))
  
  db_BIP <- reactiveVal(data.frame(
    Table1 = logical(),
    Table2 = logical(),
    Table3 = logical(),
    BIP_Name = character(),
    BIP_Code = character(),
    BIP_Description = character(),
    BIP_Date = as.Date(integer()),
    BIP_Licence = character(),
    BIP_Owner = character(),
    BIP_Provider = character(),
    BIP_Restrictions = character()
  ))
  
  db_OUT <- reactiveVal(data.frame(
    OUT_Name = character(),
    OUT_Code = character(),
    OUT_Description = character(),
    OUT_Date = as.Date(integer()),
    OUT_Licence = character()
  ))
  
  db_USED_IN <- reactiveVal(data.frame(
    OUT_Name = character(),
    BIP_Name = character()
  ))
  
  db_SEN <- reactiveVal(data.frame(
    OUT_Name = character(),
    SEN_Name = character()
  ))
  
  ### Responsive UI elements -----
  
  conditional_bip_new <- reactive({layout_columns(card(layout_columns(
    textInput(
      "bip_name",
      label = "Enter a name to refer to the BIP (this will not be published in the final table)."
    ),
    textAreaInput(
      "description",
      label = "Enter a description of the IP."
    ),
    textInput(
      "bip_code",
      label = "Enter the relevant IP reference code.",
      value = "AAGI-ALL-SP-003"
    ),
    textAreaInput(
      "licence",
      label = "Enter any restrictions/limitations on the dissemination or commercialisation of the project's outputs as a result of this IP."
    ),
    dateInput(
      "date_available",
      label = "Enter the date that the IP was made available.",
      value = input$date
    ),
    conditionalPanel(
      condition="input.radio==3",
      layout_columns(
        textAreaInput(
          "owner",
          label = "Enter the owner of the IP"
        ),
        textAreaInput(
          "provider",
          label = "Enter IP provider (if not the owner)"
        )
      )
    ), col_widths = 12)),
    actionButton("add_entry_bip", label="Add Entry to IPPO"),
    col_widths = 12
  )}) 
  
  conditional_bip_existing <- reactive({
    layout_columns(
      conditionalPanel(
        condition="input.radio==1",
        card(checkboxGroupInput(
          "existingBoxes_t1",
          "Select all entries to add",
          choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table1,"Name"],
          choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table1,"Name"]
        ))
      ),
      conditionalPanel(
        condition="input.radio==2",
        card(checkboxGroupInput(
          "existingBoxes_t2",
          "Select all entries to add",
          choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table2,"Name"],
          choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table2,"Name"]
        ))
      ),
      conditionalPanel(
        condition="input.radio==3",
        card("Select all entries to add",
             layout_columns(
               checkboxGroupInput(
                 "existingBoxes_t3_1",
                 "R",
                 choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="R","Name"],
                 choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="R","Name"]
               ),
               checkboxGroupInput(
                 "existingBoxes_t3_2",
                 "Python",
                 choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Python","Name"],
                 choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Python","Name"]
               ),
               checkboxGroupInput(
                 "existingBoxes_t3_3",
                 "Julia",
                 choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Julia","Name"],
                 choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Julia","Name"]
               ),
               checkboxGroupInput(
                 "existingBoxes_t3_4",
                 "Other",
                 choiceNames = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Other","Name"],
                 choiceValues = db_REF[!(db_REF$Name %in% db_BIP()[["BIP_Name"]]) & db_REF$Table3 & db_REF$Category=="Other","Name"]
               ),
               col_widths = 6
             ))
      ),
      actionButton("add_entry_existing", label="Add Entries to IPPO"),
      col_widths = 12
    )})
  
  conditional_bip <- reactive({
    layout_columns(
      conditionalPanel(
        condition="input.radio_2==0",
        conditional_bip_new()
      ),
      conditionalPanel(
        condition="input.radio_2==1",
        conditional_bip_existing()
      ),
      col_widths = 12
    )})
  
  conditional_output <- reactive({card(layout_columns(
    textInput(
      "output_name",
      label = "Enter a name to refer to the output (this will not be published in the final table)."
    ),
    textAreaInput(
      "description_output",
      label = "Enter output description"
    ),
    #### TO BE ADDED!!
    # "Is there a derivative report/presentation/other output generated from this work? If so, you can select the checkbox below and add a leading phrase. An additional entry will be added for that output.",
    # layout_columns(
    #   checkboxInput(
    #     "add_report",
    #     label="Derivative output?",
    #     value=FALSE
    #   ),
    #   textAreaInput(
    #     "add_leading",
    #     label=NULL,
    #     value = "Report corresponding to"
    #   ),
    #   col_widths=12
    # ),
    dateInput(
      "date_created",
      label = "Enter date that output was created",
      value = input$date
    ),
    textAreaInput(
      "licence_output",
      label = "Enter restrictions/limitations on dissemination or commercialisation of project outputs",
      value = "Refer to the corresponding project Analytical Collaboration Plan for limits to use for the parties and purposes listed therein."
    ),
    "Select the relevant background IP for this output:",
    checkboxGroupInput(
      "bip_boxes_t1",
      "Curtin",
      choiceNames = db_BIP()[db_BIP()[["Table1"]], "BIP_Name"],
      choiceValues = db_BIP()[db_BIP()[["Table1"]], "BIP_Name"]
    ),
    checkboxGroupInput(
      "bip_boxes_t2",
      "GRDC",
      choiceNames = db_BIP()[db_BIP()[["Table2"]], "BIP_Name"],
      choiceValues = db_BIP()[db_BIP()[["Table2"]], "BIP_Name"]
    ),
    checkboxGroupInput(
      "bip_boxes_t3",
      "Other",
      choiceNames = db_BIP()[db_BIP()[["Table3"]], "BIP_Name"],
      choiceValues = db_BIP()[db_BIP()[["Table3"]], "BIP_Name"]
    ),
    actionButton("add_output", label="Add Entry to IPPO"),
    col_widths = 12
  ))})
  
  conditional_correspondance <- reactive({if(nrow(db_OUT())>0){card(
    radioButtons(
      "output_radio",
      "Select the output that was sent",
      choiceNames = db_OUT()[["OUT_Name"]],
      choiceValues = db_OUT()[["OUT_Name"]]
    ),
    textAreaInput(
      "recipient",
      label = "Who was it sent to?"
    ),
    actionButton("add_recipient", label="Add Entry to IPPO")
  )} else {
    "Please add an output before adding external recipients."
  }})
  
  conditional_edit_BIP <- reactive({
    if (nrow(db_BIP()) > 0) {
      layout_columns(
        card(layout_columns(
          radioButtons(
            "radio_edit_BIP",
            "Select an entry to edit",
            choiceNames = db_BIP()[["BIP_Name"]],
            choiceValues = db_BIP()[["BIP_Name"]],
            selected = db_BIP()[["BIP_Name"]][1]
          ),
          actionButton(
            "delete_entry_BIP",
            label = "Delete entry"
          ),
          col_widths = 12
        )),
        card(
          checkboxGroupInput(
            "bip_update_tables",
            "Select which tables IP is included in.",
            choiceNames = c("Curtin", "GRDC", "Other"),
            choiceValues = c("A", "B", "C"),
            selected = c("A", "B", "C")[which(as.logical(db_BIP()[1,1:3]))]
          ),
          textAreaInput(
            "description_bip_update",
            label = "Enter a description of the IP.",
            value = db_BIP()[["BIP_Description"]][1]
          ),
          textInput(
            "bip_code_update",
            label = "Enter the relevant IP reference code.",
            value = db_BIP()[["BIP_Code"]][1]
          ),
          dateInput(
            "date_bip_update",
            label = "Enter the date that the IP was made available.",
            value = db_BIP()[["BIP_Date"]][1]
          ),
          textAreaInput(
            "licence_bip_update",
            label = "Enter restrictions/limitations on dissemination or commercialisation of project outputs as a result of this IP.",
            value = db_BIP()[["BIP_Licence"]][1]
          ),
          conditionalPanel(
            condition="input.bip_update_tables.indexOf('C') > -1",
            layout_columns(
              textAreaInput(
                "owner_bip_update",
                label = "Enter the owner of the IP",
                value = db_BIP()[["BIP_Owner"]][1]
              ),
              textAreaInput(
                "provider_bip_update",
                label = "Enter IP provider (if not the owner)",
                value = db_BIP()[["BIP_Provider"]][1]
              )
            )
          ),
          actionButton(
            "update_entry_BIP",
            label = "Update entry"
          )
        ),
        col_widths=12
      )
    } else {
      "No entries to edit!"
    }
  })
  
  conditional_edit_outputs <- reactive({
    if (nrow(db_OUT()) > 0) {
      layout_columns(
        card(layout_columns(
          radioButtons(
            "radio_edit_OUT",
            "Select an entry to edit",
            choiceNames = db_OUT()[["OUT_Name"]],
            choiceValues = db_OUT()[["OUT_Name"]],
            selected = db_OUT()[["OUT_Name"]][1]
          ),
          actionButton(
            "delete_entry_OUT",
            label = "Delete entry"
          ),
          col_widths = 12
        )),
        card(
          textAreaInput(
            "description_output_update",
            label = "Enter output description",
            value = db_OUT()[["OUT_Description"]][1]
          ),
          dateInput(
            "date_created_update",
            label = "Enter date that output was created",
            value = db_OUT()[["OUT_Date"]][1]
          ),
          textAreaInput(
            "licence_output_update",
            label = "Enter restrictions/limitations on dissemination or commercialisation of project outputs",
            value = db_OUT()[["OUT_Licence"]][1]
          ),
          "Select the relevant background IP for this output:",
          checkboxGroupInput(
            "bip_boxes_t1_update",
            "Curtin",
            choiceNames = db_BIP()[db_BIP()[["Table1"]], "BIP_Name"],
            choiceValues = db_BIP()[db_BIP()[["Table1"]], "BIP_Name"],
            selected = db_BIP()[db_BIP()[["Table1"]]][["BIP_Name"]][1]
          ),
          checkboxGroupInput(
            "bip_boxes_t2_update",
            "GRDC",
            choiceNames = db_BIP()[db_BIP()[["Table2"]], "BIP_Name"],
            choiceValues = db_BIP()[db_BIP()[["Table2"]], "BIP_Name"],
            selected = db_BIP()[db_BIP()[["Table2"]]][["BIP_Name"]][1]
          ),
          checkboxGroupInput(
            "bip_boxes_t3_update",
            "Other",
            choiceNames = db_BIP()[db_BIP()[["Table3"]], "BIP_Name"],
            choiceValues = db_BIP()[db_BIP()[["Table3"]], "BIP_Name"],
            selected = db_BIP()[db_BIP()[["Table3"]]][["BIP_Name"]][1]
          ),
          actionButton(
            "update_entry_OUT",
            label = "Update entry"
          )
        ),
        col_widths=12
      )
      
    } else {
      "No entries to edit!"
    }
  })
  
  conditional_edit_correspondance <- reactive({
    if (nrow(db_SEN()) > 0) {
        layout_columns(
          radioButtons(
            "radio_edit_SEN",
            "Select an entry to edit",
            choiceNames = paste(db_SEN()[["OUT_Name"]], db_SEN()[["SEN_Name"]], sep=" -> "),
            choiceValues = c(1:nrow(db_SEN())),
            selected = 1
          ),
          actionButton(
            "delete_entry_SEN",
            label = "Delete entry"
          ),
          col_widths = 12
        )
    } else {
      "No entries to edit!"
    }
  })
  
  conditional_editing <- reactive({
    layout_columns(
      conditionalPanel(
        condition="input.edit_radio==3",
        conditional_edit_correspondance()
      ),
      conditionalPanel(
        condition="input.edit_radio==2",
        conditional_edit_outputs()
      ),
      conditionalPanel(
        condition="input.edit_radio==1",
        conditional_edit_BIP()
      ),
      col_widths = 12
    )
  })
  
  output$input_ui <- renderUI({
    
    if (input$sec=="1-3 Background IP") {
      
      ### Background IP UI -----
      layout_columns(
        "
          Enter any IP that comes from a group external to AAGI West.\n
          This is categorised as coming from Curtin, from the GRDC, or from other sources.
        ",
        ### Default header card with options
        card({
          layout_columns(
            radioButtons(
              "radio",
              "Select option",
              choices = list("Curtin" = 1, "GRDC" = 2, "Other" = 3),
              selected = 1
            ),
            radioButtons(
              "radio_2",
              "Search existing records?",
              choices = list("Yes" = 1, "No" = 0),
              selected = 0
            )
          )
        }),
        ### Conditional cards
        conditional_bip(),
        col_widths = 12
      )
      
    } else if (input$sec=="4 Project Output") {
      
      ### Output IP UI -----
      layout_columns(
        "
          Enter any outputs this project has produced.\n
          This includes trial designs, analysis reports, script files used for analysis, graphs requested for presentations, etc.
          In general anything produced as part of this project, regardless of if it was shared, should be entered here.
        ",
        conditional_output(),
        col_widths = 12
      )
    } else if (input$sec=="5 Project Outputs to 3rd Party") {
      
      ### Distribution of Outputs UI -----
      layout_columns(
        "
          Enter any outputs that were shared with a third party.\n
          These should match to an entry in \"4 Project Outpts\".
        ",
        conditional_correspondance(),
        col_widths = 12
      )
    } else {
      
      ### Editing Entries UI -----
      layout_columns(
        "
          Select an existing record in order to edit it.
        ",
        card({layout_columns(
          radioButtons(
            "edit_radio",
            "Select option",
            choices = list("BIP" = 1, "Outputs" = 2, "Correspondance" = 3),
            selected = 1
          ),
          checkboxInput(
            "allow_deletions",
            label="Allow deletions?",
            value=F
          ),
          col_widths = c(6,6)
        )}),
        conditional_editing(),
        col_widths = 12
      )
    }
  })
  
  ### Button actions -----
  observeEvent(input$add_entry_bip, {
    if (input$radio == 1) {
      db_BIP(db_BIP() |> add_row(
        Table1 = T, Table2 = F, Table3 = F,
        BIP_Name = input$bip_name,
        BIP_Code = input$bip_code,
        BIP_Description = input$description,
        BIP_Date = input$date_available,
        BIP_Licence = input$licence,
        BIP_Owner = "",
        BIP_Provider = "",
        BIP_Restrictions = input$licence
      ))
    } else if (input$radio == 2) {
      db_BIP(db_BIP() |> add_row(
        Table1 = F, Table2 = T, Table3 = F,
        BIP_Name = input$bip_name,
        BIP_Code = input$bip_code,
        BIP_Description = input$description,
        BIP_Date = input$date_available,
        BIP_Licence = input$licence,
        BIP_Owner = "",
        BIP_Provider = "",
        BIP_Restrictions = ""
      ))
    } else if (input$radio==3) {
      db_BIP(db_BIP() |> add_row(
        Table1 = F, Table2 = F, Table3 = T,
        BIP_Name = input$bip_name,
        BIP_Code = input$bip_code,
        BIP_Description = input$description,
        BIP_Date = input$date_available,
        BIP_Licence = input$licence,
        BIP_Owner = input$owner,
        BIP_Provider = input$provider,
        BIP_Restrictions = input$licence
      ))
    }
  })
  
  observeEvent(input$save_data, {
    if (file.exists(paste("Outputs/AAGI-CU-", input$code, " - Support and Services IPPO Registrer.xlsx", sep=""))){
      sendSweetAlert(
        session = session,
        title = "Failure",
        text = "File for the provided project code already exists. Please move or delete the file before saving.",
        type = "error"
      )
    } else{
      send_to_excel(
        db_BIP(), db_OUT(), db_USED_IN(), db_SEN(),
        paste("Outputs/AAGI-CU-", input$code, " - Support and Services IPPO Registrer.xlsx", sep="")
      )
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "File saved successfully!",
        type = "success"
      )
    }
    
  })
  
  observeEvent(input$add_entry_existing, {
    if (input$radio == 1) {
      for (entry in input$existingBoxes_t1){
        curr_selection <- db_REF[db_REF$Name == entry,]
        db_BIP(db_BIP() |> add_row(
          Table1 = T, Table2 = curr_selection$Table2, Table3 = F,
          BIP_Name = curr_selection$Name,
          BIP_Code = curr_selection$Code,
          BIP_Description = curr_selection$Description,
          BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
          BIP_Licence = curr_selection$Restrictions,
          BIP_Owner = "",
          BIP_Provider = "",
          BIP_Restrictions = curr_selection$Restrictions
        ))
      }
    } else if (input$radio == 2) {
      for (entry in input$existingBoxes_t2){
        curr_selection <- db_REF[db_REF$Name == entry,]
        db_BIP(db_BIP() |> add_row(
          Table1 = curr_selection$Table1, Table2 = T, Table3 = F,
          BIP_Name = curr_selection$Name,
          BIP_Code = curr_selection$Code,
          BIP_Description = curr_selection$Description,
          BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
          BIP_Licence = curr_selection$Restrictions,
          BIP_Owner = "",
          BIP_Provider = "",
          BIP_Restrictions = curr_selection$Restrictions
        ))
      }
    } else if (input$radio==3) {
      for (entry in c(input$existingBoxes_t3_1, input$existingBoxes_t3_2, input$existingBoxes_t3_3, input$existingBoxes_t3_4)){
        curr_selection <- db_REF[db_REF$Name == entry,]
        db_BIP(db_BIP() |> add_row(
          Table1 = F, Table2 = F, Table3 = T,
          BIP_Name = curr_selection$Name,
          BIP_Code = curr_selection$Code,
          BIP_Description = curr_selection$Description,
          BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
          BIP_Licence = curr_selection$Arrangements,
          BIP_Owner = curr_selection$Owner,
          BIP_Provider = "",
          BIP_Restrictions = curr_selection$Restrictions
        ))
      }
    }
    
    
  })
  
  observeEvent(input$add_output, {
    db_OUT(db_OUT() |> add_row(
      OUT_Name = input$output_name,
      OUT_Code = "AAGI-ALL-SP-003", # Universal until otherwise stated.
      OUT_Description = input$description_output,
      OUT_Date = input$date_created,
      OUT_Licence = input$licence_output
    ))
    
    for (entry in unique(c(input$bip_boxes_t1, input$bip_boxes_t2, input$bip_boxes_t3))){
      db_USED_IN(db_USED_IN() |> add_row(
        OUT_Name = input$output_name,
        BIP_Name = entry
      ))
    }
  })
  
  observeEvent(input$add_recipient, {
    db_SEN(db_SEN() |> add_row(
      OUT_Name = input$output_radio,
      SEN_Name = input$recipient
    ))
  })
  
  observeEvent(input$load_data, {
    if (file.exists(input$load_path)){
      loaded_file <- loadWorkbook(input$load_path)

      t1_loaded_data <- read.xlsx(loaded_file, sheet=2, startRow=2, colNames=FALSE, skipEmptyCols = FALSE)
      t2_loaded_data <- read.xlsx(loaded_file, sheet=3, startRow=2, colNames=FALSE, skipEmptyCols = FALSE)
      
      for (i in 1:nrow(t1_loaded_data)){
        if (!(t1_loaded_data[i,3] %in% db_BIP()[["BIP_Description"]])){
          if (t1_loaded_data[i,3] %in% db_REF$Description){
            curr_selection <- db_REF[db_REF$Description == t1_loaded_data[i,3],]
            db_BIP(db_BIP() |> add_row(
              Table1 = T, Table2 = curr_selection$Table2, Table3 = F,
              BIP_Name = curr_selection$Name,
              BIP_Code = curr_selection$Code,
              BIP_Description = curr_selection$Description,
              BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
              BIP_Licence = curr_selection$Restrictions,
              BIP_Owner = "",
              BIP_Provider = "",
              BIP_Restrictions = curr_selection$Restrictions
            ))
          } else {
            db_BIP(db_BIP() |> add_row(
              Table1 = T, 
              Table2 = if(t1_loaded_data[i,3] %in% t2_loaded_data[,3]){T}else{F},
              Table3 = F,
              BIP_Name = paste(substr(t1_loaded_data[i,3], 1, 40), "...", sep=""),
              BIP_Code = t1_loaded_data[i,2],
              BIP_Description = t1_loaded_data[i,3],
              BIP_Date = as.Date(t1_loaded_data[i,4], origin="1899-12-30"),
              BIP_Licence = t1_loaded_data[i,5],
              BIP_Owner = "",
              BIP_Provider = "",
              BIP_Restrictions = t1_loaded_data[i,5]
            ))
          }
        }
      }

      for (i in 1:nrow(t2_loaded_data)){
        if (!(t2_loaded_data[i,3] %in% db_BIP()[["BIP_Description"]])){
          if (t2_loaded_data[i,3] %in% db_REF$Description){
            curr_selection <- db_REF[db_REF$Description == t2_loaded_data[i,3],]
            db_BIP(db_BIP() |> add_row(
              Table1 = curr_selection$Table1, Table2 = T, Table3 = F,
              BIP_Name = curr_selection$Name,
              BIP_Code = curr_selection$Code,
              BIP_Description = curr_selection$Description,
              BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
              BIP_Licence = curr_selection$Restrictions,
              BIP_Owner = "",
              BIP_Provider = "",
              BIP_Restrictions = curr_selection$Restrictions
            ))
          } else {
            db_BIP(db_BIP() |> add_row(
              Table1 = F, Table2 = T, Table3 = F,
              BIP_Name = paste(substr(t2_loaded_data[i,3], 1, 40), "...", sep=""),
              BIP_Code = t2_loaded_data[i,2],
              BIP_Description = t2_loaded_data[i,3],
              BIP_Date = as.Date(t2_loaded_data[i,4], origin="1899-12-30"),
              BIP_Licence = t2_loaded_data[i,5],
              BIP_Owner = "",
              BIP_Provider = "",
              BIP_Restrictions = t2_loaded_data[i,5]
            ))
          }
        }
      }

      t3_loaded_data <- read.xlsx(loaded_file, sheet=4, startRow=2, colNames=FALSE, skipEmptyCols = FALSE)
      for (i in 1:nrow(t3_loaded_data)){
        if (!(t3_loaded_data[i,4] %in% db_BIP()[["BIP_Description"]])){
          if (t3_loaded_data[i,4] %in% db_REF$Description){
            curr_selection <- db_REF[db_REF$Description == t3_loaded_data[i,4],]
            db_BIP(db_BIP() |> add_row(
              Table1 = F, Table2 = F, Table3 = T,
              BIP_Name = curr_selection$Name,
              BIP_Code = curr_selection$Code,
              BIP_Description = curr_selection$Description,
              BIP_Date = as.Date("18/07/2023", format="%d/%m/%Y"), # Until stated otherwise, I'm going to assume they are available since AAGI inception.
              BIP_Licence = curr_selection$Arrangements,
              BIP_Owner = curr_selection$Owner,
              BIP_Provider = "",
              BIP_Restrictions = curr_selection$Restrictions
            ))
          } else {
            db_BIP(db_BIP() |> add_row(
              Table1 = F, Table2 = F, Table3 = T,
              BIP_Name = paste(substr(t3_loaded_data[i,4], 1, 40), "...", sep=""),
              BIP_Code = t3_loaded_data[i,2],
              BIP_Description = t3_loaded_data[i,4],
              BIP_Date = as.Date(t3_loaded_data[i,5], origin="1899-12-30"),
              BIP_Licence = t3_loaded_data[i,7],
              BIP_Owner = t3_loaded_data[i,3],
              BIP_Provider = if(is.na(t3_loaded_data[i,6])){""}else{t3_loaded_data[i,6]},
              BIP_Restrictions = t3_loaded_data[i,8]
            ))
          }
        }
      }

      t4_loaded_data <- read.xlsx(loaded_file, sheet=5, startRow=2, colNames=FALSE, skipEmptyCols = FALSE)
      
      for (i in 1:nrow(t4_loaded_data)){
        db_OUT(db_OUT() |> add_row(
          OUT_Name = paste(substr(t4_loaded_data[i,3], 1, 40), "...", sep=""),
          OUT_Code = t4_loaded_data[i,2],
          OUT_Description = t4_loaded_data[i,3],
          OUT_Date = as.Date(t4_loaded_data[i,4], origin="1899-12-30"),
          OUT_Licence = t4_loaded_data[i,6]
        ))
        
        t1_init_numbers <- numeric()
        t2_init_numbers <- numeric()
        t3_init_numbers <- numeric()
        for (j in strsplit(t4_loaded_data[i,5], "\n")[[1]]){
          if(!(is.na(strsplit(j, " ")[[1]][2]))){
            if (strsplit(j, " ")[[1]][2]=="1,"){
              t1_init_numbers <- as.numeric(strsplit(
                strsplit(j, " ")[[1]][4], ","
              )[[1]])
            } else if (strsplit(j, " ")[[1]][2]=="2,"){
              t2_init_numbers <- as.numeric(strsplit(
                strsplit(j, " ")[[1]][4], ","
              )[[1]])
            } else if (strsplit(j, " ")[[1]][2]=="3,"){
              t3_init_numbers <- as.numeric(strsplit(
                strsplit(j, " ")[[1]][4], ","
              )[[1]])
            }
          }
        }
        
        for (entry in db_BIP()[db_BIP()[["BIP_Description"]] %in% unique(c(t1_loaded_data[t1_init_numbers,3], t2_loaded_data[t2_init_numbers,3], t3_loaded_data[t3_init_numbers,4])),"BIP_Name"]){
          db_USED_IN(db_USED_IN() |> add_row(
            OUT_Name = paste(substr(t4_loaded_data[i,3], 1, 40), "...", sep=""),
            BIP_Name = entry
          ))
        }
      }

      t5_loaded_data <- read.xlsx(loaded_file, sheet=6, startRow=2, colNames=FALSE, skipEmptyCols = FALSE)
      for (i in 1:nrow(t5_loaded_data)){
        db_SEN(db_SEN() |> add_row(
          OUT_Name = db_OUT()[db_OUT()["OUT_Description"]==t5_loaded_data[i,3],"OUT_Name"],
          SEN_Name = t5_loaded_data[i,2]
        ))
      }
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "File loaded successfully",
        type = "success"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Failure",
        text = "Unable to locate file with given name. Did you remember to add the .xlsx extension?",
        type = "error"
      )
    }
  })
  
  observeEvent(input$allow_deletions, {
    # if (input$allow_deletions){
    #   sendSweetAlert(
    #     session = session,
    #     title = "Caution",
    #     text = "Deletion of entries is now allowed",
    #     type = "warning"
    #   )
    # }
  })
  
  observeEvent(input$delete_entry_BIP, {
    if (input$allow_deletions){
      db_BIP(db_BIP()[db_BIP()[["BIP_Name"]]!=input$radio_edit_BIP,])
      db_USED_IN(db_USED_IN()[!(input$radio_edit_BIP %in% db_USED_IN()[["BIP_Name"]]),])
    } else {
      sendSweetAlert(
        session = session,
        title = "Caution",
        text = "Deletion of entries is turned off",
        type = "warning"
      )
    }
  })
  
  observeEvent({input$radio_edit_BIP}, {
    updateCheckboxGroupInput(session, "bip_update_tables",
      selected = c("A", "B", "C")[which(as.logical(db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,1:3]))]
    )
    updateTextAreaInput(session, "description_bip_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Description"]
    )
    updateTextInput(session, "bip_code_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Code"]
    )
    updateDateInput(session, "date_bip_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Date"]
    )
    updateTextAreaInput(session, "licence_bip_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Licence"]
    )
    updateTextAreaInput(session, "owner_bip_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Owner"]
    )
    updateTextAreaInput(session, "provider_bip_update",
                        value = db_BIP()[db_BIP()[["BIP_Name"]]==input$radio_edit_BIP,"BIP_Provider"]
    )
  })
  
  observeEvent(input$update_entry_BIP, {
    db_BIP(db_BIP() |> rows_update(
      data.frame(
        Table1 = "A" %in% input$bip_update_tables,
        Table2 = "B" %in% input$bip_update_tables,
        Table3 = "C" %in% input$bip_update_tables,
        BIP_Name = input$radio_edit_BIP,
        BIP_Code = input$bip_code_update,
        BIP_Description = input$description_bip_update,
        BIP_Date = input$date_bip_update,
        BIP_Licence = input$licence_bip_update,
        BIP_Owner = input$owner_bip_update,
        BIP_Provider = input$provider_bip_update,
        BIP_Restrictions = input$licence_bip_update
      ), by="BIP_Name"
    ))
  })
  
  observeEvent(input$delete_entry_OUT, {
    if (input$allow_deletions){
      db_OUT(db_OUT()[db_OUT()[["OUT_Name"]]!=input$radio_edit_OUT,])
      db_USED_IN(db_USED_IN()[!(input$radio_edit_OUT %in% db_USED_IN()[["OUT_Name"]]),])
      db_SEN(db_SEN()[!(input$radio_edit_OUT %in% db_SEN()[["OUT_Name"]]),])
    } else {
      sendSweetAlert(
        session = session,
        title = "Caution",
        text = "Deletion of entries is turned off",
        type = "warning"
      )
    }
  })
  
  observeEvent({input$radio_edit_OUT}, {
    updateTextAreaInput(session, "description_output_update",
                        value = db_OUT()[db_OUT()[["OUT_Name"]]==input$radio_edit_OUT,"OUT_Description"]
    )
    updateDateInput(session, "date_created_update",
                    value = db_OUT()[db_OUT()[["OUT_Name"]]==input$radio_edit_OUT,"OUT_Date"]
    )
    updateTextAreaInput(session, "licence_output_update",
                        value = db_OUT()[db_OUT()[["OUT_Name"]] == input$radio_edit_OUT,"OUT_Licence"]
    )
    updateCheckboxGroupInput(session, "bip_boxes_t1_update",
                        selected = db_BIP()[db_BIP()[["Table1"]] & db_BIP()[["BIP_Name"]] %in% db_USED_IN()[db_USED_IN()[["OUT_Name"]] == input$radio_edit_OUT, "BIP_Name"], "BIP_Name"]
    )
    updateCheckboxGroupInput(session, "bip_boxes_t2_update",
                             selected = db_BIP()[db_BIP()[["Table2"]] & db_BIP()[["BIP_Name"]] %in% db_USED_IN()[db_USED_IN()[["OUT_Name"]] == input$radio_edit_OUT, "BIP_Name"], "BIP_Name"]
    )
    updateCheckboxGroupInput(session, "bip_boxes_t3_update",
                             selected = db_BIP()[db_BIP()[["Table3"]] & db_BIP()[["BIP_Name"]] %in% db_USED_IN()[db_USED_IN()[["OUT_Name"]] == input$radio_edit_OUT, "BIP_Name"], "BIP_Name"]
    )
  })
  
  observeEvent(input$update_entry_OUT, {
    db_OUT(db_OUT() |> rows_update(
      data.frame(
        OUT_Name = input$radio_edit_OUT,
        OUT_Code = "AAGI-ALL-SP-003", # Universal until otherwise stated.
        OUT_Description = input$description_output_update,
        OUT_Date = input$date_created_update,
        OUT_Licence = input$licence_output_update
      ), by="OUT_Name"
    ))
    
    db_USED_IN(db_USED_IN() |> filter(`OUT_Name`!=input$radio_edit_OUT))
    for (entry in unique(c(input$bip_boxes_t1_update, input$bip_boxes_t2_update, input$bip_boxes_t3_update))){
      db_USED_IN(db_USED_IN() |> add_row(
        OUT_Name = input$radio_edit_OUT,
        BIP_Name = entry
      ))
    }
  })
  
  observeEvent(input$delete_entry_SEN, {
    if (input$allow_deletions){
      db_SEN(db_SEN()[-as.numeric(input$radio_edit_SEN),])
    } else {
      sendSweetAlert(
        session = session,
        title = "Caution",
        text = "Deletion of entries is turned off",
        type = "warning"
      )
    }
  })
  
  observeEvent(input$debug, {
    View(db_BIP())
    View(db_OUT())
    View(db_USED_IN())
    View(db_SEN())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)