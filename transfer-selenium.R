library(tidyverse)
library(RSelenium)
library(readr)

#============TRANSFER CASE==========#
#transfer cases
jurisdiction_select = read_csv("jurisdiction_selections_list.csv", col_types = cols())

transfer = function(caseNumber, jurisdiction){
  
  #Translate county name to health department name in select dropdown menu
  if(!exists("jurisdiction_select")){
    jurisdiction_select = read_csv("jurisdiction_selections_list.csv", col_types = cols())
  }
  
  if(toupper(jurisdiction) %in% toupper(jurisdiction_select$county)){ 
    #If county name, translate to dropdown selection name
    jurisdiction = jurisdiction_select$selection[toupper(jurisdiction_select$county) == toupper(jurisdiction)]
    
    
  }else if(jurisdiction %in% jurisdiction_select$selection){
    #If already selection name, do nothing
    
  }else{
    #If jurisdiction is not in either county name list or dropdown selection list
    message(paste(jurisdiction, "is not an acceptable selection."))
    return()
  }
  
  #search state case number
  search_scn(caseNumber)
  wait_page("Case Summary")
  
  #Check to see if CCDPH case still
  jur = get_text(".NoBorderFull > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2)")
  if(jur != "Cook County Department of Public Health"){
    message(paste(caseNumber, "not transferred because already assigned to", jur))
    return()
  }
  
  
  #click transfer case
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(2) > a:nth-child(1)")
  
  #Check to see if required fields are completed for transferring
  error = try(get_text("#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > center:nth-child(1)"),
              silent = T)
  
  if(grepl("Earliest Report Date and Date LHD Received are mandatory fields", error)){
    click("input[value = \"Cancel\"]")
    wait_page("Case Summary")
    
    #Click into Reporting Source
    click("fieldset.fieldsetHeader:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(5) > td:nth-child(1) > a:nth-child(1)")
    wait_page("Reporting Source")
    
    earliestReportMonth = get_text("#report", textbox = T)
    earliestReportDay = get_text(name.is("d1day"),textbox = T)
    earliestReportYear = get_text(name.is("d1year"), textbox = T)
    
    phReceivedMonth = get_text("#received", textbox = T)
    phReceivedDay = get_text(name.is("d2day"),textbox = T)
    phReceivedYear = get_text(name.is("d2year"), textbox = T)
    
    #If one date is filled in, just copy to the other
    if(phReceivedMonth !="" & earliestReportDay ==""){
      enter_text("#report", c(phReceivedMonth, phReceivedDay, phReceivedYear))
      Sys.sleep(2)
      click(value.is("Save"))
    }
    else if (phReceivedMonth =="" & earliestReportDay !=""){
      enter_text("#received", c(earliestReportMonth, earliestReportDay, earliestReportYear))
      Sys.sleep(2)
      click(value.is("Save"))
    }
    #If neither filled in, put error message for now
    else{
      message(paste(caseNumber, "needs Earliest Report Date or Date Public Health Received filled in.",
                    caseNumber, "has not been transferred."))
      click(value.is("Close"))
      return()
      
    }
    
    #click transfer case
    click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(2) > a:nth-child(1)")
    
  }
  
  
  #select jurisdiction from dropdown list
  Sys.sleep(2)
  select_drop_down(element = "#jurisdiction", selection = jurisdiction)
  Sys.sleep(2)
  
  #Transfer
  ifVisiblethenClick(value.is("Transfer"))

  #Accept alert
  acceptAlertwithWait()
  
  message(paste(caseNumber, "transferred to", jurisdiction))
}
