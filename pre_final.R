library(plotly)
library(data.table)
library(readxl)
library(htmlwidgets)
datum<- read_excel(paste0(getwd(),"/finito.xlsx"))
setDT(datum)

datum$buyuk <- round(datum$buyuk, 3)
datum$kucuk <- round(datum$kucuk, 3)
datum$orta <- round(datum$orta, 3)
datum$mikro <- round(datum$mikro, 3)
datum$total <- round(datum$total, 3)


names(datum)
unique(datum$firm)

graphtype <- function(x){
  if(x == "op_nt")
    paste0("Operational Profit to Net Sales")
  else if(x == "o_nt")
    paste0("Net Income from Other Operations to Net Sales")
  else if(x == "oe_nt")
    paste0("Net Extraordinary Income to Net Sales")
  else if(x == "fe")
    paste0("Financial Expenses to EBIT")
  else if(x == "p")
    paste0("Profit to Net Sales")
  else if(x == "np")
    paste0("Net Profit to Net Sales")
  else if(x == "acid")
    paste0("Acid Test Ratio")
  else if(x == "ca")
    paste0("Short-Term Liabilities to Current Assets")
  else if(x == "lcl")
    paste0("Long-Term Financial Liabilities to Total Liabilities")
  else if(x == "ldl")
    paste0("Long-Term Trade and Other Debts to Total Liabilities")
  else if(x == "ltl")
    paste0("Long-Term Liabilities to Total Liabilities")
  else if(x == "scl")
    paste0("Short-Term Financial Liabilities to Total Liabilities")
  else if(x == "sdl")
    paste0("Short-Term Trade and Other Debts to Total Liabilities")
  else if(x == "stl")
    paste0("Short-Term Liabilities to Total Liabilities")
  else if(x == "tcl")
    paste0("Total Financial Liabilities to Total Liabilities")
  else if(x == "tdl")
    paste0("Total Trade and Other Debts to Total Liabilities")
  else if(x == "tl")
    paste0("Liabilities to Total Liabilities")
  
}
unique(datum$firm)
firmtype <- function(x){
  if(x == "agri")
    paste0("Agriculture Forestry and Fishing")
  else if(x == "mini")
    paste0("Mining and Quarrying")
  else if(x == "manu")
    paste0("Manufacturing")
  else if(x == "elec")
    paste0("Electricity, Gas, Steam and Air Conditioning Supply")
  else if(x == "water")
    paste0("Water Supply; Sewerage; Waste Management and Remediation Activities")
  else if(x == "cons")
    paste0("Construction")
  else if(x == "trade")
    paste0("Trade")
  else if(x == "trans")
    paste0("Transporting and Storage")
  else if(x == "acco")
    paste0("Accommodation and Food Service Activities")
  else if(x == "info")
    paste0("Information and Communication")
  else if(x == "real")
    paste0("Real Estate Activities")
  else if(x == "prof")
    paste0("Professional, Scientific and Technical Activities")
  else if(x == "adm")
    paste0("Administrative and Support Service Activities")
  else if(x == "edu")
    paste0("Education")
  else if(x == "health")
    paste0("Human Health and Social Work Activities")
  else if(x == "art")
    paste0("Arts, Entertainment and Recreation")
  else if(x == "other")
    paste0("Other Services Activities")
  
}



# updatemenus component

ind <- list(
  list(
    label="Indicators",active = 0, type= 'dropdown',y=1.08, x=1.3 ,buttons = list(
      list(method = "restyle",args = list(), label = "--Income Statement--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[1]), label = graphtype(unique(datum$tip)[1])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[2]), label = graphtype(unique(datum$tip)[2])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[3]), label = graphtype(unique(datum$tip)[3])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[4]), label = graphtype(unique(datum$tip)[4])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[5]), label = graphtype(unique(datum$tip)[5])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[6]), label = graphtype(unique(datum$tip)[6])),
      list(method = "restyle",args = list(), label = "--Liquidity--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[7]), label = graphtype(unique(datum$tip)[7])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[8]), label = graphtype(unique(datum$tip)[8])),
      list(method = "restyle",args = list(), label = "--Indebtedness--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[9]), label = graphtype(unique(datum$tip)[9])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[10]),label = graphtype(unique(datum$tip)[10])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[11]),label = graphtype(unique(datum$tip)[11])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[12]),label = graphtype(unique(datum$tip)[12])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[13]),label = graphtype(unique(datum$tip)[13])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[14]),label = graphtype(unique(datum$tip)[14])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[15]),label = graphtype(unique(datum$tip)[15])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[16]),label = graphtype(unique(datum$tip)[16])),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$tip)[17]),label = graphtype(unique(datum$tip)[17])))),
  list(
    label="Indicators",active = 0, type= 'button',y=1.08, x=0.7,buttons = list(
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[1]), label =firmtype(unique(datum$firm)[1])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[2]), label =firmtype(unique(datum$firm)[2])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[3]), label =firmtype(unique(datum$firm)[3])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[4]), label =firmtype(unique(datum$firm)[4])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[5]), label =firmtype(unique(datum$firm)[5])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[6]), label =firmtype(unique(datum$firm)[6])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[7]), label =firmtype(unique(datum$firm)[7])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[8]), label =firmtype(unique(datum$firm)[8])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[9]), label =firmtype(unique(datum$firm)[9])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[10]), label =firmtype(unique(datum$firm)[10])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[11]), label =firmtype(unique(datum$firm)[11])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[12]), label =firmtype(unique(datum$firm)[12])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[13]), label =firmtype(unique(datum$firm)[13])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[14]), label =firmtype(unique(datum$firm)[14])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[15]), label =firmtype(unique(datum$firm)[15])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[16]), label =firmtype(unique(datum$firm)[16])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$firm)[17]), label =firmtype(unique(datum$firm)[17]))))
      
  )
 

fig<-datum %>%
  plot_ly(type = 'scatter', mod= 'line',mode = 'markers', 
          transforms = list(list(type = 'filter',target = ~tip, operation = '=',value = unique(datum$tip)[1]),
                            list(type='filter',target= ~firm, operation = '=', value = unique(datum$firm)[1])))%>%
  add_trace(x = ~time,y= ~buyuk, name = 'Large Firms',text=~buyuk, mode = "lines+markers", color = "#b2abd2") %>%
  add_trace(x = ~time,y= ~kucuk, name = 'Small-sized Firms',text=~kucuk, mode = "lines+markers", color="#b35806") %>%
  add_trace(x = ~time,y= ~orta, name = 'Medium-sized Firms',text=~orta, mode = "lines+markers", color="#e08214") %>% 
  add_trace(x = ~time,y= ~mikro, name = 'Micro-sized Firms',text=~mikro, mode = "lines+markers", color="#8073ac") %>%
  add_trace(x = ~time,y= ~total, name = 'All Firms',text=~total, mode = "lines+markers", color="#542788")%>%
  layout(title = list(text = paste0('Sectoral Evaluation',
                                    '<br>',
                                    '<sup>',
                                    'Indicators',
                                    '</sup>')),
xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)),
         yaxis=list(title="Percentage"),
         updatemenus=ind,
         annotations = list(x = 0, y = 0, text = "Source: Calculated by A. Ismet ASCI by using data from CBRT", 
              showarrow = F, xref='paper', yref='paper', 
              xshift=0, yshift=0,
              font=list(size=15, color="black")))

fig

saveWidget(fig, paste0(getwd(),'/table.html'))
