library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(stringr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(shinycssloaders)

DATA_FOLDER = '../data'
CURRENT_WORLD_RECORD = 101
COLUMNS = c('pos', 'competition', 'name', 'country', 'club', 'yards', 'gender', 
            sprintf("yard%s",seq(1:(CURRENT_WORLD_RECORD + 1))))
LOADING_ANIMATION = 'https://mir-s3-cdn-cf.behance.net/project_modules/disp/5d04ef14282093.56280fc4e27b3.gif'

# loeme andmed sisse ja puhastame/mudime neid
data = list.files(path=DATA_FOLDER, pattern = '*.csv', full.names = TRUE) %>% 
  lapply(function (f) read.csv(f, header = FALSE, sep = '\t')) %>% 
  bind_rows

colnames(data) = COLUMNS
data[data == 'DNC' | data == 'RTC' | data == 'Over'] = ''
data[data == ''] = NA

getYardsData = function(dataSelection) {
  yardData = dataSelection[,sprintf('yard%s',seq(1:CURRENT_WORLD_RECORD))]
  # eemaldame read, kus pole ühtki väärtust, sest need ei oma siin mõtet
  yardData = yardData[rowSums(is.na(yardData)) != ncol(yardData), ]
  yardData = yardData %>% 
    mutate_all(function (el) {
      minute(ms(el)) + second(ms(el)) / 60
    })
}

getYardAverages = function(dataSelection) {
  maxYards = ifelse(nrow(dataSelection) > 0, max(dataSelection$yards), 1)
  yardData = getYardsData(dataSelection)
  stack(colMeans(
    yardData[sprintf('yard%s',seq(1:maxYards))], 
    na.rm = TRUE
  )) %>% 
    mutate(ind = as.numeric(str_replace(ind, 'yard', '')))
}

getInputChoices = function(column, sort = FALSE) {
  choices = unique(data[[column]])
  if (sort) {
    choices = sort(choices)
  }
  
  return(choices)
}

statisticsTab = tabPanel(
  'Võistluste statistika',
  br(),
  sidebarLayout(
    tagAppendAttributes(
      sidebarPanel(
        pickerInput(
          'competitions', 
          'Vali võistlus', 
          choices = getInputChoices('competition'), 
          options = list(`actions-box` = TRUE, `live-search` = TRUE), 
          multiple = TRUE,
          selected = unique(data$competition)
        ),
        pickerInput(
          'participants', 
          'Vali võistleja', 
          choices = getInputChoices('name', TRUE), 
          options = list(`actions-box` = TRUE, `live-search` = TRUE), 
          multiple = TRUE,
          selected = unique(data$name)
        ),
        pickerInput(
          'countries',
          'Vali võistleja päritolu/klubi', 
          choices = getInputChoices('country', TRUE), 
          options = list(`actions-box` = TRUE, `live-search` = TRUE), 
          multiple = TRUE,
          selected = unique(data$country)
        ),
        checkboxGroupButtons(
          'genders',
          'Võistleja sugu',
          choiceNames = c('Mees', 'Naine'),
          choiceValues = c('M', 'F'),
          selected = c('M', 'F')
        )
      ),
      class = 'sticky'
    ),
    mainPanel(
      fluidRow(
        class = 'info-row',
        align = 'center',
        valueBoxOutput('totalYards'),
        valueBoxOutput('yardsMean'),
        valueBoxOutput('totalSweat')
      ),
      h3('Läbitud ringide osakaal'),
      p('Eraldi on toodud välja kümne või kaheteistkümnega jaguvad, nii-öelda ümmargused ringid'),
      withSpinner(
        plotOutput('yardsCompletedPlot'),
        image = LOADING_ANIMATION,
        image.height = 200
      ),
      br(),
      h3('Keskmised ringiajad'),
      withSpinner(
        plotOutput('yardAveragesPlot'),
        image = LOADING_ANIMATION,
        image.height = 200
      ),
      h3('Keskmised jooksmise-puhkamise vahekorrad'),
      withSpinner(
        plotOutput('yardSplitPlot'),
        image = LOADING_ANIMATION,
        image.height = 200
      ),
      br()
    )
  )
)

overviewTab = tabPanel(
  'Backyard ultra',
  h3('Mis on backyard ultra?'),
  p(a(href = 'https://backyardultra.com/', 'Backyard ultra,', target='_blank'), 'maakeeli tagahoovi ultra, on jooksuvõistluse formaat, kus võistlejad peavad 6.706 kilomeetrisel ringil (inglise keeles yard) jooksma seni, kuni jalad enam ei tööta. Igat ringi alustavad võistlejad koos, tavaliselt täistunnil, ning ringi ajapiirang on 1 tund (ehk järgmiseks täistunniks peab olema uuesti stardijoonel). Võistleja, kes ei suuda ringi tunnise limiidi sees läbida või kes otsustab järgmisele ringile mitte minna, diskvalifitseeritakse ning tema tulemuseks on DNF (did not finish). Igal võistlusel on maksimaalselt üks võitja: see, kes suudab viimasena DNF-i saanud võistlejast ühe ringi rohkem joosta. Ühe ringi pikkus on arvestatult selliselt, et 24 tunniga läbib jooksja 100 miili (100/24 = 4.167 miili = 6.706 kilomeetrit). Üldjuhul on rada maastikul.'),
  p('Tagahoovi ultrale pani aluse ', a(href='https://en.wikipedia.org/wiki/Lazarus_Lake', 'Gary "Lazarus Lake" Cantrell', target='_blank'), ' oma koera järgi nime saanud võistlusega ', a(href = 'https://en.wikipedia.org/wiki/Big%27s_Backyard_Ultra', 'Big\'s Backyard Ultra', target='_blank'), ' aastal 2011. Sellest ajast saati on formaat iga aastaga üha enam populaarsust kogunud. Nüüdseks korraldatakse võistlusi igal pool üle maailma, ', a(href = 'https://heavymetalultra.com/', 'sealhulgas Eestis.', target='_blank')),
  p('Formaadi teeb raskeks asjaolu, et teada pole võistluse pikkust, see sõltub võistlejate füüsilisest ning mentaalsest tugevusest. Võistlust ei saa otseselt võita kiiremini joostes, vaid tuleb oodata teiste võitlejate katkestamist. Iga võistleja peab ise välja töötama oma taktika - kiirem jooksmine kulutab rohkem energiat, kuid see-eest on enne järgmist starti rohkem aega puhkamiseks, seevastu on olukord täpselt vastupidine aeglasemalt jooksmise või kõndimise puhul. Töö kirjutamise hetkel on maailmarekordiks 101 ringi (677.26 km), mille saavutasid Belglased Merijn Geerts ja Ivo Steyaert 2022 koondistevahelistel maailmameistrivõistlustel. Kuna kumbki jooksja uuele ringile ei läinud, siis ametlikult kumbki neist ka ei võitnud.'),
  br(),
  h3('Mis on töö eesmärk?'),
  p('Töö sisu on jaotatud kaheks: uurida saab statistikat nii võistluste kui ka üksikisikuliste võistlejate kohta.'),
  p('Võistluste statistikas saab võistluste ning seal osalenud võistlejate kaupa pärida graafikuid nii läbitud ringide arvu kui ka keskmiste ringide aegade kohta.'),
  p('Võistleja statistikas saab nime põhjal uurida võistleja tulemuste kohta erinevatel võistlustel ning saada ülevaade tema taktikatest (jooksmise ja puhkamise ajad).'),
  p('See annab lugejale arusaama backyard ultra olemusest. Kuna võistlus on suuresti mentaalne väljakutse, siis kajastub see ka andmetes. Formaadi looja, Laz, on ka ise täheldanud, et tihti ei katkestata mitte füüsilise limiidini jõudmisel, vaid peas olevate eesmärkide täitmisel või mentaalsete blokkide saabumisel. Näiteks seetõttu on pärast 23. ringi katkestamine haruharv nähtus, pärast 24 tunni möödumist aga kõige populaarsem. Lisaks sellele saab näha, kuidas mõjutab ringi läbimisaega see, kui kaua on juba võisteldud ning näha, kas üksikisikute taktikates kajastuvad ka erinevad mustrid (näiteks regulaarne kiirem ring pikemaks puhkepausiks vms).'),
  p('Autori hüpoteesid on alljärgnevad:'),
  tags$ul(
    tags$li('Katkestamine on sagedasem pärast kümnega jaguvate ringinumbrite läbimist'),
    tags$li('Katkestamine on sagedasem pärast pärast ringe, mille läbimisel on kogudistants jaguv 50 miiliga (ehk ringid 12, 24, 36 jne)'),
    tags$li('Keskmine ringi läbimise aeg tõuseb aja möödudes kuni 50 ringi läbimiseni, sealt edasi püsib stabiilsena'),
    tags$li('Mida vähem ringe võistleja jookseb, seda rohkem tema ringiajad varieeruvad')
  ),
  p('Graafikud ning andmete kuvamine on vormistatud selliselt, et lugeja ise nendele küsimustele vastata saaks. Kindlasti on erinevusi olenevalt sellest, millist võistlust või võistlejat vaadata.'),
  br(),
  br(),
  br(),
  p(a(href='https://github.com/karlru/backyard-ultra', 'Lähtekood', target='_blank')),
  p('Karl Kevin Ruul, 2023, aine Statistiline andmeteadus ja visualiseerimine (MTMS.01.100) raames', style="color:darkgray")
)

sourcesTab = tabPanel(
  'Viited/andmed',
  h3('Viited'),
  p(a(href='https://github.com/karlru/backyard-ultra', 'Lähtekood', target='_blank')),
  p(a(href='https://backyardultra.com/', 'Rohkem infot backyard ultra kohta', target='_blank')),
  p(a(href='https://www.behance.net/gallery/14282093/Running-man', 'Jooksev mehike', target='_blank')),
  p(a(href='https://docs.google.com/spreadsheets/d/1V5zS1D-LAZwKeO-ERd9gHkHjJ4nmRlt-9IKn7OgDup8/edit#gid=1600265888', 'World Team Championship 2022 andmed', target='_blank')),
  p(a(href='https://my.raceresult.com/127933/#0_D25492', 'Heavy Metal Ultra 2019 andmed', target='_blank')),
  p(a(href='https://my.raceresult.com/156449/#0_D25492', 'Heavy Metal Ultra 2020 andmed', target='_blank')),
  p(a(href='https://my.raceresult.com/176674/#0_D25492', 'Heavy Metal Ultra 2021 andmed', target='_blank')),
  p(a(href='https://my.raceresult.com/214177/#0_D25492', 'Heavy Metal Ultra 2022 andmed', target='_blank')),
  h3('Andmed'),
  p('Tulevikus oleks väga tore, kui siia saaks ka jooksvalt toimunud ürituste andmeid lisada. Selle jaoks toon siin välja, millisel kujul on praegu kasutusel olevad andmed.'),
  p('Andmed loetakse sisse tabiga eraldatud csv failidest, kus igal real on 109 tunnust:'),
  tags$ul(
    tags$li('koht võistlusel'),
    tags$li('võistluse nimi'),
    tags$li('võistleja nimi'),
    tags$li('võistleja riik'),
    tags$li('võistleja klubi'),
    tags$li('läbitud ringide arv'),
    tags$li('sugu (M/F)'),
    tags$li('esimese ringi aeg'),
    tags$li('teise ringi aeg'),
    tags$li('...'),
    tags$li('sajaesimese ringi aeg'),
    tags$li('sajateise ringi aeg')
  ),
  p('Ringide ajad on kujul mm:ss ning lubatud on ka väärtused DNC (did not complete) ning RTC (refused to continue). Teravam silm võis tähele panna, et andmetes on ka veerg sajateise ringi aja jaoks, kuid maailmarekord on vaid 101 - rekordiomanike sajateise ringi tulemuseks on RTC, ehk nad keeldusid jätkamast.'),
  p('Näide andmereast:'),
  tableOutput('exampleDataRow')
)

ui <- fluidPage(
  
  titlePanel("Backyard ultra andmeanalüüs"),
  
  tags$style(HTML(
    "@media screen and (min-width: 768px){
          div.sticky {
            position: -webkit-sticky;
            position: sticky;
            top: 0;
            z-index: 1;
          }
          .info-row {
            height: 100px;
          }
      }"
  )),
  
  tabsetPanel(
    type = 'tabs',
    statisticsTab,
    overviewTab,
    sourcesTab
  ),
)

server <- function(input, output) {
  
  # filtreeritud andmete kättesaamine
  dataSelection = reactive({
    data %>% 
      filter(
        name %in% input$participants, 
        competition %in% input$competitions,
        country %in% input$countries,
        gender %in% input$genders
      )
  })
  
  output$yardsCompletedPlot <- renderPlot({
    ggplot(data = req(dataSelection()), aes(
      x = yards, 
      fill=factor(ifelse(yards %% 10 == 0 | yards %% 12 == 0, 'a', 'b'))
    )) +
      geom_bar(aes(y = (..count..)/sum(..count..)))  +
      labs(y = 'Võistlejate osakaal', x = 'Läbitud ringide arv') +
      theme(legend.position = 'none')
  })
  
  output$yardAveragesPlot <- renderPlot({
    yardAverages = getYardAverages(req(dataSelection()))
    
    ggplot(data = yardAverages, aes(x = ind, y = values, group=1)) + 
      geom_line() +
      labs(y = 'Keskmine aeg (min)', x = 'Ringi number') +
      theme(legend.position = 'none')
  })
  
  output$yardSplitPlot <- renderPlot({
    yardAverages = getYardAverages(req(dataSelection())) %>% 
      mutate(type = factor('Jooksmine'))
    restAverages = yardAverages %>% 
      mutate(
        values = 60 - values,
        type = factor('Puhkamine')
      )
    
    averages = rbind(yardAverages, restAverages)
    # selleks, et jooksmine ees pool oleks
    averages$type = relevel(averages$type, 'Puhkamine')
    
    ggplot(averages, aes(ind, values, fill = type)) +
      geom_bar(position = "stack", stat = "identity") +
      coord_flip() +
      scale_x_reverse() +
      labs(y = 'Keskmine kulunud aeg (min)', x = 'Ringi number') +
      theme(legend.title = element_blank()) + 
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  output$totalYards <- renderValueBox({
    valueBox(sum(dataSelection()$yards), 'analüüsitud ringi')
  })
  
  output$yardsMean <- renderValueBox({
    totalTime = sum(getYardsData(dataSelection()), na.rm = TRUE)
    totalYards = sum(dataSelection()$yards)
    averageYard = totalTime / totalYards
    minutes = floor(averageYard)
    seconds = round(averageYard %% 1 * 60)
    valueBox(paste(minutes, ':', seconds, sep = ''), 'keskmine ringiaeg')
  })
  
  output$totalSweat <- renderValueBox({
    valueBox('väga palju', 'higi ja pisaraid')
  })
  
  output$exampleDataRow <- renderTable({
    data = read.csv(
      paste(DATA_FOLDER, '/team_world_22.csv', sep = ''), 
      header = FALSE,
      sep='\t',
      nrows=1,
      col.names = COLUMNS
    )
  })
}

shinyApp(ui = ui, server = server)
