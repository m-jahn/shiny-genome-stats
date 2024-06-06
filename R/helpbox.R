helpbox <- function(width = 12) {
  column(width = width,
    h4('INFO & HELP'),
    wellPanel(
      h4('HOW TO'),
      p('Pre-fetched genome/proteome annotation can be selected with the "Select Microbial Genome" field.'),
      p('Data was fetched from Uniprot using the REST API. Users can try to fetch data for other organisms using the "Find Microbial Genome" search field.'),
      h4('DATA AND REFERENCES'),
      p('Data is obtained from ',
        a(href = 'https://www.uniprot.org', target = '_blank', 'Uniprot.org'), '.'
      ),
      p('Details about how to fetch data using Uniprots REST API can be found at',
        a(href = 'https://www.uniprot.org/help/api_queries', target = '_blank', 'www.uniprot.org/help/api_queries'), '.'
      ),
      h4('CONTACT'),
      p('For questions or reporting issues, contact Michael Jahn, Max Planck Unit for the science
        of pathogens (MPUSP), Berlin',
        a(href ='mailto:jahn@mpusp.mpg.de', target = '_blank', 'jahn@mpusp.mpg.de')
      ),
      h4('DISCLAIMER'),
      p('This software is licensed under:'),
      p('GNU GENERAL PUBLIC LICENSE'),
      p('This software can be used freely as long as it is not used
        for commercial purposes, resold, or redistributed with other licenses.
        There is no warranty for the program, to the extent permitted by applicable
        law. Except when otherwise stated in writing the copyright holders and/or
        other parties provide the program as is without warranty of any kind,
        either expressed or implied, including, but not limited to, the implied
        warranties of merchantability and fitness for a particular purpose.
        The entire risk as to the quality and performance of the program is with
        you. Should the program prove defective, you assume the cost of all
        necessary servicing, repair or correction."
      ')
    )
  )
}

fundbox <- function(width = 12) {
  column(width = width,
    h4('FUNDING'),
    wellPanel(
      h4('FUNDING AND OTHER RESOURCES'),
      p('This was work was supported by:'),
      tags$ul(
        tags$li('Max Planck Society, Germany')
      )
    )
  )
}

methbox <- function(width = 12) {
  column(width = width,
    h4('METHOD DETAILS'),
    wellPanel(
      h4('Data Filtering'),
      p('
        In order to present genome annotation data visually, some simplifications
        need to be made.
        Tables that are downloaded from Uniprot are slightly filtered before
        display. Prefetched tables have been checked for the correct number
        of genes per genome. NA values for protein length or biological processes
        are filtered out. All NA values for localization however are included
        but transformed to <Unknown>.
      '),
      h4('Localization'),
      p('
        Protein localization information from Uniprot is very incomplete. Other
        tools for localization prediction can label almost all proteins with
        sufficient accuracy. Therefore protein localization information is
        to be taken with care, as long as the source is not updated.
      '),
      h4('Pathway annotation'),
      p('
        Currently there is no visual for an overview about pathway membership
        of proteins. This data is again very limited on Uniprot. However here
        the Gene Ontology (GO) terms for Biological Process are used as a proxy
        of how many proteins belong to functional categories.
        GO terms are a hierarchically organized structured language. They start
        with broad terms and get more fine-grained further down the hierarchical
        tree. One protein can be labelled with many terms, and one term can be
        connected with many proteins (all-to-all relationship). Top N terms are
        simply counted per organism and presented as a relative fraction, to
        gauge relative importance of that function in an organism. This is of
        course not a statistically valid comparison, but can give a first
        impression.
      ')
    )
  )
}
