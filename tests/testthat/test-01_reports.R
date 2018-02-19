library(relationshipMatrix)

library(testthat)
options(warn=2)
context("Testing reports")

test_that("Simplest document", {
  doc<-doc_Document$new(foldername = 'test', author = 'Ja',  title = 'test', format = 'md')
  doc$insert_paragraph(text = "Ala ma kota")
  a<-pander::Pandoc$new()
  doc$render(a)
  a$export('/tmp/cos', open=FALSE, options='+RTS -K100000000 -RTS --filter pandoc-fignos --filter pandoc-tablenos -M "tablenos-caption-name:Tabela" -M "fignos-caption-name:Rycina"')
  ref1<-readLines('tests/testthat/test-01_ref1.md')
  ref2<-readLines('/tmp/cos.md')
  testthat::expect_equivalent(ref1, ref2)
#  save_report(a, filename = 'tmp')
})

test_that("More difficult document", {
  doc<-doc_Document$new(chart_foldername = 'test', cache_foldername = 'test', author = 'Ja',  title = 'test')
#  debugonce(doc$insert_paragraph)
  doc$insert_paragraph(text = "Ala ma kota")
  tab<-tibble("Nazwa"=c("Tata", "Mama", "Dziecko 1", "Dziecko 2"), Wiek=1:4)
  attr(tab$Nazwa, 'label')<-"Nazwa"
  doc$insert_table(caption = "Wiek poszczególnych członków rodziny", table_df = tab, tags = "demografia")

  rozdz<-doc$insert_section(text = "Rozdział 1", tags = "szczegóły", chart_foldername = "podkatalog")
  rozdz$insert_paragraph("Tekst rozdziału 1")
  rozdz$insert_paragraph("Jak wiemy z rozdziału [Rozdział 2], dzieci są bardzo grzeczne.")
#  debugonce(rozdz$insert_section)

  rozdz2<-rozdz$insert_section("Podrozdział")
  rozdz2$insert_paragraph("Tekst podrozdziału")
  rozdz3<-doc$insert_section(text="Rozdział 2", tags="dzieci")
  rozdz3$insert_paragraph("Wszyscy wiemy, że dzieci są bardzo grzeczne.")

  a<-pander::Pandoc$new()
#  debugonce(doc$render)
  doc$render(a)
  save_report(a, filename = '/tmp/cos')
})

test_that("Make documents from pieces", {
  doc<-doc_Standalone_Chapter$new(foldername = 'tmp')

  #  debugonce(doc$insert_paragraph)
  doc$insert_paragraph(text = "Ala ma kota")
  tab<-tibble("Nazwa"=c("Tata", "Mama", "Dziecko 1", "Dziecko 2"), Wiek=1:4)
  attr(tab$Nazwa, 'label')<-"Nazwa"
  doc$insert_table(table_caption = "Wiek poszczególnych członków rodziny", table_df = tab, tags = "demografia")

  rozdz<-doc$insert_section(text = "Rozdział 1", tags = "szczegóły", foldername = "podkatalog")
  rozdz$insert_paragraph("Tekst rozdziału 1")
  #  debugonce(rozdz$insert_section)

  rozdz2<-rozdz$insert_section("Podrozdział")
  rozdz2$insert_paragraph("Tekst podrozdziału")

  a<-pander::Pandoc$new()
  #  debugonce(doc$render)
  doc$render(a)
  save_report(a, filename = '/tmp/cos')
})
