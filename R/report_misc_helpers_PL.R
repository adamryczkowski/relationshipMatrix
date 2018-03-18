filter_info<-function(pAcc, language='PL', chapter) {
  browser()
  db_obj<-pAcc$serve_db()
  NA_rep<-db_obj$NA_report()
  flag_add_chapter<-FALSE

  liczebnik_przypadki<-function(liczba) danesurowe::liczebnik(liczba, mianownik = "przypadek", dopelniacz = "przypadków", lmnoga = "przypadki")

  if(db_obj$filterstring!='') {
    if(language=='PL') {
      msg_filter<-paste0("Przed wykonaniem analizy zastosowano filtr ",  db_obj$filter_label(), ", który ze zbioru o liczności ", NA_rep$nrow_total, " odrzucił ",
                         liczebnik_przypadki(NA_rep$nrow_total - db_obj$nrow))
      if(NA_rep$NA_filter>0) {
        msg_filter<-paste0(msg_filter, ", w tym ", liczebnik_przypadki(NA_rep$nrow_total - db_obj$nrow - NA_rep$NA_filter),
                    " odrzuconych na podstawie warunku filtra (`", db_obj$filterstring, "`) oraz ",
                    liczebnik_przypadki(NA_rep$NA_filter),
                    " pochodzących z braków obserwacji zmiennych, które są użyte w filtrze")
      }
      msg_filter<-paste0(msg_filter, ".\n\n")
    } else if (language=='EN') {
      browser()
    } else {
      browser()
    }
  } else {
    msg_filter<-''
  }

  all_labels<-db_obj$all_labels()
  #db1<-readxl::read_xlsx('~/tmp/db1.xlsx')
  db1<-db_obj$chunkdf(flag_include_NA=TRUE)
  NA_pattern<-mice::md.pattern(db1)
  NA_pattern<-NA_pattern[1:(nrow(NA_pattern)-1),1:(ncol(NA_pattern)-1),drop=FALSE]
  NA_pattern<-NA_pattern[,names(db1),drop=FALSE]
  NA_counts<-as.integer(rownames(NA_pattern))
  a<-as.integer(NA_pattern)
  dim(a)<-dim(NA_pattern)
  colnames(a)<-colnames(NA_pattern)
  NA_pattern<-a

  #Remove columns, that are present everywhere
  which_cols_non_missing<-which(plyr::aaply(NA_pattern, 2, sum)<nrow(NA_pattern))
  NA_pattern<-NA_pattern[,which_cols_non_missing,drop=FALSE]

  which_row_non_missing<-which(plyr::aaply(NA_pattern, 1, sum)<ncol(NA_pattern))
  NA_pattern<-NA_pattern[which_row_non_missing,,drop=FALSE]
  NA_counts<-NA_counts[which_row_non_missing, drop=FALSE]
  #At this point NA_pattern lists only non-trivial combinations of missing columns


  if(nrow(NA_pattern)==0) {
    msg<-''
  } else if(nrow(NA_pattern)==1) {
    #Only on row - one pattern of missing data
    # 2. Jeśli tylko jedna z grup != 0, to raportuję ją "[Następnie ]ze zbioru wykluczono <ncases> przypadków pochodzących z braków obserwacji dla zmiennych <które zmienne, po przecinku>". Koniec
    which_cols<-(which(NA_pattern==0))
    all_labels<-all_labels[intersect(names(all_labels), colnames(NA_pattern)[which_cols])]
    df<-tibble(labels = all_labels, .colnames=names(all_labels))
    setattr(df$labels, 'decoration', '')
    msg<-paste0("ze zbioru wykluczono ",
                danesurowe::liczebnik(NA_counts, mianownik = "przypadek pochodzący", dopelniacz = "przypadków pochodzących", lmnoga = "przypadki pochodzące"),
                " z braków obserwacji ", danesurowe::format_item_list(df = df), ". ")
  } else {
    # 3. Jeśli wszystkie niezerowe grupy tworzą dokładnie jedną ściśle rosnącą hierarchię
    # (np. ivNA, ivdvNA, ivdvgvNA, nazwaną dalej NA1, NA2, ..., NAn, a zmienne var1, var2, ... varn),
    # to raportuję "[Ponadto ]ze zbioru wykluczono ", i dalej, dla i = n do 1 (wspak): "<ncases_NA<i>> obserwacji z powodu braków obserwacji zmiennej <var<i>>[, oraz dodatkowo ]"
    # Ostatniego członu nie dodaję dla ostatniej iteracji, tj. dla i=1.
    browser()
    rowsums<-plyr::aaply(NA_pattern, 1, sum)
    ord_rowsums<-order(rowsums, decreasing = TRUE)
    NA_pattern_ord<-NA_pattern[ord_rowsums,,drop=FALSE]
    NA_counts_ord<-NA_counts[ord_rowsums,drop=FALSE]
    if(max(diff(NA_pattern))==0) {
      #Ściśle rosnąca NA_pattern

      vars<-plyr::alply(diff(NA_pattern_ord), 1, function(a) which(a!=0))
      names(vars)<-NULL
      vars<-c(list(which(NA_pattern_ord[1,]==0)),  vars)

      msg<-"ze zbioru wykluczono "
      for(i in seq_len(nrow(NA_pattern_ord))) {
        if(i==2) {
          msg<-paste0(msg, " oraz dodatkowo ")
        } else if (i>2) {
          msg<-paste0(msg, " oraz jeszcze dodatkowo ")
        }
        i_vars<-vars[[i]]
        i_labels<-all_labels[names(i_vars)]
        msg<-paste0(liczebnik_przypadki(NA_counts_ord[[i]]), " z powodu braków obserwacji ")
        df<-tibble(labels = i_labels, .colnames=names(i_labels))
        setattr(df$labels, 'decoration', '')
        msg<-paste0(msg,
                        if(nrow(df)>1) {
                          "zmiennych "
                        } else {
                          "zmiennej "
                        }, danesurowe::format_item_list(df = df))
      }
      msg<-paste0(msg, ". ")
    } else {
      islands<-plyr::alply(1-NA_pattern, 1, function(x) which(x==1))
      combs<-gtools::combinations(length(islands),2)
      intersects<-plyr::aaply(combs, 1, function(x) length(intersect(x[[1]], x[[2]]) ))
      if(sum(intersects)==0) {
        # 4. Jeśli wszystkie niezerowe grupy tworzą ściśle rozłączne zbiory (np. ivNA, dvgvNA), to raportuję:
        # "[Ponadto ]ze zbioru wykluczono ", i dalej, dla każdej z grup: "<ncases> obserwacji z powodu braków obserwacji zmiennej|zmiennych <vars>[, ]"
        # I na końcu ".". Koniec.
        msg<-paste0("ze zbioru wykluczono ")
        for(i in seq_along(islands)) {
          if(i==1) {
            #do nothing
          } else if (i==2) {
            msg<-paste0(msg, ", poza tym wykluczono ")
          } else if (i==length(islands)) {
            msg<-paste0(msg, " oraz ")
          } else {
            msg<-paste0(msg, ", ")
          }
          isl<-islands[[i]]
          pos<-which(names(isl) %in% colnames(NA_pattern))[[1]]
          counts<-NA_counts[ which(NA_pattern[,names(isl)[[1]] ]==0) ]
          i_labels<-all_labels[names(isl)]
          df<-tibble(labels = i_labels, .colnames=names(i_labels))
          setattr(df$labels, 'decoration', '')

          msg<-paste0(msg, liczebnik_przypadki(counts[[pos]]), " z powodu braków obserwacji ",
                      if(nrow(df)>1) {
                        "zmiennych "
                      } else {
                        "zmiennej "
                      }, danesurowe::format_item_list(df = df))
        }
        msg<-paste0(msg, ". ")
      } else {
        # 5. Teraz zostały nam tylko przypadki mieszane. Proponuję rozdzielić je rozpisać w tabelce i/lub zrobić wykres typu https://en.wikipedia.org/wiki/Venn_diagram
        db<-db_obj$chunkdf(flag_include_NA=TRUE)[colnames(NA_pattern)]
        db<-db1[colnames(NA_pattern)]

        uniqs<-plyr::aaply(NA_pattern, 2, function(x) paste0(x, collapse=''))
        uniqs_unique<-!duplicated(uniqs)
        db<-db[,uniqs_unique]
        dbnames<-purrr::map_chr(unique(uniqs), ~paste0(names(which(. == uniqs )), collapse=', '))
        colnames(db)<-dbnames

        lista<-plyr::llply(db, function(x) which(is.na(x)))
        browser()

        #Poniższe linijki rysują (w miarę optymalny) diagram Venna. Pozostało dodać opis, być może tabelkę,
        #i najważniejsze - wkleić prawidłowo wykres, który nie wymaga preprocesowania.

        i_labels<-all_labels[colnames(NA_pattern)]
        df<-tibble(labels = i_labels, .colnames=names(i_labels))
        setattr(df$labels, 'decoration', '')

        msg<-paste0("ze zbioru wykluczono łącznie ", liczebnik_przypadki(sum(NA_counts)),
                    " z powodów braków obserwacji zmiennych ", danesurowe::format_item_list(df = df), ". ",
                    "Analizie statystycznej poddano pozostałe ", liczebnik_przypadki(nrow(db1)), ".")

        df<-tibble('Zmienne'=plyr::alply(1-NA_pattern, 1, function(x) which(x==1) ),
                   'Liczba braków danych'=NA_counts)
        df<-rbind(df, tibble('Zmienne'="wszystkie",
                             'Liczba braków danych'=sum(NA_counts)))
        caption<-paste0("Rozkład częstości braków danych w analizowanym zbiorze i zmiennych za nie odpowiedzialnych. ",
                        "W sumie braki danych w analizowanych zmiennych są odpowiedzialne za ",
                        danesurowe::liczebnik(sum(NA_counts), mianownik = 'brak', dopelniacz = 'braków', lmnoga = 'braki'))
        tags<-c("NA_report","NA_report5")


        if(msg_filter!='') {
          if(msg!='' ) {
            msg<-paste0(msg_filter, "Następnie ", msg)
          } else {
            msg<-msg_filter
          }
        } else {
          if(msg!='' ) {
            msg<-paste0(stringr::str_to_upper(stringr::str_sub(msg, 1,1)),
                        stringr::str_sub(msg, 2))
          }
        }
        chapter$insert_paragraph(msg, tags='NA_report')
        chapter<-chapter$insert_section(text="Pochodzenie braków danych", tags=tags)
        chapter$insert_paragraph(msg, tags=tags)
        chapter$insert_table(caption=caption, table_df=df, tags=tags,  flag_header_in_md=TRUE,
                             emph_rows=c(rep(FALSE, length(NA_counts)), TRUE))
        path<-chapter$get_folders('chart')

        draw_chart_fn<-function() {
          gplots::venn(lista, simplify = TRUE)
        }
        caption<-paste0("Diagram Venna przedstawiający pochodzenie braków danych. Elipsami oznaczono zmienne, które są odpowiedzialne za braki danych a wewnątrze elips liczbę braków pochodzących od tych zmiennych, wewnątrz których liczba jest umieszczona. ")
        chapter$insert_chart(caption=caption, draw_function=draw_chart_fn, chart_prefix="venn", tags=tags)
        return()
      }
    }
  }

  if(msg_filter!='') {
    if(msg!='' ) {
      msg<-paste0(msg_filter, "Następnie ", msg, "Analizie statystycznej poddano pozostałe ", liczebnik_przypadki(nrow(db1)), ".")
    } else {
      msg<-paste0(msg_filter, "Analizie statystycznej poddano pozostałe ", liczebnik_przypadki(nrow(db1)), ".")
    }
  } else {
    if(msg!='' ) {
      msg<-paste0(stringr::str_to_upper(stringr::str_sub(msg, 1,1)),
                  stringr::str_sub(msg, 2), "Analizie statystycznej poddano pozostałe ", liczebnik_przypadki(nrow(db1)), ".")
    }
  }
  if(msg!='') {
    chapter$insert_paragraph(msg, tags='NA_report')
  }


#   Jeśli filtr, to:
#     „Przed wykonaniem analizy zastosowano filtr <nazwa_filtra>, który odrzucił <ncases> przypadków. "
# 	jeśli braki w filtrze, to:
# 		"W tym, <ncases> przypadków odrzuconych na podstawie warunku filtra (`<filterstring>`) oraz <ncases> przypadków pochodzących z braków obserwacji zmiennych, które są użyte w filtrze"
#
# oznaczenia:
# <ivNA> - braki specyficzne dla zn,
# <dvNA> - braki specyficzne dla zz,
# <gvNA> - braki specyficzne dla zm. grupującej,
# <ivdvNA> - braki współdzielone tylko między zn i zz
# <ivgvNA> - braki współdzielone tylko między zn i gv
# <dvgvNA> - braki współdzielone tylko między zz i gv
# <ivdvgvNA> - braki współdzielone przez wszystkie 3 zmienne
#
# 1. Liczę częstości każdej z tych 7 grup
# 2. Jeśli tylko jedna z grup != 0, to raportuję ją "[Następnie ]ze zbioru wykluczono <ncases> przypadków pochodzących z braków obserwacji dla zmiennych <które zmienne, po przecinku>". Koniec
# 3. Jeśli wszystkie niezerowe grupy tworzą dokładnie jedną ściśle rosnącą hierarchię (np. ivNA, ivdvNA, ivdvgvNA, nazwaną dalej NA1, NA2, ..., NAn, a zmienne var1, var2, ... varn), to raportuję "[Ponadto ]ze zbioru wykluczono ", i dalej, dla i = n do 1 (wspak): "<ncases_NA<i>> obserwacji z powodu braków obserwacji zmiennej <var<i>>[, oraz dodatkowo ]"
# Ostatniego członu nie dodaję dla ostatniej iteracji, tj. dla i=1.
# 4. Jeśli wszystkie niezerowe grupy tworzą ściśle rozłączne zbiory (np. ivNA, dvgvNA), to raportuję: "[Ponadto ]ze zbioru wykluczono ", i dalej, dla każdej z grup: "<ncases> obserwacji z powodu braków obserwacji zmiennej|zmiennych <vars>[, ]"
# I na końcu ".". Koniec.
# 5. Teraz zostały nam tylko przypadki mieszane. Proponuję rozdzielić je rozpisać w tabelce i/lub zrobić wykres typu https://en.wikipedia.org/wiki/Venn_diagram
}

group_info_PL<-function(pAcc, what="Wykres ") {

  #boxplot:

  anova_tabela <-"Wyniki analizy wariancji zmiennej zależnej <zz>. Policzono dla tej zmiennej efekt zmiennej niezależnej <zn>[, oddzielnie dla każdego poziomu <zg>]. Obliczenia zostały wykonane na zbiorze obserwacji <filtr>, w którym jest <ncases> ważnych obserwacji."
  boxplot_tabela <-"Tabela ze statystykami opisowymi użytymi w wykresie <fig_hash>. Średnia została podana razem z szerokością 95% przedziału ufności i zaokrąglona do 2 miejsc znaczących, odchylenie standardowe zostało zaokrąglone do 2 miejsc znaczących swojego błędu standardowego."

  wiolinplot_wykres <-"Wykres wiolinowy ilustrujący rozkład <zz> w podziale na <zn>. [Kolorami oznaczono poziomy zmiennej <zg>. ]Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji."
  boxplot_wykres <-"Wykres typu box plot ilustrujący wartości średnie oraz ich błędy standardowe <zz> w podziale na <zn>. [Kolorami oznaczono poziomy zmiennej <zg>. ]Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji. W polach wypisano liczność każdej z grup, a wąsami oznaczono 95% przedział ufności."


  #boxplot wyliczany:

  boxplot_wyliczany_wykres <- "Wykres typu box plot ilustrujący rozkład <zz> w podziale na <zn>. Rozkład wartości został uzyskany metodą bootstrap dla n = <nboostrap> symulacji. [Kolorami oznaczono wykresy odpowiadające poziomom <zg>. ]Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji."

  boxplot_wyliczany_tabela <- "Tabela ze statystykami pozycyjnymi wartości <zz> użytymi w wykresie <fig_hash>. Ponieważ zmienna zależna ma sens dopiero na grupie rekordów, statystyki rozkładu jej wartości można było policzyć tylko przy pomocy techniki bootstrap. Wszystkie kolumny poza „Wartością punktową” zostały policzona na podstawie rozkładu otrzymanego dla <nbootstrap> symulacji bootstrap."

  #crosstab
  crosstab_mosaic_wykres <- "Wykres mozaikowy przedstawiający związek zmiennych <zn>  i <zz>[ w podziale na <zg>]. Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji."
  crosstab_mosaic_tabela <- "Wartości użyte do wykreślenia wykresu <fig_chart> i <fig_inv_chart>."
  crosstab_logit_wykres <- "Wykres typu box plot ilustrujący rozkład logitu udziału <zzlevel1> <filtr> w podziale na <zn>. Rozkład został policzony metodą Monte-Carlo z analitycznego wzoru na odwróconą dystrybuantę rozkładu logit ze zmiennej z rozkładu beta dla n = <nbootstrap> symulacji. 95% kwantyle tego rozkładu zostały oznaczone wąsami.[ Kolorami oznaczono wykresy odpowiadające poziomom <zg>.] [Poszczególne poziomy <zg> zostały posortowane rosnąco.] [W chmurkach podano wielkość zbioru, na którym dany box został policzony.] "
  crosstab_logit_tabela <- "Dane wykorzystane w wykresie <fig_hash>. Kwantyle zostały policzone na podstawie symulacji Monte-Carlo, natomiast Wartość jest wartością punktową dla danej grupy. Aby pozbyć się nieskończoności w przypadkach, gdy liczności jednej z grup są równe 0, policzono logit z dodaną sztucznie 1 obserwacją do obu grup. "
  crosstab_pearson_chi2 <- "Policzono statystyki mierzące związek między <zn> a <zz>[ w rozbiciu na <zg>] używając test[y] niezależności χ²\uA0 Pearsona. Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji."
  crosstab_umann <- "Policzono test U Manna-Whitneya-Wilcoxona porównujący <zz> pomiędzy grupami zdeterminowanymi przez <zz>. Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji[ oddzielnie dla każdego poziomu zmiennej <zg>]."
  crosstab_spearman_rho <- "Policzono statystyki mierzące związek między <zn> a <zz>[ w rozbiciu na <zg>] używając korelacji ρ (rho) Spearmana. Analizę wykonano na zbiorze: <filtr>, w którym jest <ncases> ważnych obserwacji."

  #ts_nominal
  ts_nominal_periodogram_wykres <- "Standaryzowane periodogramy dla częstości występowania każdego z poziomów <zn>[w podziale na <zg>]. Periodogram policzono przy pomocy procedury `spec.pgram` pakietu R. [Linią przerywaną oraz punktami zaznaczono periodogram (bez wygładzania), natomiast linią ciągłą periodogram wygładzony przy pomocy metody Daniella z szerokościami jądra wygładzania (_spans_) równymi 3 i 3. ]|[Linią zaznaczono periodogram wygładzony przy pomocy metody Daniella z szerokościami jądra wygładzania (_spans_) równymi 3 i 3.] Oś Y wyznacza gęstość widma, tj. kwadrat transformaty Fouriera sygnału. Jednostką gęstości jest kwadrat jednostki zmiennej zależnej pomnożony przez jednostkę czasu. [Nie zaznaczono jednostki na osi Y, aby nie przeładować ryciny informacjami. Bezwzględne wartości widma nie są tak ważne jak kształt wykresu.]"
  ts_nominal_seasons_wykres <- "Wykres przedstawiający komponent sezonowy częstości występowania poziomów <zz> względem <zn>[ w podziale wg. <zg>]. Błędy standardowe policzono przy użyciu bayesowskiego modelu miksującego błędy liczone poprzez estymator odchylenia standardowego pomiędzy latami i błędy liczone z rozkładu Dirichleta. Parametr miksujący, τ, zbiega do zera, jeśli pomiary dla każdego roku w danym miesiącu są ze sobą zgodne oraz do odchylenia standardowego między latami, jeśli pomiary proporcji zmiennej grupującej pomiędzy latami w danym miesiącu nie są zgodne. [Najmniejszą podziałką osi X oznaczono <nice_x_unit>.]"
}
