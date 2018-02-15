#This code produces a report list based on the dispatch_reports and calls to the subsequent reporting functions.
#
#TODO: Turn this function into a task generated code, which will define tasks based on the output of the
#report_dispatcher, so the task might be run in parallel (maybe using HPC).
#
#
#Funkcja relationshipMatrix::dispatch_reports() używa report_dispatcher, aby wygenerować listę
#elementów raportu. Każdy element tej listy zawiera pojedynczy krok raportu: tekst,
#rozdział, przypis, wykres, tabelka, itp.
#Lista ta jest składana z wywołań pojedynczych funkcji generowanych przez report_dispatcher
#
#
#Wejście: report_dispatcher (funkcja), propertyAccessor(obiekt), statistics(obiekt)
#
#report_dispatcher jest funkcją, której kod jest dostarczona przez yuxiaCharts razem z funkcją dispatcher,
#która generuje listę wywołań funkcji tworzących pojedyncze kroki raportu, w formie listy o elementach typu
#reportingFunction(propertyAccessor, statistics) oraz zaktualizowany obiekt typu propertyAccessor (obiekt tego typu
#jest przekazywany by reference, więc nie ma potrzeby jego przekazywać).
#
#
#Wyjście: Lista wywołań funkcji, które generują raporty z obliczeniami.
#


#library(R6) #Not needed actually; set by the task for us

paprivate<-pAcc$.__enclos_env__$private
paprivate$reinit(propertyAccessor_cannonized, chunkdf)
report_functions<-report_dispatcher(pAcc, statistics)
propertyAccessor_cannonized<-paprivate$cannonize()
