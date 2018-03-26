
doc_table<-R6::R6Class(
  "doc_table",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, table_caption, table_df,
                        emph_rows=NULL, emph_cols=NULL,
                        strong_rows=NULL, strong_cols=NULL) {
      checkmate::assertString(table_caption)
      checkmate::assertTRUE(any(c('data.frame', 'character') %in% class(table_df)) )
#      checkmate::assertClass(table_df, 'data.frame')
      super$initialize(parent=parent, tags=tags)
      private$table_caption_<-table_caption
      #browser()
      private$emph_rows_<-emph_rows
      private$emph_cols_<-emph_cols
      private$strong_rows_<-strong_rows
      private$strong_cols_<-strong_cols

      private$table_df_<-table_df
      cell_hash<-self$get_property('cell_hash')
      private$tab_label_ <- generate_table_hash(cell_hash = cell_hash, label = table_caption)
    },
    render=function(doc) {
      #browser()
      if('data.frame' %in% class(private$table_df_)) {
        msg<-add_simple_table(tab = private$table_df_, caption = private$table_caption_, tab_label = private$tab_label_,
                              emph_rows=private$emph_rows_, emph_cols=private$emph_cols_,
                              strong_rows=private$strong_rows_, strong_cols=private$strong_cols_)
      } else {
        msg<-private$table_df_
      }

      doc$add.paragraph(paste0('\n\n', msg, '\n\n'))
    }
  ),
  active = list(
    label = function() {return(private$tab_label_)}
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    table_caption_='',
    table_df_=NULL,
    tab_label_='',
    emph_rows_=NULL,
    emph_cols_=NULL,
    strong_rows_=NULL,
    strong_cols_=NULL
  )
)


generate_table_hash<-function(prefix='', cell_hash, label, file_prefix='') {
  ans<-list(cell_hash=cell_hash, label=label, file_prefix=file_prefix)

  return(paste0(prefix, gen_nice_serial(ans, 8)))
}

serial_alphabet<-c(setdiff(LETTERS, c('B','D','I','O','Z')), 0:9)

gen_nice_serial<-function(args, str_length=6) {
  args<-args[order(names(args))]
  hash<-as.integer(digest::digest(args, raw=TRUE))
  stringr::str_sub(paste0(serial_alphabet[hash %% length(serial_alphabet
  )], collapse=''),1,str_length)
}


add_simple_table<-function(tab, caption, tab_label='',  quote_varname='`', flag_first_row_has_headers=FALSE,
                           emph_rows=NULL, emph_cols=NULL,
                           strong_rows=NULL, strong_cols=NULL) {
  #browser()
  if(! 'data.frame' %in% class(tab)) {
    browser()
  }
  mycolnames<-danesurowe::GetVarLabel(dt =  tab , varname = colnames(tab), quote_varname = quote_varname)
  if(flag_first_row_has_headers) {
    tab1<-rbind(mycolnames, as.matrix(tab)[-1,])
  } else {
    tab1<-rbind(mycolnames, as.matrix(tab))
  }

  qual1 <- evaluate_qual_of_table(tab1)

  #  tmp <- t(rbind(colnames(tab1), as.matrix(tab1))) %>% as_tibble()
  #  colnames(tmp) <- tmp[1,]
  #  tab2<-tmp[-1,]
  qual2 <- evaluate_qual_of_table(t(tab1))
  rownames(tab1)<-NULL
  colnames(tab1)<-mycolnames
  tab1<-tab1[-1,]
  if(qual1 < qual2) {
    tab <- t(tab1)
  } else {
    tab <- tab1
  }
  #  colnames(tab)<-mycolnames
  if(tab_label!='') {
    data.table::setattr(tab, 'pandoc_attributes', paste0(' {#tbl:', tab_label, '}'))
  }

  if(missing(caption)){
    e<-tryCatch(
      ret <- pander::pandoc.table.return(tab, missing='', big.mark='\uA0', split.tables = 1000000, use.hyphening = TRUE,
                                         emphasize.italics.rows=emph_rows, emphasize.italics.cols=emph_cols,
                                         emphasize.strong.rows=strong_rows, emphasize.strong.cols=strong_cols),
      error=function(e) e
    )
    if ('error' %in% class(e)) {
      ret <- pander::pandoc.table.return(tab, missing='', big.mark='\uA0', split.tables = 1000000, use.hyphening = FALSE,
                                         emphasize.italics.rows=emph_rows, emphasize.italics.cols=emph_cols,
                                         emphasize.strong.rows=strong_rows, emphasize.strong.cols=strong_cols)
    }
  } else {
    e<-tryCatch(
      ret <- pander::pandoc.table.return(tab, caption=caption, missing='',
                                         big.mark='\uA0', split.tables = 1000000, use.hyphening = TRUE,
                                         emphasize.italics.rows=emph_rows, emphasize.italics.cols=emph_cols,
                                         emphasize.strong.rows=strong_rows, emphasize.strong.cols=strong_cols),
      error=function(e) e
    )
    if ('error' %in% class(e)) {
      ret <- pander::pandoc.table.return(tab, caption=caption, missing='',
                                         big.mark='\uA0', split.tables = 1000000, use.hyphening = FALSE,
                                         emphasize.italics.rows=emph_rows, emphasize.italics.cols=emph_cols,
                                         emphasize.strong.rows=strong_rows, emphasize.strong.cols=strong_cols)
    }
  }
  return(ret)
}


evaluate_qual_of_table<-function(tab) {
  option_max_width = 130
  sum=0
  dims<-evaluate_dims_of_table(tab)
  if (dims$width > option_max_width){
    sum=-100 - dims$width
  }
  return(sum + abs(log(dims$width *1.3 / dims$height)))
}

evaluate_dims_of_table<-function(tab){
  option_char_width = 1
  option_line_width = 3
  option_char_height = 2
  option_line_height = 1
  colnames(tab)<-NULL
  rownames(tab)<-NULL

  sizes <- pmin(nchar(trimws(tab)),14)
  sizes[is.na(sizes)]<-2
  col_sizes <- plyr::aaply(sizes, 2, max)
  width <- sum(col_sizes) * option_char_width + option_line_width * (ncol(tab)+1)
  height <- (nrow(tab)+1) * option_line_height + nrow(tab) * option_char_height

  return(list(width=width, height=height))
}
