gather_all_tags<-function(doc) {
  get_tags_fn<-function(doc) {
    return(doc$.__enclos_env__$private$tags_)
  }
  ans<-iterate(doc=doc, fn=get_tags_fn)
  return(dplyr::arrange(data.frame(table(unlist(ans$value))), -Freq))
}

filter_doc<-function(doc, exclude_tags=character(0), include_tags=character(0), flag_include_tables=TRUE, flag_include_charts=TRUE, flag_include_paragraphs=TRUE, flag_remove_empty_sections=TRUE) {
  filter_fn<-function(doc) {
    if('doc_container' %in% class(doc)) {
      return(NA)
    }
    is_included(doc=doc,  exclude_tags=exclude_tags, include_tags=include_tags, flag_include_tables=flag_include_tables,
                flag_include_charts=flag_include_charts, flag_include_paragraphs=flag_include_paragraphs, flag_remove_empty_sections=flag_remove_empty_sections)
  }
  filtered_doc<-filter_doc_rec(doc, filter_fn)
  return(filtered_doc)
}

filter_doc_rec<-function(doc, filter_fn) {
  #It gathers a copy of the document using the predicate filter_fn to decide whether to include a leaf or not.
  #filter_fn returns one of the three values:
  #TRUE - include this node and all its children,
  #FALSE - remove this node all its children,
  #NA - descend the node, and include it only if there is at least one included child
  filtr<-filter_fn(doc)
  checkmate::assertFlag(filtr, na.ok=TRUE )
  if(is.na(filtr)) {
    if('doc_container' %in% class(doc)) {
      children<-doc$.__enclos_env__$private$children_
    } else {
      children<-list()
    }
    ans<-NULL
    for(chnr in seq_along(children)) {
      ch<-children[[chnr]]
      ans_ch<-filter_doc_rec(ch, filter_fn)
      if(!is.null(ans_ch)) {
        if(is.null(ans)) {
          ans<-doc$clone(deep=FALSE)
          ans$.__enclos_env__$private$children_<-list(ans_ch)
        } else {
          ans$.__enclos_env__$private$children_<-c(ans$.__enclos_env__$private$children_, list(ans_ch))
        }
        ans_ch$.__enclos_env__$private$parent_<-ans
      }
    }
    return(ans)
  } else if(filtr) {
    return(doc$clone(deep=TRUE))
  } else {
    return(NULL)
  }
}

#Function executes fn on each element of the tree of doc, that passes the conditions
iterate<-function(doc, fn,  exclude_tags=character(0), include_tags=character(0), flag_include_tables=TRUE, flag_include_charts=TRUE, flag_include_paragraphs=TRUE,
                  flag_remove_empty_sections=TRUE) {
  ans<-iterate_rec(doc=doc, fn=fn, exclude_tags=exclude_tags, include_tags=include_tags, flag_include_tables=flag_include_tables,
                   flag_include_charts=flag_include_charts, flag_include_paragraphs=flag_include_paragraphs, flag_remove_empty_sections=flag_remove_empty_sections)

  fn_count<-function(item) {
    if('value' %in% names(item)) {
      if(length(item$value)>0) {
        ans<-1
      } else {
        ans<-0
      }
    } else {
      ans<-0
    }
    if('children' %in% names(item)) {
      for(it in item$children) {
        ans<-ans+fn_count(it)
      }
    }
    return(ans)
  }

  nodes_count<-fn_count(ans)
  df<-data.table::data.table(value=rep(list(list()), nodes_count ), path=rep(list(list()), nodes_count), class=rep(NA_character_, nodes_count))

  fn_gather<-function(item, row, path) {
    if('value' %in% names(item)) {
      el<-item$value
      if(length(el)>0) {
        data.table::set(df, i = row, j = 'value', value = list(list(item$value)))
        data.table::set(df, i = row, j = 'path', value = path)
        data.table::set(df, i = row, j = 'class', value = item$class)
        row<-row+1
      }
    }
    if('children' %in% names(item)) {
      for(i in seq_along(item$children)) {
        it<-item$children[[i]]
        row<-fn_gather(it, row, c(path, i))
      }
    }
    return(row)
  }
  #debug(fn_gather)
  fn_gather(ans, 1, character(0))
  return(df)
}


iterate_rec<-function(doc, fn,  exclude_tags=character(0), include_tags=character(0), flag_include_tables=TRUE, flag_include_charts=TRUE, flag_include_paragraphs=TRUE,
                      flag_remove_empty_sections=TRUE) {
  should_include<-is_included(doc=doc, exclude_tags=exclude_tags, include_tags=include_tags, flag_include_tables=flag_include_tables,
              flag_include_charts=flag_include_charts, flag_include_paragraphs=flag_include_paragraphs, flag_remove_empty_sections=flag_remove_empty_sections)

  if(should_include) {
    ans<-list(value=fn(doc))
  } else {
    ans<-list()
  }
  if('doc_container' %in% class(doc)) {
    children<-doc$.__enclos_env__$private$children_
  } else {
    children<-list()
  }

  ch_ans_list<-list()
  for(chnr in seq_along(children)) {
    ch<-children[[chnr]]
    ch_ans<-iterate_rec(doc=ch, fn=fn, exclude_tags=exclude_tags, include_tags=include_tags, flag_include_tables=flag_include_tables,
                   flag_include_charts=flag_include_charts, flag_include_paragraphs=flag_include_paragraphs, flag_remove_empty_sections=flag_remove_empty_sections)
    if(length(ch_ans)>0) {
      ch_ans_list[[chnr]]<-ch_ans
    }
  }
  if(length(ch_ans_list)>0) {
    ans<-c(ans, list(children=ch_ans_list))
  }
  return(c(ans, class=class(doc)[[1]]))
}

is_included<-function(doc,  exclude_tags=character(0), include_tags=character(0), flag_include_tables=TRUE, flag_include_charts=TRUE, flag_include_paragraphs=TRUE,
                      flag_remove_empty_sections=TRUE) {
  mytags<-doc$.__enclos_env__$private$tags_
  if(any(exclude_tags %in% mytags )){
    return(FALSE)
  }
  if(length(include_tags)>0) {
    if(! any(include_tags %in% mytags)) {
      return(FALSE)
    }
  }
  if( any(c('doc_ggplot_chart', 'doc_plot_chart') %in% class(doc)) && !flag_include_charts) {
    return(FALSE)
  }
  if( 'doc_table' %in% class(doc) && !flag_include_tables) {
    return(FALSE)
  }
  if( 'doc_paragraph' %in% class(doc) && !flag_include_paragraphs) {
    return(FALSE)
  }
  if( 'doc_section' %in% class(doc) && flag_remove_empty_sections) {
    if(length(doc$.__enclos_env__$private$children_)==0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
