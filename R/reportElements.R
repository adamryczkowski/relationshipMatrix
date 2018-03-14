#Abstract class that can print/render the result.

#It stores a single report element. All objects support the following:
#* method for rendering (render) to markdown - a function that simply return a string. This function might take a while to compute,
#  and it might have side-effects (like creating picture files)
#* tags. Each output contain a set of string tags, that can be used to filter out the contents.
#* serialization method (implicit method, realized by saveRDS() function)
#* double link to the encapsulating object of type chapter (also child of the type reportElement)
#* render_reference(target_object) - renders text that is inserted when our object is referenced somewhere else. This
#  function will be overloaded by Chart or Table to get a custom form of text for tables or illustrations.
#  page number, chapter number, chapter name or anything else - if only supported.
#* store_statistical_method() - private method, that returns object of the type Chapter. It will store the contents
#  of the statistical methods.
#* access to special, global containers, such as
#  * Statistical methods used
#  * Definitions of transformed variables (in future)
#  * List of illustrations (not implemented)
#  * List of tables (not implemented)
#
#List of actual objects:
#1. Paragraph.
#   This is the most basic element. It basically returns the stored paragraph. Nothing more. It actually may not even finish in
#   line break, thus it can be used to provide optional text.
#2. Section.
#   This object contains list of subobjects of the type reportElement. Besides the list it contains the text (chapter name).
#   Chapter can also contain a serial number.
#3. Table.
#   Table contains a data.frame with the fields (together with some attributes), and a caption text. It has overriden
#   make_reference() function
#4. ggplot Chart.
#   Chart contains a ggplot object and label. Rendering will first plot the chart into a proper graphics format, which might
#   take a while. Like table, it also overrides a make_reference() function.
#5. Statistical_Methods.
#   Container that allows storing entries about each used method. Internally each entry is stored as chapter. Object also
#   keep track of all places that use the method. The same object can be used to store definitions of transformed variables.
#6. List_of_illustrations.
#   Container similiar to the Statistical Methods, but with a simple text for each entry instead of chapter. Also keeps track
#   of all back references.
#7. Reference.
#   Object that renders a link to the reference, based on the type of the targeted object. Internally it stores
#   reference to the targeted object, and it is the targeted object that is called, when the reference is rendered.
#8. LibraryText.
#   Standard piece of text, that is not stored in the analysis, but in the R library, by the user-supplied function.
#   Each piece of text is parametrized by a single string.
#9. Statistical_Methods_Catcher.
#   Function that implements exactly the same interface as the Statistical_Methods, but instead of
#
# List of all reserved properties:
# cellnr - number of cell responsible for the output
# dv, iv, gv - dependent, independent and grouping variable (or aggregate)
# f - filter string
# d - dispatcher used
# chart_debug - flag. If TRUE, debug string will be appended to the chart label
# chart_dpi - DPI of the generated charts. Defaults to 450
# chart_postprocess - flag. If FALSE, the chart will not be pre-processed. It will result in much faster rendering

dynamic_cast<-function(R6Object, class_name) {
  while(!'class_name' %in% class(R6Object)) {
    if(!'super' %in% names(R6Object$.__enclos_env__)) {
      R6Object<-NULL
      break
    }
    R6Object<-R6Object$.__enclos_env__$super
  }
  return(R6Object)
}


#This is a root of all report elements
doc_reportElement<-R6::R6Class(
  "doc_reportElement",
  public = list(
    initialize=function(parent, tags) {
      checkmate::assertCharacter(tags)
      checkmate::checkClass(parent, classes = 'doc_container')
      private$parent_<-parent
      private$tags_<-tags
    },
    render=function(doc) {
      browser() #This function must be overriden
    },
    pre_render=function() {
      #This function optimizes object into printing. This function may be called in parallel.
      #In this particular class - we do nothig.
    },
    render_reference=function(doc, target_element) {
      browser() #This function must be overriden
    },
    get_tags=function() {
      #Gets list of all tags
      if(!is.null(private$parent_)) {
        tags<-private$parent_$get_tags()
      } else {
        tags<-character(0)
      }
      return(union(tags, private$tags_))
    },
    has_tag=function(tag) {
      #This function can be more efficient
      all_tags<-self$get_tags()
      return(tag %in% all_tags)
    },
    debug_text=function() {
      paste0("cellnr: **", self$get_property("cellnr", -1), ' (row ', self$get_property("rownr", -1), ", col ", self$get_property("colnr", -1), ")",
             "**, dv: **", self$get_property("dv", "?"),
             "**, iv: **", self$get_property("iv", "?"),
             "**, gv: **", self$get_property("gv", "?"),
             "**, f: **", self$get_property("f", "?"),
             "**, d: ", self$get_property("dispatcher", "?"))
    },
    get_property=function(property_name, default_value=NULL) {
      if(property_name %in% names(private$properties_)) {
        return(private$properties_[[property_name]])
      } else {
        if(is.null(private$parent_)) {
          return(default_value)
        } else {
          return(private$parent_$get_property(property_name, default_value))
        }
      }
    },
    set_property=function(property_name, value, indepth_level=0) {
      checkmate::assertTRUE(indepth_level>=0)
      if(indepth_level==0) {
        private$properties_[[property_name]]<-value
      } else {
        for(ch in private$children_) {
          ch$set_property(property_name = property_name, value=value, indepth_level=indepth_level-1)
        }
      }
    },
    get_folders=function(folder_type) { #Returns special folder path
      private$parent_$get_folders(folder_type)
    },
    store_statistical_method=function(chapter_name, chapter) {
      self$parent$store_statistical_method(chapter_name=chapter_name, chapter=chapter)
    }
  ),
  active = list(
    stat_methods = function() {private$stat_methods_},
    parent = function() {private$parent_},
    discard_changes=function(value) {#If true it will discard any changes (feature used in the discovery mode)
      if(missing(value)) {
        private$parent_$discard_changes
      } else {
        private$parent_$discard_changes<-newValue
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    parent_=NULL, #Parent object (of type reportElement)
    tags_=character(0),
    properties_=list()
  )
)

#Object of this type is capable of storing other objects inside
doc_container<-R6::R6Class(
  "doc_container",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, add_depth=1, chart_foldername='', cache_foldername='') {
      checkmate::assertInt(add_depth)
      checkmate::assertTRUE(add_depth>=0)
      checkmate::assertString(cache_foldername)
      checkmate::assertString(chart_foldername)
      super$initialize(parent=parent, tags=tags)
      private$depth_weight_<-add_depth

      private$chart_foldername_<-chart_foldername
      private$cache_foldername_<-cache_foldername

    },
    render = function(doc) {
      for(obj in private$children_) {
        obj$render(doc)
      }
    },
    pre_render=function() {
      for(obj in private$children_) {
        obj$pre_render()
      }
    },
    address_string=function() {
      if(!is.null(private$parent_)) {
        return(private$parent_$address_string())
      } else {
        return('')
      }
    },
    depth=function() {
      if(!is.null(private$parent_)) {
        base_depth<-self$parent$depth()
      } else {
        base_depth<-0
      }
      base_depth<-base_depth+private$depth_weight_
      return(base_depth)
    },
    #Merges the current chapter with another. All the sections inside chapter_to_merge_with go to the corresponding sections in our document.
    #If the current document doesn't have a section, it gets added on the end. The same with the contents outside any subsections.
    #The chapter_to_merge will be destroyed after the operation.
    merge_with_chapter=function(chapter_to_merge) {
      browser()
      source<-chapter_to_merge
      source_items<-source$.__enclos_env__$private$children_
      for(source_item in source_items) {
        if('doc_section' %in% class(source_item)) {
          target_chapter<-self$get_child_chapter_by_name(source_item$title)
          if(is.null(target_chapter)) {
            add_element(source_item)
          } else {
            target_chapter$merge_with_chapter(source_item)
          }
        } else {
          add_element(source_item)
        }
      }
    },
    get_folders=function(folder_type) { #Returns special folder path
      if(!is.null(private$parent_)) {
        pathcat::path.cat(private$parent_$get_folders(folder_type), private$get_folder_direct(folder_type))
      } else {
        private$get_folder_direct(folder_type)
      }
    },
    add_element=function(obj){ #Ignoring insetion when we are in the discard state
      checkmate::assertClass(obj, 'doc_reportElement')
      if(!self$discard_changes) {
        if(!identical(obj$parent, self)) {
          obj$.__enclos_env__$private$parent_<-self
        }
        checkmate::assertTRUE(identical(obj$parent, self))
        private$children_[[length(private$children_)+1]]<-obj
      }
    },
    get_child_chapter_by_name=function(chapter_name) {
      for(ch in private$children_) {
#        ch<-dynamic_cast(R6Object = ch, class_name = 'doc_section')
        if('doc_section' %in% class(ch)) {
          if(!is.null(ch)){
            if(ch$title == chapter_name) {
              return(ch)
            }
          }
        }
      }
      return(NULL)
    },
    get_child_chapter_names=function() {
      ans<-rep(NA_character_, length(private$children))
      i<-1
      for(ch in private$children_) {
        if('section' %in% class(ch)) {
          ans[[i]]<-ch$title
          i<-i+1
        }
      }
      ans<-ans[seq_len(i-1)]
      return(ans)
    }
  ),
  active = list(
    discard_changes=function(value) {#If true it will discard any changes (feature used in the discovery mode)
      if(is.null(private$parent_)) {
        browser()
      }
      if(missing(value)) {
        private$parent_$discard_changes
      } else {
        private$parent_$discard_changes<-newValue
      }
    }
  ),

  private = list(
    get_folder_direct=function(folder_type) {
      if(folder_type=='chart') {
        return(private$chart_foldername_)
      } else if (folder_type=='cache') {
        return(private$cache_foldername_)
      } else {
        browser() #Unknown folder type
      }
    },
    next_free_section_number=function() {
      sec_nr<-1
      for(i in seq_along(private$children_)) {
        ch<-private$children[[i]]
        if('doc_container' %in% class(ch)) {
          sec_nr<-sec_nr+1
        }
      }
      return(sec_nr)
    },
    children_ = list(),
    chart_foldername_='',
    cache_foldername_='',
    depth_weight_=1 #How deep this container counts in depth()
  )
)

#This object only provides user-visible interface for adding tables, charts, paragraphs and sections
doc_Insertable<-R6::R6Class(
  "doc_Insertable",
  inherit = doc_container,
  public = list(
    initialize=function(parent, tags, chart_foldername='', cache_foldername='', depth_weight=1) {
      super$initialize(parent = parent, tags = tags, depth_weight = depth_weight,
                       chart_foldername=chart_foldername, cache_foldername=cache_foldername)
    },
    insert_paragraph=function(text, tags=character(0)) {
      par<-doc_paragraph$new(parent = self, tags = tags, text = text)
      self$add_element(par)
      return(invisible(NULL))
    },
    insert_section=function(text, tags=character(0), chart_foldername='', cache_foldername='') {
      #browser() #Napisz kod, który prawidłowo wstawia labels
      sec_nr<-private$next_free_section_number()
      sec<-doc_section$new(parent = self, tags = tags, text=text, number=sec_nr,
                           chart_foldername = chart_foldername, cache_foldername = cache_foldername)
      self$add_element(sec)
      return(sec)
    },
    insert_table=function(caption, table_df, tags=character(0), flag_header_in_md=FALSE,
                          emph_rows=NULL, emph_cols=NULL,
                          strong_rows=NULL, strong_cols=NULL) {
      if(flag_header_in_md) {
        #browser()
#        table_df<-data.table(table_df)
        for (i in seq_along(table_df)) {
          attr(table_df[[i]], 'label')<-colnames(table_df)[[i]]
        }
      }
      #browser()
      tbl<-doc_table$new(parent=self, tags=tags, table_caption=caption, table_df=table_df,
                         emph_rows=emph_rows, emph_cols=emph_cols,
                         strong_rows=strong_rows, strong_cols=strong_cols)
      self$add_element(tbl)
      return(tbl$label)
    },
    insert_chart=function(caption, gg, chart_prefix, tags=character(0)) {
      cht<-doc_chart$new(parent=self, tags=tags, chart_caption=caption, gg=gg, chart_prefix = chart_prefix)
      self$add_element(cht)
      return(cht$label)
    },
    insert_reference_chapter=function(title, id) {
      browser() #TODO
    }
  )
)

# This is a renderable section. It makes sense only on the main, renderable document
doc_section<-R6::R6Class(
  "doc_section",
  inherit = doc_Insertable,
  public = list(
    initialize=function(parent, tags, text, number=NA, chart_foldername='', cache_foldername='', depth_weight=1) {
      checkmate::assertString(text)
      checkmate::assertNumber(number, na.ok=TRUE)
      checkmate::assertInt(depth_weight)
      checkmate::assertTRUE(depth_weight>0)
      super$initialize(parent = parent, tags = tags,
                       depth_weight = depth_weight,
                       chart_foldername=chart_foldername, cache_foldername=cache_foldername)
      private$text_<-text
      private$number_<-number
    },
    render=function(doc) {
#      if(!is.na(private$number_)) {
#        text<-paste0(c(private$number_, self$parent$address_string()), collapse = '.')
#      } else {
#        text<-''
#      }

      text<-pander::pandoc.header.return(private$text_, level = self$depth())
      if(is.na(private$number_)) {
        text<-paste0(text, "{.unnumbered}")
      }
      doc$add.paragraph(text)
      super$render(doc)
    },
    address_string=function() {
      text<-paste0(c(private$number_, self$parent$address_string()), collapse = '.')
      return(text)
    },
    render_reference=function(doc, target_element) {
      text<-paste0(self$address_string(), " ", private$text_)
      doc$add.paragraph(text)
    }
  ),
  active = list(
    title = function() {private$text_}
  ),
  private = list(
    text_='',
    number_=NA_real_
  )
)

# Simple paragraph text
doc_paragraph<-R6::R6Class(
  "doc_paragraph",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, text ) {
      checkmate::assertCharacter(text)
      super$initialize(parent=parent, tags=tags)
      private$text_<-text
    },
    render=function(doc) {
      doc$add.paragraph(private$text_)
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    text_=''
  )
)

#Main document. It is basically container that stores header info.
doc_Document<-R6::R6Class(
  "doc_Document",
  inherit = doc_Insertable,
  public = list(
    initialize=function(chart_foldername=NULL, cache_foldername=NULL, author, format='docx', title, depth_weight=0) {
      super$initialize(parent=NULL, tags=character(0), depth_weight = depth_weight,
                       cache_foldername=cache_foldername, chart_foldername=chart_foldername)
      checkmate::testString(author)
      checkmate::testString(title)
      checkmate::testString(format)

      private$author_<-author
      private$format_<-format
      private$title_<-title
    },
    render=function(doc) {
      doc$author<-private$author_
      doc$format<-private$format_
      doc$title<-private$title_
      super$render(doc)
    },
    get_folders=function(folder_type) { #Returns special folder path
      if(folder_type=='chart') {
        folder<-private$chart_foldername_
      } else if(folder_type=='cache') {
        folder<-private$chart_foldername_
      } else {
        browser() #Unknown folder_type
      }
      return(pathcat::path.cat(getwd(), private$cache_foldername_))
    },
    get_chapter_by_path=function(path_list) {
#      browser()
      ch<-self
      for(path_elem in path_list[[1]]) {
        ch <- ch$get_child_chapter_by_name(path_elem)
        if(is.null(ch)) {
          browser() #Chapter not found
          break
        }

      }
      return(ch)
    }

  ),
  active = list(
    stat_methods = function() {private$stat_methods_},
    parent = function() {private$parent_},
    discard_changes=function(value) {#If true it will discard any changes (feature used in the discovery mode)
      if(missing(value)) {
        FALSE
      } else {
        if(value!=FALSE) {
          browser()
        }
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    author_="Adam Ryczkowski",
    format_="docx",
    title_=''
  )
)

#This is a standalone chapter. Everyone can make one.
#It is incapable of rendering unless it is a part of a bigger whole.
#It has two uses:
#1. as a holder of statistical methods
#2. as a holder output generated by a statistics generator
doc_Standalone_Chapter<-R6::R6Class(
  "doc_Standalone_Chapter",
  inherit = doc_Insertable,
  public = list(
    initialize=function(chart_foldername='', cache_foldername='') {
      super$initialize(parent = NULL, tags = character(0),
                       depth_weight=0, chart_foldername=chart_foldername,
                       cache_foldername=cache_foldername)
    },
    set_parent=function(newparent) {
      if(!is.null(private$parent_)) {
        stop("Parent already assigned")
      }
      private$parent_<-newparent
    },
    render=function(doc) {
      if(is.null(private$parent_)) {
        stop("Cannot render unassigned chapter")
      } else {
        super$render(doc)
      }
    },
    #Inserts our contents into the target chapter
    insert_into=function(target_chapter) {
      for(ch in private$children_) {
        target_chapter$add_element(ch)
      }
      #browser()
      props<-private$properties_
      for(i in seq_along(props)) {
        target_chapter$set_property(names(props)[[i]], props[[i]])
      }

    }
  ),
  active = list(
    discard_changes=function(value) {#If true it will discard any changes (feature used in the discovery mode)
      if(missing(value)) {
        private$discard_changes_
      } else {
        private$discard_changes_<-value
      }
    }
  ),
  private = list(
    discard_changes_=FALSE
  )
)


#Statistical_Methods
#This container only stores (and prints) all statistical methods.
#Each statistical method is implemented as a custom chapter.
#header info.
#TODO
doc_Statistical_Methods<-R6::R6Class(
  "doc_Statistical_Methods",
  inherit = doc_container,
  public = list(
    initialize=function(parent) {
      super$initialize(parent = parent, tags=character(0),
                       depth_weight = 0, foldername='')
    },
    store_statistical_method=function(chapter_name, chapter) {
      if(chapter_name %in% names(private$methods_)) {
        #do nothing. Chapter already exists
      } else {

      }
      sec_nr<-private$next_free_section_number()
      sec<-doc_section$new(parent = self, tags = tags, text=text, number=sec_nr, foldername = foldername)
      return(sec)
      self$add_element(par)

      self$parent$store_statistical_method(keyname, chapter_name)
    }
  ),
  active = list(
    stat_methods = function() {private$stat_methods_},
    parent = function() {private$parent_}
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    methods_=list()#keyed list with chaptername as keys
  )
)


save_report<-function(report, filename='/tmp/report', flag_open = TRUE) {
  pander::panderOptions('big.mark', '\uA0')
  pander::panderOptions('missing', 'b/d')
  pander::panderOptions('date', '%Y-%m-%d\uA0%X')
  pander::panderOptions('use.hyphening', TRUE)
  pander::panderOptions('decimal.mark', getOption("OutDec"))
  tmpfile <- tempfile(pattern='report_', tmpdir = getwd(), fileext = '')
  report$export(tmpfile, open=FALSE,
                options='-f markdown+implicit_header_references +RTS -K100000000 -RTS --filter pandoc-fignos --filter pandoc-tablenos -M "tablenos-caption-name:Tabela" -M "fignos-caption-name:Rycina"')
  file.copy(paste0(tmpfile,'.md'), paste0(filename, '.md'))
  unlink(paste0(tmpfile,'.md'))
  file.copy(paste0(tmpfile,'.', report$format), paste0(filename, '.', report$format))
  unlink(paste0(tmpfile,'.', report$format))
}
