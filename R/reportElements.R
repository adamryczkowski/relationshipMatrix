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
    get_folders=function(folder_type) { #Returns special folder path
      private$parent_$get_folders(folder_type)
    },
    store_statistical_method=function(chapter_name, chapter) {
      self$parent$store_statistical_method(chapter_name=chapter_name, chapter=chapter)
    }
  ),
  active = list(
    stat_methods = function() {private$stat_methods_},
    parent = function() {private$parent_}
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    parent_=NULL, #Parent object (of type reportElement)
    tags_=character(0)
  )
)

#Object of this type is capable of storing other objects inside
doc_container<-R6::R6Class(
  "doc_container",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, flag_add_depth=TRUE, chart_foldername='', cache_foldername='') {
      checkmate::assert_flag(flag_add_depth)
      checkmate::assertString(cache_foldername)
      checkmate::assertString(chart_foldername)
      super$initialize(parent=parent, tags=tags)
      private$flag_add_depth_<-flag_add_depth

      private$chart_foldername_<-chart_foldername
      private$cache_foldername_<-cache_foldername
    },
    render = function(doc) {
      for(obj in private$children_) {
        obj$render(doc)
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
      if(private$flag_add_depth_) {
        base_depth<-base_depth+1
      }
      return(base_depth)
    },
    get_folders=function(folder_type) { #Returns special folder path
      if(!is.null(private$parent_)) {
        pathcat::path.cat(private$parent_$get_folders(folder_type), private$get_folder_direct(folder_type))
      } else {
        private$get_folder_direct(folder_type)
      }
    },
    add_element=function(obj){
      checkmate::assertClass(obj, 'doc_reportElement')
      checkmate::assertTRUE(identical(obj$parent, self))
      private$children_[[length(private$children_)+1]]<-obj
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
    flag_add_depth_ = TRUE,
    chart_foldername_='',
    cache_foldername_=''
  )
)

doc_Insertable<-R6::R6Class(
  "doc_Insertable",
  inherit = doc_container,
  public = list(
    initialize=function(parent, tags, flag_add_depth, chart_foldername='', cache_foldername='') {
      super$initialize(parent = parent, tags = tags,
                       flag_add_depth = TRUE,
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
    insert_table=function(caption, table_df, tags=character(0)) {
      tbl<-doc_table$new(parent=self, tags=tags, table_caption=caption, table_df=table_df)
      self$add_element(tbl)
      return(tbl$label)
    },
    insert_chart=function(caption, gg, tags=character(0)) {
      cht<-doc_chart$new(parent=self, tags=tags, chart_caption=caption, gg=gg)
      self$add_element(cht)
      return(cht$label)
    }
  )
)

doc_section<-R6::R6Class(
  "doc_section",
  inherit = doc_Insertable,
  public = list(
    initialize=function(parent, tags, text, number=NA, chart_foldername='', cache_foldername='') {
      checkmate::assertString(text)
      checkmate::assertNumber(number, na.ok=TRUE)
      super$initialize(parent = parent, tags = tags,
                       flag_add_depth = TRUE,
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

      text<-pander::pandoc.header.return(paste0(text, private$text_), level = self$depth())
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
  private = list(
    text_='',
    number_=NA_real_
  )
)


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

#Main document. It is basically container that stores
#header info.
doc_Document<-R6::R6Class(
  "doc_Document",
  inherit = doc_Insertable,
  public = list(
    initialize=function(chart_foldername=NULL, cache_foldername=NULL, author, format='docx', title) {
      super$initialize(parent=NULL, tags=character(0),
                       cache_foldername=cache_foldername, chart_foldername=chart_foldername)
      checkmate::testString(author)
      checkmate::testString(title)
      checkmate::testString(format)
      private$flag_add_depth_<-FALSE

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
        return(private$chart_foldername_)
      } else if(folder_type=='cache') {
        return(private$cache_foldername_)
      } else {
        browser() #Unknown folder_type
      }
    }

  ),
  active = list(
    stat_methods = function() {private$stat_methods_},
    parent = function() {private$parent_}
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
                       flag_add_depth=FALSE, chart_foldername=chart_foldername,
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
    }
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
                       flag_add_depth = FASLE, foldername='')
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
