doc_ggchart<-R6::R6Class(
  "doc_ggplot_chart",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, chart_caption, gg, chart_prefix) {
      checkmate::assertString(chart_caption)
      checkmate::assertString(chart_prefix)
      checkmate::assertClass(gg, 'ggplot')
      super$initialize(parent=parent, tags=tags)
      private$chart_caption_<-chart_caption
      private$gg_<-gg
      parent_hash<-parent$address_string()
      private$chart_prefix_<-chart_prefix
      cell_hash<-self$get_property('cell_hash')
      private$chart_label_ <- generate_table_hash(cell_hash = cell_hash, label = chart_caption, file_prefix = chart_prefix)
    },
    render=function(doc) {
      if(is.na(private$rendered_chart_path_)) {
        self$pre_render()
      }
      plot_image_filename<-private$rendered_chart_path_

      setattr(plot_image_filename, 'pandoc_attributes', paste0('#fig:', private$chart_label_))
#      browser()
      caption<-private$chart_caption_
      if(self$get_property('chart_debug')) {
#        browser()
        caption<-paste0(caption, " [", self$debug_text(), ", chart: ", basename(private$rendered_chart_path_), "]")

      }
      doc$add(paste0('\n\n', pander::pandoc.image.return(plot_image_filename, caption = caption), '\n\n'))
    },
    pre_render=function() {
      #browser()
      plot_image_filename<-pathcat::path.cat(private$parent_$get_folders('chart'), paste0(private$chart_prefix_, '_', private$chart_label_, '.png'))
      plot_image_tmpfilename<-tempfile(fileext = '.png')
      if(exists('plot_archive')) rm('plot_archive')
      gg<-private$gg_
      preprocess_script_path<-system.file('process_one_png.sh', package = 'relationshipMatrix')
      plot_archive<-''
      #Here we would create depwalker object and return it
      dpi=self$get_property('chart_dpi', 450)
      chart_postprocess=self$get_property('chart_postprocess', TRUE)
      source(system.file('05_render_ggplot.R', package = 'relationshipMatrix'), local = TRUE)
      private$rendered_chart_path_<-plot_image_filename
      private$gg_<-NULL
    }
  ),
  active = list(
    label = function() {return(private$chart_label_)}
  ),
  private = list(
    chart_caption_='',
    chart_prefix_=NA_character_,
    gg_=NULL,
    chart_label_='',
    rendered_chart_path_=NA_character_
  )
)

doc_chart<-R6::R6Class(
  "doc_plot_chart",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, draw_function, chart_caption, chart_prefix) {
      checkmate::assertString(chart_caption)
      checkmate::assertString(chart_prefix)
      checkmate::assertClass(draw_function, 'function')
      super$initialize(parent=parent, tags=tags)
      private$chart_caption_<-chart_caption
      private$draw_function_<-list(fn=draw_function, env=environment(draw_function))
      #browser()
      private$chart_prefix_<-chart_prefix
      cell_hash<-self$get_property('cell_hash')
      private$chart_label_ <- generate_table_hash(cell_hash = cell_hash, label = chart_caption, file_prefix = chart_prefix)
    },
    render=function(doc) {
      if(is.na(private$rendered_chart_path_)) {
        self$pre_render()
      }
      plot_image_filename<-private$rendered_chart_path_

      setattr(plot_image_filename, 'pandoc_attributes', paste0('#fig:', private$chart_label_))
      #      browser()
      caption<-private$chart_caption_
      if(self$get_property('chart_debug')) {
        #        browser()
        caption<-paste0(caption, " [", self$debug_text(), ", chart: ", basename(private$rendered_chart_path_), "]")

      }
      doc$add(paste0('\n\n', pander::pandoc.image.return(plot_image_filename, caption = caption), '\n\n'))
    },
    pre_render=function() {
      #browser()
      plot_image_filename<-pathcat::path.cat(private$parent_$get_folders('chart'), paste0(private$chart_prefix_, '_', private$chart_label_, '.png'))
      plot_image_tmpfilename<-tempfile(fileext = '.png')
      if(exists('plot_archive')) rm('plot_archive')
      draw_function<-private$draw_function_$fn
      environment(draw_function)<-private$draw_function_$env
      preprocess_script_path<-system.file('process_one_png.sh', package = 'relationshipMatrix')
      plot_archive<-''
      #Here we would create depwalker object and return it
      dpi=self$get_property('chart_dpi', 450)
      chart_postprocess=self$get_property('chart_postprocess', TRUE)
      source(system.file('05_render_plot.R', package = 'relationshipMatrix'), local = TRUE)
      private$rendered_chart_path_<-plot_image_filename
      private$draw_function_<-NULL
    }
  ),
  active = list(
    label = function() {return(private$chart_label_)}
  ),
  private = list(
    chart_caption_='',
    chart_prefix_=NA_character_,
    draw_function_=NULL,
    draw_function_env_=NULL,
    chart_label_='',
    rendered_chart_path_=NA_character_
  ),
  lock_objects = FALSE
)

