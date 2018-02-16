doc_chart<-R6::R6Class(
  "doc_ggplot_chart",
  inherit = doc_reportElement,
  public = list(
    initialize=function(parent, tags, chart_caption, gg) {
      checkmate::assertString(chart_caption)
      checkmate::assertClass(gg, 'ggplot')
      super$initialize(parent=parent, tags=tags)
      private$chart_caption_<-chart_caption
      private$gg_<-gg
      parent_hash<-parent$address_string()
      private$chart_label_ <- generate_table_hash(parent_hash = parent_hash, label = chart_caption)
    },
    render=function(doc) {
      plot_image_filename<-private$parent_$get_folders('plot_image')

      setattr(plot_image_filename, 'pandoc_attributes', attr(private$chart_label_, 'pandoc_attributes'))
      doc$add(pander::pandoc.image.return(plot_image_filename, caption = private$chart_caption_))
    },
    pre_render=function() {
      plot_image_filename<-private$parent_$get_folders('plot_image')
      plot_image_tmpfilename<-tempfile(fileext = '.png')
      if(exists('plot_archive')) rm('plot_archive')
      gg<-private$gg_
      preprocess_script_path<-system.file('process_one_png.sh', package = 'relationshipMatrix')

      #Here we would create depwalker object and return it
      source(system.file('05_render_plot.R', package = 'relationshipMatrix'))
    }
  ),
  private = list(
    chart_caption_='',
    gg_=NULL,
    chart_label_=''
  )
)

