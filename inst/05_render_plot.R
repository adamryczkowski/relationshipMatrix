#Executed to render image
#
#It basically transforms ggplot object into a png


if(!dir.exists(dirname(plot_image_tmpfilename))) {
  dir.create(dirname(plot_image_tmpfilename), recursive = TRUE)
}


if(!is.null(gg)){
  if(chart_postprocess) {
    suppressWarnings(ggplot2::ggsave(plot=gg,filename=plot_image_filename, height=15, width=18.5, dpi = dpi, units='cm', device = 'png'))
  } else {
    suppressWarnings(ggplot2::ggsave(plot=gg,filename=plot_image_tmpfilename, height=15, width=18.5, dpi = dpi, units='cm', device = 'png'))
    if(!file.exists(plot_image_tmpfilename)) {
      browser()
    }
    system(paste0('bash "', preprocess_script_path, '" "',
                  plot_image_tmpfilename, '" "',
                  plot_image_filename, '" "',
                  plot_archive, '"'),
           ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)

  }
}
