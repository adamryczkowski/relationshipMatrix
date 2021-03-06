#Executed to render image
#
#It basically transforms ggplot object into a png


if(!dir.exists(dirname(plot_image_tmpfilename))) {
  dir.create(dirname(plot_image_tmpfilename), recursive = TRUE)
}


if(!is.null(draw_function)){
  if(chart_postprocess) {
    png(plot_image_tmpfilename, width = 18.5, height = 15, units='cm', res=dpi)
    draw_function()
    dev.off();

    if(!file.exists(plot_image_tmpfilename)) {
      browser()
    }
    system(paste0('bash "', preprocess_script_path, '" "',
                  plot_image_tmpfilename, '" "',
                  plot_image_filename, '" "',
                  plot_archive, '"'),
           ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)

  } else {
    png(plot_image_filename, width = 18.5, height = 15, units='cm', res=dpi)
    draw_function()
    dev.off();
  }
}
