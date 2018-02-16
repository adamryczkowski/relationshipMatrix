#Executed to render image
#
#It basically transforms ggplot object into a png


if(!dir.exists(dirname(plot_image_tmpfilename))) {
  dir.create(dirname(plot_image_tmpfilename), recursive = TRUE)
}

if(!is.null(gg)){
  ggsave(plot=gg,filename=plot_image_tmpfilename, height=15, width=18.5, dpi = 450, units='cm', device = 'png')
}
if(!does_chart_tmp_exist(plot_image_tmpfilename)) {
  browser()
}
system(paste0('bash "', preprocess_script_path(), '" "',
              plot_image_tmpfilename, '" "',
              plot_image_filename, '" "',
              plot_archive, '"'),
       ignore.stdout = TRUE, ignore.stderr = TRUE, wait = wait)

