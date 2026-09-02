## Quarto post-render hook (registered in _quarto.yml). Copies whatever output
## file(s) were just rendered -- pdf, docx, or any other format -- into a flat
## Rendered/ folder, dropping the Scripts/qmd/ mirroring that output-dir
## otherwise preserves. Copies rather than moves so the original stays exactly
## where Quarto -- and the RStudio/Positron Render button's own preview step --
## expects to find it immediately after rendering; deleting it here instead
## causes a 404 in the IDE's preview pane. The now-stale nested copy gets
## swept up by clean_stale_render_output.R right before the *next*
## render starts instead.

output_files <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")
output_files <- strsplit(output_files, "\n")[[1]]
output_files <- output_files[nzchar(output_files)]

if (length(output_files) > 0) {
  dir.create("Rendered", showWarnings = FALSE)
  for (f in output_files) {
    if (file.exists(f)) {
      dest <- file.path("Rendered", basename(f))
      file.copy(f, dest, overwrite = TRUE)
      message("Copied ", f, " -> ", dest)
    }
  }
}
