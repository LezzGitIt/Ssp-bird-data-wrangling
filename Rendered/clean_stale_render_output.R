## Quarto pre-render hook (registered in _quarto.yml). Removes the nested
## Scripts/ mirror folder left inside Rendered/ by the *previous* render
## (output-dir: Rendered/ still mirrors the source path, e.g.
## Rendered/Scripts/qmd/<file>.pdf) before this render starts.
## Safe to delete at this point since the IDE's own render-preview step
## already consumed that file after the previous render finished; deleting it
## inside that same render's post-render hook instead causes a 404 in the
## RStudio/Positron preview pane, since it expects the file to still exist at
## its Quarto-predicted location immediately after rendering.

## Quarto mirrors both the target file's path (Scripts/qmd/) and any
## referenced assets (e.g. Figures/) into output-dir, so both need sweeping.
nested_dirs <- file.path("Rendered", c("Scripts", "Figures", "Figures_static"))
for (nested in nested_dirs) {
  if (dir.exists(nested)) {
    unlink(nested, recursive = TRUE)
    message("Removed stale ", nested, " left over from the previous render")
  }
}
