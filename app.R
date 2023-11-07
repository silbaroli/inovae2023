# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
#2023-03-06-3

#dadadadadada

pkgload::load_all()
options("golem.app.prod" = TRUE)
cgeePanorama::run_app() 
