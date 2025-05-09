# R setup
1. [Download R](https://cran.rstudio.com/)
2. [Download RStudio](https://posit.co/download/rstudio-desktop/)
3. Open [r-context.Rproj](r-context.Rproj) file with RStudio
4. Create `.env` file ([.env.example ](.env.example))
5. Run [setup.R](./src/setup.R) with the 'Source' option (executes the entire file)


## Workflow
1. Run [responses.R](./src/responses.R) to fetch responses (and clean dataset)
2. Run [print.R](./src/print.R) to generate plots 


## Other files
- [stats.R](./src/stats.R) includes statistics
- [tex.R](./src/tex.R) generates LaTeX tables and figures
- [upload.R](./src/upload.R) upload tables and figures to server
- [cleaner.R](./src/cleaner.R) contains functions used to clean the dataset
- [utils.R](./src/utils.R) contains utility functions