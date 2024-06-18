# Instalación de los paquetes necesarios
# Autor: Martín Anaya
# Ejemplo de uso:
# Rscript instalacion_paquetes.R


install.packages('argparse', repos='http://cran.r-project.org')
install.packages('xlsx', repos='http://cran.r-project.org')
install.packages('readxl', repos='http://cran.r-project.org')
install.packages('writexl', repos='http://cran.r-project.org')
install.packages('dplyr', repos='http://cran.r-project.org')
install.packages('netmeta', repos='http://cran.r-project.org')
install.packages('mvtnorm', repos='http://cran.r-project.org')
install.packages('rgl', repos='http://cran.r-project.org')
 

#Crea la carpeta img, donde se guardan las gráficas que genera el programa
dir.create("img", showWarnings = FALSE)

#Crea la carpeta análisis, donde se guardan los resultados que genera el programa
dir.create("analisis", showWarnings = FALSE)