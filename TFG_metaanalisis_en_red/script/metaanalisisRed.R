# Herramienta para realizar metaanálisis en red
# Autor: Martín Anaya
# Ejemplo de uso:
# Rscript autom.R -d "data/data.xlsx" -f "Femenino" "Menos de primaria" "Violencia_sí" "Soltero/a" "Más de 4" "Huerto_no" "Embarazadas_sí" "Mala o muy mala" "Desastres_sí" "AyudaHumanitaria_no" -e "E6 - SMART_Corredor Seco 2010" "E9 - PROCOMIDA 2010" -t "OR" -m "A" -p "B" -r "Femenino" -g "Y" -s 1234



#Carga los paquetes necesarios sin añadir ruido a la terminal
suppressMessages(library("argparse")) # Paquete para leer la entrada del usuario
suppressMessages(library("readxl")) # Leer hojas de cálculo
suppressMessages(library("writexl")) # Escribir en hojas de cálculo
suppressMessages(library("xlsx")) # Manejo general de hojas de cálculo
suppressMessages(library("dplyr")) # Paquete para la manipulación de datos
suppressMessages(library("netmeta")) # Modelo de metaanálisis en red
suppressMessages(library("mvtnorm")) # Se utiliza para generar las probabilidades del rankograma
suppressMessages(library("rgl")) # Posibilita generar la red de evidencia en 3D


#Variable para indicar la ruta donde guardar los resultados
ruta_inconsistencia = "analisis/inconsistencia.xlsx"
ruta_resultados = "analisis/resultados.xlsx"

#------------------------- Configuración inicial

#Creamos el objeto parser y definimos los argumentos esperados
parser <- ArgumentParser()
parser$add_argument("-d", "--archivo", type="character", help="Ubicación de la base de datos")
parser$add_argument("-f", "--lista_factores", nargs='+', type="character", help="Lista de factores a considerar")
parser$add_argument("-e", "--estudios_excluidos", nargs='+', type="character", help="Lista de estudios a excluir del metaanálisis")
parser$add_argument("-t", "--tamano_efecto", type="character", choices=c("OR", "RR"), default="OR", help="Tamano del efecto (OR o RR)")
parser$add_argument("-m", "--modelo_efectos", type="character", choices=c("F", "A"), default="A", help="Modelo de efectos Fijos (F) o Aleatorios (A)")
parser$add_argument("-p", "--val_pequenos", type="character", choices=c("G", "B"), default="B", help="¿Un efecto pequeno jerarquiza mejor (G) o peor (B)? Default: B")
parser$add_argument("-r", "--referencia", type="character", help="Factor de referencia")
parser$add_argument("-g", "--guarda_salida", type="character", choices=c("Y", "N"), default="N", help="Guarda la salida en un fichero (Y) o la muestra por pantalla (N)")
parser$add_argument("-s", "--semilla", type = "integer", help = "Semilla para el generador de números aleatorios", default = 1234)


#Almacenamos la entrada del usuario
args <- parser$parse_args()

archivo_datos <- args$archivo
factores_a_analizar <- args$lista_factores
tamano_efecto <- args$tamano_efecto
estudios_excluidos <- args$estudios_excluidos

if(args$modelo_efectos == "F"){
  modelo_efectos_aleatorios <- FALSE
}else if(args$modelo_efectos == "A"){
  modelo_efectos_aleatorios <- TRUE
}

factor_referencia <- args$referencia

if(args$val_pequenos == "G"){
  valores_pequenos <- "good"
}else if(args$val_pequenos == "B"){
  valores_pequenos <- "bad"
}

if(args$guarda_salida == "Y"){
  guarda_salida <- TRUE
}else if(args$guarda_salida == "N"){
  guarda_salida <- FALSE
}

#Semilla para hacer reproducible el experimento
set.seed(args$semilla)

#Comprobaciones de los datos introducidos por el usuario para garantizar el funcionamiento del código sin errores
if (!file.exists(archivo_datos)){
  stop("La ruta a la base de datos no es correcta")
}
if (!(factor_referencia %in% factores_a_analizar)) {
  stop("El factor de referencia no está en la lista de factores a analizar.")
}
if (!(tamano_efecto %in% c("OR", "RR"))) {
  stop("El tamano de efecto seleccionado no es compatible")
}
if (modelo_efectos_aleatorios != TRUE && modelo_efectos_aleatorios != FALSE){
  stop("La variable modelo_efectos_aleatorios debe inicializarse a TRUE o FALSE")
}
if (valores_pequenos != "good" && valores_pequenos != "bad"){
  stop("La variable valores_pequenos debe inicializarse a good o bad")
}


#Imprimimos la configuración del usuario
print(">>> Configuración del usuario")
print(paste("Base de datos:", archivo_datos))
print(paste("Estudios excluidos:", paste(estudios_excluidos, collapse=", ")))
print(paste("Factores considerados:", paste(factores_a_analizar, collapse=", ")))
if(modelo_efectos_aleatorios){
  print("Modelo de efectos: aleatorios")
}else if(!modelo_efectos_aleatorios){
  print("Modelo de efectos: fijos")
}
print(paste("Factor de referencia:", factor_referencia))
print(paste("Tamano del efecto:", tamano_efecto))
if(valores_pequenos == "good"){
  print("Efectos menores jerarquizan mejor")
}else if(valores_pequenos == "bad"){
  print("Efectos menores jerarquizan peor")
}

print("Inicialización de las opciones del metaanálisis correcta")
cat("\n \n")

#------------------------- Carga y limpieza de la base de datos


# En la base de datos existen diversos factores sobre los que no se tiene información, que no están en el formato deseado
# o que el usuario ha indicado que quiere excluir del metaanálisis. En el siguiente fragmento se lee la base de datos
# (ignorando los estudios excluidos) y mediante la función limpia_datos() se procesan y almacenan en un dataframe
# que representará toda la evidencia a incluir en la red.

nombres_hojas <- excel_sheets(archivo_datos) #El nombre de cada hoja se corresponde al identificador de la investigación que aporta los datos de dicha hoja.

nombres_hojas <- nombres_hojas[!nombres_hojas %in% estudios_excluidos] #Ignoramos los estudios excluidos para formar los identificadores de estudio

print(paste("Procesando los estudios:", paste(nombres_hojas, collapse=", ")))

#datos_estudios es una lista de dataframes, en cada elemento se almacena la información de un estudio
datos_estudios <- lapply(nombres_hojas, function(hoja) {
  if (!(hoja %in% estudios_excluidos)) {
    suppressMessages(read_excel(archivo_datos, sheet = hoja))
  }else {
    NULL  #NULL si el estudio ha quedado descartado
  }
})

#Eliminar elementos NULL
datos_estudios <- datos_estudios[!sapply(datos_estudios, is.null)]


#Función para la preparación de datos
limpia_datos <- function(dataframe) {
  #Guardamos las filas que contienen el valor NA (no hay datos sobre este factor)
  filas_na <- which(rowSums(is.na(dataframe)) > 0)
  
  #Vector con las filas vacías que pretendemos eliminar. La fila anterior es su identificador por lo que también se debe borrar.
  filas_a_eliminar <- unique(c(filas_na - 1, filas_na))
  
  #Antes de llevar a cabo la eliminación, comprobamos que todos los índices están dentro del tamaño del vector
  filas_a_eliminar <- filas_a_eliminar[filas_a_eliminar > 0 & filas_a_eliminar <= nrow(dataframe)]
  
  #Eliminamos las filas
  dataframe_filtrado <- dataframe[-filas_a_eliminar, ]
  
  #Establecemos el identificador de cada columna y su tipo
  colnames(dataframe_filtrado) <- c("Factor", "ISAN_Leve", "ISAN_Severa", "Total")
  dataframe_filtrado$ISAN_Leve = suppressWarnings(as.numeric(dataframe_filtrado$ISAN_Leve))
  dataframe_filtrado$ISAN_Severa = suppressWarnings(as.numeric(dataframe_filtrado$ISAN_Severa))
  dataframe_filtrado$Total = suppressWarnings(as.numeric(dataframe_filtrado$Total))
  return(dataframe_filtrado)
}

datos_estudios_limpios <- lapply(datos_estudios, limpia_datos)

#Seleccionamos únicamente los factores que el usuario ha decidido incluir en el metaanálisis
datos_estudios_limpios <- lapply(datos_estudios_limpios, function(dataframe){
  filter(dataframe, Factor %in% factores_a_analizar)
})

#Asigna el nombre del estudio a cada dataframe
names(datos_estudios_limpios) <- nombres_hojas


#------------------------- Procesado de datos

# Se deben implementar las funciones para calcular los tamaños del efecto que se pueden utilizar
# con datos dicotómicos. Aquí se definen dos métodos para computar el Odds Ratio y el Risk Ratio respectivamente.
# Recordemos que estos valores se deben tomar en escala logarítmica.

#Función para calcular el odds ratio de dos factores
calcular_odds_ratio <- function(a, b, c, d) {
  # a = ISAN_Severa_Factor1
  # b = ISAN_Leve_Factor1
  # c = ISAN_Severa_Factor2
  # d = ISAN_Leve_Factor2
  
  #Fórmula odds ratio (tomando el logaritmo natural)
  OR <- log((a / b) / (c / d))
  
  #Fórmula para calcular el error estándar del log(OR)
  se_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d)
  
  return(list(TE = OR, seTE = se_log_OR))
}

calcular_risk_ratio <- function(a, b, c, d) {
  # a = ISAN_Severa_Factor1
  # b = ISAN_Leve_Factor1
  # c = ISAN_Severa_Factor2
  # d = ISAN_Leve_Factor2
  
  #Fórmula risk ratio (tomando el logaritmo natural)
  RR <- log((a / (a + b)) / (c / (c + d)))
  
  #Fórmula para calcular el error estándar del log(RR)
  se_log_RR <- sqrt((1/a) - (1/(a + b)) + (1/c) - (1/(c + d)))
  
  return(list(TE = RR, seTE = se_log_RR))
}

#Dataframe que almacenará los datos en formato compatible con netmeta
df_contrastes <- data.frame(Estudio = character(),
                            Factor1 = character(),
                            Factor2 = character(),
                            TE = numeric(),
                            seTE = numeric(),
                            stringsAsFactors = FALSE
)

for (index in seq_along(datos_estudios_limpios)) {
  df = datos_estudios_limpios[[index]]
  
  #Generar todas las comparaciones posibles entre factores, se almacena en forma de columna
  if (length(unique(df$Factor)) < 2) {
    next  #Si no hay al menos dos factores, no se pueden generar comparaciones
  }
  combinaciones <- combn(df$Factor, 2)
  
  
  #Calcular el tamaño del efecto y error estándar para cada comparación
  for (i in 1:ncol(combinaciones)) {
    factor1 <- combinaciones[1, i]
    factor2 <- combinaciones[2, i]
    
    a <- df$ISAN_Severa[df$Factor == factor1]
    b <- df$ISAN_Leve[df$Factor == factor1]
    c <- df$ISAN_Severa[df$Factor == factor2]
    d <- df$ISAN_Leve[df$Factor == factor2]
    
    if(tamano_efecto == "OR"){
      resultado <- calcular_odds_ratio(a, b, c, d)
    } else if(tamano_efecto == "RR"){
      resultado <- calcular_risk_ratio(a, b, c, d)
    }
    
    #Los resultados se almacenan en el dataframe df_contrastes
    df_contrastes <- rbind(df_contrastes, data.frame(
      Estudio = names(datos_estudios_limpios)[index],
      Factor1 = factor1,
      Factor2 = factor2,
      TE = resultado$TE,
      seTE = resultado$seTE,
      stringsAsFactors = FALSE
    ))
  }
  
}

#print(df_contrastes)

#------------------------- Metaanálisis en red

settings.meta(digits = 2, digits.se = 3) #Los tamaños del efecto y sus intervalos de confianza se mostrarán con dos cifras y los errores con tres

print(">>> Comenzando ajuste del modelo...")

#Función para ajustar modelos del paquete netmeta
m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = Factor1,
                     treat2 = Factor2,
                     studlab = Estudio,
                     data = df_contrastes,
                     sm = tamano_efecto, #Tamaño del efecto escogido por el usuario
                     fixed = !modelo_efectos_aleatorios,
                     random = modelo_efectos_aleatorios, #Se recomienda utilizar un modelo de efectos aleatorios porque los datos proceden de distintos países
                     reference.group = factor_referencia, #Factor de referencia escogido por el usuario
                     details.chkmultiarm = TRUE, #Imprimir estimaciones inconsistentes
                     #tol.multiarm = 1, #Tolerancia a inconsistencia
                     title = "Modelo de metaanálisis en red",
                     sep.trts = " vs ") #Separador para indicar las comparaciones de la red


print(">>> Modelo de metaanálisis ajustado")

print("Generando diagrama de red")
png(file="img/geometria_de_red.png", width=600, height=300)
netgraph(m.netmeta, plastic = TRUE, multiarm = TRUE, number.of.studies = TRUE, cex.number = 1, pos.number.of.studies = 0.42, scale = 0.75, offset = 0.05, lwd = 3, alpha.transparency = 0.3, points = TRUE, cex.points = 5, col.points = "darkcyan", start.layout = "circle", iterate = FALSE)
invisible(dev.off())

cat("\n \n")

print(">>> Estudio del supuesto de consistencia")
cat("\n")
print(">>Resultados del test de consistencia Q de Cochran")

dbt <- decomp.design(m.netmeta)

if(modelo_efectos_aleatorios){
  if(guarda_salida){
    write.xlsx(dbt$Q.inc.random, ruta_inconsistencia, sheetName = "Test Q", row.names = FALSE)
  }else{
    print(dbt$Q.inc.random) #Para efectos aleatorios
  }
}else{
  if(guarda_salida){
    write.xlsx(dbt$Q.decomp, ruta_inconsistencia, sheetName = "Test Q", row.names = TRUE)
  }else{
    print(dbt$Q.decomp) #Para efectos fijos
  }
  
}

cat("\n")

print(">>Resultados del método división de nodos")

df_divNodos <- netsplit(m.netmeta) 

if(guarda_salida){
  if(modelo_efectos_aleatorios){
    df_div_Aleatorio = data.frame(df_divNodos$random$TE, df_divNodos$direct.random$TE, df_divNodos$indirect.random$TE)
    datos_no_log <- lapply(df_div_Aleatorio, function(valores) exp(valores))
    
    datos_comparaciones = data.frame(df_divNodos$compare.random$comparison, df_divNodos$compare.random$p)
    
    df_salida <- cbind(datos_comparaciones, datos_no_log)
    colnames(df_salida) <- c("Comparación", "Valor p", "Red", "Directa", "Indirecta")
    df_salida <- df_salida[, c(1, 3, 4, 5, 2)]
    write.xlsx(df_salida, ruta_inconsistencia, sheetName = "División de nodos", row.names = FALSE, append = TRUE)
  }else if(!modelo_efectos_aleatorios){
    df_div_Fijos = data.frame(df_divNodos$common$TE, df_divNodos$direct.common$TE, df_divNodos$indirect.common$TE)
    datos_no_log <- lapply(df_div_Fijos, function(valores) exp(valores))
    
    datos_comparaciones = data.frame(df_divNodos$compare.common$comparison, df_divNodos$compare.common$p)
    
    df_salida <- cbind(datos_comparaciones, datos_no_log)
    colnames(df_salida) <- c("Comparación", "Valor p", "Red", "Directa", "Indirecta")
    df_salida <- df_salida[, c(1, 3, 4, 5, 2)]
    write.xlsx(df_salida, ruta_inconsistencia, sheetName = "División de nodos", row.names = FALSE, append = TRUE)
  } 
}else{
  print(df_divNodos)
}

png(file="img/separacionNodos_bosque.png",width=1200, height=2650)
df_divNodos %>% forest()
invisible(dev.off())

cat("\n")
print(">>Resultados del método diagrama de calor almacenados en img/diagrama_calor.png")
png(file="img/diagrama_calor.png", width=1600, height=1350)
netheat(m.netmeta, random = modelo_efectos_aleatorios, sortvar = TRUE, main ="Diagrama de calor", nchar.trts = 5)
invisible(dev.off())


#------------------------- Resultados

cat("\n \n")

print(">>> Resultados del modelo")
if(modelo_efectos_aleatorios){
  tag <- "Modelo de efectos aleatorios"
}else{
  tag <- "Modelo de efectos fijos"
}

print(">>Generando diagrama de bosque. Almacenando en img/diagrama_bosque.png")
png(file="img/diagrama_bosque.png")
forest(m.netmeta, 
       reference.group = factor_referencia,
       sortvar = TE,
       xlim = c(0.2, 3),
       smlab = paste("Factores vs. ", factor_referencia ,"\n",
                     tag),
       drop.reference.group = TRUE,
       label.left = "A favor del factor de referencia",
       label.right = "A favor del factor")
invisible(dev.off())


print(">>Generando tabla de liga")
tabla_liga <- netleague(m.netmeta, 
                        bracket = "(", # usa paréntesis para los intervalos de confianza
                        digits=2)

if(guarda_salida){
  if(modelo_efectos_aleatorios){
    write.xlsx(tabla_liga$random, ruta_resultados, sheetName = "Tabla de liga", row.names = FALSE, col.names = FALSE)
  }else{
    write.xlsx(tabla_liga$common, ruta_resultados, sheetName = "Tabla de liga", row.names = FALSE)
  }
}else{
  tabla_liga
}


print(">>Generando la jerarquía de factores")
df_jerarquia <- netrank(m.netmeta, small.values = valores_pequenos)

if(guarda_salida){
  if(modelo_efectos_aleatorios){
      df_rank <- cbind(m.netmeta$trts, df_jerarquia$ranking.random)
      colnames(df_rank) <- c("Factor","P-score")
      write.xlsx(df_rank, ruta_resultados, sheetName = "Jerarquía de factores", row.names = FALSE, append = TRUE)
    
    }else{
      df_rank <- cbind(m.netmeta$trts, df_jerarquia$ranking.common)
      colnames(df_rank) <- c("Factor","P-score")
      write.xlsx(df_rank, ruta_resultados, sheetName = "Jerarquía de factores", row.names = FALSE, append = TRUE)
    }
}else{
  df_jerarquia
}


print(">>Generando tabla de probabilidad")
df_rankog <- rankogram(m.netmeta, small.values = valores_pequenos)

if(guarda_salida){
  if(modelo_efectos_aleatorios){
    df_tabla_prob <- cbind(m.netmeta$trts, df_rankog$ranking.matrix.random)
    colnames(df_tabla_prob) <- c("Factor", paste("Puesto", seq_len(ncol(df_tabla_prob) - 1), sep = " "))
    write.xlsx(df_tabla_prob, ruta_resultados, sheetName = "Tabla de probabilidad", row.names = FALSE, append = TRUE)
    
  }else{
    df_tabla_prob <- cbind(m.netmeta$trts, df_rankog$ranking.matrix.common)
    colnames(df_tabla_prob) <- c("Factor", paste("Puesto", seq_len(ncol(df_tabla_prob) - 1), sep = " "))
    write.xlsx(df_tabla_prob, ruta_resultados, sheetName = "Tabla de probabilidad", row.names = FALSE, append = TRUE)
  }
}else{
  df_rankog
}


print(">>Generando gráfico de probabilidad acumulada. Almacenando en img/probabilidad_acumulada.png")
png(file="img/probabilidad_acumulada.png")
plot(rankogram(m.netmeta, small.values = valores_pequenos, cumulative.rankprob = TRUE))
invisible(dev.off())


print(">>Generando rankograma. Almacenando en img/rankograma.png")
png(file="img/rankograma.png")
plot(rankogram(m.netmeta, small.values = valores_pequenos, cumulative.rankprob = FALSE))
invisible(dev.off())


