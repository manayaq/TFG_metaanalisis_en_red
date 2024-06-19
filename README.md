# Trabajo de Fin de Grado

## *Metaanálisis en Red: Estimación de los factores con mayor influencia en la Inseguridad Alimentaria y Nutricional en Centroamérica*

### Autor: *Martín Anaya Quesada*

### Tutora: *Úrsula Torres Parejo*

****

## :question: Resumen del proyecto

La inseguridad alimentaria y nutricional (inSAN) se define como la condición de aquellas personas que no tienen acceso constante a alimentos inocuos que satisfagan sus necesidades nutricionales para llevar una vida activa y saludable. Desafortunadamente, esta condición afecta a millones de personas en todo el mundo, principalmente a habitantes de países en vías de desarrollo.

Para poder combatir la inSAN de forma efectiva, es vital comprender en profundidad los factores que la causan y cómo están relacionados. En este experimento, se emplearán las técnicas expuestas en la memoria sobre metaanálisis en red para sintetizar y unificar los datos recogidos en las zonas más afectadas de Centroamérica, pudiendo no solo evaluar el impacto individual de cada factor sino también sus conexiones.

De este modo estamos desarrollando una herramienta para orientar a los gobiernos y otras organizaciones en su planificación y ejecución de proyectos humanitarios, logrando una puesta en marcha de forma más precisa y efectiva de sus acciones y labores de protección social. Todo esto gracias a ser capaces de poner el foco en los grupos con un mayor nivel de vulnerabilidad según nuestro metaanálisis, y en última instancia, fomentando políticas y programas sostenibles adaptados a las necesidades de cada comunidad afectada.

****

## :exclamation: Uso del repositorio
Para descargar el código, simplemente debemos situarnos en el directorio deseado y desde una terminal ejecutar:
~~~bash
$ git clone https://github.com/manayaq/TFG_metaanalisis_en_red
~~~

O simplemente descargarlo como un archivo `zip` desde este **[enlace](https://github.com/manayaq/TFG_metaanalisis_en_red/archive/refs/heads/main.zip)**.

****

## :open_file_folder: Estructura del repositorio

- Carpeta _código auxiliar_: En este directorio se incluye el código no relacionado con el metaanálisis en sí.

- Carpeta _notebook_: Aquí se encuentra la primera versión del programa. Este consiste en un notebook en R que sirve como asistente para aquellos investigadores que se desean iniciar en la materia, puesto que detalla cada test realizado y los resultados que se esperan para que el modelo sea válido.

- Carpeta _script_: Aquí se encuentra la segunda versión del programa, un script en R que realiza el metaanálisis en red de forma automática según los parámetros introducidos por el usuario y los almacena para su posterior interpretación. 

****
## :book: Documentación
La documentación completa se encuentra en el Apéndice A de la memoria.

****
## :computer: Instalación

El código utiliza diversos paquetes de R. Para instalarlos, simplemente podemos ejecutar el script `instalacion_paquetes.R`. Este los descarga automáticamente y crea las carpetas necesarias para la salida del programa.

~~~bash
$ cd TFG_metaanalisis_en_red/script
$ Rscript ./instalacion_paquetes.R
~~~


****
## :memo: Instrucciones de uso
Para lanzar el notebook, se recomienda emplear Rstudio. Una previsualización del mismo se puede encontrar en el fichero `.html` que lo acompaña.

Para ejecutar el script, simplemente debemos:

~~~bash
$ cd script
$ Rscript ./metaanalisisRed.R -(parámetros del usuario)
~~~
La lista de parámetros implementados se encuentra en el Apéndice A de la memoria.
****


## :star: Agradecimientos
Gracias a la *Fundación Acción Contra el Hambre* por cedernos las bases de datos empleadas en este proyecto. Su labor es toda una inspiración y fundamental para que trabajos como este salgan adelante.
