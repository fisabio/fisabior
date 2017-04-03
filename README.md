
`fisabior`
==========

Acerca de este proyecto
-----------------------

`fisabior` es un paquete de R cuyo único propósito es estructurar y organizar el entorno de trabajo, facilitando el trabajo en equipo.

El proyecto generado con su función principal (`init_proj()`) se estructura en 8 directorios, lo que permite seguir un criterio unificado a la hora de trabajar en equipo o, sencillamente, retomar un trabajo individual que se aparcase hace algún tiempo. El contenido de cada uno de estos directorios es:

-   articulo: aquí se alberga todo el contenido relacionado con la publicación del proyecto, estructurándose, a su vez, en tres subdirectorios.
    -   enviado: documentos tal cual se enviaron para su publicación,
    -   revision: documentos que han sido revisados por pares y requieren modificaciones,
    -   proof: versión de imprenta de las publicaciones aceptadas.
-   cache: conjuntos de datos `*.Rdata` generados durante el análisis, los cuales se guardan de manera temporal como copia de respaldo que evita tener que ejecutar una y otra vez todos los datos. También alberga aquellos datos procedentes de informes elaborados con `knitr` para los que se establezca como opción `cache = TRUE` los cuales, dependiendo del programa empleado para generarlos, se almacenaran en subdirectorios creados junto con el informe.
-   configuracion: en este directorio se almacena todo archivo de configuración común al equipo de trabajo. Un ejemplo lo tienes en el archivo `configuracion/config.R`, el cual se ejecuta al inicio de cada sesión de RStudio para cargar tanto funciones comunes como paquetes de uso continuado.
-   datos: donde se almacenan los bancos de datos necesarios para el análisis. Se estructura en tres subdirectorios:
    -   brutos: datos TAL CUAL se reciben, sin importar su formato.
    -   procesados: datos procesados listos para cargar durante el análisis. Preferentemente se utilizará un formato de valores separados por comas, empleando el punto como separador decimal y dándole la extensión apropiada al archivo (`*.csv`), aunque también es posible emplear el formato nativo de R (`*.RData`) o una base de datos SQL.
    -   cartografia: directorio para guardar archivos de cartografía asociados al proyecto. Preferentemente se utilizará el formato Shapefile (.shp) y, en caso de emplear una proyección de los datos, se aconseja emplear la EPSG 4326 (datum WGS84).
-   figuras: gráficos en diversos formatos generados tanto en el análisis como en la redacción de informes.
-   informes:
    -   docx: documentos MS Office Word producidos por rmarkdown-knitr,
    -   odt: documentos LibreOffice Writer producidos rmarkdown-knitr,
    -   pdf-markdown: documentos PDF producidos por rmarkdown-knitr-LaTeX,
    -   pdf-latex: documentos TeX y PDF producidos por knitr-LaTeX,
    -   beamer: presentaciones TeX y PDF producidos por rmarkdown-knitr-LaTeX,
    -   html: documentos HTML producidos por rmarkdown-knitr.
-   r: directorio principal, el cual contiene todos los scripts de código R necesarios para la ejecución del proyecto. Solo contiene archivos con extensión `*.R`.
-   src: directorio de código secundario, donde se guardan los archivos con código no-R, eligiendo para ello el subdirectorio más apropiado:
    -   bugs: modelos escritos con código en lenguaje BUGS,
    -   cpp: código en C++,
    -   jags: modelos escritos con código en lenguaje JAGS (aunque es similar a BUGS, tiene sus peculiaridades),
    -   stan: modelos escritos con código en lenguaje Stan.


Instalación
-----------

Puedes instalar este paquete empleando la función `install_github()` del paquete `devtools`, para lo cual debes ejecutar el siguiente código (**¡¡OJO!!** Si usas [Windows](https://ichef.bbci.co.uk/news/660/cpsprodpb/025B/production/_85730600_monkey2.jpg) debes tener instalado el software [Rtools](http://cran.r-project.org/bin/windows/Rtools/), de modo que necesitar tener permisos de administración en tu equipo ---hay un problema con las versiones más recientes de `devtools` para Windows y es preciso instalar una versión previa---):

```
if(.Platform$OS.type == "windows") {
  download.file("https://cran.r-project.org/bin/windows/Rtools/Rtools34.exe",
    destfile = paste0(tempdir(), "/Rtools34.exe"), mode = "wb")
  system(paste0(tempdir(), "/Rtools34.exe"))
  install.packages("devtools")
  download.file("https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.11.1.tar.gz",
                destfile = paste0(tempdir(), "/devtools.tar.gz"))
  install.packages(paste0(tempdir(), "/devtools.tar.gz"), repos = NULL, type = "source")
  makeActiveBinding("refresh", function() { system("R --vanilla"); q("no") }, .GlobalEnv)
  refresh()
  devtools::install_github("fisabio/fisabior")
} else {
  install.packages("devtools")
  devtools::install_github("fisabio/fisabior")
}
```


Acerca de `fisabior`
--------------------

`fisabior` es el nombre que nos hemos dado como grupo de usuarios de R dentro de la Fundación para el Fomento de la Investigación Sanitaria y Biomédica de la Comunidad Valenciana (FISABIO). A febrero de 2017, el grupo está compuesto por:

-   Miguel Ángel Martínez-Beneito (<martinez_mig@gva.es>),
-   Gonzalo García-Donato (<garcia_gon@gva.es>),
-   Paloma Botella-Rocamora (<botella_pal@gva.es>),
-   Jordi Pérez-Panadés (<perez_jorpan@gva.es>),
-   Francisca Corpas-Burgos (<corpas_fra@gva.es>),
-   Hèctor Perpiñán-Fabuel (<perpinan_hec@gva.es>),
-   Carlos Vergara-Hernández (<carlos.vergara@uv.es>).

Si al igual que nosotros te encanta la estadística en R, o si tuvieras alguna duda acerca del grupo o su actividad, puedes contactarnos sin problema.
