# Análisis de Elecciones Venezolanas

Este repositorio contiene el análisis y visualizaciones de los datos de las elecciones venezolanas de 2024. Los datos surgen del análisis de las actas compartidas en
resultadosconvzla.com. Verificamos previamente que la descarga de las imágenes de las actas allí presentes, arroja resultados muy similares.

## Tabla de Contenidos
- [Introducción](#introducción)
- [Análisis](#análisis)
- [Gráficos](#gráficos)
- [Tablas](#tablas)
- [Uso](#uso)
- [Contribuciones](#contribuciones)
- [Licencia](#licencia)

## Introducción

Este repositorio proporciona un análisis de los datos de las elecciones venezolanas, analizando por un lado la relación entre la geografía, la participación y el % obtenido por cada candidato para la elección 2024, y por otro lado, comparando las elecciones de 2024 y 2013.

## Análisis

El análisis realizado en este repositorio incluye lo siguiente:

- Análisis exploratorio de los datos de las elecciones, incluyendo estadísticas resumidas y visualizaciones.
- Análisis de la relación entre el porcentaje de González, la participación y los votantes registrados.
- Comparación de la relación entre el porcentaje de Maduro, la participación y los votantes registrados.

## Gráficos

Los siguientes gráficos se incluyen en este repositorio:

1. Tablas que resumen los resultados por estado para las elecciones de 2013 y 2024.
2. Gráficos de dispersión facetado para el porcentaje de González y Maduro vs. participación por estado para 2024.
3. Gráficos de dispersión para el porcentaje obtenido por Maduro en cada municipio de cada Estado.
4. Gráficos de dispersión facetado donde se compara el % obtenido por Maduro en cada centro de votación respecto al promedio de cada municipio para la elección de 2013 y esa misma diferencia en la elección de 2024.
5. Gráficos de dispersión facetado donde se compara el % obtenido por Maduro en cada centro de votación respecto al promedio de cada Estado para la elección de 2013 y esa misma diferencia en la elección de 2024.

Puede acceder a los gráficos haciendo clic en los siguientes enlaces:

- [Porcentaje de González vs. participación por estado]()
- [Porcentaje de González vs. participación por municipio]()
- [Porcentaje de Maduro vs. participación por estado]()

## Tablas

Las siguientes tablas se incluyen en este repositorio:

1. [Tabla resumen de los resultados electorales por estado para 2013]()
2. [Tabla resumen de los resultados electorales por estado para 2024]()


## Uso

Para correr localmente el código de este repositorio, siga estos pasos:

1. Clone el repositorio en su máquina local.
2. Instale los paquetes de R requeridos ejecutando el siguiente comando: `install.packages(c("tidyverse", "kableExtra","ggplot2","dplyr","viridis","ggrepel"))`.
3. Abra el archivo `make_tables.R` en RStudio o cualquier otro entorno de R.
4. Ejecute el código para generar los gráficos y tablas.

## Contribución

Las contribuciones y correcciones serán bienvenidas.

## Agradecimientos

Se agradece a Walter Mebane por proveer los datos de las elecciones Venezolanas por mesa para la elección presidencial del año 2013.

