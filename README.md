# Trabajo Práctico de R

## Laboratorio de Análisis de Datos
Universidad Torcuato Di Tella

### Integrantes
- Juan Pablo Cardozo
- Irene Kozicki

## Descripción

Este repositorio contiene el trabajo práctico domiciliario de R.  
El trabajo está dividido en tres ejercicios:

1. Análisis de datos sobre la producción de soja en Argentina.
2. Análisis econométrico utilizando la base de datos Gapminder.
3. Simulación de demanda de bienes con preferencias Cobb-Douglas.

También se incluye el informe final en formato PDF con la explicación de los resultados obtenidos.

## Estructura del repositorio

El repositorio se encuentra organizado de la siguiente manera:

```text
Trabajo-practico-R/
│
├── README.md
├── informe.pdf
│
├── analisis_datos/
│   ├── script_principal.R
│   ├── funciones.R
│   └── data/
│
├── analisis_econometrico/
│   ├── script_principal.R
│   ├── Funciones_ej2.R
│   └── data/
│       └── gapminder.csv
│
└── simulacion/
    ├── script_principal.R
    ├── simulacion_funciones.R
    └── data/

## Cómo reproducir los resultados

Para reproducir el trabajo:

1. Descargar o clonar el repositorio completo.
2. Mantener la estructura de carpetas.
3. Abrir la carpeta correspondiente a cada ejercicio.
4. Ejecutar el `script_principal.R` desde el comienzo.
5. Las funciones auxiliares son cargadas mediante `source()` desde los respectivos archivos de funciones.

Los scripts utilizan rutas relativas, por lo que es importante no modificar la estructura de carpetas.

## Paquetes utilizados

El principal paquete utilizado para manipulación y visualización de datos es:

```r
library(tidyverse)
