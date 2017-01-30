---
layout: post
comments: true
title: "Introducción a la simulación en Python 1"
meta: ""
date: 2017-01-29
tags: [spanish]
---

### ¿Qué vamos a hacer?

Esto es una introducción no exhaustiva para empezar a usar el python: habrá que consultar la [documentación](https://docs.python.org/2/).

* Vamos a [aprender a usar la REPL de Python](#la-repl)

* Vamos a familiarizarnos con los [tipos básicos de python](#tipos) y resolver un par de ejercicios elementales.

* Vamos a introducir [bucles básicos](#bucles) y resolver un par de ejercicios elementales sobre ellos.


### Prerequisitos

Vamos a usar [Python 2](https://www.python.org/).

* [Documentación de Python 2](https://docs.python.org/2/)

* Para lo que vamos a hacer ahora no hace falta instalar nada: podemos usar la [REPL online](https://www.python.org/shell/).

* Sería sensato [instalar Python 2](https://www.python.org/downloads/), porque sí que vamos a necesitarlo en algún momento. Si estás en Ubuntu Python 2 está instalado por defecto.

### La REPL

REPL son las siglas de [Read-Eval-Print Loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop). Los lenguajes que tienen una REPL son muy fáciles de aprender ya que la REPL permite una interacción inmediata (a diferencia de aquellos lenguajes que requieren un paso de compilación).

Así que lanzemos la REPL (la [versión web](https://www.python.org/shell/) o la que hemos instalado localmente) y vamos a pedirle que sume dos números:

```python
Python 3.5.2 (default, Jul 17 2016, 00:00:00) 
[GCC 4.8.4] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 1 + 1
2
```

La REPL evalúa las expresiones que le pasas; si le pasas el valor a secas, hace eco del valor (aunque esté en una variable) así que es útil para inspeccionar los contenidos de las variables con las cuales trabajas:

```
>>> 3
3
>>> x = 3
>>> x
3
>>> 
```

#### help()

Aparte de evaluar directamente expresiones de Python, la REPL nos permite pedirle ayuda a python usando la función  ```help```: muchas veces no hace falta ni consultar la documentación fuera de la REPL.

Como ejemplo, vamos a pedirle ayuda a la REPL sobre el tipo ```list```:

```
>>> help(list)
```

Esto nos lleva a la documentación de ```list```: para salir, apretamos la letra ```q```:

```
Help on class list in module builtins:
class list(object)
 |  list() -> new empty list
 |  list(iterable) -> new list initialized from iterable's items
 |  
 |  Methods defined here:
 |  
 |  __add__(self, value, /)
 |      Return self+value.
 |  
 |  __contains__(self, key, /)
 |      Return key in self.
 |  
 |  __delitem__(self, key, /)
:
```

### Tipos

Se han escrito libros sobre lo que es un [tipo de datos](https://en.wikipedia.org/wiki/Data_type). Python es un [lenguaje con sistema de tipos dinámico](https://en.wikipedia.org/wiki/Type_system#DYNAMIC).

#### Variables y valores

Una **variable** en python es *un nombre que se refiere a un valor*. Las variables en Python no tienen tipo asociado.

El operador ```=``` quiere decir: "vamos a asociar el valor a la derecha del = a la variable a la izquierda del ="

En el siguiente ejemplo, hemos hecho que la variable ```x``` se refiera primero al valor 1 y luego al valor de la cadena de caracteres ```'pepe es raro'```:

```
>>> x = 1
>>> x
1
>>> x = 'pepe es raro'
>>> x
'pepe es raro'
```

Un **valor** en python sí que tiene tipo, y lo podemos inspeccionar vía la REPL. Por ejemplo, el valor ```1``` tiene tipo ```int``` (de integer, entero):

```
>>> type(1)
<class 'int'>
```

y el valor ```'pepe es raro'``` tiene tipo str (de string, cadena de caracteres):

```
>>> type('pepe es raro')
<class 'str'>
```

Varias variables se pueden referir al mismo valor.

Cuando preguntamos el tipo de una variable, estamos preguntando el tipo del valor al cual la variable se refiere, que puede cambiar a medida que se ejecuta el programa:

```
>>> var1 = 'pepe es raro'
>>> var2 = var1
>>> type(var1)
<class 'str'>
>>> var1 = 1
>>> type(var1)
<class 'int'>
```
La distinción entre valor y variable es fundamental.

#### Tipos básicos

Lo que sigue es un guión; la documentación y el ```help``` de python tienen la información en profundidad.

##### Tipos numéricos

Hay varios tipos numéricos (enteros, grandes, de coma flotante):

```
>>> type(1)
<type 'int'>
>>> type(1.0)
<type 'float'>
>>> type(1000000000000000000000000000000000000000)
<type 'long'>
```

**EJERCICIO**: qué tipo resulta de sumar un entero y un número de coma flotante, ej. ```type(1 + 1.3)```? Parece sensato este comportamiento?

##### Cadenas de caracteres

Útiles para representar texto, identificadores...

Es interesante que el operador ```+``` también está definido para cadenas de caracteres.

**EJERCICIO** Experimentar con sumar cadenas de caracteres: qué resulta de sumar `´"pepe" + "paco"``? Que pasa cuando se intenta sumar una cadena de caracteres a un número? Tiene sentido este comportamiento?









