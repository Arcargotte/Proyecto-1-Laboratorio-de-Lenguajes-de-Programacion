# Tarea 1: Motor de Aventura de Texto (CI-3661)

- **Nombres:** (Mauricio Fragachán, Alan Argotte, David Díaz)
- **Carnets:** (20-10265, 19-10664, 20-10019)

---

## Cómo Compilar y Ejecutar

Este proyecto usa `stack`. Para compilarlo y ejecutarlo:

1.  **Compilar:** `stack build`
2.  **Ejecutar:** `stack exec TextAdventureEngine-exe`

---

## Justificación de Diseño

(Esta sección es obligatoria)

### 1. Elección de Estructuras de Datos

(Explica aquí las estructuras de datos que elegiste para el inventario, las salidas de las salas y el mapa del mundo. Compara las ventajas y desventajas en este contexto).

En esta sección justificamos el uso frecuente del tipo Map (como viene definido en el módulo Data.Map, que llamamos Map) para crear las estructuras de datos que son utilizadas para el inventario, las salidas de las salas y el mundo. Para resumir nuestras justificaciones para los tres casos, la motivación principal se basa en la necesidad de agrupar elementos relacionados entre sí y la de consultarlos a partir de un identificador. Este identificador debía estar separado de la lógica de implementación de las estructuras que agrupamos en la colección. De aquí concluimos que la solución más natural para almacenar datos y consultarlos a partir de un identificador es un diccionario, que en Haskell implementa como el tipo de dato Map del módulo Data.Map. Esta solución es preferible a la de la implementación de una lista, donde cada objeto debía contener tanto identificador como lógica y la cual complicaba absurdamente la búsqueda de un objeto en particular y la implementación del dato. ¿Por qué querríamos consultar en cada índice de una lista el objeto que buscamos, para preguntar por su identificador, cuando podemos pasar el identificador directamente y obtener el objeto? En cambio, con Map usamos los métodos provenientes de la librería como Map.lookup para consultar un objeto cuyo identificador ya conocemos.

1. Inventario: Map.Map String Item
Creamos el tipo de dato Item con newtype para que contenga la descripción de un objeto del juego. Como el inventario viene 
definido en el GameState, creamos un Map cuyas llaves son los nombres de los objetos y sus valores son los objetos Item correspondientes.
2. Salidas: Creamos el tipo de dato Room. Room contiene el atributo
"exits" que es un Map.Map Direction String.
Las salidas de una sala están almacenadas en el tipo Room en el atributo "exits". Exists se un Map.Map Direction String, donde Direction es un data con cuatro posibles valores: Norte, Sur, Este, Oeste o Dirn't (la ausencia de dirección). String, en cambio, es el identificador de la sala que queda en la dirección dada por Direction. Preferimos esta implementación sobre una lista por el mismo motivo expuesto en el inicio: es conveniente separar los identificadores de los datos. Con los valores de Direction (que usamos como identificadores) podemos crear un case...of para distinguir cuando tenemos una dirección para una sala existente o cuando la dirección dada no va a algún lado (es decir, cuando en el Map.lookup nos retorna Nothing).
3. Mundo: Creamos el tipo de dato GameState. GameState contiene
el atributo "worldMap" que es un Map.Map String Room
Nuevamente, es conveniente tener separadas los identificadores de un objeto de la lógica de su implementación. worldMap es un diccionario que contiene los pares de identificador de sala (Room) y la sala en sí. Esta implementación es necesaria para el comando "Ir", que debe actualizar el currentState del juego cambiando el valor del currentRoom por aquel de la sala a la que se movió el jugador. Como tenemos un mapa que va de Direction a identificador, usamos este identificador en otro mapa que va de identificador a Room, que es el que utilizamos para actualizar currentRoom.


### 2. Separación de Lógica Pura e Impura

(Explica aquí cómo tu diseño logra esta separación. Menciona el rol de `Engine.Core` y `Main.hs` y cómo se comunican).

El diseño dado en este proyecto logra la separación entre la lógica pura e impura a través de las firmas de las funciones que implementan los distintos aspectos del juego. La función Main.hs consume el archivo mundo.txt para parsearlo; si el mundo tiene un formato inválido, envía un mensaje de feedback al usuario indicándole que hubo un error; en caso contrario, genera el GameState e inicia el gameLoop. El gameloop toma el comando desde el stdin y crea un dato de tipo cmd con lo recibido desde la entrada que es tomado como argumento por la función parseCommand en Engine.Core. La función Core es una función diádica que toma un comando válido desde el stdin y el Gamestate y retorna una 2-tupla con un mensaje y el Gamestate actualizado. Podemos resumir lo que acabamos de describir en las siguientes palabras: Main.hs llama a Gameloop, dos funciones que contienen los datos impuros que son filtrados hacia la función parseCommand que trabaja con datos puros y los convierte en comandos que cambian el estado del GameState.