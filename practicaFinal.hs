{-
ADRIAN CARLOS SKACZYLO SAWICKA 


Datos principales:
Inicialmente he definido los tipos de datos necesarios para definir Test, RespuestaTest a y Correcion

Para RespuestaTest y Correcion he decidido añadir un parametro de tipo a para que haya mas "versatilidad" a la hora de identificar a la persona
Para el tipo de dato (Correccion a) he decido añadir una variable mas para representar un mensaje de motivacion; pues en  el enunciado ponia que AL MENOS debia contener el id, y las puntuaciones

-}
data Pregunta = P Float Int Int deriving (Eq,Show) --FLoat: valor pregunta; Int: numero alternativas; Int: Respuesta Correcta
data Respuesta = B | R Int deriving Show --B:Respuesta en Blanco; R Int: Respuesta a la pregunta
data Modelo = M Int [Pregunta] deriving Show--Numero del modelo; Reordenacion de Pregunta
data Test = T  [Modelo] deriving Show--Int:Modelo; [Modelo]: Lista de Modelos
data RespuestaTest a = RT a Int [Respuesta]  deriving Show-- a: identificador; Int : modelo test; [Respuesta]: respuestas
data Correccion a = C a Float Float String    --a: identificador; Float : puntuacion total; Float: puntuacion sobre 10; mensaje motivacional


data TipoRespuesta = Blanco | Correcta | Erronea deriving (Show,Eq)
{-
CORRIGE:

corrige::Test -> RespuestaTest a->Correccion a
corrige (T ms) (RT id m' rs) = corregirModelo (seleccionarModelo (T ms) m') (RT id m' rs)


Para corregir una respuesta necesitamos primero buscar el modelo del examen mediante la funcion 

seleccionarModelo::Test->Int ->Modelo
seleccionarModelo (T ms) m' = (\[x]->x) [(M m ps)| (M m ps)<-ms,  m==m' ]

Una vez tenemos el Modelo que ha realizado la persona procedemos a corregirlo:

corregirModelo :: Modelo->RespuestaTest a ->Correccion a
corregirModelo (M m ps) (RT id m' rs) = let nota = (puntuacionDiez ps rs) in C id (puntuacionTotal ps rs)  nota (mensajeMotivacion nota)

que simplemente llama a las funciones puntuacionDiez; puntuacionTotal y mensajeMotivacion.

¿Como funciona puntuacionTotal?
puntuacionTotal (p:ps) (r:rs) = foldr (+) 0 (zipWith puntuarRespuesta (p:ps) (r:rs))

Como el orden de la pregunta y la respuesta en ambas listas es la misma corregimos las preguntas por parejas y esto lo conseguimos mediante :
zipWith corregirRespuesta (p:ps) (r:rs)

donde corregirRespuesta otorga la puntuacion de dicha respuesta

de esta manera obtendremos una lista de igual tamaño con las puntuaciones de cada pregunta, por lo que solo queda sumarlas mediante un foldr

¿Como funciona puntuacionDiez?
puntuacionDiez ps rs = sobreDiez (puntuacionTotal ps rs) (valorTotal ps)

Es simplemente calcular la puntuacionTotal y hacer una regla de 3. Para esta funcion ha hecho falta una pequeña funcion auxiliar que calcule el valor total de las preguntas.
puntuacionDiez ps rs = sobreDiez (puntuacionTotal ps rs) (valorTotal ps)

-}

corrige::Test -> RespuestaTest a->Correccion a
corrige (T ms) (RT id m' rs) = corregirModelo (seleccionarModelo (T ms) m') (RT id m' rs)

seleccionarModelo::Test->Int ->Modelo
seleccionarModelo (T ms) m' = (\[x]->x) [(M m ps)| (M m ps)<-ms,  m==m' ]


corregirModelo :: Modelo->RespuestaTest a ->Correccion a
corregirModelo (M m ps) (RT id m' rs) = let nota = (puntuacionDiez ps rs) in C id (puntuacionTotal ps rs)  nota (mensajeMotivacion nota)


puntuacionTotal:: [Pregunta]->[Respuesta]->Float
puntuacionTotal (p:ps) (r:rs) = foldr (+) 0 (zipWith puntuarRespuesta (p:ps) (r:rs))

puntuacionDiez::[Pregunta]->[Respuesta]->Float
puntuacionDiez ps rs = sobreDiez (puntuacionTotal ps rs) (valorTotal ps)


{-
Estas son las pequeñas funciones auxiliares que nos permiten calcular las de arriba

corregirRespuesta y puntuarRespuesta estan modularizadas para posteriormente usarlas sin necesidad de repetir funciones
-}
valorTotal::[Pregunta]->Float
valorTotal ps = foldr (+) 0 (map (\(P v _ _ )->v) ps) 
sobreDiez::Float->Float->Float
sobreDiez pt vt = (pt*10)/vt

corregirRespuesta::Pregunta->Respuesta->TipoRespuesta 
corregirRespuesta p B = Blanco
corregirRespuesta (P v n c) (R r)
    | c == r = Correcta
    | otherwise = Erronea

puntuarRespuesta:: Pregunta->Respuesta->Float
puntuarRespuesta p B = 0 --Respuesta en blanco no puntua
puntuarRespuesta (P v n c) r
    |(corregirRespuesta (P v n c) r) == Blanco = 0
    |(corregirRespuesta (P v n c) r) == Correcta = v
    |(corregirRespuesta (P v n c) r) == Erronea = -(v)*(1 / (fromIntegral (n - 1)))

mensajeMotivacion:: Float ->String
mensajeMotivacion nota 
    |0 <= nota && nota <= 4 = "No te desanimes"
    |4<nota && nota <5 = "Vas por buen camino"
    |5<= nota && nota <7 = "Buen trabajo"
    |7<= nota && nota <=10 = "Impresionante"


{-
ESTADISTICAS:

Para definir esta funcion y calcular sus correspondientes valores he definido los siguientes datos 
-}
type Media = Float
type ResumenNotas = [(Nota,Int)] --Representa el tipo de nota y cuantas personas han obtenido dicha nota
type FrecAbs = [(TipoRespuesta,Int)] --Representan una lista con las frecuencias absolutas de cada tipo de respuesta
type FrecRel = [(TipoRespuesta,Float)]--Representan una lista con las frecuencias relativas de cada tipo de respuesta

data Nota = Suspenso | Aprobado  | Notable  | Sobresaliente  deriving (Show,Eq) --Tipos de notas


data Estadisticas = E Media ResumenNotas FrecAbs FrecRel PreguntaFiltrada PreguntaFiltrada PreguntaFiltrada PreguntaFiltrada 



{-
La funcion estadistica es la siguiente. Simplemente construye un tipo de dato Estadistica  y llama a las correspondientes funciones que dan valor a sus atributos.
Para algunas funciones directamente les pasamos una lista de correciones mediante la funcion corrigeRespuestas, la cual corrige una a una las respuestas, aprovechando el uso de map
y la funcion corrige::Test->RespuestaTest->Correcion

corrigeRespuestas:: Test->[RespuestaTest a]->[Correccion a] 
corrigeRespuestas test rts = map (corrige test) rts



-}
estadisticas::Test ->[RespuestaTest a ]->Estadisticas
estadisticas test rts = let correcciones = corrigeRespuestas test rts in  
                        E (media correcciones) (resumenNotas correcciones) (frecuenciaAbsoluta test rts) 
                        (frecuenciaRelativa test rts) (mejorPregunta test rts) (peorPregunta test rts)
                        (masBlancos test rts) (menosBlancos test rts)


corrigeRespuestas:: Test->[RespuestaTest a]->[Correccion a] 
corrigeRespuestas test rts = map (corrige test) rts


{-
Comencemos con la funcion MEDIA:

media::[Correccion a ]->Float --Nota media sobre 10
media cs = (sumaTotal cs) / fromIntegral(length( cs ))

Es muy sencilla: recibe una lista de correciones ( que se le pasa directamente en estadisticas), suma el valor total de ellas y lo divide entre el numero de examenes
-}
media::[Correccion a ]->Float --Nota media sobre 10
media cs = (sumaTotal cs) / fromIntegral(length( cs ))
sumaTotal:: [Correccion a]->Float
sumaTotal cs = foldr (\(C _ _ nota _) suma -> suma + nota) 0 cs

--Resumen Notas
resumenNotas:: [Correccion a]->ResumenNotas
resumenNotas cs = foldr (\(C _ _ nota _) resumen-> modificarResumen (clasificarNota nota) resumen) [(Suspenso,0 ),(Aprobado,0),(Notable,0),(Sobresaliente,0)] cs

modificarResumen:: Nota ->ResumenNotas->ResumenNotas
modificarResumen  nota  resumen = [(nota',n)|(nota',n)<-resumen, nota' /=  nota] ++ [(nota',n+1)| (nota',n)<-resumen, nota == nota']

clasificarNota:: Float->Nota
clasificarNota nota
    | nota < 5    =  Suspenso
    | nota < 7    =  Aprobado
    | nota < 9    =  Notable 
    | otherwise   =  Sobresaliente

{-
FRECUENCIA ABSOLUTA Y RELATIVA 

*FRECUENCIA ABSOLUTA:
Para la frecuencia absoluta necesitamos el dato 

data TipoRespuesta = Blanco | Correcta | Erronea deriving (Show,Eq)

definido al principio de la pagina.

¿Como calculamos la frecuencia absoluto de cada tipo?
La idea principal es clasificar las respuestas de cada alumno.

Es decir,  queremos pasar de esto:

[R 1, R 2, R 1, R 4...]

a esto

[Correcta,Erronea,Blanco,Blanco,Correcta...]

Este proceso lo conseguimos mediante la funcion

clasificarRespuesta:: Test ->RespuestaTest a ->[TipoRespuesta] --[Correcta,Erronea,Blanco,Blanco,Correcta...]
clasificarRespuesta test (RT id m (r:rs)) = clasificarRespuestaAux (seleccionarModelo test m) (r:rs)

clasificarRespuestaAux :: Modelo->[Respuesta]->[TipoRespuesta]
clasificarRespuestaAux (M m ps) rs = zipWith corregirRespuesta ps rs 

donde, al igual que en CORRIGE, primero es necesario seleccionar el modelo correspondiente; y despues siguiendo la misma idea de pares 
utilizamos un zipWith que convertira cada respuesta en el tipo que es: Acierto,Erronea o Blanca

Una vez hemos clasificado las respuestas del alumno, procedemos a calcular su frecuencia absoluta (solo la de este alumno).

Ahora pasamos de 

[Correcta,Erronea,Blanco,Blanco,Correcta...]

a

[(Blanco,i),(Erronea,j),(Correcta,k)]  donde el segundo elemento del para represebta el numero de cada tipo

Esto se consigue mediante la funcion 

frecuenciaAlumno::[TipoRespuesta]->[(TipoRespuesta,Int)] --[(Blanco,i_a),(Erronea,j_a),(Correcta,k_a)] 
frecuenciaAlumno trs = foldr (\tipo frec -> map (sumarFrecuencia (tipo,1)) frec ) [(Blanco,0),(Erronea,0),(Correcta,0)] trs

Esta funcion coge cada tipo de Respuesta perteneciente a trs y la expresion landa (\tipo frec -> map (sumarFrecuencia (tipo,1)) frec ) coge dicha respuesta y  suma +1 al par 
que corresponda la Respuesta; aplicando esta expresion landa a todos los elementos de trs mediante foldr conseguimos la frecuencia absoluta de un alumno

Una vez somos capaces de calcular de manera individual  la frecuencia absoluta de cada alumno, procedemos a concatenarlas en una misma lista 

frecuenciaAlumnos:: Test->[RespuestaTest a]->[(TipoRespuesta,Int)] 
frecuenciaAlumnos test rs = concat [frecuenciaAlumno (clasificarRespuesta test r) | r <-rs ] 

y finalmente aplicamos la MISMA idea de frecuenciaAlumno en la  funcion frecuenciaAbsoluta:

frecuenciaAbsoluta::Test ->[RespuestaTest a]->[(TipoRespuesta,Int)]
frecuenciaAbsoluta test rs = foldr (\(tipo,n) frec -> map (sumarFrecuencia (tipo,n)) frec) [(Blanco,0),(Erronea,0),(Correcta,0)] (frecuenciaAlumnos test rs)


-}

frecuenciaAbsoluta::Test ->[RespuestaTest a]->[(TipoRespuesta,Int)]
frecuenciaAbsoluta test rs = foldr (\(tipo,n) frec -> map (sumarFrecuencia (tipo,n)) frec) [(Blanco,0),(Erronea,0),(Correcta,0)] (frecuenciaAlumnos test rs)

frecuenciaAlumnos:: Test->[RespuestaTest a]->[(TipoRespuesta,Int)] -- Concatenamos [(Blanco,i_a),(Erronea,j_a),(Correcta,k_a)] para todo alumno a
frecuenciaAlumnos test rs = concat [frecuenciaAlumno (clasificarRespuesta test r) | r <-rs ] 


frecuenciaAlumno::[TipoRespuesta]->[(TipoRespuesta,Int)] --[(Blanco,i_a),(Erronea,j_a),(Correcta,k_a)] 
frecuenciaAlumno trs = foldr (\tipo frec -> map (sumarFrecuencia (tipo,1)) frec ) [(Blanco,0),(Erronea,0),(Correcta,0)] trs


clasificarRespuesta:: Test ->RespuestaTest a ->[TipoRespuesta] --[Correcta,Erronea,Blanco,Blanco,Correcta...]
clasificarRespuesta test (RT id m (r:rs)) = clasificarRespuestaAux (seleccionarModelo test m) (r:rs)
clasificarRespuestaAux :: Modelo->[Respuesta]->[TipoRespuesta]
clasificarRespuestaAux (M m ps) rs = zipWith corregirRespuesta ps rs 

--Pequeña funcion auxiliar que permite modificar las listas de frecuencia [(Blanco,i),(Erronea,j),(Correcta,k)]
sumarFrecuencia::(TipoRespuesta,Int)->(TipoRespuesta,Int)->(TipoRespuesta,Int)
sumarFrecuencia (tipo,n) (tipo',m) = if (tipo == tipo') then (tipo',n+m) else (tipo',m)


{-
Frecuencia Relativa:
Para calcular la frecuencia relativa de cada tipo de Respuesta necesitamos saber su frecuencia absoluta y el numero total de 
respuestas que hay.

Luego si calculamos la frecuencia absoluta, podemos formar directamente  la lista de frecuencias relativas.
Para cada par (tipo , n) de la lista frecAbs, formamos el nuevo par (tipo, n/total respuestas)

-}

frecuenciaRelativa::Test->[RespuestaTest a]->[(TipoRespuesta,Float)]
frecuenciaRelativa test rs = let frecAbs = frecuenciaAbsoluta test rs in
                            [(tipo, fromIntegral(n)/fromIntegral (respuestasTotales rs)) | (tipo,n)<-frecAbs]

respuestasTotales::[RespuestaTest a]->Int
respuestasTotales rts = foldr (\(RT _ _ rs) sum -> (length rs) + sum) 0 rts

{-
MEJOR PREGUNTA Y PEOR PREGUNTA
Para el caso de la pregunta con mejor resultado vamos a considerar aquella que tenga un mayor numero de aciertos, y para la pregunta con peor resultado aquella que entre fallos y espacios en blanco
sume un mayor numero.

Para estas funciones he definido un nuevo tipo de dato: 

data PreguntaFiltrada = Vacio  | PF (Pregunta,Int )

pues es posible, aunque un poco improbable, que nadie sea capaz de responder a alguna de todas las preguntas o ,por el contrario, que todo el mundo conteste adecuadamente las preguntas.Estos
dos casos estan representados mediante el constructor Vacio; el otro constructor PF simplemente representa la pregunta y el numero de aciertos totales o fallos + espacios
en blancos.

Una vez tenemos el nuevo dato. Procedemos a calcular la mejor pregunta mediante la funcion mejorPregunta:

mejorPregunta:: Test->[RespuestaTest a]->PreguntaFiltrada
mejorPregunta test rts = maximo (conteoTotal test rts Correcta) 

¿Que hace mejor pregunta?

La idea es la siguiente. Queremos buscar respuestas de un tipo en concreto, por ejemplo, respuestas Correcta.
Para cada RespuestaTest , vamos a filtrar aquellas que sean Correcta mediante la funcion filtarTipoPregunta que recibe como argumentos un Test, la respuestas y el TIPO que BUSCAMOS en este caso 
particular seria Correcta. Cabe destacar, que al igual que en apartados anteriores, primero es necesario seleccionar el modelo adecuado para comprobar las respuestas.
filtrarTipoPreguntaAux solo añade aquellas que cumplen el mismo tipo que buscamos

filtrarTipoPregunta::Test->RespuestaTest a->TipoRespuesta->[Pregunta]
filtrarTipoPregunta test (RT _ m rs) tipo = filtrarTipoPreguntaAux (seleccionarModelo test m) rs tipo []

filtrarTipoPreguntaAux::Modelo->[Respuesta]->TipoRespuesta->[Pregunta]->[Pregunta]
filtrarTipoPreguntaAux modelo [] tipo acc = acc
filtrarTipoPreguntaAux (M m (p:ps)) (r:rs) tipo acc = 
                    if corregirRespuesta p r == tipo 
                    then [p] ++ filtrarTipoPreguntaAux (M m ps) rs tipo acc
                    else  filtrarTipoPreguntaAux (M m ps) rs tipo acc

Despues de esto, conceptualmente  obtendriamos una lista del siguiente estilo:

[Pregunta 3,Pregunta 1, PRegunta 2...] 

(digo conceptualmente pues el tipo Pregunta es P Float Int Int  y no tiene un identificador como tal. Solo los modelos tienen el identificador , si es que he no he interpretado mal el enunciado )

Realizando esto con cada respuesta, podemos concatenarlas en una misma lista:

preguntasTotales::Test->[RespuestaTest a]->TipoRespuesta->[Pregunta] 
preguntasTotales test rts tipo = concat [filtrarTipoPregunta test r tipo | r<-rts]

obteniendo asi una lista como :

[Pregunta 3,Pregunta 1, PRegunta 2, Pregunta 1,Pregunta 1, Pregunta 3...]

donde solo quedaria realizar la clasificaicon y conteo de cada pregunta en una lista de la forma [(Pregunta i ,n_i),(Pregunta j,n_j)...]:

conteoTotal::Test->[RespuestaTest a]->(TipoRespuesta) ->[(Pregunta,Int)] 
conteoTotal test rts tipo= foldr conteo [] (preguntasTotales test rts tipo)

conteo::Pregunta->[(Pregunta,Int)]->[(Pregunta,Int)]
conteo p ls = if (elem p [ p'|(p',n)<-ls]) then [ l | l<-ls, fst l /= p ] ++ [(p,n+1)| (p,n)<-ls]
            else ls ++ [(p,1)]

La funcion conteo  añade a la lista (Pregunta i, 1 ) si en ningun momento anterior ha aparecido la PRegunta i  o, por el contrario se suma +1 al par (Pregunta i, n)

Finalemnte, teniendo ya el conteo de cada pregunta solo queda calcular el maximo:

mejorPregunta:: Test->[RespuestaTest a]->PreguntaFiltrada
mejorPregunta test rts = maximo (conteoTotal test rts Correcta) 


-}

data PreguntaFiltrada = Vacio  | PF (Pregunta,Int )


mejorPregunta:: Test->[RespuestaTest a]->PreguntaFiltrada
mejorPregunta test rts = maximo (conteoTotal test rts Correcta) 
    
peorPregunta::Test->[RespuestaTest a]->PreguntaFiltrada
peorPregunta test rts = maximo ((conteoTotal test rts Erronea)++(conteoTotal test rts Blanco))
    

     
conteoTotal::Test->[RespuestaTest a]->(TipoRespuesta) ->[(Pregunta,Int)] --[(p_i,n_i),(p_j,n_j)...]
conteoTotal test rts tipo= foldr conteo [] (preguntasTotales test rts tipo)
    
conteo::Pregunta->[(Pregunta,Int)]->[(Pregunta,Int)]
conteo p ls = if (elem p [ p'|(p',n)<-ls]) then [ l | l<-ls, fst l /= p ] ++ [(p,n+1)| (p,n)<-ls]
            else ls ++ [(p,1)]
    
    
preguntasTotales::Test->[RespuestaTest a]->TipoRespuesta->[Pregunta] --[p0,p1,p0,p2,p2,p1...]
preguntasTotales test rts tipo = concat [filtrarTipoPregunta test r tipo | r<-rts]
    
filtrarTipoPregunta::Test->RespuestaTest a->TipoRespuesta->[Pregunta]
filtrarTipoPregunta test (RT _ m rs) tipo = filtrarTipoPreguntaAux (seleccionarModelo test m) rs tipo []
    
filtrarTipoPreguntaAux::Modelo->[Respuesta]->TipoRespuesta->[Pregunta]->[Pregunta]
filtrarTipoPreguntaAux modelo [] tipo acc = acc
filtrarTipoPreguntaAux (M m (p:ps)) (r:rs) tipo acc = 
                        if corregirRespuesta p r == tipo 
                        then [p] ++ filtrarTipoPreguntaAux (M m ps) rs tipo acc
                        else  filtrarTipoPreguntaAux (M m ps) rs tipo acc
    
    
maximo:: [(Pregunta,Int)]->PreguntaFiltrada
maximo [] = Vacio 
maximo ps = maximoAux ps ( ps !! 0)
maximoAux::[(Pregunta,Int)]->(Pregunta,Int)->PreguntaFiltrada
maximoAux [] (p,m) = PF (p,m)
maximoAux ((p,n):ps) (p',m) = if n >= m then maximoAux ps (p,n) else maximoAux ps (p',m)
    



{-
PREGUNTAS CON MAS BLANCOS Y MENOS BLANCOS
Es la misma idea que el apartado anterior, y se reutiliza el mismo codigo. Lo unico que ha sido necesario definir la funcion minimo
-}

masBlancos::Test->[RespuestaTest a]->PreguntaFiltrada
masBlancos test rts = maximo ((conteoTotal test rts Blanco))
    
menosBlancos::Test ->[RespuestaTest a]->PreguntaFiltrada
menosBlancos test rts = minimo ((conteoTotal test rts Blanco))

minimo:: [(Pregunta,Int)]->PreguntaFiltrada
minimo [] = Vacio 
minimo ps = minAux ps ( ps !! 0 )

minAux::[(Pregunta,Int)]->(Pregunta,Int)->PreguntaFiltrada
minAux [] (p,m) = PF (p,m)
minAux ((p,n):ps) (p',m) = if n <= m then minAux ps (p,n) else minAux ps (p',m)



{-
PARTE OPCIONAL:
Para la parte opcional he creado un par de funciones en las  que el usuario puede introducir los datos del Test.
-}


-- Función para leer una Pregunta
leerPregunta :: IO Pregunta
leerPregunta = do
    putStrLn "Introduce el valor de la pregunta (float):"
    valor <- readLn
    putStrLn "Introduce el número de alternativas:"
    alternativas <- readLn
    putStrLn "Introduce la respuesta correcta (número de alternativa):"
    respuestaCorrecta <- readLn
    return (P valor alternativas respuestaCorrecta)

-- Función para leer un Modelo
leerModelo :: Int -> IO Modelo
leerModelo numModelo = do
    putStrLn ("Introduce las preguntas para el modelo " ++ show numModelo)
    putStrLn "Cuántas preguntas tiene el modelo?"
    numPreguntas <- readLn
    preguntas <- mapM (const leerPregunta) [1..numPreguntas]
    return (M numModelo preguntas)

-- Función principal para interactuar con el usuario
crearTest :: IO Test
crearTest = do
    putStrLn "Introduce el número de modelos para el test:"
    numModelos <- readLn
    modelos <- mapM (leerModelo) [1..numModelos]
    return (T modelos)




--Instancias para mostrar los resultados por pantalla adecuadamente
instance Show PreguntaFiltrada where
    show (PF (pregunta,n )) = show pregunta ++ " Total:"++show n
    show Vacio = "Vacio \n"

instance Show Estadisticas where
    show(E media resumen frecAbs frecRel mejorPregunta peorPregunta masBlancos menosBlancos) =
        "La media sobre 10 de todas las notas es: "++show media ++ "\n" ++
        "La distribucion de las notas es la siguiente: "++ show resumen ++ "\n"++
        "La frecuencia absoluta de cada tipo de respuesta es:"++show frecAbs ++"\n"++
        "La frecuencia relativa de cada tipo de respuesta es:"++show frecRel ++"\n"++
        "La pregunta con mejor resultado es " ++ show (mejorPregunta)++"\n"++
        "La pregunta con peor resultado es " ++ show (peorPregunta)++ "\n"++
        "La pregunta con mayor respuestas en blanco es " ++ show ( masBlancos)++"\n"++
        "La pregunta con menor respuestas en blanco es " ++ show ( menosBlancos)++"\n"

instance Show a => Show(Correccion a) where
    show (C id puntuacionTotal puntuacionDiez mensaje) = 
        "ID: "++ show id ++"\n"++
        "Puntuacion total: "++show puntuacionTotal++"\n"++
        "Puntuacion sobre 10 : "++show puntuacionDiez ++"\n"++
        "Mensaje: "++show mensaje ++"\n"





