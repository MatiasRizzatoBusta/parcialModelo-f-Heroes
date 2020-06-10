module Lib where
import Text.Show.Functions
laVerdad = True

--------------------------------------------- Punto 1 ---------------------------------------------
data Heroe = UnHeroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareasRealizadas :: [Tarea]
}deriving (Show)

data Artefacto = UnArtefacto{
    rareza :: Int
}deriving (Show,Eq)

--------------------------------------------- Punto 2 ---------------------------------------------
pasarALaHistoria :: Heroe->Heroe
pasarALaHistoria heroe |(cumpleCondicion (>1000).reconocimiento ) heroe = heroe{epiteto = "el mitico"}
                       |(cumpleCondicion (<500).reconocimiento) heroe = heroe{epiteto = "el magnifico",artefactos = agregoArtefacto lanzaDelOlimpo (artefactos heroe)}
                       |(cumpleCondicion (>100).reconocimiento) heroe = heroe{epiteto = "hoplita",artefactos=xiphos:(artefactos heroe)}
                       |otherwise = heroe
--puedo usar guardas pq tengo contemplados los 4 casos posibles
cumpleCondicion :: (Int->Bool)->Int->Bool
cumpleCondicion condicion reconocimiento = condicion reconocimiento

agregoArtefacto :: Artefacto->[Artefacto]->[Artefacto]
agregoArtefacto artefacto artefactosHeroe= artefacto:artefactosHeroe --agrego estas dos funciones para evitar repetir logica

lanzaDelOlimpo = UnArtefacto 100
xiphos = UnArtefacto 50
relampagoDeZeus = UnArtefacto 500
--------------------------------------------- Punto 3 ---------------------------------------------
type Tarea = Heroe->Heroe

encontrarArtefacto :: Artefacto->Tarea
encontrarArtefacto artefacto heroe = heroe{reconocimiento = sumoReconocimiento (rareza artefacto) heroe,artefactos = agregoArtefacto artefacto (artefactos heroe),tareasRealizadas = agregoTarea (encontrarArtefacto artefacto) heroe}

sumoReconocimiento :: Int->Heroe->Int
sumoReconocimiento rarezaArtefacto heroe = rarezaArtefacto + (reconocimiento heroe)

agregoTarea :: Tarea->Heroe->[Tarea]
agregoTarea tarea heroe = tarea:(tareasRealizadas heroe)

escalarOlimpo :: Tarea
escalarOlimpo heroe = heroe{reconocimiento = sumoReconocimiento 500 heroe,artefactos = (agregoArtefacto relampagoDeZeus.triplicoRareza) heroe,tareasRealizadas = agregoTarea (escalarOlimpo) heroe}

triplicoRareza :: Heroe->[Artefacto]
triplicoRareza heroe = map tomoRareza (artefactos heroe)

tomoRareza :: Artefacto->Artefacto
tomoRareza artefacto = artefacto{rareza = (*3) (rareza artefacto)}

ayudarACruzarLaCalle :: Int->Tarea
ayudarACruzarLaCalle cuadrasCrusadas heroe = heroe{epiteto = queTanGroso cuadrasCrusadas,tareasRealizadas = agregoTarea (ayudarACruzarLaCalle cuadrasCrusadas) heroe }

queTanGroso :: Int->String
queTanGroso cuadrasCrusadas |cuadrasCrusadas >1=  "gros " ++ replicate cuadrasCrusadas 'o' --le hago char pq con replicate 
                            |otherwise = "groso"                                         --lo hago string y ahi concateno


data Bestia = UnaBestia{
    nombre :: String,
    debilidad :: AprovechaDebilidad
}deriving (Show)

type AprovechaDebilidad = Heroe->Bool

matarBestia :: Bestia->Tarea
matarBestia bestia heroe |explotaDebilidad (debilidad bestia) heroe = heroe{epiteto= tituloDeAsesino (nombre bestia),tareasRealizadas = agregoTarea (matarBestia bestia) heroe}
                         |otherwise =heroe{epiteto="el cobarde",artefactos = tail (artefactos heroe)}

explotaDebilidad :: AprovechaDebilidad->Heroe->Bool
explotaDebilidad debilidadBestia heroe = debilidadBestia heroe --la debilidad es una funcion que toma al heroe pq esta puede ser
--cualquier cosa que tenga el heroe y como no se que puede ser, hago que el bicho lo sepa y lo tenga codeado el 


tituloDeAsesino :: String->String
tituloDeAsesino nombreBestia = "el asesino de " ++ nombreBestia

--------------------------------------------- Punto 4 ---------------------------------------------
heracles = UnHeroe "Guardian del Olimpo" 700 [pistola,relampagoDeZeus] [matarLeonDeNamea] 

pistola = UnArtefacto 1000

--------------------------------------------- Punto 5 ---------------------------------------------
leonDeNamea = UnaBestia "Leon de Namea" ((>=20).length.epiteto)

matarLeonDeNamea :: Tarea
matarLeonDeNamea  = matarBestia leonDeNamea

--------------------------------------------- Punto 6 ---------------------------------------------
realizarTarea ::Tarea->Heroe->Heroe
realizarTarea tarea heroe = tarea heroe
--------------------------------------------- Punto 7 ---------------------------------------------
type Ganador = Heroe
type Perdedor = Heroe
presumir :: Heroe->Heroe->(Ganador,Perdedor)
presumir h1 h2 |(reconocimiento h1) > (reconocimiento h2) = (h1,h2)
               |(reconocimiento h1) < (reconocimiento h2) = (h2,h1)
               |(sumatoriaRareza h1) > (sumatoriaRareza h2) && (reconocimiento h1) == (reconocimiento h2)= (h1,h2)
               |(sumatoriaRareza h1) < (sumatoriaRareza h2) && (reconocimiento h1) == (reconocimiento h2)= (h2,h1)
               |otherwise = presumir (foldr ($) h1 (tareasRealizadas h2)) (foldr ($) h2 (tareasRealizadas h1))

sumatoriaRareza :: Heroe->Int
sumatoriaRareza = sum.map rareza.artefactos

--------------------------------------------- Punto 8 ---------------------------------------------
{-
Si dos heroes del mismo reconocimiento,sin objetos y sin ninguna tarea realizada van a estar presumiendo infinitamente porque 
la funcion nunca terminaria de compararlos porque va a entrar por la funcion en donde ambos tienen el mismo reconocimiento y va a 
evaluar los artefactos.Al no tener artefactos va a hacer que hagan las tareas del otro,pero como son vacias ningun heroe cambiaria
y por lo tanto la funcion nunca podria encontrar a un ganador
-}
--------------------------------------------- Punto 9 ---------------------------------------------
realizarLabor :: Heroe->[Tarea]->Heroe
realizarLabor heroe listaTareas = foldr ($) heroe listaTareas
--------------------------------------------- Punto 10 ---------------------------------------------
{-
Si esta funcion es invocada con una lista infinita nunca sabremos el estado final del heroe ya que este nunca pararia de realiar 
tareas
-}
