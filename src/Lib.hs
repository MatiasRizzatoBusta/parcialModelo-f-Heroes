module Lib where
import Text.Show.Functions
laVerdad = True

--------------------------------------------- Punto 1 ---------------------------------------------

data Heroe = UnHeroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareasRealizadas :: [String]
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

cumpleCondicion :: (Int->Bool)->Int->Bool
cumpleCondicion condicion reconocimiento = condicion reconocimiento

agregoArtefacto :: Artefacto->[Artefacto]->[Artefacto]
agregoArtefacto artefacto artefactosHeroe= artefacto:artefactosHeroe

lanzaDelOlimpo = UnArtefacto 100
xiphos = UnArtefacto 50
relampagoDeZeus = UnArtefacto 500
--------------------------------------------- Punto 3 ---------------------------------------------
type Tarea = Heroe->Heroe

encontrarArtefacto :: Artefacto->Tarea
encontrarArtefacto artefacto heroe = heroe{reconocimiento = sumoReconocimiento (rareza artefacto) heroe,artefactos = agregoArtefacto artefacto (artefactos heroe),tareasRealizadas = agregoTarea "encontrarArtefacto" heroe}

sumoReconocimiento :: Int->Heroe->Int
sumoReconocimiento rarezaArtefacto heroe = rarezaArtefacto + (reconocimiento heroe)

agregoTarea :: String->Heroe->[String]
agregoTarea tarea heroe = tarea:(tareasRealizadas heroe)

escalarOlimpo :: Tarea
escalarOlimpo heroe = heroe{reconocimiento = sumoReconocimiento 500 heroe,artefactos = (agregoArtefacto relampagoDeZeus.triplicoRareza) heroe,tareasRealizadas = agregoTarea "escalar Olimpo" heroe}

triplicoRareza :: Heroe->[Artefacto]
triplicoRareza heroe = map tomoRareza (artefactos heroe)

tomoRareza :: Artefacto->Artefacto
tomoRareza artefacto = artefacto{rareza = (*3) (rareza artefacto)}
{-
ayudarACruzarLaCalle :: Int->Tarea
ayudarACruzarLaCalle cuadrazCrusadas heroe = heroe{epiteto = queTanGroso cuadrazCrusadas,tareasRealizadas = agregoTarea "ayudar a cruzar la calle" heroe }

queTanGroso :: Int->String
queTanGroso cuadrazCrusadas |cuadrazCrusadas >1=  queTanGroso (cuadrazCrusadas -1) :("gros" ++ "o")
                            |otherwise = "groso"

-}

data Bestia = UnaBestia{
    nombre :: String,
    debilidad :: AprovechaDebilidad
}deriving (Show)

type AprovechaDebilidad = Heroe->Bool

matarBestia :: Bestia->Tarea
matarBestia bestia heroe |explotaDebilidad (debilidad bestia) heroe = heroe{epiteto= tituloDeAsesino (nombre bestia),tareasRealizadas = agregoTarea "matar Bestia" heroe}
                         |otherwise =heroe{epiteto="el cobarde",artefactos = tail (artefactos heroe)}

explotaDebilidad :: AprovechaDebilidad->Heroe->Bool
explotaDebilidad debilidadBestia heroe = debilidadBestia heroe --la debilidad es una funcion que toma al heroe pq esta puede ser
--cualquier cosa que tenga el heroe y como no se que puede ser, hago que el bicho lo sepa y lo tenga codeado el 


tituloDeAsesino :: String->String
tituloDeAsesino nombreBestia = "el asesino de " ++ nombreBestia

--------------------------------------------- Punto 4 ---------------------------------------------
heracles = UnHeroe "Guardian del Olimpo" 700 [pistola,relampagoDeZeus] ["matar al león de Nemea"] 

pistola = UnArtefacto 1000

--------------------------------------------- Punto 5 ---------------------------------------------
leonDeNamea = UnaBestia "Leon de Namea" ((>=20).length.epiteto)

matarLeonDeNamea :: Tarea
matarLeonDeNamea  = matarBestia leonDeNamea
