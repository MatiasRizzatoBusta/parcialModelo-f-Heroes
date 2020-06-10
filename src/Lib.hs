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
                       |(cumpleCondicion (<500).reconocimiento) heroe = heroe{epiteto = "el magnifico",artefactos = agregoArtefacto lanzaDelOlimpo heroe}
                       |(cumpleCondicion (>100).reconocimiento) heroe = heroe{epiteto = "hoplita",artefactos=xiphos:(artefactos heroe)}
                       |otherwise = heroe

cumpleCondicion :: (Int->Bool)->Int->Bool
cumpleCondicion condicion reconocimiento = condicion reconocimiento

agregoArtefacto :: Artefacto->Heroe->[Artefacto]
agregoArtefacto artefacto heroe = artefacto:(artefactos heroe)

lanzaDelOlimpo = UnArtefacto 100
xiphos = UnArtefacto 50

--------------------------------------------- Punto 3 ---------------------------------------------
type Tarea = Heroe->Heroe

encontrarArtefacto :: Artefacto->Tarea
encontrarArtefacto artefacto heroe = heroe{reconocimiento = sumoReconocimiento (rareza artefacto) heroe,artefactos = agregoArtefacto artefacto heroe,tareasRealizadas = agregoTarea "encontrarArtefacto" heroe}

sumoReconocimiento :: Int->Heroe->Int
sumoReconocimiento rarezaArtefacto heroe = rarezaArtefacto + (reconocimiento heroe)

agregoTarea :: String->Heroe->[String]
agregoTarea tarea heroe = tarea:(tareasRealizadas heroe)







