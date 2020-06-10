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

type Tarea = Heroe->Heroe

--------------------------------------------- Punto 2 ---------------------------------------------
pasarALaHistoria :: Heroe->Heroe
pasarALaHistoria heroe |(cumpleCondicion (>1000).reconocimiento ) heroe = heroe{epiteto = "el mitico"}
                       |(cumpleCondicion (<500).reconocimiento) heroe = heroe{epiteto = "el magnifico",artefactos = lanzaDelOlimpo:(artefactos heroe)}
                       |(cumpleCondicion (>100).reconocimiento) heroe = heroe{epiteto = "hoplita",artefactos=xiphos:(artefactos heroe)}
                       |otherwise = heroe

cumpleCondicion :: (Int->Bool)->Int->Bool
cumpleCondicion condicion reconocimiento = condicion reconocimiento

lanzaDelOlimpo = UnArtefacto 100
xiphos = UnArtefacto 50
