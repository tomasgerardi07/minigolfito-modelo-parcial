module Library where
import PdePreludat

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

data Tiro = Tiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

type Palo = Habilidad -> Tiro

-- Jugadores de ejemplo
bart :: Jugador
bart = Jugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = Jugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Punto 1.a
putter :: Palo
putter habilidad = Tiro {
    velocidad = 10,
    precision = 2 * precisionJugador habilidad,
    altura = 0
}

madera :: Palo
madera habilidad = Tiro {
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
}

hierro :: Number -> Palo
hierro n habilidad = Tiro {
    velocidad = n * fuerzaJugador habilidad,
    precision = precisionJugador habilidad `div` n,
    altura = max 0 (n-3)
}

-- Punto 1.b
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- Punto 2
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo . habilidad $ jugador

-- Punto 3
data Obstaculo = Obstaculo {
  puedeSuperarlo :: Tiro -> Bool,
  tiroAfectado   :: Tiro -> Tiro
}

superarObstaculo :: Obstaculo -> Tiro -> Tiro
superarObstaculo obstaculo tiro
  | (puedeSuperarlo obstaculo) tiro = (tiroAfectado obstaculo) tiro
  | otherwise                       = tiroDetenido 

tiroDetenido :: Tiro
tiroDetenido = Tiro 0 0 0

-- Rampita
rampita :: Obstaculo
rampita = Obstaculo superarRampita efectoRampita

superarRampita :: Tiro -> Bool
superarRampita tiro = precision tiro > 90 && alRasDelSuelo tiro

alRasDelSuelo :: Tiro -> Bool
alRasDelSuelo = (==0) . altura

efectoRampita :: Tiro -> Tiro
efectoRampita tiro = tiro {
  velocidad = velocidad tiro * 2,
  precision = 100,
  altura = 0
}

-- Laguna
laguna :: Number -> Obstaculo
laguna largo = Obstaculo superarLaguna (efectoLaguna largo)

superarLaguna :: Tiro -> Bool
superarLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiro = tiro {
  altura = altura tiro `div` largo
}

-- Hoyo
hoyo :: Obstaculo
hoyo = Obstaculo superarHoyo efectoHoyo

superarHoyo :: Tiro -> Bool
superarHoyo tiro = between 5 20 (velocidad tiro) && alRasDelSuelo tiro && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = tiroDetenido

-- Punto 4.a
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (puedeSuperarlo obstaculo . golpe jugador) palos

-- Punto 4.b
cuantosObstaculosSupera :: [Obstaculo] -> Tiro -> Number
cuantosObstaculosSupera [] _       = 0
cuantosObstaculosSupera (obstaculo:obstaculos) tiro
  | (puedeSuperarlo obstaculo) tiro = 1 + cuantosObstaculosSupera obstaculos (tiroAfectado obstaculo $ tiro)
  | otherwise                       = 0 

-- Punto 4.c
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (cuantosObstaculosSupera obstaculos . golpe jugador) palos

-- Punto 5
jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = map (padre . jugadorDeTorneo) . filter (not . gano puntosDeTorneo) $ puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador) $ puntosDeTorneo