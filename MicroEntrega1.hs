module MicroEntrega1 where
import Text.Show.Functions

type Instruccion = Microprocesador->Microprocesador
type Programa = [Instruccion]
data Microprocesador = Microprocesador{
     memoria::[Int] ,
     acumuladorA::Int, 
     acumuladorB::Int, 
     programCounter::Int,
     mensajeError::String,
     programa :: Programa
} deriving Show

--Microprocesadores
xt8089 = Microprocesador{
     memoria = [0,0..], 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeError= "", 
     programa = [] 
}

xt8088 =  Microprocesador{
     memoria = replicate 1024 0, 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeError= "",
     programa = []
}

fp20 = Microprocesador{
     memoria=replicate 1024 0,
     acumuladorA = 7,
     acumuladorB = 24,
     programCounter = 0,
     mensajeError = "",
     programa = []
}


microInfinito = Microprocesador{
     memoria = cycle [0],
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeError= "",
     programa = []
}
-- ¿Qué sucede al querer cargar y ejecutar el programa que suma 10 y 22 en el procesador con memoria infinita?
-- Lo ejecuta sin problemas pero al querer verlo por pantalla al ser una lista infinita va a iterar sin fin
-- y no va a ser posible ver el resultado ya que toda la pantalla va a ser tapada por ceros
-- ¿Y si queremos saber si la memoria está ordenada (punto anterior)?
-- Nunca termina de iterar

microDesorden = Microprocesador {
     memoria =[2, 5, 1, 0, 6, 9],
     acumuladorA = 0,
     acumuladorB = 0,
     programCounter = 0,
     mensajeError = "",
     programa = []
}

at8086 = Microprocesador {
     memoria =[1..20],
     acumuladorA = 0,
     acumuladorB = 0,
     programCounter = 0,
     mensajeError = "",
     programa = []
}
 
--Instrucciones
nop :: Instruccion
nop = id

avanzarCounter :: Instruccion
avanzarCounter unProcesador = unProcesador {programCounter = programCounter unProcesador +1}

lod :: Int->Instruccion
lod addr unProcesador = unProcesador{acumuladorA=memoria unProcesador !! (addr-1)}  --usamos la funcion !! para acceder al item addr de la lista (-1 para acomodar con lo pedido)

add :: Instruccion
add unProcesador = unProcesador{acumuladorA=acumuladorA unProcesador + acumuladorB unProcesador, acumuladorB = 0}

lodv :: Int->Instruccion
lodv valor unProcesador = unProcesador{acumuladorA=valor}

swap :: Instruccion
swap unProcesador = unProcesador{acumuladorA = acumuladorB unProcesador, acumuladorB=acumuladorA unProcesador}

divide :: Instruccion
divide unProcesador   
     |acumuladorB unProcesador /=0 = unProcesador{acumuladorA = acumuladorA unProcesador `div` acumuladorB unProcesador, acumuladorB = 0}
     |otherwise = unProcesador{mensajeError = "DIVISION BY ZERO"} 

str :: Int->Int->Instruccion
str addr valor unProcesador = unProcesador{memoria = reemplazar addr valor (memoria unProcesador)}

reemplazar :: Int->Int->[Int]->[Int]
reemplazar addr valor lista = (take (addr-1) lista) ++ (valor : drop (addr) lista) 

ifnz :: Microprocesador -> Programa ->Microprocesador
ifnz unProcesador instrucciones
     | acumuladorA unProcesador /=0 = ejecutar unProcesador instrucciones
     | otherwise  = unProcesador

--Carga y ejecucion
ejecutarinstruccion :: (Instruccion) -> Instruccion 
ejecutarinstruccion instruccion = instruccion.avanzarCounter

ejecutar :: Microprocesador -> Programa -> Microprocesador --Lee de derecha a izquierda
ejecutar unProcesador = foldr (ejecutarinstruccion) unProcesador

cargarPrograma :: Microprocesador -> Programa -> Microprocesador
cargarPrograma unProcesador prog = unProcesador{programa = prog}

ejecutarPrograma :: Microprocesador -> Programa -> Microprocesador --Carga y ejecuta
ejecutarPrograma unProcesador programa = ejecutarCargado (cargarPrograma unProcesador programa)

ejecutarCargado :: Microprocesador -> Microprocesador
ejecutarCargado unProcesador = ejecutar unProcesador (programa unProcesador)

--Depuracion
depurarPrograma :: Microprocesador -> Programa -> Programa
depurarPrograma unProcesador programa = filter (innecesarias unProcesador) programa

innecesarias :: Microprocesador -> (Instruccion) -> Bool
innecesarias unProcesador instruccion = sumatoria(instruccion unProcesador) /= 0

sumatoria :: Microprocesador -> Int
sumatoria unProcesador = acumuladorA unProcesador + acumuladorB unProcesador + sum (memoria unProcesador)

--Orden 
esMemoriaOrdenada :: Microprocesador -> Bool
esMemoriaOrdenada unProcesador = comprobarOrden (memoria unProcesador)

comprobarOrden [x] = True  -- recibe la memoria y comprueba si esta ordenada de menor a mayor
comprobarOrden (x:y:z) = x <= y && comprobarOrden(y:z)


