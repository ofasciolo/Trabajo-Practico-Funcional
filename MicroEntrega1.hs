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
     memoria = [0,0..], 
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
-- No podemos comprobarlo, pero sí podemos identificar si está DES-ordenada, viendo item por item si 
-- esta ordenada hasta llegar a uno que no lo este.

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
nop = avanzarCounter

avanzarCounter :: Instruccion
avanzarCounter unProcesador = unProcesador {programCounter = programCounter unProcesador +1}

leerMemoria :: [Int] -> Int -> Int
leerMemoria = (!!)
memoriaOcupada :: Microprocesador -> Int
memoriaOcupada unProcesador = length (memoria unProcesador)

lod :: Int->Instruccion
lod addr unProcesador 
	|addr > memoriaOcupada unProcesador = errorMem unProcesador
	|otherwise = unProcesador{acumuladorA=memoria unProcesador !! (addr-1)}  --usamos la funcion !! para acceder al item addr de la lista (-1 para acomodar con lo pedido)

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
str addr valor unProcesador
	| addr < 1 = errorMem unProcesador
	| addr > memoriaOcupada unProcesador = errorMem unProcesador
	| otherwise = unProcesador{memoria = reemplazar addr valor (memoria unProcesador)}

errorMem :: Instruccion
errorMem unProcesador = unProcesador{mensajeError="DIRECCION DE MEMORIA VACIA O NO VALIDA"}	
reemplazar :: Int->Int->[Int]->[Int]
reemplazar addr valor lista = (take (addr-1) lista) ++ (valor : drop (addr) lista) 

ifnz :: Microprocesador -> Programa ->Microprocesador
ifnz unProcesador programa
     | acumuladorA unProcesador /=0 = ejecutar unProcesador programa
     | otherwise  = unProcesador

--Carga y ejecucion
ejecutarInstruccion :: Instruccion -> Microprocesador -> Microprocesador 
ejecutarInstruccion instruccion unProcesador
	|((==0).length.mensajeError) unProcesador = (instruccion.avanzarCounter) unProcesador
	| otherwise = unProcesador

ejecutar :: Microprocesador -> Programa -> Microprocesador --Lee de derecha a izquierda
ejecutar unProcesador = foldr (ejecutarInstruccion) unProcesador

cargarPrograma :: Microprocesador -> Programa -> Microprocesador
cargarPrograma unProcesador unPrograma = unProcesador{programa = unPrograma}

ejecutarCargado :: Microprocesador -> Microprocesador
ejecutarCargado unProcesador = ejecutar unProcesador (programa unProcesador)

--Depuracion
depurarPrograma :: Microprocesador -> Programa -> Programa
depurarPrograma unProcesador programa = filter (necesarias unProcesador) programa

necesarias :: Microprocesador -> (Instruccion) -> Bool
necesarias unProcesador instruccion = (/= 0).sumatoria.instruccion $(unProcesador)

sumatoria :: Microprocesador -> Int
sumatoria unProcesador = acumuladorA unProcesador + acumuladorB unProcesador + sum (memoria unProcesador)

--Orden 
esMemoriaOrdenada :: Microprocesador -> Bool
esMemoriaOrdenada unProcesador = comprobarOrden (memoria unProcesador)

comprobarOrden [x] = True  -- recibe la memoria y comprueba si esta ordenada de menor a mayor
comprobarOrden (x:y:z) = x <= y && comprobarOrden(y:z)

--Programas
sumar10con22 :: Programa
sumar10con22 = [add,lodv 22, swap, lodv 10]

dividir2por0 :: Programa
dividir2por0 = [divide,lod 1, swap, lod 2, str 2 0, str 1 2]

casoDePrueba :: Programa
casoDePrueba = [swap,divide,lod 1, swap, lod 2, str 2 0, str 1 2]
