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



--                        Procesadores

-- xt8089
xt8089 = Microprocesador{
     memoria = [0,0..], 
	 acumuladorA = 0, 
	 acumuladorB = 0, 
	 programCounter = 0, 
	 mensajeError= "", 
	 programa = []
	 
}

-- xt8088
xt8088 =  Microprocesador{
     memoria = replicate 1024 0, 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeError= "",
	 programa = []
}

 -- fp20
fp20 = Microprocesador{
     memoria=replicate 1024 0,
     acumuladorA = 7,
     acumuladorB = 24,
     programCounter = 0,
     mensajeError = "",
	 programa = []
}

-- microprocesador con memoria infinita
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

-- procesador con la memoria desordenada
microDesorden = Microprocesador {
     memoria =[2, 5, 1, 0, 6, 9],
	 acumuladorA = 0,
	 acumuladorB = 0,
	 programCounter = 0,
	 mensajeError = "",
	 programa = []
}

-- at8086
at8086 = Microprocesador {
     memoria =[1..20],
	 acumuladorA = 0,
	 acumuladorB = 0,
	 programCounter = 0,
	 mensajeError = "",
	 programa = []
}
 

 
 
--                        Instrucciones

--nop
nop :: Instruccion
nop = id

avanzarCounter :: Instruccion
avanzarCounter unProcesador = unProcesador {programCounter = programCounter unProcesador +1}

--lod
lod :: Int->Instruccion
lod addr unProcesador = unProcesador{acumuladorA=memoria unProcesador !! (addr-1)}  --usamos la funcion !! para acceder al item addr de la lista (-1 para acomodar con lo pedido)

--add
add :: Instruccion
add unProcesador = unProcesador{acumuladorA=acumuladorA unProcesador + acumuladorB unProcesador, acumuladorB = 0}

--lodv
lodv :: Int->Instruccion
lodv valor unProcesador = unProcesador{acumuladorA=valor}

--swap
swap :: Instruccion
swap unProcesador = unProcesador{acumuladorA = acumuladorB unProcesador, acumuladorB=acumuladorA unProcesador}

--divide
divide :: Instruccion
divide unProcesador   
     |acumuladorB unProcesador /=0 = unProcesador{acumuladorA = acumuladorA unProcesador `div` acumuladorB unProcesador, acumuladorB = 0}
     |otherwise = unProcesador{mensajeError = "DIVISION BY ZERO"} 

--str
str :: Int->Int->Instruccion
str addr valor unProcesador = unProcesador{memoria = reemplazar addr valor (memoria unProcesador)}

reemplazar :: Int->Int->[Int]->[Int]
reemplazar addr valor lista = (take (addr-1) lista) ++ (valor : drop (addr) lista) 

--ifnz
ifnz :: Microprocesador -> Programa ->Microprocesador
ifnz unProcesador instrucciones
	 | acumuladorA unProcesador /=0 = ejecutar unProcesador instrucciones
	 | otherwise  = unProcesador
	 
	 

--                        Carga y ejecucion

-- ejecutar instruccion
ejecutarinstruccion :: (Instruccion) -> Instruccion 
ejecutarinstruccion instruccion = instruccion.avanzarCounter

-- ejecutar programa (lee de derecha a izquierda)
ejecutar :: Microprocesador -> Programa -> Microprocesador
ejecutar unProcesador = foldr (ejecutarinstruccion) unProcesador

--cargar programa
cargarPrograma :: Microprocesador -> Programa -> Microprocesador
cargarPrograma unProcesador prog = unProcesador{programa = prog}

--carga y ejecuta un programa
ejecutarPrograma :: Microprocesador -> Programa -> Microprocesador
ejecutarPrograma unProcesador programa = ejecutarCargado (cargarPrograma unProcesador programa)

--ejecutar programa cargado
ejecutarCargado :: Microprocesador -> Microprocesador
ejecutarCargado unProcesador = ejecutar unProcesador (programa unProcesador)



--                        Depuracion

--depurar programa
depurarPrograma :: Microprocesador -> Programa -> Programa
depurarPrograma unProcesador programa = filter (innecesarias unProcesador) programa

--instrucciones innecesarias
innecesarias :: Microprocesador -> (Instruccion) -> Bool
innecesarias unProcesador instruccion = sumatoria(instruccion unProcesador) /= 0

--sumatoria de todos los valores en el microprocesador (acumA, acumB, memoria)
sumatoria :: Microprocesador -> Int
sumatoria unProcesador = acumuladorA unProcesador + acumuladorB unProcesador + sum (memoria unProcesador)



--                        Orden 

--saber si la memoria de un procesador esta ordenada
esMemoriaOrdenada :: Microprocesador -> Bool
esMemoriaOrdenada unProcesador = comprobarOrden (memoria unProcesador)

-- recibe la memoria y comprueba si esta ordenada de mayor a menor
comprobarOrden [x] = True
comprobarOrden (x:y:z) = x >= y && comprobarOrden(y:z)


