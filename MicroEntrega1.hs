module MicroEntrega1 where
import Text.Show.Functions

type Instruccion = Microprocesador->Microprocesador
data Microprocesador = Microprocesador{
     memoria::[Int] ,
     acumuladorA::Int, 
     acumuladorB::Int, 
     programCounter::Int,
     mensajeError::String,
	 programas :: [Instruccion]
} deriving Show



--                        Procesadores

-- xt8089
xt8089 = Microprocesador{
     memoria = [0,0..], 
	 acumuladorA = 0, 
	 acumuladorB = 0, 
	 programCounter = 0, 
	 mensajeError= "", 
	 programas = []
	 
}

-- xt8088
xt8088 =  Microprocesador{
     memoria = replicate 1024 0, 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeError= "",
	 programas = []
	 }

 -- fp20
fp20 = Microprocesador{
     memoria=replicate 1024 0,
     acumuladorA = 7,
     acumuladorB = 24,
     programCounter = 0,
     mensajeError = "",
	 programas = []
}

-- at8086
at8086 = Microprocesador {
     memoria =[1..20],
	 acumuladorA = 0,
	 acumuladorB = 0,
	 programCounter = 0,
	 mensajeError = "",
	 programas = []
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
str addr valor unProcesador = unProcesador{memoria = reemplazar addr valor (memoria unProcesador)}

str :: Int->Int->Instruccion
reemplazar :: Int->Int->[Int]->[Int]
reemplazar addr valor lista = (take (addr-1) lista) ++ (valor : drop (addr) lista) 



--                        Ejecutar

-- ejecutar instruccion
ejecutarinstruccion :: (Instruccion) -> Instruccion 
ejecutarinstruccion instruccion = instruccion.avanzarCounter

-- ejecutar programa
ejecutar :: Microprocesador -> [Instruccion] -> Microprocesador
ejecutar unProcesador = foldr (ejecutarinstruccion) unProcesador

--------------------------------------------------------------







--3. Entrega 2


--3.3.3
ifnz :: Int->Microprocesador->Microprocesador
ifnz valor = swap.(lodv valor)
