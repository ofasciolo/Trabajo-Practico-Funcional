
module MicroEntrega1 where

--3. Entrega 1
--3.1 Modelar Micro
data Microprocesador = Microprocesador{memoria::[Int] ,acumuladorA::Int, acumuladorB::Int, programCounter::Int,mensajeError::String} deriving Show

--3.1.a
xt8088 =  Microprocesador{memoria = replicate 1024 0 , acumuladorA = 0, acumuladorB = 0,programCounter = 0, mensajeError= ""}

--3.2 Punto 2
--3.2.1
type Instruccion = Microprocesador->Microprocesador
nop :: Instruccion
nop unProcesador = unProcesador{programCounter = programCounter unProcesador +1}

--3.2.2 --- consola, sin terminar
avanzar3 :: Instruccion
avanzar3 = nop.nop.nop

--3.3 Punto 3
--3.3.1
add :: Instruccion
add unProcesador = unProcesador{acumuladorA=acumuladorA unProcesador + acumuladorB unProcesador, acumuladorB=0,programCounter = programCounter unProcesador +1}

lodv :: Int->Instruccion
lodv valor unProcesador = unProcesador{acumuladorA=valor,programCounter = programCounter unProcesador +1}

swap :: Instruccion
swap unProcesador = unProcesador{acumuladorA = acumuladorB unProcesador, acumuladorB=acumuladorA unProcesador,programCounter = programCounter unProcesador +1}

--3.3.2
sumarDosValores valor1 valor2 = (add).(lodv valor2).(swap).(lodv valor1)

--3.4 Punto 4
--3.4.1
divide :: Instruccion
divide unProcesador   
     |acumuladorB unProcesador /=0 = unProcesador{acumuladorA = acumuladorA unProcesador `div` acumuladorB unProcesador, acumuladorB=0,programCounter = programCounter unProcesador +1}
     |otherwise = unProcesador{mensajeError = "DIVISION BY ZERO",programCounter = programCounter unProcesador +1} 

str :: Int->Int->Instruccion
reemplazar :: Int->Int->[Int]->[Int]
reemplazar addr valor lista = (take (addr-1) lista) ++ (valor : drop (addr) lista) 

-- el -1 debido a que ellos empiezan desde pos 1, y nosotros desde 0.

str addr valor unProcesador = unProcesador{memoria = reemplazar addr valor (memoria unProcesador),programCounter = programCounter unProcesador + 1}

lod :: Int->Instruccion
lod addr unProcesador = unProcesador{acumuladorA=memoria unProcesador !! (addr-1), programCounter = programCounter unProcesador + 1}  --usamos la funcion !! para acceder al item addr de la lista (-1 para acomodar con lo pedido)

--3.4.2
dividirDosValores numerador denominador = (divide).(lod 1).(swap).(lod 2).(str 2 denominador).(str 1 numerador)

--4 Casos de prueba
--4.2.3
fp20 = Microprocesador{memoria=replicate 1024 0, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

--4.3.4
at8086 = Microprocesador {memoria =[1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}
