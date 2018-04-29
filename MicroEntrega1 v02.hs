module​ ​MicroEntrega1​ ​where 
--3.1
type dato = Int
data Microprocesador = Microprocesador {memoria::[dato] ,acumuladorA::Int, acumuladorB::Int, programCounter::Int,etiqueta::String} 
--3.1.a
xt8088 =  Microprocesador{memoria = memoria [] , acumuladorA = 0, acumuladorB = 0,programCounter = 0, etiqueta = ""}


--3.2.1
type instruccion = Microprocesador->Microprocesador
nop :: instruccion
nop unProcesador = unProcesador{programCounter = programCounter unProcesador +1}

--3.2.2 --- consola, sin terminar
avanzar3 :: instruccion
avanzar3 = nop.nop.nop

--3.3.1
add :: instruccion
add unProcesador = unProcesador{acumuladorA=acumuladorA unProcesador + acumuladorB unProcesador, acumuladorB=0}

lodv :: Int->instruccion
lodv valor unProcesador = unProcesador{acumuladorA= acumuladorA unProcesador + valor}

swap :: instruccion
swap unProcesador = unProcesador{acumuladorA = acumuladorB unProcesador, acumuladorB=acumuladorA unProcesador}

--3.3.2
sumar10con22 :: instruccion
avanzar4 :: instruccion

avanzar4 = nop.avanzar3
sumar10con22 = (avanzar4).(add).(lodv 22).(swap).(lodv 10)

--3.4.1
divide :: instruccion
divide unProcesador 
 | acumuladorB unProcesador /=0	= unProcesador{acumuladorA = acumuladorA unProcesador / acumuladorB unProcesador, acumuladorB=0}
 | otherwise   = unProcesador{etiqueta = "DIVISION BY ZERO", programCounter = 6} 
 
type addr = Int
type val = Int
str :: address->val->instruccion
reemplazar address->val->[a]->[a]
reemplazar addr valor lista = take addr-1 lista ++ valor : drop (addr) lista -- el -1 debido a que ellos empiezan desde pos 1, y nosotros desde 0.
str addr valor unProcesador = unProcesador{memoria = reemplazar addr valor memoria unProcesador}

lod :: address->instruccion
lod addr unProcesador = unProcesador{acumuladorA=memoria unProcesador!!(addr-1)}  --usamos la funcion !! para acceder al item addr de la lista (-1 para acomodar con lo pedido)

--3.4.2
div2por0 :: instruccion
div2por0 = (div).(lod 1).(swap).(lod 2).(str 2 0).(str 1 2)

