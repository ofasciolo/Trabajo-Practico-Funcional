module​ ​MicroEntrega1​ ​where 
--3.1
type posicion = Int
type dato = Int
data Microprocesador = Microprocesador {memoria::[dato]->posicion->dato,acumuladorA::Int, acumuladorB::Int, programCounter::Int,etiqueta::String} 
--3.1.a
xt8088 =  Microprocesador{memoria = memoria [] , acumuladorA = 0, acumuladorB = 0, etiqueta = ""}


--3.2.1
type instruccion = Microprocesador->Microprocesador
NOP :: instruccion
NOP unProcesador = unProcesador{programCounter = programCounter unProcesador +1}

--3.2.2 --- consola, sin terminar
avanzar = NOP.NOP.NOP

--3.3.1
ADD unProcesador = unProcesador{acumuladorA=acumuladorA unProcesador + acumuladorB unProcesador, acumuladorB=0}
LODV unProcesador variable= unProcesador{acumuladorA= acummuladorA unProcesador + variable


