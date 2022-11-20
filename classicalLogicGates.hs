--Autor Bartosz Cybulski 
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje implementujace aplikowanie klasycznych bramek logicznych do cyklu kwantowego

module ClassicalLogicGates where 
import Basis
import QuantumGates

--Funkcja zwraca stan 1 kubitowego cyklu kwantowego implementujacego bramke NOT
--Cykl poczatkowy posiada wartosc ["alpha |0>","beta |1>"] tak jak w literaturze qubit jest przedstawiany zwykle jako: alpha |0> + beta |1>
classicalNot ::  [String]
classicalNot = apply_X [1] (qubit (-1))

--Funkcja zwraca stan 3 kubitowego cyklu kwantowego implementujacego bramke AND
classicalAnd :: [String] 
classicalAnd =  state2
    where
        state1 = map calculateState (map (drop 1) (kronecker (kronecker [" " ++ ket "0"] (qubit 2)) (qubit 1)))
        state2 = map (drop 1) (clearState ( map calculateState (kronecker (map (" " ++) (map concat ccx)) state1)  ) [])

--Funkcja zwraca stan 3 kubitowego cyklu kwantowego implementujacego bramke OR
classicalOr :: [String]
classicalOr = state4
    where 
        state1 = map calculateState (map (drop 1) (kronecker (kronecker [" " ++ ket "1"] (qubit 2)) (qubit 1)))
        state2 = apply_X [2,3] state1
        state3 = map (drop 1) (clearState ( map calculateState (kronecker (map (" " ++) (map concat ccx)) state2)  ) [])
        state4 = apply_X [2,3] state3
--Funkcja zwraca stan 3 kubitowego cyklu kwantowego implementujacego bramke XOR
classicalXor :: [String]
classicalXor = map (drop 1) (clearState ( map calculateState (kronecker (map (" " ++) (map concat ccx)) classicalOr)  ) [])