--Autor Bartosz Cybulski 
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje automatyzujące proces wyliczania kolejnych stanów cyklu np po zaaplikowaniu bramki X na 2 kubicie w 3 kubitowym cyklu

--W tym module znajduja sie funkcje opisujace zmiane stanu cyklu kwantowego po zaaplikowaniu wybranej bramki na danym kubicie/kubitach 
--w obecnej wersji modul nie wyczerpuje wszystkich bramek kwantowych a jedynie te niezbedne do opisania logiki klasycznej

module QuantumGates where 

import Basis 


--Funkcja zwraca liste iloczynow zewnetrznych ktorych suma tworzy macierz odpowiadajaca bramce I
i:: [String]
i = [" " ++ ket "1" ++ bra "1"] ++ [" " ++ ket "0" ++ bra "0"]
--Funkcja zwraca liste iloczynow zewnetrznych ktorych suma tworzy  macierz odpowiadajaca bramce X
x :: [String]
x = [" " ++ ket "0" ++ bra "1"] ++ [" " ++ ket "1" ++ bra "0"]
--Funkcja zwraca liste macierzy ktorych suma tworzy macierz odpowiadajaca bramce CCX
ccx = matrixes
    where
        productState = map (drop 3) (kronecker (kronecker i i) [" " ++ ket "0" ++ bra "0"] ++ kronecker (kronecker i [" " ++ bra "0" ++ ket "0"] ++ kronecker x [" " ++ bra "1" ++ ket "1"]) [" " ++ bra "1" ++ ket "1"])
        vectors = map (\z -> getVectorList z []) productState
        kets = map (filter ( \z -> getVectorType z == Ket) ) vectors 
        bras = map (filter ( \z -> getVectorType z == Bra) ) vectors 
        computedKets = map ( \z -> computeVectorList z [])  kets
        computedBras = map ( \z -> computeVectorList z []) bras
        matrixes = map concat  (concatbyId  computedKets computedBras)

concatbyId :: [a] -> [a] ->[[a]]
conctabyId _ [] = [] 
concaybyId [] _ = []
concatbyId [l1] [l2] = [[l1] ++ [l2]]
concatbyId (l1:ls1) (l2:ls2) = [[l1] ++ [l2]] ++ concatbyId ls1 ls2  
--Funkcja zwraca liste macierzy ktorych suma tworzy bramke X, 
--dla cykli zawierajacych wiecej niz 1 kubit bramka X aplikowana jest jako produkt kroneckera
--  bramek identycznosciowych (dla kubitow na ktorych nie jest porzadane stosowanie X)
--  oraz bramek x (dla kubitow na ka ktorych jest porzadane zastosowanie X)
-- Macierz zapisana jest jako lista zawierajaca iloczyn zewnetrzny wektorow 
-- Argumenty wywolania to kolejno lista kubitow na ktorych ma zostac zaaplikowana X oraz wielkosc cyklu
-- Przykladowe wywolania
-- construct_X [1] 1 = [["|0>","<1|"],["|1>","<0|"]]
-- construct_X [2] 2 = [["|10>","<11|"],["|11>","<10|"],["|00>","<01|"],["|01>","<00|"]]
-- construct_X [2,3] 3 = [["|100>","<111|"],["|101>","<110|"],["|110>","<101|"],["|111>","<100|"],["|000>","<011|"],["|001>","<010|"],["|010>","<001|"],["|011>","<000|"]]
construct_X :: [Int] -> Int -> [[String]]
construct_X qubits sizeOfCircuit = 
    if null restGates then map (\z -> getVectorList z []) (map (drop 1) firstGate)
    else matrixes
    where
        gateList =  map (\z -> if elem z qubits then x else i) [1..sizeOfCircuit]
        firstGate = head gateList
        restGates = tail gateList
        productGates = map (drop sizeOfCircuit) (foldl kronecker firstGate restGates)
        vectors = map (\z -> getVectorList z []) productGates 
        kets = map (filter ( \z -> getVectorType z == Ket) ) vectors 
        bras = map (filter ( \z -> getVectorType z == Bra) ) vectors 
        computedKets = map ( \z -> computeVectorList z [])  kets
        computedBras = map ( \z -> computeVectorList z []) bras
        matrixes = map concat  (concatbyId  computedKets computedBras)
--Funkcja zwraca stan cyklu po zaaplikowaniu bramki X na kubitach ktorych pozycje sa zawarte w liscie qubitID
--Poniewaz wystepuje zjawisko splatania kwantowego nalezy rowniez zaaplikowac bramke I  na wszystkich pozostalych kubitach
--Przykladowy program 1 (zastosowanie bramki X na pojedynczym kubicie)
--  qubits = qubitList [1]
--  circuit = entangledQubitState qubits // ["alpha_1 |0>", "beta_1 |1>"]
--  qubitsList = [1]
--  apply_X qubitsList circuit = ["alpha_1 |1>", "beta_1 |0>"]
--Przykladowy program 2 (zastosowanie bramki X na drugim kubicie dwukubitowego cyklu)
--  qubits = qubitList [1,2]
--  circuit = entangledQubitState qubits // ["alpha_1_alpha_2 |00>","alpha_1_beta_2 |01>","beta_1_alpha_2 |10>","beta_1_beta_2 |11>"]
--  qubitsList = [2]
--  apply_X qubitsList circuit = ["alpha_1_alpha_2 |01>","alpha_1_beta_2 |00>","beta_1_alpha_2 |11>","beta_1_beta_2 |10>"]
--Przykladowy program 3 (zastosowanie bramki X na drugim i trzecim kubicie 3 kubitowego cyklu)
-- qubits = qubitList [1,2,3]
-- circuit = entangledQubitState qubits // ["alpha_1_alpha_2_alpha_3 |000>","alpha_1_alpha_2_beta_3 |001>","alpha_1_beta_2_alpha_3 |010>",
--                                          "alpha_1_beta_2_beta_3 |011>","beta_1_alpha_2_alpha_3 |100>","beta_1_alpha_2_beta_3 |101>",
--                                          "beta_1_beta_2_alpha_3 |110>","beta_1_beta_2_beta_3 |111>"]
-- qubitList = [2,3]
-- apply_X qubitList circuit = ["alpha_1_alpha_2_alpha_3 |011>","alpha_1_alpha_2_beta_3 |010>","alpha_1_beta_2_alpha_3 |001>",
--                              "alpha_1_beta_2_beta_3 |000>","beta_1_alpha_2_alpha_3 |111>","beta_1_alpha_2_beta_3 |110>",
--                              "beta_1_beta_2_alpha_3 |101>","beta_1_beta_2_beta_3 |100>"]
apply_X :: [Int] -> [String] -> [String] 
apply_X qubitsList circuit = map (drop 1) clearedState
    where
        clearedState = clearState calculatedState []
        calculatedState = map calculateState stateAfterApply
        stateAfterApply = (\z -> kronecker z circuit) prepareToKronecker
        prepareToKronecker = map (" " ++ ) constructed_X
        constructed_X =  map concat (construct_X qubitsList (length $ getAmplitudeList ( getAmplitudes $ head circuit) [] )) 
--TODO !!
--Funkcja zwraca liste macierzy ktorych suma tworzy bramke CCX
-- Macierz zapisana jest jako lista zawierajaca iloczyn zewnetrzny wektorow 
-- Bramka CCX aplikowana jest do 3 kubitow oznacza to ze  cykl musi byc nie mniejszy niz 3
-- argumenty Wywolania to:
--   3 elementowa lista kubitow na ktorych wywolana jest bramka (gdzie pierwsze dwa elementy to kubity kontrolne)
--   wielkosc cyklu kwantowego
--Przykladowe wywolania
-- construct_CCX [1,2,3] 3 = [["|110>","<110|"],["|100>","<100|"],["|010>","<010|"], ["|000>","<000|"],["|101>","<101|"],
--                              ["|001>","<001"],["|011>","<111|"],["|111>","<011|"]]   
construct_CCX qubits sizeOfCircuit = undefined
