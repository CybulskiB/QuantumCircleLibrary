-- Autor : Bartosz Cybulski
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje automatyzujące proces wyliczania kolejnych stanów cyklu np po zaaplikowaniu bramki X na 2 kubicie w 3 kubitowym cyklu 

--Funkcja zwracajaca zapis w postaci ketu 
-- Przykładowe użycia
-- ket 0 = |0>
-- ket 1 = |1>
ket x = "|" ++ x ++ ">"
--Funkcja zwracajaca zapis w postaci bra 
-- Przykładowe użycia 
-- bra 0 = <0| 
-- bra 1 = <1|
bra :: Show a => a -> [Char]
bra x = "<" ++ show x ++ "|"
--Funkcja zwracajaca standardowy zapis kubitu w postaci sum amplitud prawdopodobieństwa
-- alpha oznacza amplitude prawdopodobieństwa że pomiar wyniesie 0
-- beta oznacza amplitude prawdopodobieństwa że pomiar wyniesie 1
-- Zawsze alpha^2 + beta^2 = 1
qubit id 
    | id == -1  =  "alpha" ++ " " ++ ket "0" ++ " + "  ++ "beta" ++ " " ++ ket "1"
    | otherwise =  "alpha_" ++ show id ++ " " ++ ket "0" ++ " + "  ++ "beta_" ++ show id ++ " " ++ ket "1"
--Funkcja zwraca zapis kubitu w formie listy amplitud prawdopodobienstwa
-- alpha oznacza amplitude prawdopodobieństwa że pomiar wyniesie 0
-- beta oznacza amplitude prawdopodobieństwa że pomiar wyniesie 1
-- Zawsze alpha^2 + beta^2 = 1
qubitAmplitudes id
    | id == -1 = ["alpha" ++ " " ++ ket "0"] ++ ["beta" ++ " " ++ ket "1"]
    | otherwise = ["alpha_" ++ show id ++ " " ++ ket "0"]   ++ ["beta_" ++ show id ++ " " ++ ket "1"]
--Funkcja zwraca zapis poczatkowego stanu kwantowego w postaci iloczynów kroneckera dla poszczególnych kubitów
sumOfproductState [] = ""
sumOfproductState qubits_list = qubit ( head qubits_list )  ++  " X " ++ sumOfproductState ( tail qubits_list )
--Funkcja zwraca liste kubitów dla danego cyklu kwantowego 
qubitList [] = []
qubitList list = [qubitAmplitudes $ head list] ++ (qubitList $ tail list)
--Funkcja zwraca iloczyn kroneckera dla 2 kubitow 
kronecker firstQubit secondQubit = 
    [firstAlpha_secondAlpha ++ " " ++ ket "00"] ++ [firstAlpha_secondBeta ++ " " ++ ket "01"] ++ 
    [secondAlpha_firstBeta ++ " " ++ ket "10" ] ++ [firstBeta_secondBeta ++ " " ++ ket "11"]
    where 
        firstAlpha_secondAlpha = "alpha_1_alpha_2"
        firstAlpha_secondBeta = "alpha_1_beta_2"
        secondAlpha_firstBeta = "alpha_2_beta_1"
        firstBeta_secondBeta = "beta_1_beta_2"

entagledQubitState [[]] = [[]]
entagledQubitState listOfQubits  = foldl kronecker firstQubit restQubits
    where 
        firstQubit = head listOfQubits
        restQubits = tail listOfQubits
