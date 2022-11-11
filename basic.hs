-- Autor : Bartosz Cybulski
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje automatyzujące proces wyliczania kolejnych stanów cyklu np po zaaplikowaniu bramki X na 2 kubicie w 3 kubitowym cyklu 

--Funkcja zwracajaca zapis w postaci ketu 
-- Przykładowe użycia
-- ket 0 = |0>
-- ket 1 = |1>
ket :: String -> String
ket x = "|" ++ x ++ ">"
--Funkcja zwracajaca zapis w postaci bra 
-- Przykładowe użycia 
-- bra 0 = <0| 
-- bra 1 = <1|
bra :: String -> String
bra x = "<" ++ x ++ "|"
--Funkcja zwraca zapis kubitu w formie listy amplitud prawdopodobienstwa
-- alpha oznacza amplitude prawdopodobieństwa że pomiar wyniesie 0
-- beta oznacza amplitude prawdopodobieństwa że pomiar wyniesie 1
-- Zawsze alpha^2 + beta^2 = 1
qubit :: (Eq a, Num a, Show a) => a -> [String]
qubit id
    | id == -1 = ["alpha" ++ " " ++ ket "0"] ++ ["beta" ++ " " ++ ket "1"]
    | otherwise = ["alpha_" ++ show id ++ " " ++ ket "0"]   ++ ["beta_" ++ show id ++ " " ++ ket "1"]
--Funkcja zwraca liste kubitów dla danego cyklu kwantowego
-- Przykladowe wywolanie:
-- qubitList [1,2,3] = [["alpha_1 |0>","beta_1 |1>"],["alpha_2 |0>","beta_2 |1>"],["alpha_3 |0>","beta_3 |1>"]]
qubitList :: (Eq a, Num a, Show a) => [a] -> [[String]]
qubitList [] = []
qubitList list = [qubit $ head list] ++ (qubitList $ tail list)
--Funkcja otrzymuje iloczyn amplitud prawdopodobienstwa oraz stan koncowy natomiast zwraca tylko iloczyn amplitud prawdopodobienstwa
-- Przykladowe wywolania:
-- getAmplitudes "alpha_1 |0>" = "alpha_1"
-- getAmplitudes "alpha_1_alpha_2 |00>" = "alpha_1_alpha_2"
-- getAmplitudes "alpha_1_alpha_2_alpha_3 |000>" = "alpha_1_alpha_2_alpha_3"
getAmplitudes:: String -> String
getAmplitudes amplitudesAndState = head $ words amplitudesAndState
--Funkcja otrzymuje iloczyn amplitud prawdopodobienstwa oraz stan koncowy natomiast zwraca tylko stan koncowy oraz wyciaga oznacznei stanu z nawiasow dla ket'a
-- Przykladowe wywolania:
-- getState "alpha_1 |0>" = "|0>"
-- getState "alpha_1_beta_2 |01>" = "01"
-- getState "beta_1_alpha_2_alpha_3 |100>" = "100"
getState:: String -> String
getState amplitudeAndState = reverse  $ drop 1 (reverse (drop 1 (head $ tail $ words amplitudeAndState)))
--Funkcja zwraca iloczyn kroneckera dla 2 list zawierajacych amplitudy prawdopodobienstwa i stany  
-- W najprotszym przypadku jest to iloczyn 2 kubitow : 
--      kronecker ["alpha_1 |0>","beta_1 |1>"] ["alpha_2 |0>","beta_2  |1>"] = ["alpha_1_alpha_2 |00>","alpha_1_beta_2 |01>","beta_1_alpha_2 |10>","beta_1_beta_2 |11>"] 
--Można użyć jednak iloczynu kroneckera na nieograniczonej ilości kubitów dla 3 kubitów wynosi on : 
--      ["alpha_1_alpha_2_alpha3 |000>","alpha_1_alpha_2_beta_3 |001>","alpha_1_beta_2_alpha_3 |010>","alpha_1_beta_2_beta_3 |011>",
--      "beta_1_alpha_2_alpha_3 |100>","beta_1_alpha_2_beta_3 |101>","beta_1_beta_2_alpha_3 |110>","beta_1_beta_2_beta_3 |111>"]
kronecker :: [String] -> [String] -> [String]
kronecker [] _ = []
kronecker _ [] = []
kronecker (x:xs) (y:ys) = [(getAmplitudes x) ++ "_" ++ (getAmplitudes y) ++ " " ++ ket ((getState x) ++ (getState y) )] ++ (kronecker [x] ys) ++ (kronecker xs (y:ys))

 --   [firstAlpha_secondAlpha ++ " " ++ ket "00"] ++ [firstAlpha_secondBeta ++ " " ++ ket "01"] ++ 
 --   [secondAlpha_firstBeta ++ " " ++ ket "10" ] ++ [firstBeta_secondBeta ++ " " ++ ket "11"]
 --   where 
 --       firstAlpha_secondAlpha = (getAmplitudes $ head firstSum) ++  (getAmplitudes $ head secondSum)--"alpha_1_alpha_2"
  --      firstAlpha_secondBeta = "alpha_1_beta_2"
   --     secondAlpha_firstBeta = "alpha_2_beta_1"
   --     firstBeta_secondBeta = "beta_1_beta_2"
--Funkcja bierze liste kubitow a nastepnie zwraca liste iloczynow amplitud prawdopodobienstwa dla danego stanu 
-- Przykład uzycia:
-- x = qubitList [1,2,3]
-- entangledQubitState x = 
--      ["alpha_1_alpha_2_alpha_3 |000>","alpha_1_alpha_2_beta_3 |001>","alpha_1_beta_2_alpha_3 |010>","alpha_1_beta_2_beta_3 |011>",
--      "beta_1_alpha_2_alpha_3 |100>","beta_1_alpha_2_beta_3 |101>","beta_1_beta_2_alpha_3 |110>","beta_1_beta_2_beta_3 |111>"]
entangledQubitState :: [[String]] -> [String]
entangledQubitState [] = []
entangledQubitState listOfQubits  = foldl kronecker firstQubit restQubits
    where 
        firstQubit = head listOfQubits
        restQubits = tail listOfQubits
