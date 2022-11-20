-- Autor : Bartosz Cybulski
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje automatyzujące proces wyliczania kolejnych stanów cyklu np po zaaplikowaniu bramki X na 2 kubicie w 3 kubitowym cyklu

--W tym module znajduja sie podstawy takie jak sposob przedstawienia kubitow oraz funkcja do inicjalizacji poczatkowego stanu  
module Basis where 

data VectorType = Ket | Bra | KetBra deriving(Show,Eq)
data OperationType = Ket_Ket | Bra_Bra | Bra_Ket | Ket_Bra | KetBra_KetBra deriving(Show)
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
-- getAmplitudes " |1><1|" = ""
getAmplitudes:: String -> String
getAmplitudes amplitudesAndState = drop 13  (head $ words ("nullAmplitude" ++ amplitudesAndState))
--Funkcja otrzymuje iloczyn amplitud prawdopodobienstwa oraz stan koncowy natomiast zwraca tylko stan koncowy oraz wyciaga oznacznei stanu z nawiasow dla ket'a
-- Przykladowe wywolania:
-- getState "alpha_1 |0>" = "|0>"
-- getState "alpha_1_beta_2 |01>" = "|01>"
-- getState "beta_1_alpha_2_alpha_3 |100>" = "|100>"
-- getState " |1><1|" = " |1><1|"
getState:: String -> String
getState amplitudeAndState = head $ tail $ words ("nullAmplitude" ++ amplitudeAndState)
--Funkcja zwraca iloczyn kroneckera dla 2 list zawierajacych amplitudy prawdopodobienstwa i stany  
-- W najprotszym przypadku jest to iloczyn 2 kubitow : 
--      kronecker ["alpha_1 |0>","beta_1 |1>"] ["alpha_2 |0>","beta_2  |1>"] = ["alpha_1_alpha_2 |0>|0>","alpha_1_beta_2 |0>|1>","beta_1_alpha_2 |1>|0>","beta_1_beta_2 |1>|1>"] 
--Można użyć jednak iloczynu kroneckera na nieograniczonej ilości kubitów dla 3 kubitów wynosi on : 
--      ["alpha_1_alpha_2_alpha3 |0>|0>|0>","alpha_1_alpha_2_beta_3 |0>|0>|1>","alpha_1_beta_2_alpha_3 |0>|1>|0>","alpha_1_beta_2_beta_3 |0>|1>|1>",
--      "beta_1_alpha_2_alpha_3 |1>|0>|0>","beta_1_alpha_2_beta_3 |1>|0>|1>","beta_1_beta_2_alpha_3 |1>|1>|0>","beta_1_beta_2_beta_3 |1>|1>|1>"]
-- Mozna rowniez uzyc go do obliczenia iloczynu bramek 
--      kronecker [" |1><1|"," |0><0|"] [" |0><1|"," |1><0|"] = ["_ |1><1||0><1|", "_ |1><1||1><0|","_ |0><0||0><1|","_|0><0||1><0|"]
--      kronecker ["alpha_1_alpha_2 |00>","alpha_1_beta_2 |01>","beta_1_alpha_2 |10>","beta_1_beta_2 |11>"] [" |10>"," <11|"] = 
kronecker :: [String] -> [String] -> [String]
kronecker [] _ = []
kronecker _ [] = []
kronecker (x:xs) (y:ys) = [(getAmplitudes x) ++ "_" ++ (getAmplitudes y) ++ " " ++ (getState x) ++ (getState y) ] ++ (kronecker [x] ys) ++ (kronecker xs (y:ys))
--Fukcja wyciaga wartosci z keta lub bra
-- Przykladowe wywolanie 
-- getValue "|01>" = "01"
-- getValue "<1|" = "1"
-- getValue "<10|" = "10"
getValue:: String -> String 
getValue vector = reverse $ drop 1 (reverse $ drop 1 vector) 
--Funkcja wylicza iloczyn kroneckera dla  2 ketow 
--Przykladowe wywolania
-- kroneckerKetProduct "|0>" "|1>" = "|01>"
-- kroneckerKetProduct "|1>" "|0>" = "|10>"
-- kroneckerKetProduct "|10>" "|1>" = "|101>"
-- kroneckerKetProduct "|11>" "|00>" = "|1100>"
kroneckerKetProduct :: String -> String -> String 
kroneckerKetProduct firstKet secondKet= ket $  (getValue firstKet   ++ getValue secondKet)
--Funkcja wylicza iloczyn kroneckera dla  2 bra 
--Przykladowe wywolania
-- kroneckerBraProduct "<0|" "<1|" = "<01|"
-- kroneckerBraProduct "<1|" "<0|" = "<10|"
-- kroneckerBraProduct "<10|" "<1|" = "<101|"
-- kroneckerBraProduct "<11|" "<00|" = "<1100|"
kroneckerBraProduct :: String -> String -> String 
kroneckerBraProduct firstBra secondBra = bra $ (getValue firstBra ++ getValue secondBra)
--Funkcja wylicza iloczyn wewnetrzny dla bra i keta
--Z uwagi ze w programowaniu kwantowym wektory i macierze skladaja sie tylko z 0 i 1 i
--      loczyn wewnetrzny rowny jest 1 tylko jezeli  bra rowne jest odwroconemu ketowi (ich wartosci sa sobie rowne)
--Przykladowe wywolania
-- innerProduct "<0|" "|1>" = "0"
-- innerProduct "<1|"  "|1>" = "1"
-- innerProduct "<10|" "|10>" = "1"
-- innerProduct "<01|" "|10>" = "0" 
innerProduct :: String -> String -> String  
innerProduct bra ket = 
    if (getValue bra) == (getValue ket) then "1"
    else "0" 

--Funkcja zwraca macierz w postaci iloczynu zewnetrznego 2 stanow 
outerProduct :: String -> String -> String
outerProduct = undefined
--Funkcja okresla jakiego typu jest dany wektor 
--Przykladowe wywolania 
-- getVectorType "|1111>" = Ket
-- getVectorType "<000|" = Bra
-- getVectorType "|1><1|" = KetBra
getVectorType :: String->VectorType
getVectorType vector = 
    case start_end of
        "<|" -> Bra
        "|>" -> Ket
        "||" -> KetBra
    where 
        start_end = (take 1 vector) ++ (take 1 (reverse vector)) 
--Funkcja okresla jakiego rodzaju iloczynu nalezy uzyc pomiedzy 2 wektorami 
--Przyklady uzycia
--  getOperationType "|1>" "|00>" = Ket_Ket
--  getOperationType "<10|" "<0|" = Bra_Bra
--  getOperationType "<0|" "|1>"  = Bra_Ket
--  getOperationType "|11>" "<00|" = Ket_Bra
getOperationType :: String -> String -> OperationType
getOperationType firstVector secondVector = 
    case (getVectorType firstVector, getVectorType secondVector) of
        (Ket,Ket) -> Ket_Ket
        (Bra,Bra) -> Bra_Bra
        (Bra,Ket) -> Bra_Ket 
        (Ket,Bra) -> Ket_Bra
        (KetBra,KetBra) -> KetBra_KetBra 
--Funkcja otrzymuje dwa ciagi znakow odpowiadajace kolejnym bra lub ketom a nastepnie przeksztalca je do odpowiedniej formy 
--z uwagi na aplikowanie funkcji do bramek moze nastapic wyzerowanie stanu metoda liczenia jest nastepujaca 
--      ket x ket y = ket xy 
--      bra x bra y = bra xy 
--      bra x ket y = 1 jeżeli x = y 
--      bra x ket y = 0  jeżeli x != y 
--      ket x bra y = ket x bra y 
--      (ket x bra y) (ket z bra w) = ket xz bra yw
-- Przykladowe wywolania 
--      computeState "|0>" "|0>" = "|00>"
--      computeState "<1|" "|0>" = "0" 
--      computeState "<0|" "|0>" = "1" 
--      computeState "<10|" "|10>" = "1"
--      computeState "<10|" "|01>" = "0"
--      computeState "|1>" "<0|" = "|1><0|" 
--      computeState "|11>" "<00|" = "|11><00|"
--      computeState "<111|" "<0|" = "<1110|"
--      computeState "|1><1|" "|0><1|"" = "|10><11|"
computeState:: String -> String -> String
computeState [] [] = [] 
computeState firstVector secondVector = 
    case getOperationType firstVector secondVector of 
        Ket_Ket -> kroneckerKetProduct firstVector secondVector 
        Bra_Bra -> kroneckerBraProduct firstVector secondVector 
        Bra_Ket -> innerProduct firstVector secondVector
        Ket_Bra -> firstVector ++ secondVector
--        KetBra_KetBra -> 
--Funkcja na ktorej pozycji w ciagu znakow znajduje sie jeden z 2  okreslonych znakow, 
-- w przypadku nie wystepowania zwracana jest wielkosc ciagu


--Notatka to pomocnicza funkcja ale niezbyt elegancka podczas rozmowy z Profesorem trzeba ja wskazac



--Przyklady uzycia   
-- findChar "0000>" 0 '>' '|' = 5
-- findChar "<1|" 0 '|' '>' = 3
-- findChar "1111" 0 '0' '0' = 4
-- findChar "0000>" 0 '0' '|' = 1\
findChar :: String -> Int -> Char -> Char -> Int 
findChar [] acc _ _ = acc
findChar (x:xs) acc char1 char2 = if x == char1 || x == char2 then (acc + 1) else findChar xs (acc + 1) char1 char2
--Funkcja bierze ciag znakow w postaci kolejnych bra i ketow oraz liste jako akumulator a nastepnie zwraca liste je w formie listy
-- Przyklady uzycia 
-- getVectorList "|0>|0>|0>" [] = ["|0>","|0>","|0>"]
-- getVectorList "<1|<1|<1|" [] = ["<1|","<1|","<1|"]
-- getVectorList "|0><1||0>" [] = ["|0>","<1|","|0>"]
getVectorList :: String -> [String] ->[String]
getVectorList "" accumulator = accumulator
getVectorList (x:xs) accumulator = getVectorList vectors' accumulator'
    where 
        size = findChar xs 0 '>' '|'
        accumulator' = accumulator ++ [[x] ++ take size xs]
        vectors' = drop (size + 1) (x:xs)
--Funkcja bierzei ciag znakow w postaci kolejnych iloczynow zewnetrznych ket i bra opisujacych dana macierz a nastepnie konstruuje z nich liste
-- Przyklady uzycia
-- getMatrixList "|1><1||0><1|" [] = ["|1><1|","|0><1|""]
getMatrixList :: [String] -> String  -> [String]
getMatrixList  accumulator "" = accumulator
getMatrixList accumulator (x:xs) = getMatrixList accumulator' matrixes 
    where 
        size = findChar xs 0 '|' '|' 
        accumulator' = accumulator ++ [[x] ++ take size xs]
        matrixes = drop (size + 1) (x:xs)
--Kolejna funkcja ktora dziala ale brzydko


--Funkcja jako argument przyjmuje liste macierz w postaci  
separateVectors :: [[String]] -> [[[String]]]
separateVectors = undefined 

--Funkcja bierze ciag znaakow w postaci kolejnych amplitud prawdopodobienstwa i zwraca ich list
-- Przyklady uzycia
-- getAmplitudeList "alpha_1" [] = ["alpha_1"]
-- getAmplitudeList "alpha_1_alpha_2" [] = ["alpha_1","alpha_2"]
-- getAmplitudeList "alpha_1_alpha_2_alpha_3" [] = ["alpha_1","alpha_2","alpha_3"]
-- getAmplitudeList "alpha_1_alpha_2_alpha_3_alpha_4" [] = ["alpha_1","alpha_2","alpha_3","alpha_4"]

getAmplitudeList :: String -> [String] -> [String]
getAmplitudeList "" accumulator = accumulator
getAmplitudeList amplitudes accumulator = getAmplitudeList amplitudes' accumulator'
    where 
        size = 1 + findChar amplitudes 0 '_' '_'
        accumulator' = accumulator ++ [take size amplitudes]
        amplitudes' = drop (size + 1) amplitudes 
--Funkcja bierze liste kolejnych bra i ketow a nastepnie zwraca liste policzonych bra i ketow (liczenie odbywa sie od lewej)
-- Przyklady uzycia 
-- computeVectorList ["|0>","|0>","|0>"] [] = ["|000>"]
-- computeVectorList ["|0>","<1|","|1>"] [] = ["|0>","1"]
-- computeVectorList ["|0>","<1|","|0>"] [] = ["|0>","0"]
-- computeVectorList ["<0|","<0|","|00>"] [] = ["1"]
computeVectorList :: [String] -> [String] -> [String]
computeVectorList [] computed = computed 
computeVectorList [x] computed = computed ++ [x]
computeVectorList (x:y:xs) computed = 
    case getOperationType x y of
        Ket_Bra -> computeVectorList (y:xs) (computed ++ [x])
        Bra_Ket -> computeVectorList xs (computed ++ [innerProduct x y] )
        Ket_Ket -> computeVectorList ([kroneckerKetProduct x y] ++ xs) computed 
        Bra_Bra -> computeVectorList ([kroneckerBraProduct x y] ++ xs) computed 

--Funkcja bierze ciag znakow w postaci amplitudy ++ " " ++ stany a nastepnie wykonuje obliczenia na okreslonych stanach i zwraca amplitudy wraz z policzonymi stanami
-- funkcja zaklada ze pomeidzy poszczegolnymi stanami znajduje sie iloczyn kroneckera 
-- sluzy do wyliczania stanu cyklu "po" przejsciu kubitow przez okreslona bramke kwantowa nie do budowy bramki
-- Przyklady uzycia
-- calculateState "alpha_1_alpha2_alpha_3 |0>|0>|0>"  = "alpha_1_alpha_2_alpha_3 |000>"
-- calculateState "alpha_1alpha_2 |0><1||1>" = "alpha_1_alpha_2 |0>1"
-- calculateState "alpha1_alpha_2 |0><1||0>" = "alpha_1_alpha_2 |0>0"
calculateState :: String -> String 
calculateState amplitudeWithState = amplitude ++ " " ++ calculatedState
    where
        amplitude = getAmplitudes amplitudeWithState
        state = getState amplitudeWithState
        calculatedState = concat $ computeVectorList (getVectorList state []) []
--Funkcja bierze liste kubitow a nastepnie zwraca liste iloczynow amplitud prawdopodobienstwa dla danego stanu 
-- Odpowiada to zainicjowaniu cyklu kwantowego (wszystkie kubity zawarte w cyklu sa splatane)
-- Przykład uzycia:
-- x = qubitList [1,2,3]
-- entangledQubitState x = 
--      ["alpha_1_alpha_2_alpha_3 |000>","alpha_1_alpha_2_beta_3 |001>","alpha_1_beta_2_alpha_3 |010>","alpha_1_beta_2_beta_3 |011>",
--      "beta_1_alpha_2_alpha_3 |100>","beta_1_alpha_2_beta_3 |101>","beta_1_beta_2_alpha_3 |110>","beta_1_beta_2_beta_3 |111>"]
entangledQubitState :: [[String]] -> [String]
entangledQubitState [] = []
entangledQubitState listOfQubits  = map calculateState (foldl kronecker firstQubit restQubits)
    where 
        firstQubit = head listOfQubits
        restQubits = tail listOfQubits
--Funkcja oczyszcza liste amplitud prawdopodobienstwa i stanow z tych elementow w ktorych amplitudy prawdopodobienstwa osiagaja 0
-- dodatkow usuwa takze niepotrzebne 1
--Przykladowe wywolania:
-- clearState ["_alpha_1 |0>0","_beta_1 |0>1","_alpha_1 |1>1","_beta_1 |1>0"] [] = ["_beta_1 |0>","_alpha_1 |1>"]
clearState :: [String] -> [String] -> [String]
clearState [] cleared = cleared
clearState  (x:xs) cleared =
    if  take 1 (reverse x) == "0" then clearState xs cleared
    else clearState xs ( [reverse (drop 1 (reverse x))] ++ cleared)