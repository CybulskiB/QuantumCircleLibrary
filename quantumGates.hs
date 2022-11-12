--Autor Bartosz Cybulski 
--Biblioteka zawiera zestaw funkcji  pomagajacych w projektowaniu własnego cyklu kwantowego 
--Znajdują się tu funkcje automatyzujące proces wyliczania kolejnych stanów cyklu np po zaaplikowaniu bramki X na 2 kubicie w 3 kubitowym cyklu

--W tym module znajduja sie funkcje opisujace zmiane stanu cyklu kwantowego po zaaplikowaniu wybranej bramki na danym kubicie/kubitach 
--w obecnej wersji modul nie wyczerpuje wszystkich bramek kwantowych a jedynie te niezbedne do opisania logiki klasycznej

module QuantumGates where 

import Basic 

i = [" " ++ ket "1" ++ bra "1"] ++ [ket "0" ++ bra "0"]

x = [" " ++ ket "0" ++ bra "1"] ++ [ket "1" ++ bra "0"]

