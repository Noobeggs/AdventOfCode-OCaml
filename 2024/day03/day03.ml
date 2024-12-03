let parse input =
  let open Angstrom in
  let integer = take_while1 Util.P.is_digit >>| int_of_string in
  let multiply =
    lift2 (fun a b -> a * b) (string "mul(" *> integer) (char ',' *> integer <* char ')')
  in
  let p = many (multiply <|> lift (fun _ -> 0) any_char) in
  parse_string ~consume:All p input |> Result.get_ok
;;

let () =
  let input = Util.get_input "2024/day03/input" in
  let products = parse input in
  List.fold_left (fun sum product -> sum + product) 0 products
  |> print_int
  |> print_newline
;;

let parse2 input =
  let open Angstrom in
  let do_instr = string "do()" in
  let dont_instr = string "don't()" in
  let skip =
    dont_instr <* (many_till any_char do_instr <|> many_till any_char end_of_input)
  in
  let integer = take_while1 Util.P.is_digit >>| int_of_string in
  let multiply =
    lift2 (fun a b -> a * b) (string "mul(" *> integer) (char ',' *> integer <* char ')')
  in
  let p = many (lift (fun _ -> 0) skip <|> multiply <|> lift (fun _ -> 0) any_char) in
  parse_string ~consume:All p input |> Result.get_ok
;;

(* let example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\ndon't()mul(1,6)";; *)
(* let example = *)
(* "$  mul(402,190))&<why(211,617)how()/;mul(506,313)[^^<!$#when(636,198),]mul(744,268)#&!what()&!;mul(631,641)where()@select()!@?(:;how()mul(25,288)&~^select()mul(683,657)'mul(505,315)+~:-mul(671,741)]{mul(397,54))?mul(511,935)where()select()where()-mul(525,623)mul(206,770){/}don't()from()mul(260,967)-how() -/[^(mul(500,994)!:mul(391,833)#)>who(),where(376,378)why();$mul(394,346)%:]^from()>:mul(130,944)who()>where()select()}:mul(952,439)?/how()from()^$why()do()@*{mul(557,916)&(who()mul(445,933):(,;>?mul(108,268)don't()select(574,441)-mul(132,870)from(){mul(441,660)why(),from();mul(919,820):#/%)why()*mul(194,790);#/^#select()?mul(981,514)$what(796,873)when() %from()mul(164,772):&:select()@mul(396,80)select()what()%),) mul(213,928){?who();@@mul(320,229)where(348,623)*when()when(180,565)mul(214,912)!mul(848,738)from()+):^how()-&[mul(378,433)select():%~mul(696,421)mul(364,341)?^don't()*&mul(89,441)how()select()what(),select()mul(423,822),?&(how()']what())$mul(21,844)&,select()/>^)'mul(469,230))when()where()*,#mul(569,594)why()what():{ select(200,250){())mul(661,628)#)why()from(325,19)who() mul(758,985){%&where()!how(213,616)*mul(394,768)mul(831,12)*where()>who(986,151)mul(962,504)$:<!)where()%$&when()mul(900,577)<>^[>;mul(948,363)$@;]mul(857,273) how() ,:!select(),[mul(31,766)} *mul(372,138)+,#!~(>;%mul(774,958)#}&+why()*when()mul(560,137)}mul(72,718)when()^{]':{how()/mul(985,120)@*[do()$how()mul(891,69)select()mul(546,834)@$mul(767,879)$$!/&why()?mul(876,495){^/&)who()<~]mul(911,71){?#mul(116,462);:%<^'?}~why()mul(258,286)from()^#^?how()&who()mul(97,540)select()who()do(){^~~<why(955,532)@[ how()mul(682,75)~){?where()<mul(63,26)-where()^ <^mul(336,475)&when()mul(637,386)+*when()<^]mul(846,915)!mul(795,805)$what()[~#*@:/@mul(370,651)?$select()^how()}!mul(705,839)~*;%!^+why()do()!:)[#{mul(112,21)*mul(285,252)mul(451,396){&> )'mul(57,589)when()**mul(554,575)*+^mul(856,360)mul(530,200)@/&}*where()mul(588,2)who()$]mul(604,72)who()}from(524,212)mul(250,680)<;@;@how()?#mul(555,956)%(;:+<{(who()select()mul(377,822)+when()@[&{&from();*mul(457,538)mul(52,298)who()%when()%who()how()from(9,264)mul(452,941)where()+?,-don't()when()select()<mul(956,185)^}don't(){$/~how()][ #)mul(717,593,when(64,768):what()%(mul(922,407)#why(646,569)$(~:{#mul(3,438)+/#<*select())how()don't()!where();mul(853,725)%?}/!from()<?*mul!+>$''mul(229,512){{from()what()?!'^select()mul(185,787)<*mul(688,834)  @@what(),}select()>mul(825,27))~&!;mul(796,378)*+(-select(); mul!):{!;who()$how()why()don't()select()how(451,129)]#*^mul(852,354)[mul(596,889)mul(476,207):#what()-+mul(467,323) from()why()}(}^&:mul(955,498)]{;mul(797,177)mul(321,350):mul(784,667)mul(707,927)~%#why()why()what();mul(515,511)from(589,691)mul*&[mul(884,558)/mul(824,926))+from(331,316)where()!*!:mul(938,881)]mul(287,358what()@from(226,364)+:^)-when(361,935)select()mul(324,887)@where()select())(mul(430>&][mul(381,417)}*why()why()!*#~mul(729,14)>%what()'where(){}mul(483,820)when(206,921)&when()@how()mul(608,185)how(5,376)%mul(150,182)><when()@:@mul(953,86)mul(332,881)*mul(688,895)^<select()!%<+mul(5,990)>]/!( (+mul(78,588)what()@ why()when()*+from();;mul(837,417)who()mul(786,516)'how()%:~;%]who()<mul(576,960),@why()<<+'mul(160,572)*why()where()-when(353,689){[;mul(634,920)mul(482,207)( (when()<>:;-mul(917,130) *)
(* " *)

let () =
  let input = Util.get_input "2024/day03/input" in
  (* let input = example in *)
  let products = parse2 input in
  let () =
    print_int (List.length (List.filter (fun a -> a <> 0) products)) |> print_newline
  in
  List.fold_left
    (fun sum product -> (* print_int (sum + product) |> print_newline; *) sum + product)
    0
    products
  |> print_int
  |> print_newline
;;
