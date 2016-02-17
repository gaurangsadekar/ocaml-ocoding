{ type token = EOF | Word of string }

rule token = parse
| eof { EOF }
| ['a'-'z' 'A'-'Z']+ as word { Word(word) }
| _ { token lexbuf }

{
  module StringMap = Map.Make(String);;
  let lexbuf = Lexing.from_channel stdin in
  let wordlist = 
      let rec next l = match token lexbuf with 
                       | EOF -> l
                       | Word(s) -> next (s::l)
      in next []
  in
    let rec getCounts cm = function
        | [] -> cm
        | x::xs -> try
            let count = StringMap.find x cm in
                getCounts (StringMap.add x (count + 1) cm) xs
            with Not_found -> getCounts (StringMap.add x 1 cm) xs 
    in
    let printCounts words = 
        let countmap = getCounts (StringMap.empty) words in
        let tups = StringMap.fold 
            (fun word count tuplist -> (count, word)::tuplist) countmap []
        in
        let wordcounts = 
           List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) 
                     tups in
        List.iter (fun (count, word) -> print_endline ((string_of_int count)^" "^word))
        wordcounts
    in printCounts wordlist;; 
}

