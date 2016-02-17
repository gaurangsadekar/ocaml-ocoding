let subtractLists l1 l2 =
  let rec subtract l1 l2 res carry =
    (match l1, l2 with
    | x::xs, y::ys -> let borr_x = (x - carry) in
      if (borr_x >= y)
          then subtract xs ys ((borr_x - y)::res) 0
      else subtract xs ys ((10 + borr_x - y)::res) 1
    | x::xs, [] ->
      if carry = 1 && x = 0
        then subtract xs [] ((10 + x - carry)::res) 1
      else (x - carry)::res
    | [], [] -> res
    | _,_ -> res) in (* this case will never be reached because the problem 
                        assumes that list 1 is bigger than list 2 *)
    let rl1 = List.rev l1 and rl2 = List.rev l2 in
        subtract rl1 rl2 [] 0;;

assert(subtractLists [2;5;3] [5;7] = [1; 9; 6]) ;;
assert(subtractLists [1;0;0] [5;8] = [0;4; 2]) ;;
assert(subtractLists [9;0] [8;7] = [0;3]) ;;
assert(subtractLists [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0] = 
                        [0;5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0]) ;;
