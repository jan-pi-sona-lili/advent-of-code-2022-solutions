open List

let moves = "move 2 from 4 to 6 ..."
|> String.split_on_char '\n' 
|> map (fun x -> Scanf.sscanf x "%s %d %s %d %s %d" (fun _ a _ b _ c -> a,b,c))

let data = "    [C]         [Q]         [V]    ..."
|> String.split_on_char '\n'
|> map (fun x -> of_seq (String.to_seq x))
|> map (filteri (fun i _ -> (i mod 4) = 1))
|> (let rec transpose = function [] -> [] | []::_ -> [] | rows  -> (map hd rows) :: transpose (map tl rows) in transpose)
|> map (filter ((<>) ' '))

let split_at l n =
        let rec aux part1 part2 = function
        | 0 -> (rev part1, part2)
        | n -> aux ((hd part2) :: part1) (tl part2) (n-1)
        in
        aux [] l n

let replace i x l =
        let before, (_::after) = split_at l i in before @ (x::after)

let solution f = 
        fold_left (fun l (a,b,c) -> 
                let move,rest = split_at (nth l (b-1)) a in
                let original = nth l (c-1) in
                replace (c-1) ((f move) @ original) l
                |> replace (b-1) rest) data moves
        |> map hd
        |> iter print_char
        |> print_newline

let solution1 = solution rev
let solution2 = solution Fun.id