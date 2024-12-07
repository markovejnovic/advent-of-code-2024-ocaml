open Printf

let read_data (filename: string): (int * int) list =
    let parse_line (line: string): (int * int) =
        let xs = 
            (String.split_on_char ' ' line)
            |> List.filter (fun x -> x <> "")
            |> List.map int_of_string
        in (List.nth xs 0, List.nth xs 1)
    in

    let in_chann = open_in filename in
    let rec read_lines (lines: string list) =
        match input_line in_chann with
        | (line: string) -> List.rev (line :: read_lines lines)
        | exception End_of_file -> lines
    in read_lines []
    |> List.map parse_line

let transpose (data: (int * int) list): (int list * int list) =
    let first_col = List.map fst data in
    let second_col = List.map snd data in
    (first_col, second_col)

let solve (data: (int list * int list)): int =
    let ordered xs = List.sort compare xs in
    let zip = List.combine (ordered (fst data)) (ordered (snd data)) in
    let distances = List.map (fun (x, y) -> abs (x - y)) zip in
    List.fold_left (+) 0 distances


let () =
    read_data "input"
    |> transpose
    |> solve
    |> printf "%d\n"
