open Printf

let read_data (filename: string): (int list) list =
    let parse_line (line: string): (int list) =
        (String.split_on_char ' ' line)
        |> List.map int_of_string
    in
    let in_chann = open_in filename in
    let rec read_lines (lines: string list) =
        match input_line in_chann with
        | (line: string) -> List.rev (line :: read_lines lines)
        | exception End_of_file -> lines
    in read_lines []
    |> List.map parse_line

let adjacent_difference (xs: int list): int list =
    List.map2 (-) (List.tl xs) (xs |> List.rev |> List.tl |> List.rev)

let monotonic_from_diff (xs: int list): bool =
    let all_pos = List.map (fun x -> x >= 0) xs
        |> List.fold_left (&&) true in
    let all_neg = List.map (fun x -> x <= 0) xs
        |> List.fold_left (&&) true in
    all_pos || all_neg


let record_ok (levels: int list): bool =
    let level_ok x = (abs x >= 1) && (abs x <= 3) in

    let diffs = adjacent_difference levels in
    let all_levels_ok = List.map level_ok diffs
        |> List.fold_left (&&) true in
    all_levels_ok && monotonic_from_diff diffs


let solve (data: (int list) list) = data
    |> List.map record_ok
    |> List.map (fun x -> if x then 1 else 0)
    |> List.fold_left (+) 0

let () =
    read_data "input"
    |> solve
    |> printf "%d\n"
