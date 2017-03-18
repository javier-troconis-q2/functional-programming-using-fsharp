﻿//2.1
let k n = n % 2 = 0 || n % 3 = 0
//2.2
let (+.) x y : System.String = x + y

let rec pow = 
    function 
    | (s, 1) -> s
    | (s, n) -> pow (s + s, n - 1)

//2.3
let isIthChar (str : System.String, i, ch) = str.[i] = ch

//2.4 ??
//2.5
let occInString (str, ch) = 
    str
    |> Seq.filter (fun x -> x = ch)
    |> Seq.length

//2.6
let notDivisibleBy (d, n) = n % d <> 0

let rec gcd = 
    function 
    | (x, 0) -> x
    | (x, y) -> gcd (y, x % y)

type tree<'c> = 
    | Empty
    | Node of 'c tree * 'c * 'c tree

type k<'a, 'b> = 
    | A of 'b
    | B of 'a

type list'<'t> = 
    | Head of 't
    | Tail of 't list'

let rec create_list t = 
    match t with
    | Empty -> []
    | Node(a, b, c) -> create_list a @ [ b ] @ create_list c

let make_list n = 
    let rec make_list' n a = 
        match n with
        | 0 -> a
        | _ -> make_list' (n - 1) (n :: a)
    make_list' n []

let reverse_tail l = 
    let rec reverse_tail' l a = 
        match l with
        | [] -> a
        | h :: t -> reverse_tail' t (h :: a)
    reverse_tail' l []

let reverse_tail1 l = 
    let rec reverse_tail1' = 
        function 
        | ([], a) -> a
        | (h :: t, a) -> reverse_tail1' (t, h :: a)
    reverse_tail1' (l, [])
