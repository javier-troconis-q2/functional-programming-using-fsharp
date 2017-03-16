//2.1
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


type 'c tree = 
    | Empty
    | Node of 'c tree * 'c * 'c tree

let rec create_list t =
    match t with
    | Empty -> [] 
    | Node (a,b,c) -> 
        create_list a @ [b] @ create_list c

