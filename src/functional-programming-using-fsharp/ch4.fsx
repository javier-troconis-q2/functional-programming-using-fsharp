type Label = string

type Code = string

type Price = int

type Inventory = (Code * (Label * Price)) list

type Quantity = int

type PurchaseItem = Code * Quantity

type Purchase = PurchaseItem list

type BillItem = Label * Quantity * Price

type Bill = BillItem list * Price

type MakeBill = Inventory -> Purchase -> Bill

let inventory = 
    [ ("a", ("product a", 1))
      ("b", ("product b", 2))
      ("c", ("product c", 3)) ]

let purchase = 
    [ ("a", 4)
      ("c", 1)
      ("b", 2) ]

let rec find_product code = 
    function 
    | (x, y) :: _ when x = code -> y
    | _ :: t -> find_product code t
    | _ -> failwith "failed to find product"

let rec make_bill inventory = 
    function 
    | [] -> ([], 0)
    | (code, quantity) :: t -> 
        let (c, d) = make_bill inventory t
        let (description, price) = find_product code inventory
        (description, quantity, price * quantity) :: c, quantity * price + d
