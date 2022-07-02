let double x = 2 * x
let square x = x * x

let quad x = double (double x)
let fourth x = square (square x)

let twice f x = f (f x)

let quad' x = twice double x
let fourth' x = twice square x

let apply f x = f x

let pipeline x f = f x
let (|>) = pipeline

let x = 5 |> double