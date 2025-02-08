let rec fact n=
  if n=0 then 1
  else n*(fact(n-1));;

let rec fact_acc n acc=
  if n=0 then acc
  else fact_acc(n-1)(n*acc);;
  
let rec append l1 l2=
  match l1 with 
    []->l2 |
    x::ll->x::append(ll)(l2);;

let rec rev l1 =
  match l1 with 
    [] -> l1 |
    x::ll -> append (rev ll) [x];;
  
let rec rev_acc l1 lacc=
  match l1 with 
    []->lacc |
    x::ll ->rev_acc ll (x::lacc)
              
              
