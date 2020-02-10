type 'a gnezdenje =
| Element of 'a
| Podseznam of 'a gnezdenje list

let primer = (Element 1; Element 2; Podseznam [Element 3; Podseznam [Element 4]; Podseznam []]; Podseznam [Element 5])

let simpl = (Podseznam[Element 2])

let rec glob x = 
    match x with
    |Element a -> 1
    |Podseznam b -> 1 + glob b

(* Začnem z max = 1 in vred = 1, potem pa za vsak '[' dodam 1 in vsak ']' odstranim 1 od vred, sproti če vrednost večja od max je to novi max*)

let preslikaj = failwith "todo"

let rec splosci = failwith "todo"
    
(* Odstranim vse notranje '[' in ']', nato počistim prazne prostore *)

