module type TRY = sig
  type 'a t = Success of 'a
              | Failure of exn

  val return: 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val recover: 'a t -> (exn -> 'a t) -> 'a t
  val filter: 'a t -> ('a -> bool) -> 'a t
  val flatten : 'a t t -> 'a t
end

module Try : TRY = struct
  type 'a t = Success of 'a
  | Failure of exn
  let return x = Success x
  let bind x f : 'b t =
    match x with
    | Success n -> f n
    | Failure e -> Failure e
  let recover x ferr =
    match x with
    | Success _ -> x
    | Failure e -> ferr e

  let filter x f: 'a t =
    match x with
    | Success n when (f n) = false ->  Failure (Invalid_argument "Monad does not satisfy predicate")
    | Success _ -> x
    | Failure e -> Failure e

  let flatten (x: 'a t t) =
    match x with
    | Success n -> n
    | Failure e -> Failure e

end
