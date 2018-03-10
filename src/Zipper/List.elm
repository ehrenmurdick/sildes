module Zipper.List exposing (..)


type Zipper a
    = Zipper a (List a) (List a)


toList : Zipper a -> List a
toList z =
    let
        (Zipper x xs ps) =
            z
    in
        case ps of
            [] ->
                x :: xs

            _ ->
                toList (prev z)


zipper : a -> List a -> Zipper a
zipper x xs =
    Zipper x xs []


map : (Bool -> a -> b) -> Zipper a -> Zipper b
map f z =
    let
        (Zipper x xs ps) =
            z
    in
        (Zipper (f True x) (List.map (f False) xs) (List.map (f False) ps))


next : Zipper a -> Zipper a
next z =
    let
        (Zipper x xs ps) =
            z
    in
        case xs of
            [] ->
                z

            y :: ys ->
                (Zipper y ys (x :: ps))


flip : Zipper a -> Zipper a
flip z =
    let
        (Zipper x xs ps) =
            z
    in
        (Zipper x ps xs)


prev : Zipper a -> Zipper a
prev z =
    z
        |> flip
        |> next
        |> flip


update : (a -> a) -> Zipper a -> Zipper a
update f z =
    let
        (Zipper x xs ns) =
            z
    in
        (Zipper (f x) xs ns)


current : Zipper a -> a
current z =
    let
        (Zipper x _ _) =
            z
    in
        x
