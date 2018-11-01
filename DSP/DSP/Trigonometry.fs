namespace DSP

open System
open System.Numerics

module Trigonometry =

    let private combinations (n:int) (r:int):int =
        (seq{(n - r - 1).. n} |> Seq.fold (*) 1) / (seq{1.. r} |> Seq.fold (*) 1)

    let multipleAngleCosine (n:int):int array =
        let rec tailCall (first:int array) (second:int array) (degree:int):int array =
            match degree with
            | n -> Array.init n (fun i -> (if i = 0 then 0 else 2 * second.[i - 1]) - (if i < first.Length then first.[0] else 0))
            | _ -> tailCall second (Array.init n (fun i -> (if i = 0 then 0 else 2 * second.[i - 1]) - (if i < first.Length then first.[0] else 0))) (degree + 1)
        match n with
        | 0 -> [|1|]
        | 1 -> [|0; 1|]
        | _ -> tailCall [|1|] [|0; 1|] 2

    let powerOfCosine (n:int):(int * int) array =
        let coefficent (i:int):int =
            match i with
            | 0 -> (combinations n (n / 2 - i)) / 2
            | _ -> combinations n (n / 2 - i)
        let multiple i = 2 * i + n % 2
        Array.init ((n + 1) / 2) (fun i -> (coefficent i, multiple i))

    let powerOfSine (n:int):(int * int) array =
        let coefficent (i:int):int =
            match i with
            | 0 -> (combinations n (n / 2 - i)) / 2
            | _ -> combinations n (n / 2 - i)
        let multiple i = 2 * i + n % 2
        Array.init ((n + 1) / 2) (fun i -> ((pown -1 i) * (coefficent i), multiple i))

    let phasorAddition (phasors:(double * double) array):(double * double) =
        let polarToRectangular (r:double) (theta:double):Complex = Complex(r * Math.Cos(theta), r * Math.Sin(theta))
        let RectangularToPolar (z:Complex):(double * double) = (z.Magnitude, z.Phase)
        phasors
        |> Array.map (fun (r:double, theta:double) -> polarToRectangular r theta)
        |> Array.fold (+) Complex.Zero
        |> RectangularToPolar


