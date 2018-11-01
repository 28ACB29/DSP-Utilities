namespace DSP

open System
open System.Collections.Generic

module Statistics =

    let private divideBy x y =
        y / x

    let standardDeviation (signal:double[]) =
        let mean =
            signal
            |> Array.average
        let squaredDifference a b = pown (a - b) 2
        signal
        |> Array.fold (fun accumulator element -> accumulator + squaredDifference mean element) 0.0
        |> divideBy ((double)(signal.Length - 1))
        |> Math.Sqrt

    let rootMeanSquare (signal:double[]) =
        signal
        |> Array.fold (fun accumulator element -> accumulator + (pown element 2)) 0.0
        |> divideBy ((double)signal.Length)
        |> Math.Sqrt

    let generateHistogram (signal:'a[]):Map<'a, int> =
        signal
        |> Array.countBy id
        |> Map.ofArray
