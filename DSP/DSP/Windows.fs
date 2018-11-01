namespace DSP

open System

module Windows =

    let private square (x:'a):'a = pown x 2

    let private lastIndex (length:int) = (double)length - 1.0

    let private midpoint (length:int) = ((double)length - 1.0) / 2.0

    let private normalizedDeviation (center:double) (scale:double) (x:double):double = (x - center) / scale

    let private sinc (x:double):double = (Math.Sin (Math.PI * x)) / (Math.PI * x)

    let private logistic (x:double):double = 0.5 + 0.5 * Math.Tanh(0.5 * x)

    let gaussian (length:int) (sigma:double):double array =
        let distribution (x:int):double =
            x
            |> double
            |> normalizedDeviation (midpoint length) (sigma * midpoint length)
            |> square
            |> (*) -0.5
            |> Math.Exp
        Array.init length (fun i -> distribution i)

    let hamming (length:int) (alpha:double) (beta:double):double array =
        Array.init length (fun i -> alpha - beta * Math.Cos((2.0 * Math.PI * (double)i) / (lastIndex length)))

    let hann (length:int) =
        let definition (x:int):double =
            x
            |> double
            |> (*) (Math.PI / (lastIndex length))
            |> Math.Sin
            |> square
            |> (-) 1.0
        Array.init length (fun i -> definition i)

    let lanzcos (length:int):double array =
        let norm (x:int):double = 2.0 * (double)x / (lastIndex length) - 1.0
        let definition (x:int):double =
            x
            |> norm
            |> sinc
        Array.init length (fun i -> definition i)

    let planck (length:int) (epsilon:double):double array =
        let expression (x:int):double = (double)x / (midpoint length) - 1.0
        let plus (x:double):double = 1.0 + x
        let minus (x:double):double = 1.0 - x
        let z (x:double):double = 2.0 * epsilon * (1.0 / x + 1.0 / (2.0 * epsilon + x))
        let definition (x:int):double =
            let left = (int)(epsilon * (lastIndex length))
            let right = (int)((1.0 - epsilon) * (lastIndex length))
            match x with
            | x when x > 0 && x < left ->
                x
                |> expression
                |> plus
                |> z
                |> (*) -1.0
                |> logistic
            | x when x > left && x < right -> 1.0
            | x when x > right && x < (int)(lastIndex length) ->
                x
                |> expression
                |> minus
                |> z
                |> (*) -1.0
                |> logistic
            | _ -> 0.0
        Array.init length (fun i -> definition i)

    let poisson (length:int) (tau:double):double array =
        let distribution (x:int):double =
            x
            |> double
            |> normalizedDeviation (midpoint length) tau
            |> Math.Abs
            |> (*) -1.0
            |> Math.Exp
        Array.init length (fun i -> distribution i)

    let powerOfCosine (length:int) (alpha:int):double array =
        let power (x:double):double = pown x alpha
        let definition (x:int):double =
            x
            |> double
            |> (*) (-Math.PI / (lastIndex length))
            |> Math.Sin
            |> power
        Array.init length (fun i -> definition i)

    let tukey (length:int) (alpha:double):double array =
        let scaledMidpoint:double =
            length
            |> midpoint
            |> (*) alpha
        let definition (x:int):double =
            match x with
            | x when x > 0 && x < (int)scaledMidpoint ->
                (double)x / scaledMidpoint - 1.0
                |> (*) (0.5 * Math.PI)
                |> Math.Cos
                |> square
            | x when x > (int)scaledMidpoint && x < length - 1 - (int)scaledMidpoint -> 1.0
            | x when x > length - 1 - (int)scaledMidpoint && x < length ->
                ((double)x - (lastIndex length)) / scaledMidpoint + 1.0
                |> (*) (0.5 * Math.PI)
                |> Math.Cos
                |> square
            | _ -> 0.0
        Array.init length (fun i -> definition i)

    let welch (length:int):double array =
        let distribution (x:int):double =
            x
            |> double
            |> normalizedDeviation (midpoint length) (midpoint length)
            |> square
            |> (-) 1.0
        Array.init length (fun i -> distribution i)

