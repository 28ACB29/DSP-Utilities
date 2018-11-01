namespace DSP

open System
open System.Numerics

module Transform =

    let private window (data:'a array) (i:int):'a seq =
        seq{for j in Math.Max (0, (data.Length - 1 - i))..Math.Min (i, data.Length - 1) do yield data.[j]}

    let convolution (signal:double array) (impulse:double array) =
        Array.init (signal.Length + impulse.Length - 1) (fun i -> Seq.fold2 (fun accumulator element1 element2 -> accumulator + element1 * element2) 0.0 (window signal i) ((window impulse i) |> Seq.rev))

    let correlation (signal:double array) (impulse:double array) =
        Array.init (signal.Length + impulse.Length - 1) (fun i -> Seq.fold2 (fun accumulator element1 element2 -> accumulator + element1 * element2) 0.0 (window signal i) (window impulse i))

    let dft (signal:double array) =
        let argument i j = 2.0 * Math.PI * (double)i * (double)j / (double)signal.Length
        let polarToRectangular (r:double) (theta:double) = new Complex(r * Math.Cos(theta), r * Math.Sin(theta))
        Array.init signal.Length (fun i -> Array.fold2 (fun accumulator element1 element2 -> accumulator + new Complex(element1, 0.0) * element2) Complex.Zero signal (Array.init signal.Length (fun j -> polarToRectangular (1.0 / Math.Sqrt((double)signal.Length)) (argument i j))))


