#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: MathNet.Numerics.MKL.Win-x64"
#r "nuget: Plotly.NET, 5.0.0"


open System
open System.Diagnostics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open MathNet.Numerics.Providers.LinearAlgebra
open Plotly.NET


//LinearAlgebraControl.UseNativeMKL()
let isPositiveDefinite (matrix: Matrix<float>) =
    let evd = matrix.Evd()
    evd.EigenValues
    |> Seq.forall (fun c -> c.Real > 0.0)

let normalEquationsCholeskySafe (A: Matrix<float>) (b: Vector<float>) =
    let At = A.Transpose()
    let AtA = At * A
    let Atb = At * b

    if isPositiveDefinite AtA then        
        let cholesky = AtA.Cholesky()
        cholesky.Solve Atb
    else
        printfn "Matrix is NOT positive definite."
        AtA.Solve Atb


let householderQR (A: Matrix<float>) =
    let m = A.RowCount
    let n = A.ColumnCount
    let tau = DenseVector.zero n

    let mutable A = A.Clone()

    for j in 0 .. n - 1 do        
        let x = A.SubMatrix(j, m - j, j, 1).Column(0)
        let normx = x.L2Norm()
        let s = if x.[0] >= 0.0 then -1.0 else 1.0
        let u1 = x.[0] - s * normx

        let w = x / u1
        w.[0] <- 1.0
        
        for i in 1 .. w.Count - 1 do
            A.[j + i, j] <- w.[i]
        
        A.[j, j] <- s * normx       
        tau.[j] <- -s * u1 / normx

        //reflection to the matrix remaing bits.
        let subA = A.SubMatrix(j, m - j, j + 1, n - j - 1)

        let wT_A = w.ToRowMatrix() * subA
        let update = tau.[j] * w.ToColumnMatrix() * wT_A
        A.SetSubMatrix(j, m - j, j + 1, n - j - 1, subA - update)

    A, tau


//we apply the Q^T to vector b
let applyQTransposeToB (A: Matrix<float>) (tau: Vector<float>) (b: Vector<float>) : Vector<float> =
    let m = A.RowCount
    let n = tau.Count
    let bCopy = b.Clone()

    for j in 0 .. n - 1 do
        let w = DenseVector.init (m - j) (fun i ->
            if i = 0 then 1.0 else A.[j + i, j]
        )
        let dot = w.DotProduct(bCopy.SubVector(j, m - j))
        let scale = tau.[j] * dot
        for i in 0 .. m - j - 1 do
            bCopy.[j + i] <- bCopy.[j + i] - scale * w.[i]

    bCopy

//Back substitution to solve R x = Qt b
let backSubstitute (R: Matrix<float>) (b: Vector<float>) : Vector<float> =
    let n = R.ColumnCount
    let x = DenseVector.zero n
    for i in [n - 1 .. -1 .. 0] do
        let sum = 
            [i + 1 .. n - 1]
            |> List.sumBy (fun j -> R.[i, j] * x.[j])
        x.[i] <- (b.[i] - sum) / R.[i, i]
    x


let solveLeastSquares (A: Matrix<float>) (y: Vector<float>) : Vector<float> =
    
    let extractR (A: Matrix<float>) (n: int) : Matrix<float> =
        let R = DenseMatrix.zero n n
        for i in 0 .. n - 1 do
            for j in i .. n - 1 do
                R.[i, j] <- A.[i, j]
        R

    let m = A.RowCount
    let n = A.ColumnCount
    let QR, tau = householderQR A
    let Qt_b = applyQTransposeToB QR tau y
    let R = extractR QR n
    backSubstitute R (Qt_b.SubVector(0, n))


let generateTestData (m: int) (n: int) = 
    let generateRandomMatrix (m: int) (n: int) : Matrix<float> =
        let rnd = Normal.WithMeanStdDev(0.0, 1.0) 
        DenseMatrix.init m n (fun _ _ -> rnd.Sample())

    let generateRandomVector (length: int) =
        let rnd = System.Random()
        Vector.Build.Dense(length, fun _ -> rnd.NextDouble() * 100.0)

    let A = generateRandomMatrix m n 
    let b = generateRandomVector m
    A,b

let A,b = generateTestData 1000 10


// Solve least squares using SVD
let solveLeastSquaresSVD (A: Matrix<float>) (b: Vector<float>) : Vector<float> =
    let svd = A.Svd true
    let s = svd.S.ToArray()
    let tolerance = 1e-10 * float (max A.RowCount A.ColumnCount) * s.[0]    
    let rank = s |> Array.filter (fun x -> x > tolerance) |> Array.length

    let sigmaInv = DenseMatrix.init rank rank (fun i j ->
        if i = j && s.[i] > tolerance then 1.0 / s.[i] else 0.0)

    let U = svd.U.SubMatrix(0, A.RowCount, 0, rank)
    let Vt = svd.VT.SubMatrix(0, rank, 0, A.ColumnCount)

    let pseudoInverse = Vt.Transpose() * sigmaInv * U.Transpose()
    pseudoInverse * b
  

solveLeastSquaresSVD A b

let benchmark name n (f: unit -> Vector<float>) =
    let sw = Stopwatch()
    let times = ResizeArray<float>()
    let mutable last = DenseVector.zero 1

    for _ in 1 .. n do
        sw.Restart()
        last <- f()
        sw.Stop()
        times.Add(float sw.Elapsed.TotalMilliseconds)

    let avgTime = Seq.average times
    printfn "%s ran %d times. Avg time: %.3f ms" name n avgTime
    name, times |> List.ofSeq


let results =
    [ benchmark "Custom QR" 100 (fun () -> solveLeastSquares A b)
      benchmark "Math.NET QR" 100 (fun () -> A.QR().Solve b)
      benchmark "Normal Eq (Cholesky)" 100 (fun () -> normalEquationsCholeskySafe A b)
      benchmark "SVD Solver" 50 (fun () -> solveLeastSquaresSVD A b)
       ]


let chart =
    results
    |> List.map (fun (name, times) ->
        let indexedTimes = times |> List.mapi (fun i t -> i, t) 
        Chart.Line(indexedTimes, Name = name)
    )
    |> Chart.combine
    |> Chart.withTitle "Solver Benchmark Times (ms)"
    |> Chart.withXAxisStyle (TitleText = "Run Index")
    |> Chart.withYAxisStyle (TitleText = "Time per Run (ms)")

chart |> Chart.show

