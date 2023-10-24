namespace Week8

open System
open System.Drawing

module Starter =
    type Fexpr =
        | Const of float
        | X
        | Add of Fexpr * Fexpr
        | Sub of Fexpr * Fexpr
        | Mul of Fexpr * Fexpr
        | Div of Fexpr * Fexpr
        | Sin of Fexpr
        | Cos of Fexpr
        | Log of Fexpr
        | Exp of Fexpr

    // 6.1
    let rec red = function
        // Some reductions 0.0+fe2 is reduced to fe2
        | Add(Const x, fe2) when x = 0.0 -> fe2
        | Add(fe1, Const x) when x = 0.0 -> fe1
        | Sub(fe1, Const x) when x = 0.0 -> fe1
        // Some reductions 1.0*fe2 is reduced to fe2
        | Mul(Const x, fe2) when x = 1.0 -> fe2
        | Div(Const x, fe2) when x = 1.0 -> fe2
        | Cos(Const x) when x > Math.PI -> Cos(Const (x-Math.PI)) 
        | Sin(Const x) when x > Math.PI -> Sin(Const (x-Math.PI))
        
        // Recursive down
        | Add(f1, f2) -> Add(red f1, red f2)
        | Sub(f1, f2) -> Sub(red f1, red f2)
        | Mul(f1, f2) -> Mul(red f1, red f2)
        | Div(f1, f2) -> Div(red f1, red f2)
        | Cos(f1) -> Cos(red f1)
        | Sin(f1) -> Sin(red f1)
        | Log(f1) -> Log(red f1)
        | Exp(f1) -> Exp(red f1)
        | a -> a

    // 6.2
    let rec toPostfixString = function
        | Const x -> string x
        | X -> "x"
        // e.g. transform fe1 + fe2 -> fe1 fe2 +
        | Add(fe1, fe2) -> "(" + toPostfixString fe1 + ") (" + toPostfixString fe2 + ") +"
        | Sub(fe1, fe2) -> "(" + toPostfixString fe1 + ") (" + toPostfixString fe2 + ") -"
        | Mul(fe1, fe2) -> "(" + toPostfixString fe1 + ") (" + toPostfixString fe2 + ") *"
        | Div(fe1, fe2) -> "(" + toPostfixString fe1 + ") (" + toPostfixString fe2 + ") /"
        | Sin(fe) -> "sin(" + toPostfixString fe + ")"
        | Cos(fe) -> "cos(" + toPostfixString fe + ")"
        | Log(fe) -> "log(" + toPostfixString fe + ")"
        | Exp(fe) -> "exp(" + toPostfixString fe + ")"
        


    // 6.4
    type BinTree<'a, 'b> =
        | Leaf of 'a
        | Node of BinTree<'a, 'b> * 'b * BinTree<'a, 'b>

    //1 
    let rec leafVals = function
        | Leaf x     -> Set [x]
        | Node (a, _, b) -> Set.union (leafVals a) (leafVals b)
        
    // 2
    let rec nodeVals = function
        | Node (a, x, b) -> Set.unionMany [nodeVals a; nodeVals b; Set [x]]
        | _ -> Set []

    let rec vals = function
        | Leaf x -> (Set[x], Set [])
        | Node (a, x, b) ->
            let (leafs, nodes) = vals a
            let (leafs', nodes') = vals b
            (Set.union leafs leafs', Set.unionMany [nodes; nodes'; Set [x]])
            
    // 6.5
    type AncTree = | Unspec
                   | Info of AncTree * String * AncTree

    let ancestorTree = Info(Info(Info(Unspec, "Jens", Unspec), "Ole", Info(Unspec, "Magrethe", Unspec)),
                            "Henrik",
                            Info(Info(Unspec, "MorFar", Unspec), "Hanna", Info(Unspec, "Mormor", Unspec))
                           )
    let rec maleAnc = function
        | Unspec                         -> Set []
        | Info(Info(a, father, b), _, f) ->
            let maleTree = maleAnc (Info(a, father, b))
            let femaleTree = maleAnc f 
            Set.unionMany [Set [father]; maleTree; femaleTree;]
        | Info(a, name, b) -> Set.union (maleAnc a) (maleAnc b)
        
    // 6.9
    // 1)
//    type Department =
//        | Department of string * Department list
    // 2)
    type Department' =
        | Department of string * float * Department' list
    // 3)
    let rec nameIncome = function
        | Department (name, income, dl) ->
            (name, income) :: (List.collect nameIncome dl)
    
    // 4)
    let rec income = function
        | Department (_, gross, dl) ->
            gross + (List.fold (fun grossIncome department -> (income department)+grossIncome) 0. dl)
            
    // 5)
    let rec nameTotalIncome = function
        | Department (name, i, dl) ->
            (name, income (Department (name, i, dl))) :: (List.collect nameTotalIncome dl)

    // 6)
    let format d =
        let rec recFormat indent = function
            | Department (name, i, dl) ->
                let thisD = sprintf "%s%s: %f\n" indent name i
                (List.fold (fun acc d -> acc + (recFormat ("  " + indent) d)) thisD dl)  
        recFormat "\n" d