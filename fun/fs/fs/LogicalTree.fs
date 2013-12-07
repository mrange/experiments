namespace FolderSize

open SharpDX

type LayoutRotation = 
    | D0
    | D90
    | D180
    | D270

type LayoutTransform = 
    {
        Rotation : LayoutRotation
        Scaling  : float32
    }

type LogicalTree = 
    | Empty
    | Leaf              of AnimatedRectangleF*State:obj*Visual:VisualTree
    | RenderTransform   of RenderTransform:Matrix3x2*InvertedRenderTransform:Matrix3x2*Child:LogicalTree
//    | LayoutTransform   of LayoutTransform:LayoutTransform*Child:LogicalTree
    | Fork              of Left:LogicalTree*Right:LogicalTree
    | Group             of Children:LogicalTree list

module Logical = 
    
    let rec GetBounds time lt = 
        match lt with
        | Empty                         -> RectangleF.Empty
        | Leaf              (b,_,_)     -> b time
        | RenderTransform   (_,_,c)     -> GetBounds time c
        | Fork              (l,r)       -> (GetBounds time l) <+> (GetBounds time r)
        | Group             (cs)        -> cs |> List.fold (fun s c -> s <+> (GetBounds time c)) RectangleF.Empty

