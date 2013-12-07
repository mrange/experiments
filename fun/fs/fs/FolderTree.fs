namespace FolderSize

open System.Collections.Generic
open System.Threading


open SharpDX

module FolderTree = 

    type MutableFolder = 
        {
            Folder                  : Scanner.Folder
            Parent                  : MutableFolder option

            mutable Children        : List<MutableFolder>
            mutable Depth           : int
            mutable TotalFileCount  : int64
            mutable TotalFileSize   : int64
        }

        member x.AddFolder (vf : MutableFolder) = 
            x.Children.Add vf
            x.UpdateRecursively vf

        member x.UpdateRecursively (vf : MutableFolder) = 
            x.Depth <- max x.Depth <| vf.Depth + 1
            x.TotalFileCount <- x.TotalFileCount + vf.TotalFileCount
            x.TotalFileSize <- x.TotalFileSize + vf.TotalFileSize

            let parent = x.Parent
            match parent with
            | Some p    -> p.UpdateRecursively vf
            | _         -> ()

        static member New sf p = 
            let vf = { Folder = sf; Parent = p; Children = List<MutableFolder>(); Depth = 1; TotalFileSize = sf.FileSize; TotalFileCount = sf.FileCount }
            match p with
            | Some pp   -> pp.AddFolder vf
            | _         -> ()
            vf

    type Folder = 
        {
            Folder                  : Scanner.Folder

            Children                : Folder list
            Depth                   : int
            TotalFileCount          : int64
            TotalFileSize           : int64
        }
        static member New f c d tfc tfs = {Folder = f; Children = c; Depth = d; TotalFileCount = tfc; TotalFileSize = tfs}

    let rec MakeFolder (mf : MutableFolder) : Folder = 
        let c = mf.Children |> Seq.map MakeFolder |> Seq.sortBy (fun f -> -f.TotalFileSize) |> Seq.toList

        Folder.New mf.Folder c mf.Depth mf.TotalFileCount mf.TotalFileSize

    let FoldFolder (map : Map<Scanner.Folder, MutableFolder>,root : MutableFolder option) (sf : Scanner.Folder) = 
        let pvf = 
            match sf.Parent with
            | Some p    -> map.TryFind p
            | _         -> None
        let vf = MutableFolder.New sf pvf
        let map' = map |> Map.add sf vf
        map', Some (root <??> vf)

    let EmptyFoldFolder : Map<Scanner.Folder, MutableFolder>*MutableFolder option = Map.empty,None

    let FormatFolder (f : Folder) = 
        let offset = 8L
        let sz, u =
            match f.TotalFileSize with
            | sz when sz < offset*1024L                     -> float sz                               , "B"
            | sz when sz < offset*1024L*1024L               -> float sz / 1024.                       , "KiB"
            | sz when sz < offset*1024L*1024L*1024L         -> float sz / (1024.*1024.)               , "MiB"
            | sz when sz < offset*1024L*1024L*1024L*1024L   -> float sz / (1024.*1024.*1024.)         , "GiB"
            | sz                                            -> float sz / (1024.*1024.*1024.*1024.)   , "TiB"
        let formatter = sprintf "%s %.2f %s"
        formatter f.Folder.Name sz u

    let rec CreateVisualTree 
        (stroke     : BrushDescriptor       ) 
        (fill       : BrushDescriptor       ) 
        (foreground : BrushDescriptor       ) 
        (textFormat : TextFormatDescriptor  ) 
        (xscale     : float32               ) 
        (yscale     : float32               ) 
        (ycutoff    : float32               ) 
        (x          : float32               ) 
        (y          : float32               ) 
        (f          : Folder                ) 
        : VisualTree
        =
        let xpos    = xscale * x
        let ypos    = yscale * y
        let width   = xscale
        let height  = yscale * (float32 f.TotalFileSize)

        if ycutoff > height then VisualTree.Empty
        else
            let children = 
                f.Children
                |> ListEx.foldMap   (fun (rsz : int64) cf -> 
                                        let vt = CreateVisualTree stroke fill foreground textFormat xscale yscale ycutoff (x + 1.F) (y + float32 rsz) cf
                                        (rsz + cf.TotalFileSize),vt
                                    ) 0L
                |> List.filter (fun c -> Visual.HasVisuals c)

            let astroke         = Animated.Brush_Solid stroke
            let afill           = Animated.Brush_Solid fill
            let aforeground     = Animated.Brush_Solid foreground
            let arect           = Animated.Constant <| SharpDX.RectangleF(xpos, ypos, width, height)
            let astrokeWidth    = Animated.Constant <| 1.0F

            let rect    = VisualTree.Rectangle (astroke,afill,arect,astrokeWidth)
            let text    = VisualTree.Text (FormatFolder f, textFormat, arect, aforeground)
            let folder  = VisualTree.Group [rect;text]

            match children with
            | []    -> folder
            | [x]   -> VisualTree.Fork (x, folder)
            | _     -> VisualTree.Fork (VisualTree.Group children, folder)

            

    let FoldVisualTree (s : VisualTree) (f : Folder) = 
        let xscale  = 1.F / (float32 <| max 1 f.Depth)
        let yscale  = 1.F / (float32 <| max 1L f.TotalFileSize)
        let ycutoff = 0.01F

        let stroke      = SolidColor <| ColorDescriptor.Color Color.Black
        let fill        = SolidColor <| ColorDescriptor.Color Color.WhiteSmoke
        let foreground  = SolidColor <| ColorDescriptor.Color Color.Black
        let textFormat  = TextFormatDescriptor.New "Verdana" 24.0F

        let vt = f |> CreateVisualTree stroke fill foreground textFormat xscale yscale ycutoff 0.F 0.F

        s,vt

    let BuildPipe (os : IObservableSource<Scanner.Folder>) = 
        let o = os 
                |> ObservableEx.asyncFold (Some ThreadPriority.Lowest) 100L FoldFolder EmptyFoldFolder
                |> Observable.map (fun (_,root) -> root)
                |> ObservableEx.deref
                |> Observable.map (fun root -> MakeFolder root)
                |> ObservableEx.foldMap FoldVisualTree VisualTree.Empty
        os,o

    
