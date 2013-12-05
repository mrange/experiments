namespace FolderSize

open System.Collections.Generic

module FolderTree = 

    type MutableFolder = 
        {
            Folder                  : Scanner.Folder
            Parent                  : MutableFolder option

            mutable Children        : List<MutableFolder>
            mutable TotalFileCount  : int64
            mutable TotalFileSize   : int64
        }

        member x.AddFolder (vf : MutableFolder) = 
            x.Children.Add vf
            x.UpdateRecursively vf

        member x.UpdateRecursively (vf : MutableFolder) = 
            x.TotalFileCount <- x.TotalFileCount + vf.TotalFileCount
            x.TotalFileSize <- x.TotalFileSize + vf.TotalFileSize

            let parent = x.Parent
            match parent with
            | Some p    -> p.UpdateRecursively vf
            | _         -> ()

        static member New sf p = 
            let vf = { Folder = sf; Parent = p; Children = List<MutableFolder>(); TotalFileSize = sf.FileSize; TotalFileCount = sf.FileCount }
            match p with
            | Some pp   -> pp.AddFolder vf
            | _         -> ()
            vf

    type Folder = 
        {
            Folder                  : Scanner.Folder

            Children                : Folder list
            TotalFileCount          : int64
            TotalFileSize           : int64
        }
        static member New f c tfc tfs = {Folder = f; Children = c; TotalFileCount = tfc; TotalFileSize = tfs}

    let rec MakeFolder (mf : MutableFolder) : Folder = 
        let c = mf.Children |> Seq.map MakeFolder |> Seq.toList

        Folder.New mf.Folder c mf.TotalFileCount mf.TotalFileSize

    let FoldFolder (map : Map<Scanner.Folder, MutableFolder>,root : MutableFolder option) (sf : Scanner.Folder) = 
        let pvf = 
            match sf.Parent with
            | Some p    -> map.TryFind p
            | _         -> None
        let vf = MutableFolder.New sf pvf
        let map' = map |> Map.add sf vf
        map', Some (root <??> vf)

    let EmptyFoldFolder : Map<Scanner.Folder, MutableFolder>*MutableFolder option = Map.empty,None

    let BuildPipe (os : IObservableSource<Scanner.Folder>) = 
        let o = os 
                |> ObservableEx.asyncFold 
                        100 
                        FoldFolder
                        EmptyFoldFolder
                |> Observable.map (fun (_,root) -> root)
                |> ObservableEx.deref
                |> Observable.map (fun root -> MakeFolder root)
        os,o

    
