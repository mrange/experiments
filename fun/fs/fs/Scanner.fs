namespace FolderSize

open System
open System.Threading
open System.IO

module Scanner = 

    type Folder = 
        {
            Parent              : Folder option
            Name                : string
            FileCount           : int
            FileSize            : Int64
            PhysicalFileSize    : Int64
        }
        static member New p nm fc fz pfz = {Parent = p; Name = nm; FileCount = fc; FileSize = fz; PhysicalFileSize = pfz}
    
    type ScannerMessage =
        {
            Path    : string
            Parent  : Folder option
        }
        static member New p pp = {Path = p; Parent = pp}

    let Start (rootFolder : string) : IObservableSource<Folder> = 
        
        let cts = new CancellationTokenSource ()
        let ct = cts.Token

        let onStart (source : IObservableSource<Folder>) =    
            let processor (input : MailboxProcessor<ScannerMessage>)  = 
                async {
                    let takeWhile = Seq.takeWhile (fun _ -> not ct.IsCancellationRequested)
                    while not ct.IsCancellationRequested do
                        let! message = input.Receive ()

                        try 
                            let path    = message.Path
                            let parent  = message.Parent
                            if not ct.IsCancellationRequested && Directory.Exists message.Path && not ct.IsCancellationRequested then 
                            
                                let name = Path.GetDirectoryName path

                                let fc, fz = 
                                    Directory.EnumerateFiles path
                                    |> takeWhile
                                    |> Seq.map (fun p -> FileInfo p)
                                    |> Seq.fold (fun (fc, fz) file -> fc + 1, fz + file.Length) (0,0L)
                            

                                let folder = Folder.New parent name fc fz fz

                                if not ct.IsCancellationRequested then
                                    source.Next folder

                                for d in Directory.EnumerateDirectories path |> takeWhile do
                                    input.Post <| ScannerMessage.New d (Some folder) 

                        with
                        | e -> source.Error e

                        ()
                }

            let mbp = MailboxProcessor<ScannerMessage>.Start (processor, ct)    

            mbp.Post <| ScannerMessage.New rootFolder None

            mbp

        let onDispose mbp = TryRun cts.Cancel
                            TryRun cts.Dispose
                            TryDispose mbp

        let obs = new ObservableSource<_,_> (onStart, onDispose)

        upcast obs    



