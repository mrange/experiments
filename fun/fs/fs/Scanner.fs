namespace FolderSize

open System
open System.Threading
open System.IO

module Scanner = 

    type Folder = 
        {
            Parent              : Folder option
            Name                : string
            FileCount           : int64
            FileSize            : int64
        }
        static member New p nm fc fz = {Parent = p; Name = nm; FileCount = fc; FileSize = fz}
    
    type Message =
        {
            Path    : string
            Parent  : Folder option
        }
        static member New p pp = {Path = p; Parent = pp}

    let Start (rootFolder : string) : IObservableSource<Folder> = 
        
        let root = Path.GetFullPath rootFolder

        let cts = new CancellationTokenSource ()
        let ct = cts.Token

        let onStart (source : IObservableSource<Folder>) =    
            let processor (input : MailboxProcessor<Message>)  = 
                async {
                    let takeWhile = Seq.takeWhile (fun _ -> not ct.IsCancellationRequested)
                    while not ct.IsCancellationRequested do
                        let! message = input.Receive ()

                        try 
                            let path    = message.Path
                            let parent  = message.Parent
                            if not ct.IsCancellationRequested && Directory.Exists message.Path && not ct.IsCancellationRequested then 
                            
                                let name = Path.GetFileName path

                                let fc, fz = 
                                    Directory.EnumerateFiles path
                                    |> takeWhile
                                    |> Seq.map (fun p -> FileInfo p)
                                    |> Seq.fold (fun (fc, fz) file -> fc + 1L, fz + file.Length) (0L,0L)
                            

                                let folder = Folder.New parent name fc fz

                                if not ct.IsCancellationRequested then
                                    source.Next folder

                                for d in Directory.EnumerateDirectories path |> takeWhile do
                                    input.Post <| Message.New d (Some folder) 

                        with
                        | e -> source.Error e

                        ()
                }

            let mbp = MailboxProcessor<Message>.Start (processor, ct)    

            mbp.Post <| Message.New root None

            mbp

        let onDispose mbp = TryRun cts.Cancel
                            TryDispose cts
                            TryDispose mbp

        let obs = new ObservableSource<_,_> (onStart, onDispose)

        upcast obs    



