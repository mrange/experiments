namespace Overhead.Microsoft.FSharp.Control

module SR =
    let controlContinuationInvokedMultipleTimes     = "controlContinuationInvokedMultipleTimes"
    let mismatchIAREnd                              = "mismatchIAREnd"
    let mismatchIARCancel                           = "mismatchIARCancel"
    let failedReadEnoughBytes                       = "failedReadEnoughBytes"
    let mailboxReceiveTimedOut                      = "mailboxReceiveTimedOut"
    let mailboxScanTimedOut                         = "mailboxScanTimedOut"
    let mailboxProcessorAlreadyStarted              = "mailboxProcessorAlreadyStarted"
    let mailboxProcessorPostAndReplyTimedOut        = "mailboxProcessorPostAndReplyTimedOut"
    let mailboxProcessorPostAndAsyncReplyTimedOut   = "mailboxProcessorPostAndAsyncReplyTimedOut"
    
    let GetString (k : string) = k

