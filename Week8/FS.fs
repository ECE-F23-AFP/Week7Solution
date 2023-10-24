module Week8.FS

// 1)
type ElementInfo = {name: string; extension: string; size: int}

type FileSys = Element list
and Element =
    | File of ElementInfo
    | Dir of ElementInfo * FileSys


// 2)
let rec namesFileSys = function
    | []         -> []
    | head::rest -> (namesElement head) @ (namesFileSys rest)
and namesElement = function
    | File s         -> [s.name]
    | Dir (info, fs) -> info.name :: (namesFileSys fs)

// 3)

let rec searchFileSystem ext = function
    | []         -> []
    | head::rest -> (searchElement ext head) @ (searchFileSystem ext rest)
and searchElement ext = function
    | File info when info.extension = ext -> [info.name]
    | File _                              -> []
    | Dir (info, fs)                      ->
        let tmp = if (info.extension = ext) then [info.name] else []
        tmp @ searchFileSystem ext fs

// 4)
let rec traverseFileSystem fFile fDir = function
    | []         -> []
    | head::rest -> (traverseElement fFile fDir head) @ (traverseFileSystem fFile fDir rest)
and traverseElement fFile fDir = function
    | File info      -> fFile info
    | Dir (info, fs) -> (fDir info) @ traverseFileSystem fFile fDir fs


let namesTraverseFileSystem fs =
    let fFile f = [f.name]
    traverseFileSystem fFile fFile fs
    
let searchTraverseFileSystem ext fs =
    let fFile info = if info.extension = ext then [info.name] else []
    traverseFileSystem fFile fFile fs

// 5)

 