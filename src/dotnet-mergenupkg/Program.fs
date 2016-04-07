open System
open System.IO
open System.IO.Compression
open System.Xml.Linq
open System.Linq

open Railway

type DotnetFrameworkId = { Id: string; LongName: string }
    
let knownDotnetFrameworks = [
    yield { Id = "dnxcore50"; LongName = "DNXCore5.0" }
    for x in ["4.0"; "4.5"; "4.5.2"; "4.6"; "4.6.1"] do
        yield { Id = sprintf "net%s" (x.Replace(".", "")); LongName = sprintf ".NETFramework%s" x }
    for x in 1.1m .. 0.1m .. 1.5m do 
        yield { Id = sprintf "netstandard%M" x; LongName = sprintf ".NETStandard%M" x }
    ]

[<RequireQualifiedAccess>]
module NuspecManifestSchema =

    /// Baseline schema 
    [<Literal>]
    let V1 = "http://schemas.microsoft.com/packaging/2010/07/nuspec.xsd"

    /// Added copyrights, references and release notes
    [<Literal>]
    let V2 = "http://schemas.microsoft.com/packaging/2011/08/nuspec.xsd"

    /// Used if the version is a semantic version.
    [<Literal>]
    let V3 = "http://schemas.microsoft.com/packaging/2011/10/nuspec.xsd"

    /// Added 'targetFramework' attribute for 'dependency' elements.
    /// Allow framework folders under 'content' and 'tools' folders. 
    [<Literal>]
    let V4 = "http://schemas.microsoft.com/packaging/2012/06/nuspec.xsd"

    /// Added 'targetFramework' attribute for 'references' elements.
    /// Added 'minClientVersion' attribute
    [<Literal>]
    let V5 = "http://schemas.microsoft.com/packaging/2013/01/nuspec.xsd"

    /// Allows XDT transformation
    [<Literal>]
    let V6 = "http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd"


module XLinqHelpers =
    let xe name (e: XElement) = match e.Element(name) with null -> None | x -> Some x

    let rec changeNamespaceElement (xmlns: XNamespace) (e: XElement) : XElement =
        let xname (name: XName) = xmlns + name.LocalName
        let ex = new XElement(e.Name |> xname, (e.Attributes()), [| for x in e.Elements() -> x |> changeNamespaceElement xmlns |])
        if (not e.HasElements) then ex.Value <- e.Value
        ex

    let changeNamespace (xmlns: XNamespace) (e: XDocument) : XDocument =
        let doc = XDocument(e)
        doc.Root.Attributes().Where(fun a -> a.IsNamespaceDeclaration).Remove()
        doc.Root |> changeNamespaceElement xmlns |> doc.Root.ReplaceWith
        doc

type NuspecXmlDoc = XDocument
type NuspecFrameworkGroup = XElement

module NuspecXml =

    open XLinqHelpers

    let private dependencies (nuspecDoc: NuspecXmlDoc) = 
        let xn name = XName.Get(name, nuspecDoc.Root.Name.NamespaceName)
        nuspecDoc.Root |> xe (xn "metadata") |> Option.bind (xe (xn "dependencies"))

    let private flatListOfDeps (dependencies: XElement) =
        dependencies.Elements()
        |> Seq.filter (fun n -> n.Name.LocalName = "dependency")
        |> Seq.exists (fun _ -> true)

    let dependencyGroup fw nuspecDoc : NuspecFrameworkGroup option =
        match nuspecDoc |> dependencies with
        | None -> None
        | Some d ->
            let filterByFw (e: XElement) =
                match fw, (e.Attributes() |> Seq.tryFind (fun a -> a.Name.LocalName = "targetFramework")) with
                | None, None -> true
                | None, Some a -> a.Value = ""
                | Some _, None -> false
                | Some { LongName = longName }, Some a -> a.Value = longName
            d.Elements()
            |> Seq.filter (fun e -> e.Name.LocalName = "group")
            |> Seq.filter filterByFw
            |> Seq.tryHead
            |> Option.map XElement

    let addDependencyGroup (group: NuspecFrameworkGroup) (nuspecDoc: NuspecXmlDoc) =
        let doc = XDocument(nuspecDoc)
        match doc |> dependencies with
        | None -> failwith "TODO add dependencies node"
        | Some d ->
            let g = group |> changeNamespaceElement d.Name.Namespace
            d.Add(g)
            doc


    /// Specifying Dependencies in version 2.0 and above
    /// https://docs.nuget.org/create/nuspec-reference
    let normalize (nuspecDoc: NuspecXmlDoc) =

        //at least nuspec schema V4 is needed, to support dependencies group
        let upgradeNamespaceIfNeeded (e: NuspecXmlDoc) =
            match e.Root.Name.NamespaceName with
            | NuspecManifestSchema.V1
            | NuspecManifestSchema.V2
            | NuspecManifestSchema.V3 ->
                e |> changeNamespace (NuspecManifestSchema.V4 |> XNamespace.Get)
            | _ ->
                e

        //use group for dependencies, instead of flat list
        let useDependenciesGroup (e: NuspecXmlDoc) =
            let doc = XDocument(e)
            match doc |> dependencies with
            | None -> e
            | Some d when flatListOfDeps d ->
                let fallbackDeps = d.Elements() |> Seq.toArray
                let group = XElement(XName.Get("group", doc.Root.Name.NamespaceName), fallbackDeps)
                d.ReplaceNodes(group)
                doc
            | Some _ -> e

        nuspecDoc 
        |> upgradeNamespaceIfNeeded
        |> useDependenciesGroup
        

type NupkgFile = ZipArchive

module Nupkg =

    let private nuspecEntry (archive: NupkgFile) =
        archive.Entries
        |> Seq.tryFind (fun entry -> entry.FullName.EndsWith(".nuspec"))

    let private mapNuspec f archive =
        match archive |> nuspecEntry with
        | None -> None
        | Some nuspec ->
            use stream = nuspec.Open()
            let xml : NuspecXmlDoc = XDocument.Load(stream)
            Some (f xml)

    let private updateNuspec (f: NuspecXmlDoc -> NuspecXmlDoc) archive =
        match archive |> nuspecEntry with
        | None -> ()
        | Some nuspec ->
            let xml =
                use stream = nuspec.Open()
                XDocument.Load(stream)
            let doc = f xml
            use writer = new StreamWriter(nuspec.Open())
            doc.Save(writer, SaveOptions.OmitDuplicateNamespaces)

    let private addFile path (stream: Stream) (archive: NupkgFile) =
        let e = archive.CreateEntry(path)
        use entryStream = e.Open()
        stream.CopyTo(entryStream)

    let private getFiles filterBy (archive: NupkgFile) =
        archive.Entries
        |> Seq.filter (fun e -> filterBy e.FullName)

    let openFile nupkgPath mode = 
        ZipFile.Open(nupkgPath, mode)
        |> Ok

    let mergeDependency (other: NupkgFile) (fw: DotnetFrameworkId) (source: NupkgFile) =
        //normalize source nuspec
        source |> updateNuspec (NuspecXml.normalize)

        let group = other |> mapNuspec (NuspecXml.dependencyGroup (Some fw))

        match group with
        | None -> Error [ "nuspec not found" ]
        | Some (None) -> Error [ sprintf "framework (%s - %s) not found" fw.Id fw.LongName ]
        | Some (Some g) ->
            //add dependency to nuspec
            source |> updateNuspec (NuspecXml.addDependencyGroup g)

            //add files to nupkg
            other
            |> getFiles (fun e -> e.StartsWith(sprintf "lib/%s/" fw.Id, StringComparison.OrdinalIgnoreCase))
            |> Seq.iter (fun e -> 
                use stream = e.Open()
                source |> addFile e.FullName stream )
            Ok ()


module DotnetCliFSharpHelpers =

    let addNetcoreToNupkg nupkgNetcorePath fw nupkgPath = attempt {

        use! sourceNupkg = Nupkg.openFile nupkgPath ZipArchiveMode.Update

        use! cliNupkg = Nupkg.openFile nupkgNetcorePath ZipArchiveMode.Read

        do! sourceNupkg |> Nupkg.mergeDependency cliNupkg fw 
        }


type FilePath = string
type DirectoryPath = string

type CmdLineArguments =
    | Help
    | Merge of MergeArguments
and MergeArguments = {
        SourceNupkg: FilePath;
        CliNupkg: FilePath;
        TargetFramework: DotnetFrameworkId;
    }

module CommandLineArgParser =

    type private CmdLineArgumentsRaw = {
        Source: string option;
        Other: string option;
        Frameworks: (string * string option) list;
        Help: bool;
    }

    let private parseArgs argv =
        let rec inner s r =
            match s with
            | [] -> Ok r
            | "--source" :: x :: xs -> inner xs { r with Source = Some x }
            | "--other" :: x :: xs -> inner xs { r with Other = Some x }
            | "--framework" :: x :: "--framework-name" :: n :: xs -> inner xs { r with Frameworks = [(x, Some n)] |> List.append r.Frameworks }
            | "--framework" :: x :: xs -> inner xs { r with Frameworks = [(x, None)] |> List.append r.Frameworks }
            | "-h" :: xs -> inner xs { r with Help = true }
            | "--help" :: xs -> inner xs { r with Help = true }
            | xs -> Error [ (sprintf "invalid args: %A" xs) ]

        { Source = None; Other = None; Help = false; Frameworks = [] }
        |> inner argv 


    let private validateArgs argv = attempt {
        let required name o =
            match o with None -> Error [sprintf "argument %s is required" name] | Some x -> Ok x

        if argv.Help then
            return Help
        else
            let! source = argv.Source |> required "--source"
            let! other = argv.Other |> required "--other"

            let validateFrameworkId fwId =
                let f =
                    knownDotnetFrameworks
                    |> List.tryFind (fun f -> f.Id.Equals(fwId, StringComparison.OrdinalIgnoreCase))
                match f with
                | Some fw -> Ok fw
                | None ->
                knownDotnetFrameworks 
                |> List.map (fun { Id = i; LongName = l } -> sprintf "- %s: %s" i l)
                |> List.append [sprintf "Invalid framework id '%s'" fwId; ""; "Known frameworks: "] 
                |> Error

            let! fws = 
                match argv.Frameworks with
                | [] ->  Error [sprintf "argument %s is required" "--framework"]
                | [ l, None ] -> l |> validateFrameworkId
                | [ id, Some n ] -> Ok { Id = id; LongName = n }
                | l -> Error [sprintf "multiple framework not supported"]

            return Merge { SourceNupkg = source; CliNupkg = other; TargetFramework = fws; }
        }

    let parse argv = attempt {
        let! raw = parseArgs argv

        return! validateArgs raw
        }
        

let showHelp () =
    """
Usage:
  dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard1.5
  dotnet mergenupkg --source a.nupkg --other b.nupkg --framework dnxcore50 --framework-name "DNXCore5.0"
  dotnet mergenupkg --help
 
Options:
  -h --help          Show this screen.
  --source           The path of nupkg file to update
  --other            The path of nupkg file to merge
  --framework        The framework id
  --framework-name   The framework name, optional if framework id is known
    """
    |> printfn "%s"

[<EntryPoint>]
let main argv = 
    match argv |> List.ofArray |> CommandLineArgParser.parse |> runAttempt with
    | Error x ->
        x |> List.iter (printfn "%s")
        printfn ""
        showHelp()
        1
    | Ok Help ->
        printfn "MergeNupkg."
        printfn " "
        showHelp()
        0
    | Ok (Merge { SourceNupkg = src; CliNupkg = other; TargetFramework = fw }) ->
        match src |> DotnetCliFSharpHelpers.addNetcoreToNupkg other fw |> runAttempt with
        | Ok () -> 0
        | Error x -> 
            x |> List.iter (printfn "%s")
            2
