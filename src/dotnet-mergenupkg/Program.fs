open System
open System.IO
open System.IO.Compression
open System.Xml.Linq
open System.Linq

open Railway

type DotnetFrameworkId = { Id: string; LongName: string }
    
let knownDotnetFrameworks = [
    yield { Id = "dnxcore50"; LongName = "DNXCore5.0" }
    for x in ["1.0"; "2.0"; "2.1"; "3.0"] do
        yield { Id = sprintf "netcoreapp%s" x; LongName = sprintf ".NETCoreApp%s" x }
    for x in ["2.0"; "3.0"; "3.5"; "4.0"; "4.5"; "4.5.2"; "4.6"; "4.6.1"; "4.6.2"; "4.7"; "4.7.1"] do
        yield { Id = sprintf "net%s" (x.Replace(".", "")); LongName = sprintf ".NETFramework%s" x }
    let netstandard = 
        [ for x in 1.0m .. 0.1m .. 1.6m do yield x
          yield 2.0m ]
    for x in netstandard do 
        yield { Id = sprintf "netstandard%M" x; LongName = sprintf ".NETStandard%M" x }
    for x in [ "net45+netcore45"; "net45+netcore45+wp8"; "net45+netcore45+wpa81+wp8" ] do 
        yield { Id = sprintf "portable-%s" x; LongName = sprintf ".NETPortable,Version=v0.0,Profile=%s" x }
    ]

type NuspecPackageType = string

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
        | None ->
            let xn name = XName.Get(name, doc.Root.Name.NamespaceName)
            let m = doc.Root |> xe (xn "metadata") |> Option.get
            m.Add(new XElement(xn "dependencies"))
            match doc |> dependencies with
            | None ->
                failwith "TODO added dependencies node, but cannot find it"
            | Some d ->
                let g = group |> changeNamespaceElement d.Name.Namespace
                d.Add(g)
                doc
        | Some d ->
            let g = group |> changeNamespaceElement d.Name.Namespace
            d.Add(g)
            doc

    let private packageTypesXml (nuspecDoc: NuspecXmlDoc) = 
        let xn name = XName.Get(name, nuspecDoc.Root.Name.NamespaceName)
        nuspecDoc.Root |> xe (xn "metadata") |> Option.bind (xe (xn "packageTypes"))

    let packageTypes (nuspecDoc: NuspecXmlDoc) : NuspecPackageType list =
        match nuspecDoc |> packageTypesXml with
        | None -> []
        | Some d ->
            d.Elements()
            |> Seq.filter (fun e -> e.Name.LocalName = "packageType")
            |> Seq.choose (fun e -> e.Attributes() |> Seq.tryPick (fun a -> if a.Name.LocalName = "name" then Some a.Value else None))
            |> List.ofSeq

    /// Add the packageTypes.packageType if not exists already
    let addPackageType (packageType: NuspecPackageType) (nuspecDoc: NuspecXmlDoc) =
        let doc = XDocument(nuspecDoc)
        let xn name = XName.Get(name, doc.Root.Name.NamespaceName)
        let createPackageTypeElement packageType =
            let g = new XElement(xn "packageType")
            g.SetAttributeValue(XName.Get "name", packageType)
            g
        match doc |> packageTypesXml with
        | None ->
            let m = doc.Root |> xe (xn "metadata") |> Option.get
            m.Add(new XElement(xn "packageTypes"))
            match doc |> packageTypesXml with
            | None ->
                failwith "TODO added dependencies node, but cannot find it"
            | Some d ->
                d.Add(createPackageTypeElement packageType)
                doc
        | Some d ->
            let packageTypes =
                d.Elements()
                |> Seq.filter (fun e -> e.Name.LocalName = "packageType")
                |> Seq.choose (fun e -> e.Attributes() |> Seq.tryPick (fun a -> if a.Name.LocalName = "name" then Some a.Value else None))
                |> List.ofSeq
            if packageTypes |> List.contains packageType then
                nuspecDoc
            else
                d.Add(createPackageTypeElement packageType)
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
        

    /// Replace the fallbackgroup, to be a tfm group
    let replaceFallbackGroup getDefaultFallbackGroupTFMs (nuspecDoc: NuspecXmlDoc) =
        let doc = XDocument(nuspecDoc)
        match doc |> dependencies with
        | None -> nuspecDoc
        | Some deps ->
            let isGroupWithTfm f (d: XElement) =
                if d.Name.LocalName = "group" then
                    let tfm = d.Attribute(XName.Get("targetFramework")) |> Option.ofObj |> Option.map (fun a -> a.Value)
                    f tfm
                else
                    false
            let isFallbackGroup =
                isGroupWithTfm (fun tfm ->
                    match tfm with
                    | None -> true
                    | Some x -> String.IsNullOrEmpty(x) )

            match deps.Elements() |> Seq.tryFind isFallbackGroup with
            | None ->
                nuspecDoc
            | Some d ->
                let known, unknown =
                    getDefaultFallbackGroupTFMs ()
                    |> List.fold (fun (k,u) t ->
                            match t with
                            | Choice1Of2 x -> (x :: k, u)
                            | Choice2Of2 x -> (k, x :: u)
                        ) ([],[])

                match known, unknown with
                | [], [] ->
                    // no libs
                    nuspecDoc
                | tfms, [] ->
                    // all tfms are known, replace fallback group
                    for tfm in tfms do
                        if deps.Elements() |> Seq.exists (isGroupWithTfm (fun s -> s = Some(tfm.LongName) || s = Some(tfm.Id))) then
                            // a group for that tfm is already specified, no need to add it
                            ()
                        else
                            let dupe = XElement(d) //deep copy
                            dupe.SetAttributeValue(XName.Get("targetFramework"), tfm.LongName)
                            deps.Add(dupe)
                    d.Remove()
                    doc
                | tfms, unknownTfms ->
                    // there are some unknown tfm. leave it as now
                    let warning f = Printf.ksprintf (printfn "warning: %s") f
                    warning "Found a fallback group for dependencies in the nuspec."
                    warning "more info in https://github.com/enricosada/dotnet-mergenupkg/issues/8"
                    warning ""
                    warning "Tried to infer the list of frameworks from the lib dir (inside the nupkg)"
                    warning "but some target frameworks are unknown:"
                    unknownTfms
                    |> List.iter (warning "- '%s'")
                    warning "the target frameworks found and known are:"
                    tfms
                    |> List.iter (fun fw -> warning "- '%s' (%s)" fw.Id fw.LongName)
                    warning ""
                    warning "Cannot replace the fallback group, if there are unknown target frameworks."
                    warning "Leaving the fallback group as is."
                    warning ""
                    warning "please open an issue in https://github.com/enricosada/dotnet-mergenupkg/issues "
                    nuspecDoc


type NupkgFile = ZipArchive

module Nupkg =

    let private nuspecEntry (archive: NupkgFile) =
        archive.Entries
        |> Seq.tryFind (fun entry -> entry.FullName.EndsWith(".nuspec"))

    let private parseDocFrom (nuspec: ZipArchiveEntry) : NuspecXmlDoc =
        use stream = nuspec.Open()
        use reader = new StreamReader(stream)
        let text = reader.ReadToEnd()
        try
            XDocument.Parse(text)
        with :? System.Xml.XmlException ->
            eprintfn "invalid xml '%s' " text
            reraise()

    let private mapNuspec f archive =
        match archive |> nuspecEntry with
        | None -> None
        | Some nuspec ->
            let xml = parseDocFrom nuspec
            Some (f xml)

    let private addFile path (stream: Stream) (archive: NupkgFile) =
        let e = archive.CreateEntry(path)
        use entryStream = e.Open()
        stream.CopyTo(entryStream)

    let private updateNuspec (f: NuspecXmlDoc -> NuspecXmlDoc) archive =
        match archive |> nuspecEntry with
        | None -> ()
        | Some nuspec ->
            let nuspecName = nuspec.FullName

            let xml = parseDocFrom nuspec

            let doc = f xml

            use ms = new MemoryStream()
            doc.Save(ms, SaveOptions.OmitDuplicateNamespaces)
            ms.Position <- 0L
            
            // delete it, and readd it.
            // safer than write the rewrite the stream
            nuspec.Delete()
            archive |> addFile nuspecName ms

    let private getFiles filterBy (archive: NupkgFile) =
        archive.Entries
        |> Seq.filter (fun e -> filterBy e.FullName)

    let openFile nupkgPath mode =
        match nupkgPath |> File.Exists with
        | true -> ZipFile.Open(nupkgPath, mode) |> Ok
        | false -> Error [ sprintf "nupkg '%s' not found" nupkgPath ]

    let inferFallbackGroup (npkg: NupkgFile) () =
        let pickKnownFramework fwId =
            knownDotnetFrameworks
            |> List.tryFind (fun f -> f.Id.Equals(fwId, StringComparison.OrdinalIgnoreCase))

        npkg
        |> getFiles (fun e -> e.StartsWith("lib/", StringComparison.OrdinalIgnoreCase) && e.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
        |> Seq.map (fun e -> e.FullName)
#if NETCOREAPP1_0
        |> Seq.map (fun path -> path.Replace("lib/", ""))
#else
        |> Seq.map (fun path -> path.Replace("lib/", "", StringComparison.OrdinalIgnoreCase))
#endif
        |> Seq.map (fun path -> path |> Seq.takeWhile ((<>) '/') |> Seq.toArray |> String)
        |> Seq.map (fun tfm ->
            match pickKnownFramework tfm with
            | Some fw -> Choice1Of2 fw
            | None -> Choice2Of2 tfm)
        |> Seq.toList

    let mergeDependency (other: NupkgFile) (fw: DotnetFrameworkId) (source: NupkgFile) =
        //normalize source nuspec
        source |> updateNuspec (NuspecXml.normalize)

        //remove fallbackgroup
        source |> updateNuspec (NuspecXml.replaceFallbackGroup (inferFallbackGroup source))

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

    let mergeTools (other: NupkgFile) (source: NupkgFile) =
        //normalize source nuspec
        source |> updateNuspec NuspecXml.normalize

        let packageTypesOpt = other |> mapNuspec NuspecXml.packageTypes

        match packageTypesOpt with
        | None -> Error [ "nuspec not found" ]
        | Some _packageTypes ->
            //add package type to nuspec
            source |> updateNuspec (NuspecXml.addPackageType "DotnetTool")

            //add files to nupkg
            other
            |> getFiles (fun e -> e.StartsWith("tools/", StringComparison.OrdinalIgnoreCase))
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

    let addToolsToNupkg nupkgNetcorePath nupkgPath = attempt {

        use! sourceNupkg = Nupkg.openFile nupkgPath ZipArchiveMode.Update

        use! cliNupkg = Nupkg.openFile nupkgNetcorePath ZipArchiveMode.Read

        do! sourceNupkg |> Nupkg.mergeTools cliNupkg 
        }


type FilePath = string
type DirectoryPath = string

type CmdLineArguments =
    | Help
    | Merge of MergeArguments
    | MergeTools of MergeToolsArguments
and MergeArguments = {
        SourceNupkg: FilePath;
        CliNupkg: FilePath;
        TargetFramework: DotnetFrameworkId;
    }
and MergeToolsArguments = {
        SourceNupkg: FilePath;
        CliNupkg: FilePath;
    }

module CommandLineArgParser =

    type private CmdLineArgumentsRaw = {
        Source: string option;
        Other: string option;
        Frameworks: (string * string option) list;
        Tools: bool;
        Help: bool;
    }

    let private parseArgs argv =
        let trimQuotes (s: string) = s.Trim('"')  
        let rec inner s r =
            match s with
            | [] -> Ok r
            | "--source" :: x :: xs -> inner xs { r with Source = Some (trimQuotes x) }
            | "--other" :: x :: xs -> inner xs { r with Other = Some (trimQuotes x) }
            | "--framework" :: x :: "--framework-name" :: n :: xs -> inner xs { r with Frameworks = [((trimQuotes x), Some (trimQuotes x))] |> List.append r.Frameworks }
            | "--framework" :: x :: xs -> inner xs { r with Frameworks = [((trimQuotes x), None)] |> List.append r.Frameworks }
            | "--tools" :: xs -> inner xs { r with Tools = true }
            | "-h" :: xs -> inner xs { r with Help = true }
            | "--help" :: xs -> inner xs { r with Help = true }
            | xs -> Error [ (sprintf "invalid args: %A" xs) ]

        { Source = None; Other = None; Tools = false; Help = false; Frameworks = [] }
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

            if argv.Tools then
                let! fws = 
                    match argv.Frameworks with
                    | [] ->  Ok None
                    | l -> Error [sprintf "multiple framework not supported"]

                return MergeTools { SourceNupkg = source; CliNupkg = other }
            else
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
  dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard1.6
  dotnet mergenupkg --source a.nupkg --other b.nupkg --framework dnxcore50 --framework-name "DNXCore5.0"
  dotnet mergenupkg --help
 
Options:
  -h --help          Show this screen.
  --source           The path of nupkg file to update
  --other            The path of nupkg file to merge
  --framework        The framework id
  --framework-name   The framework name, optional if framework id is known
  --tools            Merge the dotnet tools instead of lib
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
    | Ok (MergeTools { SourceNupkg = src; CliNupkg = other }) ->
        match src |> DotnetCliFSharpHelpers.addToolsToNupkg other |> runAttempt with
        | Ok () -> 0
        | Error x -> 
            x |> List.iter (printfn "%s")
            2
