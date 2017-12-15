module DotnetMergeNupkg.Tests

open System
open System.IO
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open FileUtils
open Medallion.Shell
open System.IO.Compression
open System.Xml.Linq

let RepoDir = (__SOURCE_DIRECTORY__ /".." /"..") |> Path.GetFullPath
let TestRunDir = RepoDir/"test"/"testrun"
let ArtifactsDir = RepoDir/"artifacts"
let NupkgsDir = ArtifactsDir/"nupkgs"

let SamplePkgVersion = "1.0.0"
let SamplePkgDir = TestRunDir/"pkgs"/"SamplePkgDir"

let checkExitCodeZero (cmd: Command) =
    Expect.equal 0 cmd.Result.ExitCode "command finished with exit code non-zero."

let packLibs (fs: FileUtils) =
    fs.rm_rf (TestRunDir/"pkgs")
    fs.mkdir_p (TestRunDir/"pkgs")

    let dotnetPack workDir =
      fs.cd workDir
      fs.shellExecRun "dotnet" [ "pack"; "-o"; SamplePkgDir; sprintf "/p:Version=%s" SamplePkgVersion ]
      |> checkExitCodeZero

    dotnetPack (RepoDir/"test"/"examples"/"sample1-dotnet")
    dotnetPack (RepoDir/"test"/"examples"/"sample2-dotnetcore")
    dotnetPack (RepoDir/"test"/"examples"/"sample3-dotnetcore-1_6")

let prepareTool (fs: FileUtils) pkgUnderTestVersion =
    fs.rm_rf (TestRunDir/"sdk2")
    fs.mkdir_p (TestRunDir/"sdk2")

    fs.cp (RepoDir/"test"/"usetool"/"tools.proj") (TestRunDir/"sdk2")
    fs.createFile (TestRunDir/"sdk2"/"nuget.config") (writeLines 
      [ "<configuration>"
        "  <packageSources>"
        sprintf """    <add key="local" value="%s" />""" NupkgsDir
        "  </packageSources>"
        "</configuration>" ])
    fs.createFile (TestRunDir/"sdk2"/"Directory.Build.props") (writeLines 
      [ """<Project ToolsVersion="15.0">"""
        "  <PropertyGroup>"
        sprintf """    <PkgUnderTestVersion>%s</PkgUnderTestVersion>""" pkgUnderTestVersion
        "  </PropertyGroup>"
        "</Project>" ])

    fs.cd (TestRunDir/"sdk2")
    fs.shellExecRun "dotnet" [ "restore"; "--packages"; "packages" ]
    |> checkExitCodeZero

let merge (fs: FileUtils) sourceNupkgPath otherNupkgPath tfm =
    fs.cd (TestRunDir/"sdk2")
    fs.shellExecRun "dotnet" [ "mergenupkg"; "--source"; sourceNupkgPath; "--other"; otherNupkgPath; "--framework"; tfm ]

type TestAssetProjInfo =
  { ProjDir: string;
    PackageName: string;
    AssemblyName: string;
    Assembly: string;
    Nuspec: TestAssetProjInfoNuspec list }
and TestAssetProjInfoNuspec =
  | FallbackGroup of dependencies: TestAssetProjInfoNuspecDep list
  | GroupWithTFM of tfm: string * dependencies: TestAssetProjInfoNuspecDep list
and TestAssetProjInfoNuspecDep =
  | PackageDep of id: string * version: string * exclude: string option

let nupkgReadonlyPath source =
    SamplePkgDir/(sprintf "%s.%s.nupkg" source.AssemblyName SamplePkgVersion)

let copyNupkgFromAssets (fs: FileUtils) source outDir =
    fs.mkdir_p outDir

    let path = nupkgReadonlyPath source
    let sourceNupkgPath = outDir/(Path.GetFileName path)

    fs.cp path sourceNupkgPath
    sourceNupkgPath

type NupkgContent =
  { PackageName: string
    Files: string list
    Nuspec: TestAssetProjInfoNuspec list }

let checkNupkgContent (fs: FileUtils) dir sourceNupkgPath nupkgModel =
    fs.rm_rf dir
    fs.mkdir_p dir
    fs.unzip sourceNupkgPath dir

    let nuspecPath = dir/(sprintf "%s.nuspec" nupkgModel.PackageName)

    for file in nuspecPath :: nupkgModel.Files do
      Expect.isTrue (File.Exists(dir/file)) (sprintf "expected file '%s' not found." (dir/file))

    use nuspec = fs.readFile nuspecPath
    let xml = XDocument.Load(nuspec)
    let xn name = XName.Get(name, xml.Root.Name.NamespaceName)
    let invalidXml format = Printf.ksprintf (fun s -> sprintf "%s. xml was '%O'" s xml) format
    let el name (x: XElement) =
      match x.Element(xn name) with
      | null -> failwith (invalidXml "element '%s' not found in %O" name x)
      | x -> x
    let attr name (x: XElement) =
      x.Attribute(XName.Get(name)) |> Option.ofObj |> Option.map (fun a -> a.Value)
    let depsXml =
      [ let e = xml.Root |> el "metadata" |> el "dependencies"
        yield! e.Elements() ]

    Expect.equal depsXml.Length nupkgModel.Nuspec.Length (invalidXml "expected same length of '%A'" nupkgModel.Nuspec)

    let depsParsed =
      [ for x in depsXml ->
          Expect.equal "group" (x.Name.LocalName) (invalidXml "unknown node '%O'" x)
          let group =
            match x |> attr "targetFramework" with
            | None -> fun deps -> FallbackGroup(deps)
            | Some id -> fun deps -> GroupWithTFM(tfm = id, dependencies = deps)
          let deps =
            [ for d in x.Elements() ->
                Expect.equal "dependency" (d.Name.LocalName) (invalidXml "unknown node '%O'" d)
                let id = match d |> attr "id" with Some a -> a | None -> failwith (invalidXml "expected attr 'id' in '%O'" d)
                let version = match d |> attr "version" with Some a -> a | None -> failwith (invalidXml "expected attr 'version' in '%O'" d)
                let exclude = d |> attr "exclude"
                PackageDep(id = id, version = version, exclude = exclude) ]
          group deps ]

    Expect.sequenceEqual depsParsed nupkgModel.Nuspec (sprintf "expecting deps '%A' but was '%O' (parsed as '%A')" nupkgModel.Nuspec depsXml depsParsed)

let ``samples1 Net45`` =
  { ProjDir = "sample1-dotnet";
    PackageName = "Lib1";
    AssemblyName = "Lib1";
    Nuspec = [ GroupWithTFM (tfm = ".NETFramework4.5", dependencies = []) ]
    Assembly = "lib"/"net45"/"Lib1.dll" }
let ``samples2 NetStandard2.0`` =
  { ProjDir = "sample2-dotnetcore";
    PackageName = "Lib2";
    AssemblyName = "Lib2";
    Nuspec = [ GroupWithTFM (tfm = ".NETStandard2.0", dependencies = []) ]
    Assembly = "lib"/"netstandard2.0"/"Lib2.dll"
  }
let ``samples3 NetStandard1.6`` =
  { ProjDir = "sample3-dotnetcore-1_6";
    PackageName = "Lib3";
    AssemblyName = "Lib3";
    Nuspec =
       [ GroupWithTFM (tfm = ".NETStandard1.6", 
                       dependencies = [ PackageDep (id = "NETStandard.Library", version = "1.6.1", exclude = Some "Build,Analyzers") ]) ]
    Assembly = "lib"/"netstandard1.6"/"Lib3.dll" }

let tests pkgUnderTestVersion =

  let prepareTestsAssets = lazy(
      let logger = Log.create "Tests Assets"
      let fs = FileUtils(logger)

      // restore tool
      prepareTool fs pkgUnderTestVersion

      // restore samples, and pack it
      packLibs fs
    )

  let withLog name f test =
    test name (fun () ->
      prepareTestsAssets.Force()

      let logger = Log.create (sprintf "Test '%s'" name)
      let fs = FileUtils(logger)
      f logger fs)

  let inDir (fs: FileUtils) dirName =
    let outDir = TestRunDir/dirName
    fs.rm_rf outDir
    fs.mkdir_p outDir
    fs.cd outDir
    outDir

  testList "merge" [
    testCase |> withLog "can merge net45+netstandard1.6" (fun _ fs ->
        let testDir = inDir fs "out_net45_plus_netstandard16"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples1 Net45`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples3 NetStandard1.6`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard1.6"
        |> checkExitCodeZero

        { PackageName = ``samples1 Net45``.PackageName
          Files = [ ``samples1 Net45``.Assembly; ``samples3 NetStandard1.6``.Assembly ]
          Nuspec = ``samples1 Net45``.Nuspec @ ``samples3 NetStandard1.6``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
    )

    testCase |> withLog "can merge net45+netstandard2.0" (fun _ fs ->
        let testDir = inDir fs "out_net45_plus_netstandard20"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples1 Net45`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        { PackageName = ``samples1 Net45``.PackageName
          Files = [ ``samples1 Net45``.Assembly; ``samples2 NetStandard2.0``.Assembly ]
          Nuspec = ``samples1 Net45``.Nuspec @ ``samples2 NetStandard2.0``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
    )

    testCase |> withLog "can merge multiple times" (fun _ fs ->
        let testDir = inDir fs "out_multiple"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples1 Net45`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        let otherNupkgPath2 = copyNupkgFromAssets fs ``samples3 NetStandard1.6`` testDir

        merge fs sourceNupkgPath otherNupkgPath2 "netstandard1.6"
        |> checkExitCodeZero

        { PackageName = ``samples1 Net45``.PackageName
          Files = [ ``samples1 Net45``.Assembly
                    ``samples2 NetStandard2.0``.Assembly
                    ``samples3 NetStandard1.6``.Assembly ]
          Nuspec = ``samples1 Net45``.Nuspec
                   @ ``samples2 NetStandard2.0``.Nuspec
                   @ ``samples3 NetStandard1.6``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
    )
  ]
