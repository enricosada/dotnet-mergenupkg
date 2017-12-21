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
open DotnetMergeNupkg.TestAssets

let RepoDir = (__SOURCE_DIRECTORY__ /".." /"..") |> Path.GetFullPath
let TestRunDir = RepoDir/"test"/"testrun"
let NupkgsDir = RepoDir/"bin"/"nupkg"

let SamplePkgVersion = "1.0.0"
let SamplePkgDir = TestRunDir/"pkgs"/"SamplePkgDir"

let checkExitCodeZero (cmd: Command) =
    Expect.equal 0 cmd.Result.ExitCode "command finished with exit code non-zero."

let packSamples (fs: FileUtils) =

    fs.rm_rf (TestRunDir/"pkgs")

    fs.mkdir_p (TestRunDir/"pkgs")

    let dotnetPack additionalArgs workDir =
      fs.cd workDir
      fs.shellExecRun "dotnet" ([ "pack"; "-o"; SamplePkgDir; sprintf "/p:Version=%s" SamplePkgVersion ] @ additionalArgs)
      |> checkExitCodeZero

    let repo s = RepoDir/"test"/"examples"/s.ProjDir

    [ ``samples1 Net45``
      ``samples2 NetStandard2.0``
      ``samples3 NetStandard1.6``
      ``samples4 fallbackgrp``
      ``samples5 fallbackgrp spec201108``
      ``samples6 fallbackgrp multifw``
      ``samples7 fallbackgrp multifw with unknown fw``
      ``samples8 NetCoreApp2.0`` ]
    |> List.map repo
    |> List.iter (dotnetPack [])

    [ ``samples2 NetStandard2.0 with xmldoc`` ]
    |> List.iter (fun proj -> repo proj |> dotnetPack [ "/p:GenerateDocumentationFile=true"; sprintf "/p:PackageId=%s" proj.PackageName ])

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

let nupkgReadonlyPath source =
    SamplePkgDir/(sprintf "%s.%s.nupkg" source.PackageName SamplePkgVersion)

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

    Expect.containsAll depsParsed nupkgModel.Nuspec (sprintf "expecting deps '\n%A\n' but was '\n%O\n' (parsed as '\n%A\n')" nupkgModel.Nuspec depsXml depsParsed)
    Expect.containsAll nupkgModel.Nuspec depsParsed (sprintf "expecting deps '\n%A\n' but was '\n%O\n' (parsed as '\n%A\n')" nupkgModel.Nuspec depsXml depsParsed)

let tests pkgUnderTestVersion =

  let prepareTestsAssets = lazy(
      let logger = Log.create "Tests Assets"
      let fs = FileUtils(logger)

      // restore tool
      prepareTool fs pkgUnderTestVersion

      // restore samples, and pack it
      packSamples fs
    )

  let withLog name f test =
    test name (fun () ->
      prepareTestsAssets.Force()

      let logger = Log.create (sprintf "Test '%s'" name)
      let fs = FileUtils(logger)
      f logger fs)

  let withLogAsync name f test =
    test name (async {
      prepareTestsAssets.Force()

      let logger = Log.create (sprintf "Test '%s'" name)
      let fs = FileUtils(logger)
      do! f logger fs })

  let inDir (fs: FileUtils) dirName =
    let outDir = TestRunDir/dirName
    fs.rm_rf outDir
    fs.mkdir_p outDir
    fs.cd outDir
    outDir

  [ 
    testList "merge" [
      testCase |> withLog "can merge net45+netstandard1.6" (fun _ fs ->
        let testDir = inDir fs "out_net45_plus_netstandard16"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples1 Net45`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples3 NetStandard1.6`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard1.6"
        |> checkExitCodeZero

        { PackageName = ``samples1 Net45``.PackageName
          Files = ``samples1 Net45``.Files @ ``samples3 NetStandard1.6``.Files
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
          Files = ``samples1 Net45``.Files @ ``samples2 NetStandard2.0``.Files
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
          Files = [ ``samples1 Net45``.Files
                    ``samples2 NetStandard2.0``.Files
                    ``samples3 NetStandard1.6``.Files ] |> List.collect id
          Nuspec = ``samples1 Net45``.Nuspec
                   @ ``samples2 NetStandard2.0``.Nuspec
                   @ ``samples3 NetStandard1.6``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )
    ]

    testList "fallbackgroup" [
      testCase |> withLog "infer it, if group with no tfm" (fun _ fs ->
        let testDir = inDir fs "out_fallbackgrp"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples4 fallbackgrp`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        { PackageName = ``samples4 fallbackgrp``.PackageName
          Files = ``samples4 fallbackgrp``.Files @ ``samples2 NetStandard2.0``.Files
          Nuspec = 
           [ GroupWithTFM(
              tfm = ".NETFramework4.0",
              dependencies =
                [ PackageDep (id = "FSharp.Core", version = "4.0.0", exclude = None) ]) ]
            @ ``samples2 NetStandard2.0``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )

      testCase |> withLog "infer it, no group but deps" (fun _ fs ->
        let testDir = inDir fs "out_fallbackgrp_spec201108"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples5 fallbackgrp spec201108`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        { PackageName = ``samples5 fallbackgrp spec201108``.PackageName
          Files = ``samples5 fallbackgrp spec201108``.Files @ ``samples2 NetStandard2.0``.Files
          Nuspec = 
           [ GroupWithTFM(
              tfm = ".NETFramework4.5",
              dependencies =
                [ PackageDep (id = "Argu", version = "3.2.0", exclude = None) ]) ]
            @ ``samples2 NetStandard2.0``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )

      testCase |> withLog "infer it, multi fw" (fun _ fs ->
        let testDir = inDir fs "out_fallbackgrp_multifw"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples6 fallbackgrp multifw`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        let group tfm =
           GroupWithTFM(tfm, [ PackageDep (id = "Argu", version = "3.2.0", exclude = None) ])

        { PackageName = ``samples6 fallbackgrp multifw``.PackageName
          Files = ``samples6 fallbackgrp multifw``.Files @ ``samples2 NetStandard2.0``.Files
          Nuspec =
           [ group ".NETFramework2.0"
             group ".NETFramework3.5"
             group ".NETFramework4.6.1" ]
            @ ``samples2 NetStandard2.0``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )

      testCase |> withLog "infer failed, multi fw, one unknown" (fun _ fs ->
        let testDir = inDir fs "out_fallbackgrp_multifw-unk"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples7 fallbackgrp multifw with unknown fw`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0`` testDir

        let cmd = merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        cmd |> checkExitCodeZero

        Expect.stringContains cmd.Result.StandardOutput "warning" "check output"
        Expect.stringContains cmd.Result.StandardOutput "fallback" "check output"
        Expect.stringContains cmd.Result.StandardOutput "net20" "check output"
        Expect.stringContains cmd.Result.StandardOutput "net461" "check output"
        Expect.stringContains cmd.Result.StandardOutput "unknfw123" "check output"

        { PackageName = ``samples7 fallbackgrp multifw with unknown fw``.PackageName
          Files = ``samples7 fallbackgrp multifw with unknown fw``.Files @ ``samples2 NetStandard2.0``.Files
          Nuspec =
            ``samples7 fallbackgrp multifw with unknown fw``.Nuspec
            @ ``samples2 NetStandard2.0``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )
    ]

    testList "xml" [
      testCase |> withLog "merge the xmldoc file" (fun _ fs ->
        let testDir = inDir fs "out_xmldoc"
        let sourceNupkgPath = copyNupkgFromAssets fs ``samples1 Net45`` testDir
        let otherNupkgPath = copyNupkgFromAssets fs ``samples2 NetStandard2.0 with xmldoc`` testDir

        merge fs sourceNupkgPath otherNupkgPath "netstandard2.0"
        |> checkExitCodeZero

        { PackageName = ``samples1 Net45``.PackageName
          Files = ``samples1 Net45``.Files @ ``samples2 NetStandard2.0 with xmldoc``.Files
          Nuspec = ``samples1 Net45``.Nuspec @ ``samples2 NetStandard2.0 with xmldoc``.Nuspec }
        |> checkNupkgContent fs (testDir/"out") sourceNupkgPath
      )
    ]

    testList "realworld" [

      let rw pkg version deps =
        testCaseAsync |> withLogAsync (sprintf "%s-v%s" pkg version) (fun _ fs -> async {
          let testDir = inDir fs (sprintf "realworld_%s-%s" pkg (version.Replace('.','_')))

          let pkgUrl = sprintf "https://www.nuget.org/api/v2/package/%s/%s" pkg version

          fs.mkdir_p testDir

          let! sourceNupkgPath = async {
              let path = testDir/(sprintf "%s.%s.nupkg" pkg version)

              use client = new System.Net.Http.HttpClient()

              let! response =
                client.GetAsync(pkgUrl)
                |> Async.AwaitTask

              use fs = File.Create(path)
              let! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
              do! stream.CopyToAsync(fs) |> Async.AwaitTask

              return path
            }
          
          fs.cp sourceNupkgPath (Path.ChangeExtension(sourceNupkgPath, ".orig.nupkg"))

          let otherNupkgPath = copyNupkgFromAssets fs ``samples8 NetCoreApp2.0`` testDir

          merge fs sourceNupkgPath otherNupkgPath "netcoreapp2.0"
          |> checkExitCodeZero

          { PackageName = pkg
            Files = ``samples8 NetCoreApp2.0``.Files
            Nuspec =
              deps
              @ ``samples8 NetCoreApp2.0``.Nuspec }
          |> checkNupkgContent fs (testDir/"out") sourceNupkgPath

          return ()
          })

      yield!
        [ "FsCheck", "3.0.0-alpha2",
            [ GroupWithTFM(".NETFramework4.5.2",
                [ PackageDep("FSharp.Core", "4.1.0", None) ])
              GroupWithTFM(".NETStandard1.6",
                [ PackageDep("NETStandard.Library", "1.6.1", Some ("Build,Analyzers"))
                  PackageDep("FSharp.Core", "4.1.18", Some ("Build,Analyzers")) ]) ]
          "Argu", "4.0.0",
            [ GroupWithTFM(".NETFramework4.0",
                [ PackageDep("FSharp.Core", "3.1.0", None) ])
              GroupWithTFM(".NETStandard2.0",
                [ PackageDep("FSharp.Core", "4.2.1", Some ("Build,Analyzers")) ]) ]
          "Suave" , "2.2.1",
            [ GroupWithTFM(".NETFramework4.0",
                [ PackageDep("FSharp.Core", "4.0.0.1", None) ])
              GroupWithTFM(".NETStandard1.6",
                [ PackageDep("NETStandard.Library", "1.6.1", Some ("Build,Analyzers"))
                  PackageDep("FSharp.Core", "4.1.17", Some ("Build,Analyzers"))
                  PackageDep("System.Data.Common", "4.1.0", Some ("Build,Analyzers"))
                  PackageDep("System.Diagnostics.Process", "4.1.0", Some ("Build,Analyzers"))
                  PackageDep("System.Security.Cryptography.Primitives", "4.3.0", Some ("Build,Analyzers"))
                  PackageDep("System.Net.Security", "4.0.0", Some ("Build,Analyzers"))
                  PackageDep("System.Globalization.Extensions", "4.3.0", Some ("Build,Analyzers"))
                  PackageDep("System.Security.Claims", "4.0.1", Some ("Build,Analyzers"))
                  PackageDep("System.Runtime.Serialization.Json", "4.0.2", Some ("Build,Analyzers")) ]) ]
          "Hopac", "0.3.23",
            [ GroupWithTFM(".NETFramework4.5",
                [ PackageDep("FSharp.Core", "3.1.2.5", None) ])
              GroupWithTFM(".NETStandard1.6",
                [ PackageDep("FSharp.Core", "[4.0.1.7-alpha, )", None)
                  PackageDep("NETStandard.Library", "[1.6.0, )", None) ]) ]
      //     "Unquote", "4.0.0", [
      //       net45
      // <group targetFramework=".NETStandard2.0">
      //   <dependency id="FSharp.Core" version="4.2.3" exclude="Build,Analyzers"></dependency>
      // </group>
      //     ]
           ]
        |> List.map (fun (pkg, ver, deps) -> rw pkg ver deps)

    ]
  ]
  |> testList "suite"
