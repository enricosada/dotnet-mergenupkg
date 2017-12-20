module DotnetMergeNupkg.TestAssets

open FileUtils

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

let ``samples4 fallbackgrp`` =
  { ProjDir = "sample4-fallbackgrp";
    PackageName = "Lib4";
    AssemblyName = "Lib4";
    Nuspec =
       [ FallbackGroup(
          dependencies =
            [ PackageDep (id = "FSharp.Core", version = "4.0.0", exclude = None) ]) ]
    Assembly = "lib"/"net40"/"Lib4.dll" }

let ``samples5 fallbackgrp spec201108`` =
  { ProjDir = "sample5-fallbackgrp-spec201108";
    PackageName = "Lib5";
    AssemblyName = "Lib5";
    Nuspec =
       [ FallbackGroup(
          dependencies =
            [ PackageDep (id = "Argu", version = "3.2.0", exclude = None) ]) ]
    Assembly = "lib"/"net45"/"Lib5.dll" }

let ``samples6 fallbackgrp multifw`` =
  { ProjDir = "sample6-fallbackgrp-multifw";
    PackageName = "Lib6";
    AssemblyName = "Lib6";
    Nuspec =
       [ FallbackGroup(
          dependencies =
            [ PackageDep (id = "Argu", version = "3.2.0", exclude = None) ]) ]
    Assembly = "lib"/"net35"/"Lib6.dll" }

let ``samples7 fallbackgrp multifw with unknown fw`` =
  { ProjDir = "sample7-fallbackgrp-multifw-unkn";
    PackageName = "Lib7";
    AssemblyName = "Lib7";
    Nuspec =
       [ FallbackGroup(
          dependencies =
            [ PackageDep (id = "Newtonsoft.Json", version = "9.0.1", exclude = None) ]) ]
    Assembly = "lib"/"net461"/"Lib7.dll" }

let ``samples8 NetCoreApp2.0`` =
  { ProjDir = "sample8-dotnetcore-1_0";
    PackageName = "Lib8";
    AssemblyName = "Lib8";
    Nuspec = [ GroupWithTFM (tfm = ".NETCoreApp2.0", dependencies = []) ]
    Assembly = "lib"/"netcoreapp2.0"/"Lib8.dll"
  }


let allSamples =
  [ ``samples1 Net45``
    ``samples2 NetStandard2.0``
    ``samples3 NetStandard1.6``
    ``samples4 fallbackgrp``
    ``samples5 fallbackgrp spec201108``
    ``samples6 fallbackgrp multifw``
    ``samples7 fallbackgrp multifw with unknown fw``
    ``samples8 NetCoreApp2.0`` ]
