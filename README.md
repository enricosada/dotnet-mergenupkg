# dotnet-mergenupkg

[![Build status](https://ci.appveyor.com/api/projects/status/nrms1e19gmcl2rty?svg=true)](https://ci.appveyor.com/project/enricosada/dotnet-mergenupkg)
[![Build status](https://travis-ci.org/enricosada/dotnet-mergenupkg.svg)](https://travis-ci.org/enricosada/dotnet-mergenupkg)

Merge two nuget package dependencies

Useful to add .NET Core dependencies (assemblies, nuspec deps) built with .NET Core Sdk inside 
an existing .nupkg

```
dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard1.6
```

see `dotnet mergenupkg --help` for more info

# Usage

Add it as .NET CLI tool

```xml
  <ItemGroup>
    <DotNetCliToolReference Include="dotnet-mergenupkg" Version="2.0.*" />
  </ItemGroup>
```

After a `restore`, can be invoked from same directory with

```
dotnet mergenupkg --help
```

**NOTE** like every .net cli tool, must run in msbuild project file directory where is declared.
So if you run it from build script, change the current directory before execute the command.



## Build

Clone repo.

Run:

```bash
dotnet build
```

To run tests:

```bash
dotnet test -v n
```

To create packages:

```bash
dotnet pack
```

will create packages in `bin\nupkg`

pass `/p:Version=1.2.3` to create a package with version `1.2.3`
