# dotnet-mergenupkg

[![Build status](https://ci.appveyor.com/api/projects/status/nrms1e19gmcl2rty?svg=true)](https://ci.appveyor.com/project/enricosada/dotnet-mergenupkg)
[![Build status](https://travis-ci.org/enricosada/dotnet-mergenupkg.svg)](https://travis-ci.org/enricosada/dotnet-mergenupkg)

Merge two nuget package

Useful to add .NET Core dependencies (assemblies, nuspec deps, tools) built with .NET Core Sdk inside 
an existing .nupkg

```
dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard2.0
```

The above command will modify the package `a.nupkg` adding the package dependencies and files (assemblies, xmldoc) for target framework `netstandard2.0` from package `b.nupkg`

With `--tools` merge the dotnet tools instead of lib, to bundle a .net core framework dependent app, who can be useful if a package need to run a tool from a known location (like in a `.Sdk` package)

see `dotnet mergenupkg --help` for more info

# Usage

Add it as .NET Tool

```bash
dotnet tool install -g dotnet-mergenupkg --verson 3.*
```

After that, it can be invoked with

```
dotnet mergenupkg --help
```

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
