# dotnet-mergenupkg

Merge two nuget package dependencies

Useful to add .NET Core dependencies (assemblies, nuspec deps) built with .NET CLI inside 
an existing .nupkg

```
dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard1.6
```

see `dotnet mergenupkg --help` for more info

# Installation

It's a normal console app, so you can download and run it with .NET Core.

But it can be used as a .NET CLI Extension ( **PREFERRED WAY** ).

1. add it to tools in project.json
2. dotnet restore
3. run from same directory of project.json


## add it to tools in project.json

so just add in `project.json` inside tools

like:

```json
    "tools": {
        "dotnet-mergenupkg": { "version": "1.0.*" }
    },
```

## dotnet restore

Run `dotnet restore` (it's like an additional package)

## run from same directory of project.json

After that 

```
dotnet mergenupkg --help
``

**NOTE** the command must run in the `project.json` directory, 
because each project can have different tools.
So if you run it from build script, change the current directory before execute the command.

