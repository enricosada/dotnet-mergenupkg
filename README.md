# dotnet-mergenupkg

Merge two nuget package dependencies

Useful to add .NET Core dependencies (assemblies, nuspec deps) built with .NET CLI inside 
an existing .nupkg

```
dotnet mergenupkg --source a.nupkg --other b.nupkg --framework netstandard1.5
```

see `dotnet mergenupkg --help` for more info
