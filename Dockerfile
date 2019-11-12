FROM vbfox/fable-build
RUN dotnet tool restore
RUN dotnet fake build -t Build