FROM nojaf/fable
WORKDIR /build
COPY . ./
RUN dotnet tool restore
RUN dotnet fake build -t Build