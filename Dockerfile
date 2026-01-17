FROM docker.io/nojaf/fable:2.7
WORKDIR /build
COPY .config .
RUN dotnet tool restore
COPY paket.* .
RUN dotnet paket restore
COPY yarn.lock .
RUN yarn install --frozen-lockfile
COPY . .
RUN dotnet fake build -t Build
ENTRYPOINT dotnet fake build -t Run