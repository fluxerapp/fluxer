FROM golang:1.25.5-alpine AS build

WORKDIR /build

COPY fluxer_app/proxy/go.mod ./
COPY fluxer_app/proxy/go.sum* ./
RUN go mod download

COPY fluxer_app/proxy/ .

COPY fluxer_app/dist/index.html assets/index.html
COPY fluxer_app/dist/manifest.json assets/manifest.json
COPY fluxer_app/dist/sw.js* assets/
COPY fluxer_app/dist/version.json assets/version.json

ARG TARGETOS=linux
ARG TARGETARCH=amd64
RUN CGO_ENABLED=0 GOOS=${TARGETOS} GOARCH=${TARGETARCH} go build -o fluxer-app-proxy

FROM alpine:3.19

RUN apk add --no-cache ca-certificates curl

WORKDIR /app

COPY --from=build /build/fluxer-app-proxy /app/fluxer-app-proxy

USER nobody

EXPOSE 8080
ENV PORT=8080

ENTRYPOINT ["/app/fluxer-app-proxy"]
