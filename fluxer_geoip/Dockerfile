FROM golang:1.25.5 AS builder
WORKDIR /src

ENV CGO_ENABLED=0 GOOS=linux

COPY go.mod go.sum ./
RUN go mod download

COPY . .
RUN go build -trimpath -buildvcs=false -ldflags="-s -w" -o /out/fluxer_geoip ./...

FROM gcr.io/distroless/static:nonroot
WORKDIR /app

VOLUME ["/data"]

EXPOSE 8080

COPY --from=builder /out/fluxer_geoip /app/fluxer_geoip
USER nonroot:nonroot
ENTRYPOINT ["/app/fluxer_geoip"]
