FROM golang:1.25.5

WORKDIR /workspace

COPY go.mod go.sum ./
RUN go mod download

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates coreutils && \
    rm -rf /var/lib/apt/lists/*

COPY . .

RUN ls -la tests/integration/fixtures/ || echo "No fixtures found"

ENV CGO_ENABLED=0

# Pre-compile tests to fail early on build errors and speed up runtime
RUN go test -c -o integration.test -v ./tests/integration

CMD ["sh", "-c", "echo 'Starting integration tests...' && stdbuf -oL -eL ./integration.test -test.v -test.count=1 -test.timeout=0"]