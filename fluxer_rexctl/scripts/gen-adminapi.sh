#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
set -eu
cd "$(dirname "$0")/.."
spec=../fluxer_admin/openapi-admin.json
go run github.com/oapi-codegen/oapi-codegen/v2/cmd/oapi-codegen@v2.7.2 -config internal/adminapi/oapi-codegen.yaml "$spec"
go run ./internal/adminapi/schemagen "$spec" internal/adminapi/configschema.json
gofmt -w internal/adminapi/gen.go
