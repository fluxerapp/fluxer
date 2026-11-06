// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import "testing"

func TestSetsImageOnlyCountsFluxerImages(t *testing.T) {
	images := map[string]string{"api": "ghcr.io/fluxerapp/fluxer-api@sha256:1", "postgres": "postgres:16-alpine@sha256:2"}
	if setsImage([]byte("services:\n  postgres:\n    image: postgres:16.9-alpine\n"), images) {
		t.Fatal("custom postgres image counted as replacing a Fluxer image")
	}
	if !setsImage([]byte("services:\n  api:\n    image: example/api:dev\n"), images) {
		t.Fatal("custom api image didn't count")
	}
}
