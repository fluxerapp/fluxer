// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	_ "embed"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

//go:generate sh -c "cd ../../../fluxer_native && GOTOOLCHAIN=local go run ./cmd/fluxerd print-env --catalog --json > ../fluxer_rexctl/internal/settings/catalog.json"

//go:embed catalog.json
var catalogJSON []byte

var embeddedCatalog = sync.OnceValue(func() []stack.Setting {
	c, err := stack.ParseCatalog(catalogJSON)
	if err != nil {
		panic(err)
	}
	return c
})

func Catalog() []stack.Setting { return embeddedCatalog() }

func NativeSet(release string) *stack.Set {
	return stack.CatalogSet(release, Catalog())
}
