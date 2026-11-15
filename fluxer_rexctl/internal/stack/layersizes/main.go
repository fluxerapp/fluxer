// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"maps"
	"net/http"
	"net/url"
	"os"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/distribution/reference"
	"github.com/klauspost/compress/zstd"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var platforms = []string{"linux/amd64", "linux/arm64"}

const (
	mediaIndex     = "application/vnd.oci.image.index.v1+json"
	mediaList      = "application/vnd.docker.distribution.manifest.list.v2+json"
	mediaManifest  = "application/vnd.oci.image.manifest.v1+json"
	mediaDockerV2  = "application/vnd.docker.distribution.manifest.v2+json"
	parallelLayers = 4
	maxManifest    = 4 << 20
)

func main() {
	setPath := flag.String("set", "internal/stack/imageset.json", "the image set file to add sizes to")
	cachePath := flag.String("cache", "", "a file that keeps layer sizes between runs")
	pin := flag.Bool("pin", false, "pin 3rd party images in the compose file to the digest of the tag")
	flag.Parse()
	if err := run(context.Background(), *setPath, *cachePath, *pin); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type manifest struct {
	MediaType string `json:"mediaType"`
	Manifests []struct {
		MediaType string `json:"mediaType"`
		Digest    string `json:"digest"`
		Platform  struct {
			OS           string `json:"os"`
			Architecture string `json:"architecture"`
		} `json:"platform"`
	} `json:"manifests"`
	Config struct {
		Digest string `json:"digest"`
	} `json:"config"`
	Layers []struct {
		MediaType string `json:"mediaType"`
		Digest    string `json:"digest"`
		Size      uint64 `json:"size"`
	} `json:"layers"`
}

type blob struct {
	repo      repo
	digest    string
	mediaType string
	size      uint64
	diffID    string
}

func run(ctx context.Context, setPath, cachePath string, pin bool) error {
	data, err := os.ReadFile(setPath)
	if err != nil {
		return err
	}
	var set stack.ImageSet
	if err := json.Unmarshal(data, &set); err != nil {
		return err
	}
	third, err := thirdParty(stack.Embedded())
	if err != nil {
		return err
	}
	c := &client{http: &http.Client{Timeout: 30 * time.Minute}, tokens: map[string]string{}}
	if pin {
		if err := pinThirdParty(ctx, c, &set, third); err != nil {
			return err
		}
	}
	var refs []string
	if !pin {
		refs = slices.Collect(maps.Values(third))
	}
	for _, ref := range set.Images {
		refs = append(refs, ref)
	}
	slices.Sort(refs)
	refs = slices.Compact(refs)

	cache := map[string]stack.Layer{}
	if cachePath != "" {
		if b, err := os.ReadFile(cachePath); err == nil {
			if err := json.Unmarshal(b, &cache); err != nil {
				return fmt.Errorf("%s: %w", cachePath, err)
			}
		}
	}

	set.Layers = map[string]stack.Layer{}
	set.Platforms = map[string]map[string][]string{}
	var todo []blob
	queued := map[string]bool{}
	for _, ref := range refs {
		r, err := parse(ref)
		if err != nil {
			return err
		}
		top, _, err := c.manifest(ctx, r, r.ref)
		if err != nil {
			return fmt.Errorf("%s: %w", ref, err)
		}
		for _, p := range platforms {
			m, err := c.platformManifest(ctx, r, top, p)
			if err != nil {
				return fmt.Errorf("%s %s: %w", ref, p, err)
			}
			diffIDs, err := c.diffIDs(ctx, r, m.Config.Digest)
			if err != nil {
				return fmt.Errorf("%s %s: %w", ref, p, err)
			}
			if len(diffIDs) != len(m.Layers) {
				return fmt.Errorf("%s %s: %d layers but %d diff ids", ref, p, len(m.Layers), len(diffIDs))
			}
			if set.Platforms[p] == nil {
				set.Platforms[p] = map[string][]string{}
			}
			digests := make([]string, len(m.Layers))
			for i, l := range m.Layers {
				digests[i] = l.Digest
				if cached, ok := cache[l.Digest]; ok && cached.DiffID == diffIDs[i] {
					set.Layers[l.Digest] = cached
					continue
				}
				if !queued[l.Digest] {
					queued[l.Digest] = true
					todo = append(todo, blob{repo: r, digest: l.Digest, mediaType: l.MediaType, size: l.Size, diffID: diffIDs[i]})
				}
			}
			set.Platforms[p][ref] = digests
		}
	}

	var mu sync.Mutex
	var errs []error
	work := make(chan blob)
	var wg sync.WaitGroup
	for range parallelLayers {
		wg.Go(func() {
			for b := range work {
				n, err := c.unpacked(ctx, b)
				mu.Lock()
				if err != nil {
					errs = append(errs, fmt.Errorf("%s: %w", b.digest, err))
				} else {
					set.Layers[b.digest] = stack.Layer{DiffID: b.diffID, Compressed: b.size, Unpacked: n}
				}
				mu.Unlock()
			}
		})
	}
	for _, b := range todo {
		work <- b
	}
	close(work)
	wg.Wait()
	if err := errors.Join(errs...); err != nil {
		return err
	}

	if cachePath != "" {
		maps.Copy(cache, set.Layers)
		if err := writeJSON(cachePath, cache); err != nil {
			return err
		}
	}
	fmt.Printf("sized %d images, %d layers (%d read now)\n", len(refs), len(set.Layers), len(todo))
	return writeJSON(setPath, set)
}

func thirdParty(set *stack.Set) (map[string]string, error) {
	files := []string{stack.ComposeFile}
	voice, err := fs.Glob(set.Files, "voice/*.compose.yml")
	if err != nil {
		return nil, err
	}
	files = append(files, voice...)
	out := map[string]string{}
	for _, name := range files {
		compose, err := stack.ReadFile(set, name)
		if err != nil {
			return nil, err
		}
		for svc, value := range stack.ComposeImages(compose) {
			if strings.HasPrefix(value, "${FLUXER_REGISTRY") {
				continue
			}
			if def, ok := stack.ImageDefault(value); ok {
				value = def
			}
			if seen, ok := out[svc]; ok && seen != value {
				return nil, fmt.Errorf("%s runs %s in one compose file and %s in another, so it can't have one pinned image", svc, seen, value)
			}
			out[svc] = value
		}
	}
	return out, nil
}

type resolver interface {
	digest(ctx context.Context, ref string) (string, error)
}

func (c *client) digest(ctx context.Context, ref string) (string, error) {
	r, err := parse(ref)
	if err != nil {
		return "", err
	}
	_, d, err := c.manifest(ctx, r, r.ref)
	return d, err
}

func pinThirdParty(ctx context.Context, c resolver, set *stack.ImageSet, third map[string]string) error {
	if set.Images == nil {
		set.Images = map[string]string{}
	}
	resolved := map[string]string{}
	for _, svc := range slices.Sorted(maps.Keys(third)) {
		ref := third[svc]
		if _, ok := set.Images[svc]; ok {
			return fmt.Errorf("%s already has an image in the image set", svc)
		}
		if strings.Contains(ref, "@") {
			set.Images[svc] = ref
			continue
		}
		if _, ok := resolved[ref]; !ok {
			d, err := c.digest(ctx, ref)
			if err != nil {
				return fmt.Errorf("%s: %w", ref, err)
			}
			resolved[ref] = d
		}
		set.Images[svc] = ref + "@" + resolved[ref]
	}
	return nil
}

func writeJSON(path string, v any) error {
	b, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, append(b, '\n'), 0o644)
}

type repo struct {
	registry string
	name     string
	ref      string
}

func parse(s string) (repo, error) {
	named, err := reference.ParseNormalizedNamed(s)
	if err != nil {
		return repo{}, fmt.Errorf("%s: %w", s, err)
	}
	r := repo{registry: reference.Domain(named), name: reference.Path(named), ref: "latest"}
	if r.registry == "docker.io" {
		r.registry = "registry-1.docker.io"
	}
	if d, ok := named.(reference.Digested); ok {
		r.ref = d.Digest().String()
	} else if t, ok := named.(reference.Tagged); ok {
		r.ref = t.Tag()
	}
	return r, nil
}

type client struct {
	http   *http.Client
	mu     sync.Mutex
	tokens map[string]string
}

func (c *client) get(ctx context.Context, r repo, path, accept string) (*http.Response, error) {
	u := "https://" + r.registry + "/v2/" + r.name + path
	for attempt := 0; ; attempt++ {
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, u, nil)
		if err != nil {
			return nil, err
		}
		if accept != "" {
			req.Header.Set("Accept", accept)
		}
		c.mu.Lock()
		tok := c.tokens[r.registry+"/"+r.name]
		c.mu.Unlock()
		if tok != "" {
			req.Header.Set("Authorization", "Bearer "+tok)
		}
		res, err := c.http.Do(req)
		if err != nil {
			return nil, err
		}
		if res.StatusCode == http.StatusUnauthorized && attempt == 0 {
			challenge := res.Header.Get("WWW-Authenticate")
			res.Body.Close()
			if err := c.login(ctx, r, challenge); err != nil {
				return nil, err
			}
			continue
		}
		if res.StatusCode != http.StatusOK {
			res.Body.Close()
			return nil, fmt.Errorf("GET %s: %s", u, res.Status)
		}
		return res, nil
	}
}

func (c *client) login(ctx context.Context, r repo, challenge string) error {
	params := map[string]string{}
	rest, ok := strings.CutPrefix(challenge, "Bearer ")
	if !ok {
		return fmt.Errorf("%s asks for %q, which this tool can't answer", r.registry, challenge)
	}
	for _, part := range strings.Split(rest, ",") {
		k, v, ok := strings.Cut(strings.TrimSpace(part), "=")
		if ok {
			params[k] = strings.Trim(v, `"`)
		}
	}
	q := url.Values{}
	if s := params["service"]; s != "" {
		q.Set("service", s)
	}
	q.Set("scope", "repository:"+r.name+":pull")
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, params["realm"]+"?"+q.Encode(), nil)
	if err != nil {
		return err
	}
	if tok := os.Getenv("GHCR_TOKEN"); tok != "" && r.registry == "ghcr.io" {
		req.SetBasicAuth("token", tok)
	}
	res, err := c.http.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		return fmt.Errorf("token for %s/%s: %s", r.registry, r.name, res.Status)
	}
	var body struct {
		Token       string `json:"token"`
		AccessToken string `json:"access_token"`
	}
	if err := json.NewDecoder(io.LimitReader(res.Body, maxManifest)).Decode(&body); err != nil {
		return err
	}
	tok := body.Token
	if tok == "" {
		tok = body.AccessToken
	}
	c.mu.Lock()
	c.tokens[r.registry+"/"+r.name] = tok
	c.mu.Unlock()
	return nil
}

func (c *client) manifest(ctx context.Context, r repo, ref string) (manifest, string, error) {
	res, err := c.get(ctx, r, "/manifests/"+ref, strings.Join([]string{mediaIndex, mediaList, mediaManifest, mediaDockerV2}, ", "))
	if err != nil {
		return manifest{}, "", err
	}
	defer res.Body.Close()
	data, err := io.ReadAll(io.LimitReader(res.Body, maxManifest+1))
	if err != nil {
		return manifest{}, "", err
	}
	if len(data) > maxManifest {
		return manifest{}, "", fmt.Errorf("manifest %s is larger than %d bytes", ref, maxManifest)
	}
	var m manifest
	if err := json.Unmarshal(data, &m); err != nil {
		return manifest{}, "", err
	}
	if m.MediaType == "" {
		m.MediaType = res.Header.Get("Content-Type")
	}
	sum := sha256.Sum256(data)
	return m, "sha256:" + hex.EncodeToString(sum[:]), nil
}

func (c *client) platformManifest(ctx context.Context, r repo, top manifest, platform string) (manifest, error) {
	if top.MediaType != mediaIndex && top.MediaType != mediaList {
		return manifest{}, fmt.Errorf("has a single-platform manifest, so it has no %s image", platform)
	}
	goos, arch, _ := strings.Cut(platform, "/")
	for _, d := range top.Manifests {
		if d.Platform.OS == goos && d.Platform.Architecture == arch {
			m, _, err := c.manifest(ctx, r, d.Digest)
			return m, err
		}
	}
	return manifest{}, fmt.Errorf("has no %s image", platform)
}

func (c *client) diffIDs(ctx context.Context, r repo, digest string) ([]string, error) {
	res, err := c.get(ctx, r, "/blobs/"+digest, "")
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	var cfg struct {
		RootFS struct {
			DiffIDs []string `json:"diff_ids"`
		} `json:"rootfs"`
	}
	if err := json.NewDecoder(io.LimitReader(res.Body, maxManifest)).Decode(&cfg); err != nil {
		return nil, err
	}
	return cfg.RootFS.DiffIDs, nil
}

func (c *client) unpacked(ctx context.Context, b blob) (uint64, error) {
	res, err := c.get(ctx, b.repo, "/blobs/"+b.digest, "")
	if err != nil {
		return 0, err
	}
	defer res.Body.Close()
	var r io.Reader
	switch {
	case strings.HasSuffix(b.mediaType, "zstd"):
		z, err := zstd.NewReader(res.Body)
		if err != nil {
			return 0, err
		}
		defer z.Close()
		r = z
	case strings.HasSuffix(b.mediaType, "gzip"):
		g, err := gzip.NewReader(res.Body)
		if err != nil {
			return 0, err
		}
		defer g.Close()
		r = g
	case strings.HasSuffix(b.mediaType, ".tar") || strings.HasSuffix(b.mediaType, "diff.tar"):
		r = res.Body
	default:
		return 0, fmt.Errorf("has media type %s, which this tool can't unpack", b.mediaType)
	}
	h := sha256.New()
	n, err := io.Copy(h, r)
	if err != nil {
		return 0, err
	}
	if got := "sha256:" + hex.EncodeToString(h.Sum(nil)); got != b.diffID {
		return 0, fmt.Errorf("unpacks to %s, but the image config lists %s", got, b.diffID)
	}
	return uint64(n), nil
}
