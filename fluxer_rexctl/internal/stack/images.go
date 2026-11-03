// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	_ "embed"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path"
	"strconv"
	"strings"

	"go.yaml.in/yaml/v4"
)

const ImageSetEnv = "REXCTL_IMAGE_SET"

//go:embed imageset.json
var imageSetJSON []byte

type ImageSet struct {
	Release string            `json:"release"`
	Images  map[string]string `json:"images"`
}

func EmbeddedImages() (ImageSet, error) {
	var s ImageSet
	data := imageSetJSON
	if p := os.Getenv(ImageSetEnv); p != "" {
		b, err := os.ReadFile(p)
		if err != nil {
			return s, err
		}
		data = b
	}
	if err := json.Unmarshal(data, &s); err != nil {
		return s, err
	}
	return s, nil
}

func (s ImageSet) Image(service string) (string, error) {
	ref, ok := s.Images[service]
	if !ok {
		return "", errors.New("stack: no image for " + service)
	}
	return ref, nil
}

type imageLine struct {
	service string
	line    int
	column  int
	value   string
}

func imageLines(compose []byte) ([]imageLine, error) {
	var doc yaml.Node
	if err := yaml.Unmarshal(compose, &doc); err != nil {
		return nil, fmt.Errorf("stack: the compose file isn't valid YAML: %w", err)
	}
	if doc.Kind != yaml.DocumentNode || len(doc.Content) == 0 {
		return nil, errors.New("stack: the compose file is empty")
	}
	services := mappingValue(doc.Content[0], "services")
	if services == nil || services.Kind != yaml.MappingNode {
		return nil, errors.New("stack: the compose file has no services")
	}
	var out []imageLine
	for i := 0; i+1 < len(services.Content); i += 2 {
		image := mappingValue(services.Content[i+1], "image")
		if image == nil || image.Kind != yaml.ScalarNode {
			continue
		}
		out = append(out, imageLine{service: services.Content[i].Value, line: image.Line, column: image.Column, value: image.Value})
	}
	return out, nil
}

func ComposeImages(compose []byte) map[string]string {
	found, err := imageLines(compose)
	if err != nil {
		return nil
	}
	out := make(map[string]string, len(found))
	for _, img := range found {
		out[img.service] = img.value
	}
	return out
}

func mappingValue(n *yaml.Node, key string) *yaml.Node {
	if n == nil || n.Kind != yaml.MappingNode {
		return nil
	}
	for i := 0; i+1 < len(n.Content); i += 2 {
		if n.Content[i].Value == key {
			return n.Content[i+1]
		}
	}
	return nil
}

func pinImages(compose []byte, release string, images map[string]string) ([]byte, error) {
	if len(images) == 0 {
		return compose, nil
	}
	found, err := imageLines(compose)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(string(compose), "\n")
	for _, img := range found {
		ref, pinned := images[img.service]
		var value string
		switch {
		case strings.HasPrefix(img.value, "${FLUXER_REGISTRY"):
			if !pinned {
				return nil, fmt.Errorf("stack: image set %s has no image for %s", release, img.service)
			}
			value = ref
		case !pinned:
			continue
		default:
			if name, _, ok := defaultedVar(img.value); ok {
				value = "${" + name + ":-" + ref + "}"
			} else {
				value = ref
			}
		}
		i := img.line - 1
		if i < 0 || i >= len(lines) || img.column-1 > len(lines[i]) {
			return nil, fmt.Errorf("stack: can't place the image for %s", img.service)
		}
		lines[i] = lines[i][:img.column-1] + value
	}
	return []byte(strings.Join(lines, "\n")), nil
}

func UnpinImages(compose []byte) []byte {
	shipped, err := ReadFile(Embedded(), ComposeFile)
	if err != nil {
		return compose
	}
	templates, err := imageLines(shipped)
	if err != nil {
		return compose
	}
	var prefix, suffix string
	for _, t := range templates {
		head, tag, ok := strings.Cut(t.value, ":${")
		if slash := strings.LastIndex(head, "/"); ok && slash >= 0 && strings.HasPrefix(t.value, "${FLUXER_REGISTRY") {
			prefix, suffix = head[:slash+1], ":${"+tag
			break
		}
	}
	found, err := imageLines(compose)
	if err != nil || prefix == "" {
		return compose
	}
	lines := strings.Split(string(compose), "\n")
	for _, img := range found {
		repo, digest, pinned := strings.Cut(img.value, "@sha256:")
		i := img.line - 1
		if !pinned || len(digest) != 64 || i < 0 || i >= len(lines) || img.column-1 > len(lines[i]) {
			continue
		}
		lines[i] = lines[i][:img.column-1] + prefix + path.Base(repo) + suffix
	}
	return []byte(strings.Join(lines, "\n"))
}

func defaultedVar(value string) (name, def string, ok bool) {
	inner, ok := strings.CutPrefix(value, "${")
	if !ok {
		return "", "", false
	}
	inner, ok = strings.CutSuffix(inner, "}")
	if !ok {
		return "", "", false
	}
	name, def, ok = strings.Cut(inner, ":-")
	if !ok || name == "" || strings.ContainsAny(name, "${}") {
		return "", "", false
	}
	return name, def, true
}

func PostgresMajor(ref string) (int, error) {
	ref = strings.TrimSpace(ref)
	if at := strings.Index(ref, "@"); at >= 0 {
		ref = ref[:at]
	}
	base := path.Base(ref)
	_, tag, ok := strings.Cut(base, ":")
	if !ok || tag == "" {
		return 0, fmt.Errorf("stack: %q has no version tag", ref)
	}
	digits := tag
	for i, r := range tag {
		if r < '0' || r > '9' {
			digits = tag[:i]
			break
		}
	}
	major, err := strconv.Atoi(digits)
	if err != nil || major == 0 {
		return 0, fmt.Errorf("stack: %q has no major version", ref)
	}
	return major, nil
}
