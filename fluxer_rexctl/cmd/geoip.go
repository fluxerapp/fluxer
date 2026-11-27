// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	geoIPPrefix    = "geoip-"
	geoIPSuffix    = ".mmdb"
	mmdbMarker     = "\xab\xcd\xefMaxMind.com"
	mmdbTypeKey    = "database_type"
	mmdbMetaWindow = 128 << 10
)

var errNotMMDB = errors.New("geoip: not a MaxMind database")

func mmdbType(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil {
		return "", err
	}
	if !fi.Mode().IsRegular() {
		return "", errNotMMDB
	}
	n := min(fi.Size(), mmdbMetaWindow)
	buf := make([]byte, n)
	if _, err := f.ReadAt(buf, fi.Size()-n); err != nil && !errors.Is(err, io.EOF) {
		return "", err
	}
	i := bytes.LastIndex(buf, []byte(mmdbMarker))
	if i < 0 {
		return "", errNotMMDB
	}
	meta := buf[i+len(mmdbMarker):]
	j := bytes.Index(meta, []byte(mmdbTypeKey))
	if j < 0 || j+len(mmdbTypeKey) >= len(meta) {
		return "", nil
	}
	rest := meta[j+len(mmdbTypeKey):]
	if rest[0]>>5 != 2 {
		return "", nil
	}
	size := int(rest[0] & 0x1f)
	if size >= 29 || 1+size > len(rest) {
		return "", nil
	}
	return string(rest[1 : 1+size]), nil
}

func checkGeoIPFile(path string) error {
	kind, err := mmdbType(path)
	switch {
	case errors.Is(err, errNotMMDB):
		return ui.Refused(fmt.Sprintf("%s isn't a MaxMind database.", path), "Pass a GeoLite2 or GeoIP2 City or Country database, a file that ends in .mmdb.")
	case err != nil:
		return ui.Failed(fmt.Sprintf("Couldn't read %s.", path), "Pass the path to a MaxMind .mmdb file on this machine.")
	}
	lower := strings.ToLower(kind)
	if kind != "" && !strings.Contains(lower, "city") && !strings.Contains(lower, "country") && !strings.Contains(lower, "enterprise") {
		return ui.Refused(fmt.Sprintf("%s is a %s database, and Fluxer needs one that knows countries.", path, kind), "Pass a GeoLite2 or GeoIP2 City or Country database.")
	}
	return nil
}

func geoIPFileName(sum []byte) string {
	return geoIPPrefix + hex.EncodeToString(sum[:6]) + geoIPSuffix
}

func geoIPName(src string) (string, error) {
	f, err := os.Open(src)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return geoIPFileName(h.Sum(nil)), nil
}

func copyGeoIP(src, dir string) (string, func(), error) {
	readFailed := ui.Failed(fmt.Sprintf("Couldn't read %s.", src), "Pass the path to a MaxMind .mmdb file on this machine.")
	name, err := geoIPName(src)
	if err != nil {
		return "", nil, readFailed
	}
	undo, err := saveGeoIP(src, dir, name)
	if err != nil {
		return "", nil, err
	}
	return name, undo, nil
}

func saveGeoIP(src, dir, name string) (func(), error) {
	saveFailed := func(err error) error {
		return ui.Wrap(err, "Couldn't save the GeoIP database.", "Check that you can write to "+dir+", then try again.")
	}
	dest := filepath.Join(dir, name)
	if fileExists(dest) {
		return func() {}, nil
	}
	in, err := os.Open(src)
	if err != nil {
		return nil, ui.Failed(fmt.Sprintf("Couldn't read %s.", src), "Pass the path to a MaxMind .mmdb file on this machine.")
	}
	defer in.Close()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, saveFailed(err)
	}
	tmp, err := os.CreateTemp(dir, "."+geoIPPrefix+"*.tmp")
	if err != nil {
		return nil, saveFailed(err)
	}
	defer os.Remove(tmp.Name())
	defer tmp.Close()
	h := sha256.New()
	if _, err := io.Copy(io.MultiWriter(tmp, h), in); err != nil {
		var pe *os.PathError
		if errors.As(err, &pe) && pe.Path == src {
			return nil, ui.Failed(fmt.Sprintf("Couldn't read %s.", src), "Pass the path to a MaxMind .mmdb file on this machine.")
		}
		return nil, saveFailed(err)
	}
	if geoIPFileName(h.Sum(nil)) != name {
		return nil, ui.Failed(fmt.Sprintf("%s changed while rexctl copied it.", src), "Wait until the file is complete, then try again.")
	}
	if err := tmp.Chmod(0o644); err != nil {
		return nil, saveFailed(err)
	}
	if err := tmp.Sync(); err != nil {
		return nil, saveFailed(err)
	}
	if err := tmp.Close(); err != nil {
		return nil, saveFailed(err)
	}
	if err := os.Rename(tmp.Name(), dest); err != nil {
		return nil, saveFailed(err)
	}
	return func() { os.Remove(dest) }, nil
}

func geoIPChanged(changes map[string]*string) bool {
	_, named := changes[settings.GeoIPDatabase]
	_, raw := changes[settings.GeoIPVar]
	return named || raw
}

func pruneGeoIP(inst *instance.Instance) {
	if legacyCompose(inst) {
		return
	}
	var keep []string
	for _, k := range []string{settings.GeoIPDatabase, settings.GeoIPVar} {
		keep = append(keep, strings.TrimPrefix(strings.TrimSpace(inst.State.Settings[k]), settings.GeoIPMountPoint))
	}
	dir := inst.Path(instance.GeoIPDir)
	entries, err := os.ReadDir(dir)
	if err != nil {
		return
	}
	for _, e := range entries {
		name := e.Name()
		if !slices.Contains(keep, name) && strings.HasPrefix(name, geoIPPrefix) && strings.HasSuffix(name, geoIPSuffix) && e.Type().IsRegular() {
			os.Remove(filepath.Join(dir, name))
		}
	}
}

func cleanPath(path string) string {
	path = filepath.Clean(path)
	if real, err := filepath.EvalSymlinks(path); err == nil {
		return real
	}
	return path
}

func geoIPUnder(path, dir string) bool {
	if path == "" || strings.HasPrefix(path, "s3://") {
		return false
	}
	rel, err := filepath.Rel(filepath.Clean(dir), filepath.Clean(path))
	return err == nil && filepath.IsLocal(rel)
}

func mountedFile(svc types.ServiceConfig, path string) string {
	src := ""
	for _, v := range svc.Volumes {
		if v.Type == types.VolumeTypeBind && v.Source != "" && geoIPUnder(path, v.Target) {
			rel, _ := filepath.Rel(filepath.Clean(v.Target), filepath.Clean(path))
			src = filepath.Join(v.Source, rel)
		}
	}
	return src
}

func mountedAsIs(ctx context.Context, app *App, inst *instance.Instance, path string) bool {
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return false
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return false
	}
	want := cleanPath(path)
	for _, name := range []string{"api", "worker"} {
		svc, ok := p.Model.Services[name]
		if !ok {
			return false
		}
		if src := mountedFile(svc, path); src == "" || cleanPath(src) != want {
			return false
		}
	}
	return true
}

func stageGeoIP(ctx context.Context, app *App, inst *instance.Instance, value string) (string, func(), error) {
	keep := func() {}
	if strings.HasPrefix(value, "s3://") || native.MappedPath(value) {
		return value, keep, nil
	}
	if legacyCompose(inst) {
		if !fileExists(value) {
			return value, keep, nil
		}
		if filepath.IsAbs(value) && mountedAsIs(ctx, app, inst, value) {
			return value, keep, checkGeoIPFile(value)
		}
		return "", nil, ui.Refused(fmt.Sprintf("On the older layout, Fluxer's containers can't see %s on this machine.", value),
			"Mount its folder into api and worker with rexctl config edit --compose and set geoip.database to the path inside the containers, or move to the new layout with rexctl migrate first.")
	}
	if !filepath.IsAbs(value) {
		if abs, err := filepath.Abs(value); err == nil {
			value = abs
		}
	}
	if err := checkGeoIPFile(value); err != nil {
		return "", nil, err
	}
	name, undo, err := copyGeoIP(value, inst.Path(instance.GeoIPDir))
	if err != nil {
		return "", nil, err
	}
	return settings.GeoIPMountPoint + name, undo, nil
}
