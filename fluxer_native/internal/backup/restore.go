// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bufio"
	"bytes"
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	maxSmallFile    = 1 << 20
	restoredEnvFile = "fluxer.env.restored"
)

type RestoreOptions struct {
	EnvFile  string
	Progress func(stage string)
}

func Restore(ctx context.Context, t Target, path string, opts RestoreOptions) (*Manifest, string, error) {
	progress := opts.Progress
	if progress == nil {
		progress = func(string) {}
	}
	progress(StageVerifying)
	m, err := Verify(path)
	if err != nil {
		return m, "", err
	}
	if m.Scope == ScopeWithoutUploads {
		return m, "", errors.New("backup: this backup has no uploads and only restores onto the instance that made it")
	}
	for _, v := range m.Volumes {
		if _, ok := volumeDir(t.Layout, v); !ok {
			return m, "", fmt.Errorf("backup: volume %s has no place in native mode", v)
		}
	}
	if m.Database != "" && t.externalDatabase() {
		return m, "", fmt.Errorf("backup: the database is external, restore %s from the backup into it with pg_restore", DumpName)
	}
	held, err := lockData(t.Layout)
	if err != nil {
		return m, "", err
	}
	defer held.Release()

	var pg *postgresServer
	var restoreDump func(io.Reader) error
	if m.Database != "" {
		progress(StageDatabase)
		if pg, err = t.startPostgres(ctx, t.Layout.Tmp("restore")); err != nil {
			return m, "", err
		}
		defer func() {
			if pg != nil {
				pg.stop()
			}
		}()
		restoreDump = func(r io.Reader) error { return t.restoreDump(ctx, pg.sock, r) }
	}
	files, err := unpack(path, t.Layout, m, restoreDump, progress)
	if err != nil {
		return m, "", err
	}
	if pg != nil {
		err, pg = pg.stop(), nil
		if err != nil {
			return m, "", err
		}
	}
	for _, v := range volumeDirs {
		if slices.Contains(m.Volumes, v.name) || uploadsVolume(v.name) && m.HasSkipped(SkippedUploads) || leftBehind(m, v.name) {
			continue
		}
		if err := clearDir(v.dir(t.Layout)); err != nil {
			return m, "", err
		}
	}
	progress(StageFiles)
	if err := restoreSecrets(t.Layout.SecretsFile(), files); err != nil {
		return m, "", err
	}
	if err := restoreModes(t.Layout, m, files); err != nil {
		return m, "", err
	}
	envCopy := ""
	if data, ok := files[EnvFileName]; ok && opts.EnvFile != "" {
		if envCopy, err = restoreEnvFile(opts.EnvFile, t.Layout.Data, data); err != nil {
			return m, "", err
		}
	}
	return m, envCopy, nil
}

func leftBehind(m *Manifest, volume string) bool {
	return volume == spec.SeaweedFSVolume && slices.Contains(m.Volumes, spec.ObjectsVolume)
}

func restoreEnvFile(target, dataDir string, data []byte) (string, error) {
	if cur, err := os.ReadFile(target); err == nil && bytes.Equal(cur, data) {
		return "", nil
	}
	err := writeAtomic(target, data, 0o640)
	if !errors.Is(err, fs.ErrPermission) {
		return "", err
	}
	saved := filepath.Join(dataDir, restoredEnvFile)
	if err := writeAtomic(saved, data, 0o600); err != nil {
		return "", err
	}
	return saved, nil
}

func unpack(path string, l spec.Layout, m *Manifest, restoreDump func(io.Reader) error, progress func(string)) (map[string][]byte, error) {
	files := map[string][]byte{}
	var root *os.Root
	current := ""
	var done []string
	defer func() {
		if root != nil {
			root.Close()
		}
	}()
	err := walkArchive(path, func(hdr *tar.Header, r io.Reader) error {
		if hdr.Name == DumpName {
			if restoreDump == nil {
				return nil
			}
			return restoreDump(r)
		}
		if strings.HasPrefix(hdr.Name, FilesPrefix) {
			if hdr.Typeflag == tar.TypeReg && slices.Contains([]string{EnvFileName, SecretsFileName, ModesFileName, RexctlStateName}, hdr.Name) {
				data, err := io.ReadAll(io.LimitReader(r, maxSmallFile+1))
				if err != nil {
					return err
				}
				if len(data) > maxSmallFile {
					return fmt.Errorf("%w: %s is too large", ErrDamaged, hdr.Name)
				}
				files[hdr.Name] = data
			}
			return nil
		}
		vol, inner, ok := volumeOf(hdr.Name)
		if !ok {
			return nil
		}
		if !slices.Contains(m.Volumes, vol) {
			return fmt.Errorf("%w: %s isn't in the manifest", ErrDamaged, vol)
		}
		if vol != current {
			if slices.Contains(done, vol) {
				return fmt.Errorf("%w: %s is split in the archive", ErrDamaged, vol)
			}
			if current == "" {
				progress(StageVolumes)
			}
			done = append(done, vol)
			current = vol
			if root != nil {
				root.Close()
				root = nil
			}
			dir, _ := volumeDir(l, vol)
			if err := clearDir(dir); err != nil {
				return err
			}
			var err error
			if root, err = os.OpenRoot(dir); err != nil {
				return err
			}
		}
		return extract(root, vol, inner, hdr, r)
	})
	return files, err
}

func extract(root *os.Root, vol, inner string, hdr *tar.Header, r io.Reader) error {
	if inner == "." {
		return nil
	}
	if err := root.MkdirAll(filepath.Dir(inner), 0o700); err != nil {
		return err
	}
	mode := os.FileMode(hdr.Mode).Perm()
	switch hdr.Typeflag {
	case tar.TypeDir:
		if err := root.MkdirAll(inner, 0o700); err != nil {
			return err
		}
		return root.Chmod(inner, mode|0o700)
	case tar.TypeReg:
		f, err := root.OpenFile(inner, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o600)
		if err != nil {
			return err
		}
		if _, err := io.Copy(f, r); err != nil {
			f.Close()
			return err
		}
		if err := f.Close(); err != nil {
			return err
		}
		if err := root.Chmod(inner, mode|0o600); err != nil {
			return err
		}
		return root.Chtimes(inner, hdr.ModTime, hdr.ModTime)
	case tar.TypeSymlink:
		return root.Symlink(hdr.Linkname, inner)
	case tar.TypeLink:
		lv, target, ok := volumeOf(hdr.Linkname)
		if !ok || lv != vol || target == "." {
			return fmt.Errorf("%w: unsafe link %q", ErrDamaged, hdr.Linkname)
		}
		return root.Link(target, inner)
	}
	return nil
}

func clearDir(dir string) error {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return err
	}
	ents, err := os.ReadDir(dir)
	if err != nil {
		return err
	}
	for _, e := range ents {
		if err := os.RemoveAll(filepath.Join(dir, e.Name())); err != nil {
			return err
		}
	}
	return nil
}

func restoreModes(l spec.Layout, m *Manifest, files map[string][]byte) error {
	modes, err := config.ReadModes(l)
	if err != nil {
		return err
	}
	archive := parseEnv(files[ModesFileName])
	for k, v := range parseEnv(files[EnvFileName]) {
		if v != "" {
			archive[k] = v
		}
	}
	out := map[string]string{}
	for _, k := range []string{spec.RustProcessEnv, spec.StorageBackendEnv, spec.KVModeEnv} {
		if v := cmp.Or(archive[k], modes[k]); v != "" {
			out[k] = v
		}
	}
	maps.Copy(out, spec.RestoreDefaults(m.Volumes, archive))
	return config.WriteModes(l, out)
}

func restoreSecrets(target string, files map[string][]byte) error {
	var from map[string]string
	if data, ok := files[SecretsFileName]; ok {
		from = parseEnv(data)
	} else if data, ok := files[RexctlStateName]; ok {
		var st struct {
			Secrets map[string]string `json:"secrets"`
		}
		if err := json.Unmarshal(data, &st); err != nil {
			return fmt.Errorf("%w: %s: %v", ErrDamaged, RexctlStateName, err)
		}
		from = st.Secrets
	}
	if len(from) == 0 {
		return nil
	}
	merged, err := config.ReadEnvFile(target)
	if errors.Is(err, os.ErrNotExist) {
		merged, err = map[string]string{}, nil
	}
	if err != nil {
		return err
	}
	for k, v := range from {
		if k == "POSTGRES_PASSWORD" && merged[k] != "" {
			continue
		}
		merged[k] = v
	}
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(merged)) {
		fmt.Fprintf(&b, "%s=%s\n", k, merged[k])
	}
	return writeAtomic(target, b.Bytes(), 0o600)
}

func parseEnv(data []byte) map[string]string {
	out := map[string]string{}
	sc := bufio.NewScanner(bytes.NewReader(data))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if k, v, ok := strings.Cut(line, "="); ok && strings.TrimSpace(k) != "" {
			out[strings.TrimSpace(k)] = strings.TrimSpace(v)
		}
	}
	return out
}

func writeAtomic(target string, data []byte, mode os.FileMode) error {
	if err := os.MkdirAll(filepath.Dir(target), 0o700); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(target), "."+filepath.Base(target)+".*")
	if err != nil {
		return err
	}
	name := tmp.Name()
	defer os.Remove(name)
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Chmod(mode); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(name, target)
}
