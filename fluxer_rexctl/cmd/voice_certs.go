// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"io"
	"io/fs"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const (
	voiceCaddyStore    = "edge/caddy"
	composeCaddyStore  = "/data/caddy"
	voiceCertsMaxBytes = 32 << 20
)

var voiceCertDirs = []string{"certificates", "acme"}

type certFile struct {
	name string
	data []byte
	dir  bool
}

func readCertTar(r io.Reader, files []certFile) ([]certFile, error) {
	tr := tar.NewReader(io.LimitReader(r, voiceCertsMaxBytes))
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return files, nil
		}
		if err != nil {
			return files, err
		}
		name := path.Clean(hdr.Name)
		if name == "." || strings.HasPrefix(name, "../") || path.IsAbs(name) {
			continue
		}
		switch hdr.Typeflag {
		case tar.TypeDir:
			files = append(files, certFile{name: name, dir: true})
		case tar.TypeReg:
			data, err := io.ReadAll(tr)
			if err != nil {
				return files, err
			}
			files = append(files, certFile{name: name, data: data})
		}
	}
}

func exportVoiceCerts(ctx context.Context, old *instance.Instance, oldEngine *engine.Engine) []certFile {
	var files []certFile
	switch {
	case old.State.Runtime == instance.RuntimeNative:
		base := filepath.Join(native.New(old).Root(), voiceCaddyStore)
		for _, dir := range voiceCertDirs {
			_ = filepath.WalkDir(filepath.Join(base, dir), func(p string, d fs.DirEntry, err error) error {
				if err != nil {
					return nil
				}
				rel, _ := filepath.Rel(base, p)
				if d.IsDir() {
					files = append(files, certFile{name: filepath.ToSlash(rel), dir: true})
					return nil
				}
				if data, err := os.ReadFile(p); err == nil && d.Type().IsRegular() {
					files = append(files, certFile{name: filepath.ToSlash(rel), data: data})
				}
				return nil
			})
		}
	case old.State.Runtime == instance.RuntimeContainer:
		e, err := rt.VoiceEngine(ctx, old)
		if err != nil {
			return nil
		}
		files = copyCertsOut(ctx, e, rt.VoiceContainer, rt.ContainerData+"/"+voiceCaddyStore)
	case oldEngine != nil && ownComposeEdge(old):
		cs, err := oldEngine.Named(voice.EdgeProject, old.Path(voice.EdgeDir)).PS(ctx)
		if err != nil {
			return nil
		}
		for _, c := range cs {
			if c.Service == "edge" {
				files = copyCertsOut(ctx, oldEngine, c.ID, composeCaddyStore)
			}
		}
	}
	return files
}

func copyCertsOut(ctx context.Context, e *engine.Engine, name, store string) []certFile {
	var files []certFile
	for _, dir := range voiceCertDirs {
		rc, err := e.ReadSingleTar(ctx, name, store+"/"+dir)
		if err != nil {
			continue
		}
		files, _ = readCertTar(rc, files)
		rc.Close()
	}
	return files
}

func importNativeVoiceCerts(ctx context.Context, inst *instance.Instance, files []certFile) error {
	if len(files) == 0 {
		return nil
	}
	if err := native.EnsureUser(ctx); err != nil {
		return err
	}
	u, err := user.Lookup(native.ServiceUser)
	if err != nil {
		return err
	}
	uid, _ := strconv.Atoi(u.Uid)
	gid, _ := strconv.Atoi(u.Gid)
	root := native.New(inst).Root()
	base := filepath.Join(root, voiceCaddyStore)
	for _, dir := range []string{root, filepath.Dir(base), base} {
		if err := os.MkdirAll(dir, 0o700); err != nil {
			return err
		}
		if err := os.Chown(dir, uid, gid); err != nil {
			return err
		}
	}
	for _, f := range files {
		p := filepath.Join(base, filepath.FromSlash(f.name))
		if f.dir {
			err = os.MkdirAll(p, 0o700)
		} else if err = os.MkdirAll(filepath.Dir(p), 0o700); err == nil {
			err = os.WriteFile(p, f.data, 0o600)
		}
		if err == nil {
			err = os.Chown(p, uid, gid)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func containerCertSeed(e *engine.Engine, files []certFile) func(context.Context) error {
	if len(files) == 0 {
		return nil
	}
	return func(ctx context.Context) error {
		var b bytes.Buffer
		tw := tar.NewWriter(&b)
		for _, dir := range []string{"edge", voiceCaddyStore} {
			if err := tw.WriteHeader(&tar.Header{Name: dir + "/", Typeflag: tar.TypeDir, Mode: 0o700}); err != nil {
				return err
			}
		}
		for _, f := range files {
			name := voiceCaddyStore + "/" + f.name
			if f.dir {
				if err := tw.WriteHeader(&tar.Header{Name: name + "/", Typeflag: tar.TypeDir, Mode: 0o700}); err != nil {
					return err
				}
				continue
			}
			if err := tw.WriteHeader(&tar.Header{Name: name, Typeflag: tar.TypeReg, Mode: 0o600, Size: int64(len(f.data))}); err != nil {
				return err
			}
			if _, err := tw.Write(f.data); err != nil {
				return err
			}
		}
		if err := tw.Close(); err != nil {
			return err
		}
		return e.WriteSingleTar(ctx, rt.VoiceContainer, rt.ContainerData, &b)
	}
}
