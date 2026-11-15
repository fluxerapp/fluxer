// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"
	"slices"
	"strings"
	"time"
	"unicode"

	"github.com/klauspost/compress/zstd"
)

const (
	FormatVersion       = 2
	formatFull          = 1
	Extension           = ".tar.zst"
	ManifestName        = "manifest.json"
	SumsName            = "SHA256SUMS"
	PartialSuffix       = ".partial"
	DumpName            = "postgres/fluxer.dump"
	FilesPrefix         = "files/"
	VolumesPrefix       = "volumes/"
	EnvFileName         = FilesPrefix + "fluxer.env"
	SecretsFileName     = FilesPrefix + "secrets.env"
	ModesFileName       = FilesPrefix + "modes.env"
	RexctlStateName     = FilesPrefix + ".rexctl/state.json"
	SkippedDatabase     = "database"
	SkippedUploads      = "uploads"
	ScopeWithoutUploads = "without-uploads"
)

const (
	StageFiles     = "files"
	StageDatabase  = "database"
	StageStopping  = "stopping"
	StageVolumes   = "volumes"
	StageStarting  = "starting"
	StageVerifying = "verifying"
)

const (
	trailerMagic    = "RXBKMAN1"
	trailerSize     = 16
	skippableMagic  = 0x184D2A5E
	maxManifestSize = 512 << 20
)

var (
	ErrNotBackup = errors.New("backup: not a Fluxer backup")
	ErrDamaged   = errors.New("backup: damaged")
	ErrNewer     = errors.New("backup: made by a newer version")
)

type Entry struct {
	Path   string `json:"path"`
	Size   int64  `json:"size"`
	SHA256 string `json:"sha256"`
}

type Manifest struct {
	Format    int       `json:"format"`
	Release   string    `json:"release"`
	Project   string    `json:"project"`
	Domain    string    `json:"domain"`
	CreatedAt time.Time `json:"created_at"`
	Reason    string    `json:"reason,omitempty"`
	Database  string    `json:"database"`
	Volumes   []string  `json:"volumes"`
	Skipped   []string  `json:"skipped,omitempty"`
	Scope     string    `json:"scope,omitempty"`
	Entries   []Entry   `json:"entries"`
}

func (m *Manifest) HasSkipped(what string) bool {
	return slices.Contains(m.Skipped, what)
}

type archiveWriter struct {
	f       *os.File
	zw      *zstd.Encoder
	tw      *tar.Writer
	entries []Entry
}

func createArchive(path string) (*archiveWriter, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
	if err != nil {
		return nil, err
	}
	zw, err := zstd.NewWriter(f, zstd.WithEncoderLevel(zstd.SpeedDefault))
	if err != nil {
		f.Close()
		return nil, err
	}
	return &archiveWriter{f: f, zw: zw, tw: tar.NewWriter(zw)}, nil
}

func (w *archiveWriter) abort() {
	w.zw.Close()
	w.f.Close()
	os.Remove(w.f.Name())
}

func (w *archiveWriter) add(hdr *tar.Header, r io.Reader) error {
	if err := checkName(hdr.Name); err != nil {
		return err
	}
	hdr.Format = tar.FormatPAX
	if hdr.ModTime.IsZero() {
		hdr.ModTime = time.Now()
	}
	if err := w.tw.WriteHeader(hdr); err != nil {
		return err
	}
	if hdr.Typeflag != tar.TypeReg {
		return nil
	}
	h := sha256.New()
	n, err := io.Copy(io.MultiWriter(w.tw, h), io.LimitReader(r, hdr.Size))
	if err != nil {
		return err
	}
	if n != hdr.Size {
		return fmt.Errorf("backup: %s shrank while it was copied", hdr.Name)
	}
	w.entries = append(w.entries, Entry{Path: hdr.Name, Size: n, SHA256: hex.EncodeToString(h.Sum(nil))})
	return nil
}

func (w *archiveWriter) addBytes(name string, mode int64, data []byte) error {
	return w.add(&tar.Header{Name: name, Mode: mode, Size: int64(len(data)), Typeflag: tar.TypeReg}, bytes.NewReader(data))
}

func (w *archiveWriter) addFile(src, name string) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil {
		return err
	}
	return w.add(&tar.Header{Name: name, Mode: int64(fi.Mode().Perm()), Size: fi.Size(), ModTime: fi.ModTime(), Typeflag: tar.TypeReg}, f)
}

func (w *archiveWriter) addTree(src, prefix string, skip []string) error {
	return filepath.WalkDir(src, func(p string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(src, p)
		if err != nil {
			return err
		}
		if slices.Contains(skip, filepath.ToSlash(rel)) {
			if d.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}
		name := prefix
		if rel != "." {
			name += "/" + filepath.ToSlash(rel)
		}
		fi, err := d.Info()
		if err != nil {
			return err
		}
		hdr := &tar.Header{Name: name, Mode: int64(fi.Mode().Perm()), ModTime: fi.ModTime()}
		switch {
		case d.IsDir():
			hdr.Name += "/"
			hdr.Typeflag = tar.TypeDir
			return w.add(hdr, nil)
		case fi.Mode()&os.ModeSymlink != 0:
			target, err := os.Readlink(p)
			if err != nil {
				return err
			}
			hdr.Typeflag, hdr.Linkname = tar.TypeSymlink, target
			return w.add(hdr, nil)
		case fi.Mode().IsRegular():
			return w.addFile(p, name)
		}
		return nil
	})
}

func (w *archiveWriter) finish(m *Manifest) error {
	m.Format = formatFull
	m.Entries = append([]Entry{}, w.entries...)
	if m.Volumes == nil {
		m.Volumes = []string{}
	}
	data, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		return err
	}
	if err := w.addBytes(ManifestName, 0o600, data); err != nil {
		return err
	}
	if err := w.addBytes(SumsName, 0o600, sums(append(m.Entries, entryFor(ManifestName, data)))); err != nil {
		return err
	}
	if err := w.tw.Close(); err != nil {
		return err
	}
	if err := w.zw.Close(); err != nil {
		return err
	}
	if _, err := w.f.Write(trailer(data)); err != nil {
		return err
	}
	if err := w.f.Sync(); err != nil {
		return err
	}
	return w.f.Close()
}

func sums(entries []Entry) []byte {
	var b bytes.Buffer
	for _, e := range entries {
		fmt.Fprintf(&b, "%s  %s\n", e.SHA256, e.Path)
	}
	return b.Bytes()
}

func entryFor(name string, data []byte) Entry {
	h := sha256.Sum256(data)
	return Entry{Path: name, Size: int64(len(data)), SHA256: hex.EncodeToString(h[:])}
}

func trailer(manifest []byte) []byte {
	payload := len(manifest) + trailerSize
	out := make([]byte, 8, 8+payload)
	binary.LittleEndian.PutUint32(out[0:4], skippableMagic)
	binary.LittleEndian.PutUint32(out[4:8], uint32(payload))
	out = append(out, manifest...)
	out = binary.LittleEndian.AppendUint64(out, uint64(len(manifest)))
	return append(out, trailerMagic...)
}

func readTrailer(path string) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil {
		return nil, err
	}
	size := fi.Size()
	if size < trailerSize+8 {
		return nil, ErrNotBackup
	}
	tail := make([]byte, trailerSize)
	if _, err := f.ReadAt(tail, size-trailerSize); err != nil {
		return nil, err
	}
	if string(tail[8:]) != trailerMagic {
		return nil, ErrNotBackup
	}
	n := binary.LittleEndian.Uint64(tail[:8])
	if n == 0 || n > maxManifestSize || int64(n)+trailerSize+8 > size {
		return nil, ErrNotBackup
	}
	frame := make([]byte, 8+int(n))
	if _, err := f.ReadAt(frame, size-trailerSize-int64(n)-8); err != nil {
		return nil, err
	}
	if binary.LittleEndian.Uint32(frame[0:4]) != skippableMagic || binary.LittleEndian.Uint32(frame[4:8]) != uint32(n)+trailerSize {
		return nil, ErrNotBackup
	}
	return frame[8:], nil
}

func parseManifest(data []byte) (*Manifest, error) {
	var m Manifest
	if err := json.Unmarshal(data, &m); err != nil {
		return nil, ErrNotBackup
	}
	if slices.ContainsFunc([]string{m.Domain, m.Project, m.Release, m.Reason}, func(s string) bool { return strings.ContainsFunc(s, unicode.IsControl) }) {
		return nil, ErrNotBackup
	}
	if m.Format < 1 {
		return nil, ErrNotBackup
	}
	if m.Format > FormatVersion {
		return &m, ErrNewer
	}
	return &m, nil
}

func walkArchive(path string, fn func(hdr *tar.Header, r io.Reader) error) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	zr, err := zstd.NewReader(bufio.NewReaderSize(f, 1<<20), zstd.WithDecoderConcurrency(1))
	if err != nil {
		return err
	}
	defer zr.Close()
	tr := tar.NewReader(zr)
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return fmt.Errorf("%w: %v", ErrDamaged, err)
		}
		if err := checkName(hdr.Name); err != nil {
			return err
		}
		if err := fn(hdr, tr); err != nil {
			return err
		}
	}
}

func checkName(name string) error {
	clean := strings.TrimSuffix(name, "/")
	if clean == "" || strings.HasPrefix(clean, "/") || strings.Contains(clean, "\\") || path.Clean(clean) != clean {
		return fmt.Errorf("%w: unsafe path %q", ErrDamaged, name)
	}
	for _, part := range strings.Split(clean, "/") {
		if part == ".." || part == "." {
			return fmt.Errorf("%w: unsafe path %q", ErrDamaged, name)
		}
	}
	return nil
}

func Verify(path string) (*Manifest, error) {
	data, err := readTrailer(path)
	if err != nil {
		return nil, err
	}
	m, err := parseManifest(data)
	if err != nil {
		return m, err
	}
	seen := map[string]Entry{}
	var manifestSum, sumsSum []byte
	err = walkArchive(path, func(hdr *tar.Header, r io.Reader) error {
		if hdr.Typeflag != tar.TypeReg {
			return nil
		}
		switch hdr.Name {
		case ManifestName:
			if hdr.Size != int64(len(data)) {
				return fmt.Errorf("%w: the index doesn't match the manifest", ErrDamaged)
			}
			sum, err := hashReader(r)
			manifestSum = sum
			return err
		case SumsName:
			sum, err := hashReader(r)
			sumsSum = sum
			return err
		}
		if manifestSum != nil {
			return fmt.Errorf("%w: %s comes after the manifest", ErrDamaged, hdr.Name)
		}
		h := sha256.New()
		n, err := io.Copy(h, r)
		if err != nil {
			return fmt.Errorf("%w: %v", ErrDamaged, err)
		}
		if _, dup := seen[hdr.Name]; dup {
			return fmt.Errorf("%w: %s appears twice", ErrDamaged, hdr.Name)
		}
		seen[hdr.Name] = Entry{Path: hdr.Name, Size: n, SHA256: hex.EncodeToString(h.Sum(nil))}
		return nil
	})
	if err != nil {
		return m, err
	}
	if manifestSum == nil || sumsSum == nil {
		return m, fmt.Errorf("%w: the manifest is missing", ErrDamaged)
	}
	if want := sha256.Sum256(data); !bytes.Equal(manifestSum, want[:]) {
		return m, fmt.Errorf("%w: the index doesn't match the manifest", ErrDamaged)
	}
	if len(m.Entries) != len(seen) {
		return m, fmt.Errorf("%w: the manifest lists %d files and the archive has %d", ErrDamaged, len(m.Entries), len(seen))
	}
	for _, want := range m.Entries {
		got, ok := seen[want.Path]
		if !ok {
			return m, fmt.Errorf("%w: %s is missing", ErrDamaged, want.Path)
		}
		if got != want {
			return m, fmt.Errorf("%w: %s doesn't match its checksum", ErrDamaged, want.Path)
		}
	}
	if want := sha256.Sum256(sums(append(m.Entries, entryFor(ManifestName, data)))); !bytes.Equal(sumsSum, want[:]) {
		return m, fmt.Errorf("%w: %s doesn't match the manifest", ErrDamaged, SumsName)
	}
	return m, nil
}

func hashReader(r io.Reader) ([]byte, error) {
	h := sha256.New()
	if _, err := io.Copy(h, r); err != nil {
		return nil, fmt.Errorf("%w: %v", ErrDamaged, err)
	}
	return h.Sum(nil), nil
}

func volumeOf(name string) (string, string, bool) {
	if !strings.HasPrefix(name, VolumesPrefix) {
		return "", "", false
	}
	rest := strings.TrimPrefix(name, VolumesPrefix)
	vol, inner, _ := strings.Cut(rest, "/")
	if vol == "" {
		return "", "", false
	}
	if inner == "" {
		inner = "."
	}
	return vol, inner, true
}

func fileName(domain string, at time.Time, n int) string {
	var b strings.Builder
	for _, r := range strings.ToLower(domain) {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '.', r == '-':
			b.WriteRune(r)
		default:
			b.WriteRune('-')
		}
	}
	d := strings.Trim(b.String(), ".-")
	if d == "" {
		d = "instance"
	}
	stamp := at.UTC().Format("20060102-150405")
	if n > 0 {
		return fmt.Sprintf("fluxer-%s-%s-%d%s", d, stamp, n, Extension)
	}
	return fmt.Sprintf("fluxer-%s-%s%s", d, stamp, Extension)
}

func newPath(dir, domain string, at time.Time) (string, error) {
	for n := 0; n < 100; n++ {
		p := filepath.Join(dir, fileName(domain, at, n))
		_, err := os.Lstat(p)
		if errors.Is(err, os.ErrNotExist) {
			_, err = os.Lstat(p + PartialSuffix)
			if errors.Is(err, os.ErrNotExist) {
				return p, nil
			}
		}
		if err != nil && !errors.Is(err, os.ErrNotExist) {
			return "", err
		}
	}
	return "", errors.New("backup: no free file name")
}

func removePartials(dir string) {
	ents, err := os.ReadDir(dir)
	if err != nil {
		return
	}
	for _, e := range ents {
		if strings.HasSuffix(e.Name(), PartialSuffix) {
			os.Remove(filepath.Join(dir, e.Name()))
		}
	}
}
