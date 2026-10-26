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
	"strings"
	"time"

	"github.com/klauspost/compress/zstd"
)

const (
	trailerMagic    = "RXBKMAN1"
	trailerSize     = 16
	skippableMagic  = 0x184D2A5E
	maxManifestSize = 512 << 20
)

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
	n, err := io.Copy(io.MultiWriter(w.tw, h), r)
	if err != nil {
		return err
	}
	if n != hdr.Size {
		return fmt.Errorf("backup: %s changed size while it was copied", hdr.Name)
	}
	w.entries = append(w.entries, Entry{Path: hdr.Name, Size: n, SHA256: hex.EncodeToString(h.Sum(nil))})
	return nil
}

func (w *archiveWriter) addBytes(name string, mode int64, data []byte) error {
	return w.add(&tar.Header{Name: name, Mode: mode, Size: int64(len(data)), Typeflag: tar.TypeReg}, bytes.NewReader(data))
}

func (w *archiveWriter) finish(m *Manifest) error {
	m.Format = FormatVersion
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

type volumeSpool struct {
	f  *os.File
	tw *tar.Writer
}

func createSpool(path string) (*volumeSpool, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
	if err != nil {
		return nil, err
	}
	return &volumeSpool{f: f, tw: tar.NewWriter(f)}, nil
}

func (s *volumeSpool) add(hdr *tar.Header, r io.Reader) error {
	if err := checkName(hdr.Name); err != nil {
		return err
	}
	hdr.Format = tar.FormatPAX
	if hdr.ModTime.IsZero() {
		hdr.ModTime = time.Now()
	}
	if err := s.tw.WriteHeader(hdr); err != nil {
		return err
	}
	if hdr.Typeflag != tar.TypeReg {
		return nil
	}
	n, err := io.Copy(s.tw, r)
	if err != nil {
		return err
	}
	if n != hdr.Size {
		return fmt.Errorf("backup: %s changed size while it was copied", hdr.Name)
	}
	return nil
}

func (s *volumeSpool) finish() error {
	if err := s.tw.Close(); err != nil {
		return err
	}
	if err := s.f.Sync(); err != nil {
		return err
	}
	return s.f.Close()
}

func (s *volumeSpool) abort() {
	s.tw.Close()
	s.f.Close()
	os.Remove(s.f.Name())
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

func ReadManifest(path string) (*Manifest, error) {
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
	return parseManifest(frame[8:])
}

func parseManifest(data []byte) (*Manifest, error) {
	var m Manifest
	if err := json.Unmarshal(data, &m); err != nil {
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

type entryFunc func(hdr *tar.Header, r io.Reader) error

func walkArchive(path string, fn entryFunc) error {
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
	head, err := ReadManifest(path)
	if err != nil {
		return head, err
	}
	seen := map[string]Entry{}
	var manifest, sumsData []byte
	err = walkArchive(path, func(hdr *tar.Header, r io.Reader) error {
		if hdr.Typeflag != tar.TypeReg {
			return nil
		}
		switch hdr.Name {
		case ManifestName:
			b, err := io.ReadAll(io.LimitReader(r, maxManifestSize))
			manifest = b
			return err
		case SumsName:
			b, err := io.ReadAll(io.LimitReader(r, maxManifestSize))
			sumsData = b
			return err
		}
		if manifest != nil {
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
		return head, err
	}
	if manifest == nil || sumsData == nil {
		return head, fmt.Errorf("%w: the manifest is missing", ErrDamaged)
	}
	m, err := parseManifest(manifest)
	if err != nil {
		return head, err
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
	if !bytes.Equal(sumsData, sums(append(m.Entries, entryFor(ManifestName, manifest)))) {
		return m, fmt.Errorf("%w: %s doesn't match the manifest", ErrDamaged, SumsName)
	}
	if !sameManifest(head, m) {
		return m, fmt.Errorf("%w: the index doesn't match the manifest", ErrDamaged)
	}
	return m, nil
}

func sameManifest(a, b *Manifest) bool {
	x, err1 := json.Marshal(a)
	y, err2 := json.Marshal(b)
	return err1 == nil && err2 == nil && bytes.Equal(x, y)
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
