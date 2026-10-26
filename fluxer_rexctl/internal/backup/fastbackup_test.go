// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

type spoolEntry struct {
	name string
	typ  byte
	data []byte
}

func writeSpool(t *testing.T, path string, entries []spoolEntry) {
	t.Helper()
	s, err := createSpool(path)
	if err != nil {
		t.Fatalf("create spool: %v", err)
	}
	for _, e := range entries {
		hdr := &tar.Header{Name: e.name, Mode: 0o644, Typeflag: e.typ, ModTime: time.Unix(1700000000, 0)}
		if e.typ == tar.TypeReg {
			hdr.Size = int64(len(e.data))
		}
		if err := s.add(hdr, bytes.NewReader(e.data)); err != nil {
			s.abort()
			t.Fatalf("spool add %s: %v", e.name, err)
		}
	}
	if err := s.finish(); err != nil {
		s.abort()
		t.Fatalf("finish spool: %v", err)
	}
}

func TestVolumeSpoolStaysUncompressed(t *testing.T) {
	path := filepath.Join(t.TempDir(), "spool")
	writeSpool(t, path, []spoolEntry{
		{name: VolumesPrefix + "seaweedfs-data/", typ: tar.TypeDir},
		{name: VolumesPrefix + "seaweedfs-data/a.bin", typ: tar.TypeReg, data: []byte("hello volume")},
	})
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(raw) >= 4 && raw[0] == 0x28 && raw[1] == 0xb5 && raw[2] == 0x2f && raw[3] == 0xfd {
		t.Fatal("the offline spool is a zstd stream; compression still runs while Fluxer is stopped")
	}
	tr := tar.NewReader(bytes.NewReader(raw))
	hdr, err := tr.Next()
	for err == nil && hdr.Typeflag == tar.TypeDir {
		hdr, err = tr.Next()
	}
	if err != nil {
		t.Fatalf("the spool is not a plain readable tar: %v", err)
	}
	body, _ := io.ReadAll(tr)
	if hdr.Name != VolumesPrefix+"seaweedfs-data/a.bin" || string(body) != "hello volume" {
		t.Fatalf("spool content = %q %q", hdr.Name, body)
	}
}

func TestFastBackupArchiveRoundTrip(t *testing.T) {
	dir := t.TempDir()
	spool := filepath.Join(dir, "spool")
	big := bytes.Repeat([]byte{0x7f, 0x11, 0x42, 0xa0}, 2048)
	entries := []spoolEntry{
		{name: VolumesPrefix + "seaweedfs-data/", typ: tar.TypeDir},
		{name: VolumesPrefix + "seaweedfs-data/a.bin", typ: tar.TypeReg, data: big},
		{name: VolumesPrefix + "seaweedfs-data/sub/b.txt", typ: tar.TypeReg, data: []byte("second file")},
		{name: VolumesPrefix + "valkey-data/dump.rdb", typ: tar.TypeReg, data: []byte("valkey state")},
	}
	writeSpool(t, spool, entries)

	final := filepath.Join(dir, "backup"+Extension)
	partial := final + PartialSuffix
	w, err := createArchive(partial)
	if err != nil {
		t.Fatal(err)
	}
	if err := w.addBytes(FilesPrefix+"compose.yaml", 0o644, []byte("services: {}")); err != nil {
		t.Fatal(err)
	}
	if err := w.addBytes(DumpName, 0o600, []byte("PGDMP fake dump")); err != nil {
		t.Fatal(err)
	}
	if err := compressSpool(context.Background(), w, spool); err != nil {
		t.Fatalf("compressSpool: %v", err)
	}
	m := &Manifest{
		Project:   "fluxer",
		Domain:    "chat.example.com",
		CreatedAt: time.Now().UTC().Truncate(time.Second),
		Database:  DumpName,
		Volumes:   []string{"seaweedfs-data", "valkey-data"},
	}
	if err := w.finish(m); err != nil {
		t.Fatalf("finish: %v", err)
	}
	if err := os.Rename(partial, final); err != nil {
		t.Fatal(err)
	}

	if _, err := Verify(final); err != nil {
		t.Fatalf("Verify rejected the recompressed archive: %v", err)
	}

	got := map[string]string{}
	err = walkArchive(final, func(hdr *tar.Header, r io.Reader) error {
		if hdr.Typeflag != tar.TypeReg {
			return nil
		}
		b, err := io.ReadAll(r)
		if err != nil {
			return err
		}
		got[hdr.Name] = string(b)
		return nil
	})
	if err != nil {
		t.Fatalf("walkArchive: %v", err)
	}
	want := map[string]string{
		FilesPrefix + "compose.yaml":               "services: {}",
		DumpName:                                   "PGDMP fake dump",
		VolumesPrefix + "seaweedfs-data/a.bin":     string(big),
		VolumesPrefix + "seaweedfs-data/sub/b.txt": "second file",
		VolumesPrefix + "valkey-data/dump.rdb":     "valkey state",
	}
	for name, body := range want {
		if got[name] != body {
			t.Fatalf("%s round-tripped to %d bytes, want %d", name, len(got[name]), len(body))
		}
	}
	if _, ok := got[ManifestName]; !ok {
		t.Fatal("the archive has no manifest")
	}
}

func TestCompressSpoolCancelKeepsInstanceUp(t *testing.T) {
	dir := t.TempDir()
	spool := filepath.Join(dir, "spool")
	writeSpool(t, spool, []spoolEntry{{name: VolumesPrefix + "valkey-data/x", typ: tar.TypeReg, data: []byte("x")}})
	w, err := createArchive(filepath.Join(dir, "out"+PartialSuffix))
	if err != nil {
		t.Fatal(err)
	}
	defer w.abort()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	err = compressSpool(ctx, w, spool)
	var ie *InterruptedError
	if !errors.As(err, &ie) || !ie.Restarted {
		t.Fatalf("a cancelled online compression returned %v, want an interrupted error flagged as restarted", err)
	}
}

func TestCompressSpoolCorruptReportsCompressError(t *testing.T) {
	dir := t.TempDir()
	spool := filepath.Join(dir, "spool")
	if err := os.WriteFile(spool, bytes.Repeat([]byte{0xff}, 1024), 0o600); err != nil {
		t.Fatal(err)
	}
	w, err := createArchive(filepath.Join(dir, "out"+PartialSuffix))
	if err != nil {
		t.Fatal(err)
	}
	defer w.abort()
	err = compressSpool(context.Background(), w, spool)
	var ce *CompressError
	if !errors.As(err, &ce) {
		t.Fatalf("a broken spool returned %v, want a CompressError", err)
	}
	var ie *InterruptedError
	if errors.As(err, &ie) {
		t.Fatal("a broken spool must not look like an interruption")
	}
}

func TestBackupDiskNeedCountsRawIntermediate(t *testing.T) {
	const db = uint64(1 << 30)
	const vol = uint64(10 << 30)
	got := backupDiskNeed(db, vol)
	want := (db + vol*2) * diskHeadroomPct / 100
	if got != want {
		t.Fatalf("backupDiskNeed = %d, want %d", got, want)
	}
	single := (db + vol) * diskHeadroomPct / 100
	if got <= single {
		t.Fatalf("backupDiskNeed = %d does not reserve room for the raw intermediate (a single copy needs %d)", got, single)
	}
	if backupDiskNeed(db, 0) != db*diskHeadroomPct/100 {
		t.Fatal("with no volumes the need should not double anything")
	}
}

func TestCompressErrorMessageNamesCompression(t *testing.T) {
	e := &CompressError{Err: errors.New("no space left on device")}
	if !strings.Contains(e.Error(), "compressing") {
		t.Fatalf("CompressError message = %q", e.Error())
	}
	if !errors.Is(e, e.Err) {
		t.Fatal("CompressError should unwrap to its cause")
	}
}
