// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"filippo.io/age"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const ageSuffix = ".age"

func ageRecipients(keys []string) ([]age.Recipient, error) {
	out := make([]age.Recipient, 0, len(keys))
	for _, k := range keys {
		r, err := age.ParseX25519Recipient(strings.TrimSpace(k))
		if err != nil {
			return nil, ui.Usage(fmt.Sprintf("%q isn't an age public key. Pass one that starts with age1, which age-keygen prints.", k))
		}
		out = append(out, r)
	}
	return out, nil
}

func ageScratchDir(near string) (string, error) {
	if dir, err := os.MkdirTemp(filepath.Dir(near), ".fluxer-age-"); err == nil {
		return dir, nil
	}
	return os.MkdirTemp("", "fluxer-age-")
}

func encryptBackup(src string, recipients []age.Recipient) (string, func(), error) {
	dir, err := ageScratchDir(src)
	if err != nil {
		return "", nil, err
	}
	cleanup := func() { _ = os.RemoveAll(dir) }
	out := filepath.Join(dir, filepath.Base(src)+ageSuffix)
	if err := ageCopy(src, out, func(w io.Writer) (io.WriteCloser, error) { return age.Encrypt(w, recipients...) }, nil); err != nil {
		cleanup()
		return "", nil, err
	}
	return out, cleanup, nil
}

func decryptBackup(src, identityFile string) (string, func(), error) {
	if identityFile == "" {
		return "", nil, ui.Usage("This backup is encrypted with age. Pass the key file that decrypts it with --age-identity.")
	}
	f, err := os.Open(identityFile)
	if err != nil {
		return "", nil, ui.Wrap(err, "Couldn't read "+identityFile+".", "Check the path you passed to --age-identity.")
	}
	ids, err := age.ParseIdentities(f)
	f.Close()
	if err != nil {
		return "", nil, ui.Wrap(err, identityFile+" holds no age key.", "Pass the file age-keygen wrote, which starts with AGE-SECRET-KEY-.")
	}
	dir, err := ageScratchDir(src)
	if err != nil {
		return "", nil, err
	}
	cleanup := func() { _ = os.RemoveAll(dir) }
	out := filepath.Join(dir, strings.TrimSuffix(filepath.Base(src), ageSuffix))
	err = ageCopy(src, out, nil, func(r io.Reader) (io.Reader, error) { return age.Decrypt(r, ids...) })
	if err != nil {
		cleanup()
		var none *age.NoIdentityMatchError
		if errors.As(err, &none) {
			return "", nil, ui.Refused("The key in "+identityFile+" doesn't decrypt this backup.", "Pass the key that belongs to the public key you encrypted it for.")
		}
		return "", nil, ui.Wrap(err, "Couldn't decrypt "+filepath.Base(src)+".", "Check that the file copied whole, then try again.")
	}
	return out, cleanup, nil
}

func ageCopy(src, dst string, wrapW func(io.Writer) (io.WriteCloser, error), wrapR func(io.Reader) (io.Reader, error)) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
	if err != nil {
		return err
	}
	defer out.Close()
	var r io.Reader = in
	if wrapR != nil {
		if r, err = wrapR(in); err != nil {
			return err
		}
	}
	var w io.WriteCloser = out
	if wrapW != nil {
		if w, err = wrapW(out); err != nil {
			return err
		}
	}
	if _, err := io.Copy(w, r); err != nil {
		return err
	}
	if wrapW != nil {
		if err := w.Close(); err != nil {
			return err
		}
	}
	return out.Close()
}
