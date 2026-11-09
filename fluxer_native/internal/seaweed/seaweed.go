// SPDX-License-Identifier: AGPL-3.0-or-later

package seaweed

import (
	"bytes"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const InitCommand = "seaweedfs-init"

const (
	shellTimeout = 10 * time.Second
	retryDelay   = 2 * time.Second
)

func Argv(weed, dataDir, configDir string, composeArgs []string) (server, filer []string) {
	config := "-config_dir=" + configDir
	server = []string{weed, config}
	filer = []string{weed, config, "filer"}
	for _, a := range composeArgs {
		switch {
		case a == "-s3" || strings.HasPrefix(a, "-s3."):
			filer = append(filer, a)
		case strings.HasPrefix(a, "-dir="):
			server = append(server, "-dir="+dataDir)
		default:
			server = append(server, a)
		}
	}
	server = append(server,
		"-ip="+spec.Loopback,
		"-ip.bind="+spec.Loopback,
		"-master.port="+strconv.Itoa(spec.PortWeedMaster),
		"-volume.port="+strconv.Itoa(spec.PortWeedVolume),
		"-volume.preStopSeconds=0",
		"-master.peers=none",
	)
	filer = append(filer,
		"-master="+spec.Addr(spec.PortWeedMaster),
		"-ip="+spec.Loopback,
		"-ip.bind="+spec.Loopback,
		"-port="+strconv.Itoa(spec.PortWeedFiler),
		"-defaultStoreDir="+dataDir,
		"-dirListLimit=1000",
		"-s3.port="+strconv.Itoa(spec.PortSeaweedS3),
		"-s3.port.iceberg=0",
		"-s3.port.lance=0",
		"-localSocket="+filepath.Join(configDir, "filer.sock"),
		"-s3.localSocket="+filepath.Join(configDir, "s3.sock"),
	)
	return server, filer
}

func VolumeProbe(configDir string) spec.Probe {
	return HTTPSProbe(configDir, spec.PortWeedVolume, "/healthz")
}

func LeaderProbe(configDir string) spec.Probe {
	return HTTPSProbe(configDir, spec.PortWeedMaster, "/cluster/healthz")
}

func InitArgv(self, weed, configDir string) []string {
	return []string{self, InitCommand, "-weed", weed, "-config-dir", configDir, "-master", spec.Addr(spec.PortWeedMaster)}
}

type Init struct {
	Weed       string
	ConfigDir  string
	Master     string
	Buckets    []string
	AccessKey  string
	SecretKey  string
	Uploads    string
	UploadsTTL string
	Attempts   int
	Delay      time.Duration
	Stdout     io.Writer
	Stderr     io.Writer
}

func InitMain(ctx context.Context, args []string, getenv func(string) string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet(InitCommand, flag.ContinueOnError)
	fs.SetOutput(stderr)
	weed := fs.String("weed", "weed", "path of the weed binary")
	configDir := fs.String("config-dir", "", "directory with the weed security.toml")
	master := fs.String("master", spec.Addr(spec.PortWeedMaster), "address of the weed master")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	attempts, err := strconv.Atoi(getenv("FLUXER_SEAWEEDFS_INIT_ATTEMPTS"))
	if err != nil {
		fmt.Fprintf(stderr, "seaweedfs-init FLUXER_SEAWEEDFS_INIT_ATTEMPTS is not a number: %q\n", getenv("FLUXER_SEAWEEDFS_INIT_ATTEMPTS"))
		return 1
	}
	in := Init{
		Weed:      *weed,
		ConfigDir: *configDir,
		Master:    *master,
		Buckets: []string{
			getenv("FLUXER_S3_BUCKET_CDN"),
			getenv("FLUXER_S3_BUCKET_UPLOADS"),
			getenv("FLUXER_S3_BUCKET_REPORTS"),
			getenv("FLUXER_S3_BUCKET_HARVESTS"),
		},
		AccessKey:  getenv("FLUXER_S3_ACCESS_KEY"),
		SecretKey:  getenv("FLUXER_S3_SECRET_KEY"),
		Uploads:    getenv("FLUXER_S3_BUCKET_UPLOADS"),
		UploadsTTL: getenv("FLUXER_SEAWEEDFS_UPLOADS_TTL"),
		Attempts:   attempts,
		Delay:      retryDelay,
		Stdout:     stdout,
		Stderr:     stderr,
	}
	if err := in.Run(ctx); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func (in Init) Run(ctx context.Context) error {
	missing := in.Buckets
	failure := ""
	for range in.Attempts {
		if !in.dialMaster(ctx) {
			if err := in.sleep(ctx); err != nil {
				return err
			}
			continue
		}
		listed, _ := in.weedShell(ctx, "s3.bucket.list")
		missing = missingBuckets(listed, in.Buckets)
		failure = ""
		if len(missing) == 0 {
			ttlRule := fmt.Sprintf("fs.configure -locationPrefix=/buckets/%s/ -ttl=%s -apply", in.Uploads, in.UploadsTTL)
			if in.UploadsTTL == "off" {
				ttlRule = fmt.Sprintf("fs.configure -locationPrefix=/buckets/%s/ -delete -apply", in.Uploads)
			}
			failure = "could not configure the S3 identity"
			identity := fmt.Sprintf("s3.configure -user=fluxer -access_key=%s -secret_key=%s -actions=Admin,Read,Write,List,Tagging -apply", in.AccessKey, in.SecretKey)
			if _, err := in.weedShell(ctx, identity); err == nil {
				failure = "could not set the expiry of " + in.Uploads
				if _, err := in.weedShell(ctx, ttlRule); err == nil {
					fmt.Fprintln(in.Stdout, "buckets ready")
					return nil
				}
			}
			if err := in.sleep(ctx); err != nil {
				return err
			}
			continue
		}
		for _, b := range missing {
			_, _ = in.weedShell(ctx, "s3.bucket.create -name "+b)
		}
		if err := in.sleep(ctx); err != nil {
			return err
		}
	}
	if failure != "" {
		return errors.New("seaweedfs-init " + failure)
	}
	return errors.New("seaweedfs-init could not verify buckets: " + strings.Join(missing, " "))
}

func missingBuckets(listed string, buckets []string) []string {
	var missing []string
	for _, b := range buckets {
		found := false
		for line := range strings.Lines(listed) {
			rest, ok := strings.CutPrefix(strings.TrimLeft(line, " \t"), b)
			if ok && rest != "" && (rest[0] == ' ' || rest[0] == '\t') {
				found = true
				break
			}
		}
		if !found {
			missing = append(missing, b)
		}
	}
	return missing
}

func (in Init) sleep(ctx context.Context) error {
	t := time.NewTimer(in.Delay)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-t.C:
		return nil
	}
}

func (in Init) dialMaster(ctx context.Context) bool {
	var d net.Dialer
	ctx, cancel := context.WithTimeout(ctx, shellTimeout)
	defer cancel()
	conn, err := d.DialContext(ctx, "tcp", in.Master)
	if err != nil {
		return false
	}
	conn.Close()
	return true
}

func (in Init) weedShell(ctx context.Context, command string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, shellTimeout)
	defer cancel()
	cmd := exec.CommandContext(ctx, in.Weed, "-config_dir="+in.ConfigDir, "shell", "-master="+in.Master)
	cmd.Stdin = strings.NewReader(command + "\n")
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	err := cmd.Run()
	return out.String(), err
}
