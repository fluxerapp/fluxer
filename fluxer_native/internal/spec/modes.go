// SPDX-License-Identifier: AGPL-3.0-or-later

package spec

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

const (
	RustProcessEnv       = "FLUXER_RUST_PROCESS"
	ServerServicesEnv    = "FLUXER_SERVER_SERVICES"
	AdminUpstreamEnv     = "FLUXER_ADMIN_UPSTREAM"
	RustWorkerThreadsEnv = "FLUXER_RUST_SERVER_TOKIO_WORKER_THREADS"
	RustMemoryLimitEnv   = "FLUXER_RUST_MEMORY_LIMIT"

	StorageBackendEnv        = "FLUXER_STORAGE_BACKEND"
	StorageRootEnv           = "FLUXER_STORAGE_ROOT"
	StorageUploadsTTLEnv     = "FLUXER_STORAGE_UPLOADS_TTL"
	SeaweedFSUploadsTTLEnv   = "FLUXER_SEAWEEDFS_UPLOADS_TTL"
	StorageReadOnlyEnv       = "FLUXER_STORAGE_READ_ONLY"
	MediaProxyStorageEnv     = "FLUXER_MEDIA_PROXY_STORAGE_BACKEND"
	MediaProxyStorageRootEnv = "FLUXER_MEDIA_PROXY_STORAGE_ROOT"
	UVThreadpoolSizeEnv      = "UV_THREADPOOL_SIZE"
	S3EndpointEnv            = "FLUXER_S3_ENDPOINT"

	KVModeEnv             = "FLUXER_KV_MODE"
	KVPostgresMaxConnsEnv = "FLUXER_KV_POSTGRES_MAX_CONNECTIONS"
	KVPostgresMaxBytesEnv = "FLUXER_KV_POSTGRES_MAX_BYTES"
	KVURLEnv              = "FLUXER_KV_URL"
	DatabaseBackendEnv    = "FLUXER_DATABASE_BACKEND"
)

type RustProcess string

const (
	RustSingle     RustProcess = "single"
	RustPerService RustProcess = "per-service"
)

type StorageBackend string

const (
	StorageS3         StorageBackend = "s3"
	StorageFilesystem StorageBackend = "filesystem"
)

type KVMode string

const (
	KVStandalone KVMode = "standalone"
	KVCluster    KVMode = "cluster"
	KVPostgres   KVMode = "postgres"
)

const (
	DefaultUploadsTTL       = "1d"
	UploadsTTLOff           = "off"
	FilesystemUVThreadpool  = "16"
	MediaProxyStorageLocal  = "local"
	DefaultAdminUpstream    = "admin:8080"
	SingleAdminUpstream     = "admin:18083"
	PostgresDatabaseBackend = "postgres"
)

const (
	ObjectsDirName        = "objects"
	ObjectsLayoutFile     = ".layout"
	ObjectsStagingDir     = ".staging"
	ObjectsMetaDir        = ".meta"
	ObjectsLayoutVersion  = "1"
	ObjectsLayoutContents = `{"v":` + ObjectsLayoutVersion + `}`
	ValkeyMigratedPrefix  = "valkey.migrated-"
	ComposeObjectsRoot    = "/var/lib/fluxer/objects"
	ObjectsVolume         = "objects-data"
	SeaweedFSVolume       = "seaweedfs-data"
	ValkeyVolume          = "valkey-data"
	MigrateStorageCommand = "rexctl migrate --migrate=storage"
	MigrateKVCommand      = "rexctl migrate --migrate=kv"
)

var ObjectsStagingDirs = []string{"tmp", "multipart", "trash"}

var InstallDefaults = map[string]string{
	RustProcessEnv:    string(RustSingle),
	StorageBackendEnv: string(StorageFilesystem),
	KVModeEnv:         string(KVPostgres),
}

var UploadsTTLPattern = regexp.MustCompile(`^\d+[mhdwMy]$`)

func ValidUploadsTTL(v string) bool {
	return v == UploadsTTLOff || UploadsTTLPattern.MatchString(v)
}

func (l Layout) ObjectsDir() string { return filepath.Join(l.Data, ObjectsDirName) }

func (l Layout) ObjectsLayout() string { return filepath.Join(l.ObjectsDir(), ObjectsLayoutFile) }

func RestoreDefaults(volumes []string, archiveEnv map[string]string) map[string]string {
	out := map[string]string{}
	has := func(name string) bool {
		for _, v := range volumes {
			if v == name {
				return true
			}
		}
		return false
	}
	if (has(SeaweedFSVolume) || archiveEnv[S3EndpointEnv] != "") && archiveEnv[StorageBackendEnv] == "" {
		out[StorageBackendEnv] = string(StorageS3)
	}
	if (has(ValkeyVolume) || archiveEnv[KVURLEnv] != "") && archiveEnv[KVModeEnv] == "" {
		out[KVModeEnv] = string(KVStandalone)
	}
	return out
}

func FreshModes(env map[string]string) map[string]string {
	out := map[string]string{}
	for k, v := range InstallDefaults {
		if strings.TrimSpace(env[k]) == "" {
			out[k] = v
		}
	}
	if _, ok := out[StorageBackendEnv]; ok && strings.TrimSpace(env[S3EndpointEnv]) != "" {
		out[StorageBackendEnv] = string(StorageS3)
	}
	if _, ok := out[KVModeEnv]; ok {
		if b := strings.TrimSpace(env[DatabaseBackendEnv]); strings.TrimSpace(env[KVURLEnv]) != "" || b != "" && b != PostgresDatabaseBackend {
			out[KVModeEnv] = string(KVStandalone)
		}
	}
	return out
}

type Modes struct {
	Rust            RustProcess
	Storage         StorageBackend
	KV              KVMode
	StorageRoot     string
	UploadsTTL      string
	StorageReadOnly bool
	BundledValkey   bool
	BundledWeed     bool
}

var (
	ErrInvalidMode     = errors.New("spec: invalid mode")
	ErrNeedsMigration  = errors.New("spec: data from the previous mode is still in place")
	ErrModeCombination = errors.New("spec: settings that can't be used together")
)

func ResolveModes(env map[string]string, dataDir string) (Modes, error) {
	get := func(k string) string { return strings.TrimSpace(env[k]) }
	m := Modes{
		Rust:    RustProcess(get(RustProcessEnv)),
		Storage: StorageBackend(get(StorageBackendEnv)),
		KV:      KVMode(get(KVModeEnv)),
	}
	switch m.Rust {
	case "":
		m.Rust = RustPerService
	case RustSingle, RustPerService:
	default:
		return Modes{}, fmt.Errorf("%w: %s is %q, use %s or %s", ErrInvalidMode, RustProcessEnv, m.Rust, RustSingle, RustPerService)
	}
	switch m.Storage {
	case "":
		m.Storage = StorageS3
	case StorageS3, StorageFilesystem:
	default:
		return Modes{}, fmt.Errorf("%w: %s is %q, use %s or %s", ErrInvalidMode, StorageBackendEnv, m.Storage, StorageS3, StorageFilesystem)
	}
	switch m.KV {
	case "":
		m.KV = KVStandalone
	case KVStandalone, KVCluster, KVPostgres:
	default:
		return Modes{}, fmt.Errorf("%w: %s is %q, use %s, %s or %s", ErrInvalidMode, KVModeEnv, m.KV, KVStandalone, KVCluster, KVPostgres)
	}

	m.UploadsTTL = get(StorageUploadsTTLEnv)
	if m.UploadsTTL == "" {
		m.UploadsTTL = get(SeaweedFSUploadsTTLEnv)
	}
	if m.UploadsTTL == "" {
		m.UploadsTTL = DefaultUploadsTTL
	}
	if !ValidUploadsTTL(m.UploadsTTL) {
		return Modes{}, fmt.Errorf("%w: the uploads expiry %q is not a number followed by m, h, d, w, M or y, or off", ErrInvalidMode, m.UploadsTTL)
	}
	if v := get(StorageReadOnlyEnv); v != "" {
		ro, err := strconv.ParseBool(v)
		if err != nil {
			return Modes{}, fmt.Errorf("%w: %s is %q, use true or false", ErrInvalidMode, StorageReadOnlyEnv, v)
		}
		m.StorageReadOnly = ro
	}

	layout := Layout{Data: dataDir}
	if m.Storage == StorageFilesystem {
		if get(S3EndpointEnv) != "" {
			return Modes{}, fmt.Errorf("%w: %s=%s stores files on this machine, so %s must be empty", ErrModeCombination, StorageBackendEnv, StorageFilesystem, S3EndpointEnv)
		}
		m.StorageRoot = layout.ObjectsDir()
		if nonEmpty(layout.SeaweedFSDir()) && !exists(layout.ObjectsLayout()) {
			return Modes{}, fmt.Errorf("%w: %s holds SeaweedFS data and %s does not exist. Move the files over with %s, or set %s=%s", ErrNeedsMigration, layout.SeaweedFSDir(), layout.ObjectsLayout(), MigrateStorageCommand, StorageBackendEnv, StorageS3)
		}
	} else {
		m.BundledWeed = get(S3EndpointEnv) == ""
	}
	switch m.KV {
	case KVPostgres:
		if get(KVURLEnv) != "" {
			return Modes{}, fmt.Errorf("%w: %s=%s keeps this data in Postgres, so %s must be empty", ErrModeCombination, KVModeEnv, KVPostgres, KVURLEnv)
		}
		if b := get(DatabaseBackendEnv); b != "" && b != PostgresDatabaseBackend {
			return Modes{}, fmt.Errorf("%w: %s=%s needs %s=%s", ErrModeCombination, KVModeEnv, KVPostgres, DatabaseBackendEnv, PostgresDatabaseBackend)
		}
		if nonEmpty(layout.ValkeyDir()) {
			return Modes{}, fmt.Errorf("%w: %s holds Valkey data. Move it over with %s, or set %s=%s", ErrNeedsMigration, layout.ValkeyDir(), MigrateKVCommand, KVModeEnv, KVStandalone)
		}
	case KVStandalone:
		m.BundledValkey = get(KVURLEnv) == ""
	}
	return m, nil
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func nonEmpty(dir string) bool {
	entries, err := os.ReadDir(dir)
	return err == nil && len(entries) > 0
}
