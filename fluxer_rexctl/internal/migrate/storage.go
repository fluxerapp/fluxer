// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	StorageReadOnlyVar = "FLUXER_STORAGE_READ_ONLY"
	GeoIPBucket        = "fluxer-geoip"
	AttachmentChecks   = 50
)

var (
	storageWriters = []string{"api", "worker", "media-proxy", "gateway"}
	storageUsers   = []string{"api", "worker", "media-proxy", stack.ObjectsInitService}
	storageOn      = map[string]string{settings.StorageBackend: settings.StorageFilesystem, StorageReadOnlyVar: "true"}
	bucketVars     = [][2]string{
		{"FLUXER_S3_BUCKET_CDN", "fluxer"},
		{"FLUXER_S3_BUCKET_UPLOADS", "fluxer-uploads"},
		{"FLUXER_S3_BUCKET_REPORTS", "fluxer-reports"},
		{"FLUXER_S3_BUCKET_HARVESTS", "fluxer-harvests"},
	}
)

var (
	ErrWritesOpen    = errors.New("migrate: new uploads are stored as files only")
	ErrNotSwitched   = errors.New("migrate: this change wasn't made by rexctl migrate")
	ErrAlreadyOpen   = errors.New("migrate: uploads are already open")
	ErrNoNewBackup   = errors.New("migrate: no backup since the change")
	ErrExternalS3    = errors.New("migrate: uploads are in your own object store")
	ErrUndoElsewhere = errors.New("migrate: the move off SeaweedFS was made before the instance left Docker")
	ErrNoSeaweedFS   = errors.New("migrate: SeaweedFS's data is no longer here")
	ErrSharedBucket  = errors.New("migrate: the uploads bucket shares its name with another bucket")
)

const attachmentScript = `(async()=>{const fs=require('node:fs'),path=require('node:path');` +
	`const e=process.env,want=Number(process.argv[1]),root=e.FLUXER_STORAGE_ROOT,base=e.FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT;` +
	`const keys=[];const walk=async(dir,rel)=>{let list;try{list=await fs.promises.readdir(dir,{withFileTypes:true})}catch{return}` +
	`for(const d of list){if(keys.length>=5000)return;const r=rel+'/'+d.name;` +
	`if(d.isDirectory())await walk(path.join(dir,d.name),r);else if(d.isFile())keys.push(r)}};` +
	`await walk(path.join(root,e.FLUXER_S3_BUCKET_CDN||'fluxer','attachments'),'attachments');` +
	`for(let i=keys.length-1;i>0;i--){const j=Math.floor(Math.random()*(i+1));[keys[i],keys[j]]=[keys[j],keys[i]]}` +
	`const pick=keys.slice(0,want),missing=[];let ok=0,refused=0;` +
	`for(const k of pick){const r=await fetch(base+'/'+k.split('/').map(encodeURIComponent).join('/'),{method:'HEAD'});` +
	`if(r.ok)ok++;else if(r.status===404)missing.push(k);else refused++}` +
	`console.log(JSON.stringify({checked:pick.length,ok,missing,refused}))})()` +
	`.catch(e=>{console.error(String(e&&e.message||e));process.exit(1)})`

type attachmentReport struct {
	Checked int      `json:"checked"`
	OK      int      `json:"ok"`
	Missing []string `json:"missing"`
	Refused int      `json:"refused"`
}

type storageReport struct {
	Root     string `json:"root"`
	Verified bool   `json:"verified"`
	Buckets  []struct {
		Bucket     string   `json:"bucket"`
		Objects    int      `json:"objects"`
		Bytes      int64    `json:"bytes"`
		Sampled    int      `json:"sampled"`
		Mismatches []string `json:"mismatches"`
	} `json:"buckets"`
	Skipped []struct {
		Bucket string `json:"bucket"`
		Key    string `json:"key"`
		Reason string `json:"reason"`
	} `json:"skipped"`
}

func (r storageReport) problems() string {
	var out []string
	for _, b := range r.Buckets {
		for _, m := range b.Mismatches {
			out = append(out, b.Bucket+": "+m)
		}
	}
	if len(out) > 5 {
		out = append(out[:5], fmt.Sprintf("and %d more", len(out)-5))
	}
	return strings.Join(out, ", ")
}

func (r storageReport) totals() (objects int, bytes int64) {
	for _, b := range r.Buckets {
		objects += b.Objects
		bytes += b.Bytes
	}
	return objects, bytes
}

func CheckBuckets(env map[string]string) error {
	names := map[string]string{}
	for _, v := range bucketVars {
		names[v[0]] = strings.TrimSpace(env[v[0]])
		if names[v[0]] == "" {
			names[v[0]] = v[1]
		}
	}
	uploads := names["FLUXER_S3_BUCKET_UPLOADS"]
	for _, v := range bucketVars {
		if v[0] != "FLUXER_S3_BUCKET_UPLOADS" && names[v[0]] == uploads {
			return fmt.Errorf("%w: FLUXER_S3_BUCKET_UPLOADS and %s are both %q", ErrSharedBucket, v[0], uploads)
		}
	}
	if uploads == GeoIPBucket {
		return fmt.Errorf("%w: FLUXER_S3_BUCKET_UPLOADS is %q", ErrSharedBucket, GeoIPBucket)
	}
	return nil
}

type StorageStep struct {
	host     Host
	prev     map[string]string
	prepared bool
	switched bool
}

func NewStorageStep() *StorageStep { return &StorageStep{} }

func (s *StorageStep) Option() Option    { return OptStorage }
func (s *StorageStep) Writers() []string { return storageWriters }

func (s *StorageStep) Preflight(ctx context.Context, r *Run) error {
	h := r.Host
	if strings.TrimSpace(h.Settings()[settings.StorageEndpoint]) != "" {
		return fmt.Errorf("%w at storage.endpoint, and this change moves files out of the bundled SeaweedFS", ErrExternalS3)
	}
	env, err := h.Env(nil)
	if err != nil {
		return err
	}
	if err := CheckBuckets(env); err != nil {
		return err
	}
	if err := h.CheckTarget(ctx, storageOn); err != nil {
		return err
	}
	return h.CheckObjects(ctx)
}

func (s *StorageStep) Migrate(ctx context.Context, r *Run) error {
	s.host = r.Host
	if err := r.Host.PrepareObjects(ctx); err != nil {
		return err
	}
	s.prepared = true
	rep, err := s.run(ctx, r)
	if err != nil {
		return err
	}
	objects, bytes := rep.totals()
	r.note("Moved %d files (%s) out of SeaweedFS, and left out %d that Fluxer no longer needs.", objects, ui.Bytes(bytes), len(rep.Skipped))
	return nil
}

func (s *StorageStep) Verify(context.Context, *Run) error { return nil }

func (s *StorageStep) run(ctx context.Context, r *Run) (storageReport, error) {
	args := []string{ImportStorage, "--root", r.Host.ObjectsRoot()}
	var rep storageReport
	if err := runJSON(ctx, r.Host.Importer(), Job{Args: args, Objects: true, Stderr: r.Log}, &rep); err != nil {
		return rep, err
	}
	if !rep.Verified {
		return rep, fmt.Errorf("the files don't match SeaweedFS: %s", rep.problems())
	}
	return rep, nil
}

func (s *StorageStep) Switch(ctx context.Context, r *Run) error {
	s.prev = previous(r.Host.Settings(), storageOn)
	if err := r.Host.Apply(ctx, storageOn); err != nil {
		return err
	}
	s.switched = true
	r.State.Storage = &Switched{At: r.now(), Runtime: r.Host.Runtime(), Previous: s.prev}
	return nil
}

func (s *StorageStep) Check(ctx context.Context, r *Run) error {
	var rep attachmentReport
	j := Job{Script: attachmentScript, Args: []string{strconv.Itoa(AttachmentChecks)}, Objects: true, Stderr: r.Log}
	if err := runJSON(ctx, r.Host.Importer(), j, &rep); err != nil {
		return fmt.Errorf("checking attachments through media-proxy: %w", err)
	}
	if len(rep.Missing) > 0 {
		return fmt.Errorf("media-proxy can't find %d of %d attachments, such as %s", len(rep.Missing), rep.Checked, rep.Missing[0])
	}
	if rep.Refused > 0 {
		r.note("media-proxy refused %d of %d attachments rexctl asked for, likely because attachment links need a signature.", rep.Refused, rep.Checked)
	}
	return nil
}

func (s *StorageStep) Rollback(ctx context.Context, _ *Run) error {
	if s.host == nil || !s.prepared {
		return nil
	}
	return dropObjects(ctx, s.host, s.prev, s.switched)
}

func dropObjects(ctx context.Context, h Host, prev map[string]string, switched bool) error {
	if err := h.Remove(ctx, storageUsers); err != nil {
		return err
	}
	if err := h.DropData(ctx, stack.ObjectsVolume); err != nil {
		return err
	}
	if !switched {
		return nil
	}
	return h.Apply(ctx, prev)
}

func OpenWrites(ctx context.Context, h Host, st *State, at time.Time) error {
	switch {
	case st.Storage == nil:
		return ErrNotSwitched
	case st.Storage.Open():
		return ErrAlreadyOpen
	}
	if err := h.Apply(ctx, map[string]string{StorageReadOnlyVar: ""}); err != nil {
		return err
	}
	at = at.UTC()
	st.Storage.OpenedAt = &at
	if err := h.Start(ctx); err != nil {
		return err
	}
	return h.Check(ctx)
}

func UndoStorage(ctx context.Context, h Host, st *State) error {
	switch {
	case st.Storage == nil:
		return ErrNotSwitched
	case st.Storage.Open(), strings.TrimSpace(h.Settings()[StorageReadOnlyVar]) != "true":
		return ErrWritesOpen
	case st.Storage.Runtime != "" && st.Storage.Runtime != h.Runtime():
		return ErrUndoElsewhere
	}
	if ok, err := h.HoldsData(ctx, stack.SeaweedFSVolume); err != nil {
		return err
	} else if !ok {
		return ErrNoSeaweedFS
	}
	if err := h.Stop(ctx, storageWriters); err != nil {
		return err
	}
	if err := dropObjects(ctx, h, st.Storage.Previous, true); err != nil {
		return err
	}
	if err := h.Start(ctx); err != nil {
		return err
	}
	if err := h.Check(ctx); err != nil {
		return err
	}
	st.Storage = nil
	return nil
}
