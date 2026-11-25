// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"net/netip"
	"net/url"
	"regexp"
	"runtime"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/objectstore"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type storageFlags struct {
	Endpoint   string
	Public     string
	Region     string
	PathStyle  bool
	AccessKey  string
	SecretFile string
	Prefix     string
}

type storageChoice struct {
	objectstore.Store
	Public  string
	Prefix  string
	Buckets map[string]string
}

var storageFlagNames = []string{
	"storage-endpoint", "storage-public-endpoint", "storage-region", "storage-path-style", "storage-access-key", "storage-secret-key-file", "storage-bucket-prefix",
}

var storageBucketVars = []string{"FLUXER_S3_BUCKET_CDN", "FLUXER_S3_BUCKET_UPLOADS", "FLUXER_S3_BUCKET_REPORTS", "FLUXER_S3_BUCKET_HARVESTS"}

var regionSuffixes = []string{".amazonaws.com.cn", ".amazonaws.com", ".backblazeb2.com", ".wasabisys.com", ".digitaloceanspaces.com", ".scw.cloud", ".your-objectstorage.com", ".linodeobjects.com"}

var reservedBucketPrefixes = []string{"xn--", "sthree-", "amzn-s3-demo-"}

var reservedBucketSuffixes = []string{"-s3alias", "--ol-s3", ".mrap", "--x-s3", "--table-s3"}

var bucketPrefixPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9-]*$`)

func (f *storageFlags) bind(c *cobra.Command) {
	fl := c.Flags()
	fl.StringVar(&f.Endpoint, "storage-endpoint", "", "your own S3-compatible object store, like https://s3.example.com. Without it, Fluxer keeps uploads as files in its data folder")
	fl.StringVar(&f.Public, "storage-public-endpoint", "", "the object store address browsers use, when it differs from --storage-endpoint")
	fl.StringVar(&f.Region, "storage-region", "", "the object store region (found from --storage-endpoint when it can be)")
	fl.BoolVar(&f.PathStyle, "storage-path-style", true, "put the bucket in the path of object store addresses, like https://s3.example.com/bucket")
	fl.StringVar(&f.AccessKey, "storage-access-key", "", "the object store access key")
	fl.StringVar(&f.SecretFile, "storage-secret-key-file", "", "file with the object store secret key, or - for stdin")
	fl.StringVar(&f.Prefix, "storage-bucket-prefix", "", "text to put before each bucket name, like chat- for chat-fluxer")
}

func (f *storageFlags) passed(c *cobra.Command) bool {
	for _, name := range storageFlagNames {
		if fl := c.Flags().Lookup(name); fl != nil && fl.Changed {
			return true
		}
	}
	return false
}

func (f *storageFlags) missing(c *cobra.Command) []string {
	if !f.passed(c) {
		return nil
	}
	var out []string
	for _, r := range []struct{ flag, value string }{
		{"storage-endpoint", f.Endpoint},
		{"storage-access-key", f.AccessKey},
		{"storage-secret-key-file", f.SecretFile},
	} {
		if r.value == "" {
			out = append(out, "--"+r.flag)
		}
	}
	return out
}

func (f *storageFlags) validate(c *cobra.Command) error {
	for _, r := range []struct {
		flag  string
		check func(string) error
	}{
		{"storage-endpoint", validStorageEndpoint},
		{"storage-public-endpoint", validPublicEndpoint},
		{"storage-region", storageValidator(settings.StorageRegion)},
		{"storage-access-key", storageValidator(settings.StorageAccess)},
		{"storage-bucket-prefix", validBucketPrefix},
	} {
		fl := c.Flags().Lookup(r.flag)
		if fl == nil || !fl.Changed {
			continue
		}
		if err := r.check(strings.TrimSpace(fl.Value.String())); err != nil {
			if noUserInfo(fl.Value.String()) != nil {
				return ui.Usage(fmt.Sprintf("Leave the user name and password out of --%s.", r.flag))
			}
			return ui.Usage(fmt.Sprintf("--%s can't be %q. %s", r.flag, fl.Value.String(), err.Error()))
		}
	}
	return nil
}

func storageValidator(name string) func(string) error {
	k, err := settings.Lookup(settings.NativeSet(stack.EmbeddedRelease()), nil, name, nil)
	if err != nil || k.Validate == nil {
		return func(string) error { return nil }
	}
	return k.Validate
}

func validStorageEndpoint(v string) error {
	if err := storageValidator(settings.StorageEndpoint)(v); err != nil {
		return err
	}
	if err := noUserInfo(v); err != nil {
		return err
	}
	u, err := url.Parse(strings.TrimSpace(v))
	if err != nil {
		return err
	}
	host := strings.ToLower(u.Hostname())
	local := host == "localhost" || strings.HasSuffix(host, ".localhost")
	if ip, err := netip.ParseAddr(host); err == nil {
		ip = ip.Unmap()
		local = ip.IsLoopback() || ip.IsLinkLocalUnicast() || ip.IsUnspecified()
	}
	if local {
		return setupInputError(fmt.Sprintf("Fluxer's containers can't connect to this machine at %s. Use the object store's domain name or this machine's network IP address.", host))
	}
	return nil
}

func validPublicEndpoint(v string) error {
	if err := storageValidator(settings.StoragePublic)(v); err != nil {
		return err
	}
	return noUserInfo(v)
}

func noUserInfo(v string) error {
	if u, err := url.Parse(strings.TrimSpace(v)); err == nil && u.User != nil {
		return setupInputError("Leave the user name and password out of the address.")
	}
	return nil
}

func bucketDefaults() map[string]string {
	out := map[string]string{}
	for _, v := range storageBucketVars {
		if b := instance.ReleaseDefault(settings.NativeSet(stack.EmbeddedRelease()), v); b != "" {
			out[v] = b
		}
	}
	return out
}

func validBucketPrefix(v string) error {
	if v == "" {
		return nil
	}
	longest := 0
	for _, b := range bucketDefaults() {
		longest = max(longest, len(b))
	}
	most := 63 - longest
	if !bucketPrefixPattern.MatchString(v) || len(v) > most {
		return setupInputError(fmt.Sprintf("Use up to %d lowercase letters, numbers or hyphens, starting with a letter or number.", most))
	}
	for _, b := range bucketDefaults() {
		for _, r := range reservedBucketPrefixes {
			if strings.HasPrefix(v+b, r) {
				return setupInputError(fmt.Sprintf("Bucket names can't start with %s. Use a different prefix.", r))
			}
		}
		for _, r := range reservedBucketSuffixes {
			if strings.HasSuffix(v+b, r) {
				return setupInputError(fmt.Sprintf("Bucket names can't end with %s. Use a different prefix.", r))
			}
		}
	}
	return nil
}

func (s *storageChoice) bucketNames() []string {
	var out []string
	for _, v := range storageBucketVars {
		if b, ok := s.Buckets[v]; ok {
			out = append(out, b)
		}
	}
	return out
}

func guessRegion(endpoint string) string {
	u, err := url.Parse(endpoint)
	if err == nil {
		host := strings.ToLower(u.Hostname())
		if strings.HasSuffix(host, ".r2.cloudflarestorage.com") {
			return "auto"
		}
		for _, suffix := range regionSuffixes {
			rest, ok := strings.CutSuffix(host, suffix)
			if !ok {
				continue
			}
			label := strings.TrimPrefix(rest[strings.LastIndex(rest, ".")+1:], "s3-")
			if label == "external-1" {
				return "us-east-1"
			}
			if label != "" && label != "s3" {
				return label
			}
		}
	}
	return instance.ReleaseDefault(settings.NativeSet(stack.EmbeddedRelease()), "FLUXER_S3_REGION")
}

const (
	storeLocal = "local"
	storeS3    = "s3"
)

func (in *installer) storageFields() []ui.Field {
	f := &in.f.Storage
	if in.store == "" {
		in.store = storeLocal
		if f.passed(in.c) {
			in.store = storeS3
		}
	}
	s3 := func() bool { return in.store != storeS3 }
	return []ui.Field{
		{
			Title: "Where do you want to keep uploaded files?",
			Value: &in.store,
			Options: []ui.Option{
				{Label: "On this machine", Value: storeLocal},
				{Label: "An S3-compatible service", Value: storeS3, Hint: "Amazon S3, Backblaze B2, Cloudflare R2, MinIO and others"},
			},
			Hide: func() bool { return f.passed(in.c) },
		},
		{Flag: "storage-endpoint", FlagLabel: "--storage-endpoint", Title: "Object store address", Placeholder: "https://s3.example.com", Value: &f.Endpoint, Validate: validStorageEndpoint, Hide: s3},
		{Flag: "storage-access-key", FlagLabel: "--storage-access-key", Title: "Access key", Value: &f.AccessKey, Validate: storageValidator(settings.StorageAccess), Hide: s3},
		{Title: "Secret key", Value: &in.secret, Secret: true, Validate: storageValidator(settings.StorageSecret), Hide: func() bool { return s3() || f.SecretFile != "" }},
		{Flag: "storage-region", FlagLabel: "--storage-region", Title: "Region", Value: &f.Region, Optional: true, DescriptionFunc: func() string {
			if r := guessRegion(strings.TrimSpace(f.Endpoint)); r != "" {
				return "Leave it empty to use " + r + "."
			}
			return ""
		}, Validate: optional(storageValidator(settings.StorageRegion)), Hide: s3},
		{Flag: "storage-bucket-prefix", FlagLabel: "--storage-bucket-prefix", Title: "Text to put before each bucket name", Description: "Bucket names are shared by every account on some providers. A prefix like chat- makes them yours.", Value: &f.Prefix, Optional: true, Validate: optional(validBucketPrefix), Hide: s3},
		{Flag: "storage-public-endpoint", FlagLabel: "--storage-public-endpoint", Title: "Address browsers use, if it's different", Value: &f.Public, Optional: true, Validate: optional(validPublicEndpoint), Hide: s3},
		{
			Title: "How often should rexctl back up your instance?",
			Value: &in.backups,
			Options: []ui.Option{
				{Label: "Every night", Value: host.ScheduleDaily, Hint: "at about 03:00"},
				{Label: "Every week", Value: host.ScheduleWeekly, Hint: "on Sunday at about 03:00"},
				{Label: "Not now", Value: host.ScheduleOff, Hint: "rexctl backup schedule turns it on later"},
			},
			Default: host.ScheduleDaily,
			Hide:    func() bool { return !in.offersBackups() },
		},
	}
}

func (in *installer) offersBackups() bool {
	return runtime.GOOS == "linux" && in.app.UI.Interactive() && !in.app.UI.Yes() && host.UpgradeScheduler() != ""
}

func (in *installer) scheduleBackups(ctx context.Context) {
	if !in.offersBackups() || in.backups == "" || in.backups == host.ScheduleOff {
		return
	}
	if _, err := setBackupSchedule(ctx, in.app, in.inst, in.backups); err != nil {
		in.app.UI.ReportError(err)
		return
	}
	in.scheduled = true
}

func (in *installer) backupsRow() []ui.KV {
	if !in.scheduled {
		return nil
	}
	when := "Every night"
	if in.backups != host.ScheduleDaily {
		when = "Every Sunday"
	}
	kept := "all kept"
	if keep := in.inst.KeepBackups(); keep != instance.KeepAllBackups {
		kept = fmt.Sprintf("newest %d kept", keep)
	}
	return []ui.KV{{Key: "Backups", Value: when + ", " + kept}}
}

func optional(check func(string) error) func(string) error {
	return func(v string) error {
		if strings.TrimSpace(v) == "" {
			return nil
		}
		return check(v)
	}
}

func (in *installer) storageSummary() string {
	where := "On this machine"
	in.storage = nil
	if in.store == storeS3 {
		in.storage = in.storageChoice()
		where = "S3-compatible service at " + in.storage.Endpoint
	}
	if !in.offersBackups() {
		return where
	}
	switch in.backups {
	case host.ScheduleDaily:
		return where + ", backed up every night"
	case host.ScheduleWeekly:
		return where + ", backed up every week"
	}
	return where + ", no scheduled backups"
}

func (in *installer) storageChoice() *storageChoice {
	f := &in.f.Storage
	region := strings.TrimSpace(f.Region)
	if region == "" {
		region = guessRegion(strings.TrimSpace(f.Endpoint))
	}
	prefix := strings.TrimSpace(f.Prefix)
	buckets := bucketDefaults()
	for v, b := range buckets {
		buckets[v] = prefix + b
	}
	return &storageChoice{
		Store: objectstore.Store{
			Endpoint:  strings.TrimSpace(f.Endpoint),
			Region:    region,
			AccessKey: strings.TrimSpace(f.AccessKey),
			SecretKey: in.secret,
			PathStyle: f.PathStyle,
		},
		Public:  strings.TrimSpace(f.Public),
		Prefix:  prefix,
		Buckets: buckets,
	}
}

func (in *installer) checkStorage(ctx context.Context, c *ui.Checklist) error {
	if in.storage == nil {
		return nil
	}
	if in.f.DryRun {
		in.change(ui.PlanRun, fmt.Sprintf("Check that Fluxer can use the buckets %s", ui.JoinAnd(in.storage.bucketNames())), nil)
		return nil
	}
	for {
		s := in.storage
		buckets := s.bucketNames()
		var problem *storageProblem
		var cause error
		c.Check("storage", "Object store", func(ctx context.Context) (ui.State, string, error) {
			err := s.Check(ctx, buckets)
			if err == nil {
				return ui.StateOK, "Fluxer can read and write the buckets " + ui.JoinAnd(buckets), nil
			}
			var se *objectstore.Error
			if !errors.As(err, &se) {
				return ui.StateFail, "", err
			}
			p := in.storageFailure(se)
			problem, cause = &p, err
			return ui.StateFail, p.msg, nil
		})
		if problem == nil {
			return nil
		}
		in.app.UI.Detail("%v", cause)
		if !in.app.UI.Interactive() {
			return ui.Wrap(cause, problem.msg, problem.fix+", then run rexctl install again."+problem.hint)
		}
		choice, err := c.Recover(problem.fix+"."+problem.hint+" What do you want to do?", []ui.Option{
			{Label: "Check again", Value: "again"},
			{Label: "Stop here", Value: "stop"},
		})
		if err != nil {
			return err
		}
		if choice == "stop" {
			return ui.Refused(problem.msg, problem.fix+", then run rexctl install again."+problem.hint)
		}
	}
}

type storageProblem struct {
	msg  string
	fix  string
	hint string
}

func (in *installer) storageFailure(se *objectstore.Error) storageProblem {
	s := in.storage
	host := s.Endpoint
	if u, err := url.Parse(s.Endpoint); err == nil {
		host = u.Host
	}
	switch se.Problem {
	case objectstore.Unreachable:
		return storageProblem{msg: fmt.Sprintf("Couldn't connect to your object store at %s.", s.Endpoint), fix: "Check the address and that this machine can connect to it"}
	case objectstore.Untrusted:
		return storageProblem{
			msg: fmt.Sprintf("This machine doesn't trust the certificate of your object store at %s.", host),
			fix: "Fluxer's containers only trust certificates from public certificate authorities, so trusting it on this machine isn't enough. Give the object store a certificate from a public authority, like Let's Encrypt",
		}
	case objectstore.WrongName:
		return storageProblem{
			msg: fmt.Sprintf("The certificate of your object store isn't valid for %s.", host),
			fix: "Use the address the certificate was issued for, or give the object store a certificate for this address",
		}
	case objectstore.Credentials:
		return storageProblem{msg: "Your object store didn't accept the access key and secret key.", fix: "Check both keys"}
	case objectstore.WrongRegion:
		return storageProblem{msg: "Your object store expects a different region.", fix: "Check the region your provider shows for the bucket"}
	case objectstore.MissingBuckets:
		if len(se.Buckets) == 1 {
			return storageProblem{
				msg:  fmt.Sprintf("Your object store has no bucket called %s.", se.Buckets[0]),
				fix:  fmt.Sprintf("Fluxer needs a bucket with exactly this name. Create %s", se.Buckets[0]),
				hint: in.prefixHint(),
			}
		}
		return storageProblem{
			msg:  fmt.Sprintf("Your object store doesn't have the buckets %s.", ui.JoinAnd(se.Buckets)),
			fix:  fmt.Sprintf("Fluxer needs buckets with exactly these names. Create %s", ui.JoinAnd(se.Buckets)),
			hint: in.prefixHint(),
		}
	case objectstore.Denied:
		return storageProblem{
			msg:  fmt.Sprintf("The access key can't read and write files in the bucket %s.", ui.JoinAnd(se.Buckets)),
			fix:  fmt.Sprintf("Give the key read, write and delete access to the buckets %s", ui.JoinAnd(s.bucketNames())),
			hint: in.prefixHint(),
		}
	}
	return storageProblem{msg: fmt.Sprintf("Your object store returned an error: %s.", strings.TrimSuffix(se.Error(), ".")), fix: "Check your object store settings"}
}

func (in *installer) prefixHint() string {
	if in.storage.Prefix != "" {
		return ""
	}
	how := "pass it with --storage-bucket-prefix"
	return fmt.Sprintf(" If another account already has those names on your provider, create the buckets with a prefix, like chat-%s, and %s.", in.storage.bucketNames()[0], how)
}

func (s *storageChoice) values() map[string]string {
	out := map[string]string{
		settings.StorageEndpoint: s.Endpoint,
		settings.StorageRegion:   s.Region,
		settings.StorageAccess:   s.AccessKey,
		settings.StorageSecret:   s.SecretKey,
	}
	if !s.PathStyle {
		out[settings.StoragePath] = "false"
	}
	if s.Public != "" {
		out[settings.StoragePublic] = s.Public
	}
	if s.Prefix != "" {
		maps.Copy(out, s.Buckets)
	}
	return out
}
