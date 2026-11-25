// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"strings"
)

const (
	groupNetwork  = "Network"
	groupEmail    = "Email"
	groupVoice    = "Voice"
	groupStorage  = "Storage"
	groupAccounts = "Accounts"
	groupBranding = "Branding"
	groupAdvanced = "Advanced"
)

var Groups = []string{groupNetwork, groupEmail, groupVoice, groupStorage, groupAccounts, groupBranding, groupAdvanced}

var typeText = map[string]string{
	"hostname": "a domain name, like chat.example.com",
	"address":  "an address and port, like " + DefaultBind + ", or only a port to listen on this machine only",
	"port":     "a port number from 1 to 65535",
	"list":     "a comma-separated list",
	"ip":       "an IP address, or nothing to let Fluxer find it",
	"url":      "a full address that starts with http:// or https://",
	"bool":     "true or false",
	"secret":   "a secret",
	"image":    "an image reference, like postgres:16-alpine",
	"registry": "a registry host, like registry.example.com",
	"endpoint": "an engine address, like " + engineExample(),
}

func Describe(k Key) string {
	switch {
	case k.Takes != "":
		return k.Takes
	case len(k.Options) > 0:
		return joinOr(k.Options)
	case k.Secret:
		return typeText["secret"]
	}
	if t, ok := typeText[k.Type]; ok {
		return t
	}
	return "text"
}

func joinOr(items []string) string {
	switch len(items) {
	case 0:
		return ""
	case 1:
		return items[0]
	}
	return strings.Join(items[:len(items)-1], ", ") + " or " + items[len(items)-1]
}

var groupPrefixes = []struct {
	prefix string
	group  string
}{
	{"integrations.email.", groupEmail},
	{"app_public.branding.", groupBranding},
	{"app_public.legal.", groupBranding},
	{"voice.", groupVoice},
	{"storage.", groupStorage},
	{"media.", groupStorage},
	{"backups.", groupStorage},
	{"disk.", groupStorage},
	{"edge.", groupNetwork},
	{"network.", groupNetwork},
	{"domain_migration.", groupNetwork},
	{"registration.", groupAccounts},
	{"accounts.", groupAccounts},
	{"auth.", groupAccounts},
	{"sso.", groupAccounts},
	{"captcha.", groupAccounts},
	{"FLUXER_EMAIL_", groupEmail},
	{"FLUXER_SMTP_", groupEmail},
	{"FLUXER_LIVEKIT_", groupVoice},
	{"LIVEKIT_", groupVoice},
	{"FLUXER_VOICE_", groupVoice},
	{"FLUXER_S3_", groupStorage},
	{"FLUXER_STORAGE_", groupStorage},
	{"FLUXER_MEDIA_", groupStorage},
	{"FLUXER_EDGE_", groupNetwork},
	{"FLUXER_PUBLIC_", groupNetwork},
	{"FLUXER_SSO_", groupAccounts},
	{"FLUXER_AUTH_", groupAccounts},
}

func SectionGroup(section string) string {
	switch section {
	case "address", "edge", "network":
		return groupNetwork
	case "email":
		return groupEmail
	case "voice":
		return groupVoice
	case "storage", "media":
		return groupStorage
	case "sso":
		return groupAccounts
	case "instance":
		return groupBranding
	}
	return groupAdvanced
}

func Group(name string) string {
	switch name {
	case Domain, HTTPProxy, HTTPSProxy, NoProxy, "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY":
		return groupNetwork
	}
	for _, p := range groupPrefixes {
		if strings.HasPrefix(name, p.prefix) {
			return p.group
		}
	}
	return groupAdvanced
}
