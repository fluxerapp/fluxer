// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import "strings"

type BanReason struct {
	Name  string
	Label string
	Code  int
}

var BanReasons = []BanReason{
	{"spam", "Spam", 3},
	{"scam", "Scams or phishing", 8},
	{"harassment", "Harassment or bullying", 11},
	{"hate", "Hate speech", 20},
	{"threats", "Threats or violent speech", 23},
	{"raids", "Coordinated raids", 5},
	{"automation", "Automation or self-bots", 6},
	{"ban-evasion", "Ban evasion", 17},
	{"impersonation", "Impersonation", 22},
	{"malware", "Malicious links or malware", 21},
	{"doxxing", "Sharing private information", 10},
	{"intimate-images", "Sharing intimate images without consent", 7},
	{"explicit-content", "Unwanted explicit content", 15},
	{"adult-content", "Adult content in the wrong place", 26},
	{"violence", "Graphic violence", 16},
	{"self-harm", "Promoting self-harm", 25},
	{"illegal", "Illegal goods or activity", 27},
	{"deception", "Harmful deception", 28},
	{"exploits", "Cheating or exploits", 4},
	{"report-abuse", "Abusing reports", 29},
}

func FindBanReason(name string) (BanReason, bool) {
	key := strings.ToLower(strings.TrimSpace(name))
	key = strings.ReplaceAll(key, " ", "-")
	for _, r := range BanReasons {
		if r.Name == key {
			return r, true
		}
	}
	return BanReason{}, false
}

func BanReasonNames() []string {
	names := make([]string, len(BanReasons))
	for i, r := range BanReasons {
		names[i] = r.Name
	}
	return names
}
