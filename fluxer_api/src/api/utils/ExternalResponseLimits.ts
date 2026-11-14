// SPDX-License-Identifier: AGPL-3.0-or-later

export const EXTERNAL_RESPONSE_LIMITS = {
	ssoUserInfoBytes: 512 * 1024,
	ssoTokenBytes: 256 * 1024,
	ssoDiscoveryBytes: 256 * 1024,
	ssoJwksBytes: 256 * 1024,
	pwnedPasswordsBytes: 1024 * 1024,
	statusPageSummaryBytes: 1024 * 1024,
	fileBlocklistBytes: 25 * 1024 * 1024,
	urlBlocklistBytes: 25 * 1024 * 1024,
} as const;
