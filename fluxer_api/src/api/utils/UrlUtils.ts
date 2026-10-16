// SPDX-License-Identifier: AGPL-3.0-or-later

export function trimTrailingSlash(url: string): string {
	return url.replace(/\/+$/u, '');
}
