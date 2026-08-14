// SPDX-License-Identifier: AGPL-3.0-or-later

export function resolveSettingsTitle(primaryTitle: string | null, fallbackTitle: string | null): string | null {
	if (primaryTitle != null && primaryTitle.length > 0) {
		return primaryTitle;
	}
	return fallbackTitle;
}
