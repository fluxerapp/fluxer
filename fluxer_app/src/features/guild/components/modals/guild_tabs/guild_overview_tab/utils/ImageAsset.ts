// SPDX-License-Identifier: AGPL-3.0-or-later

export function revokeObjectUrl(url: string | null | undefined): void {
	if (!url) return;
	if (!url.startsWith('blob:')) return;
	try {
		URL.revokeObjectURL(url);
	} catch {}
}
