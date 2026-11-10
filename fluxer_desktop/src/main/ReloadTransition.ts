// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BrowserWindow} from 'electron';

export const RELOAD_DIM_FILTER = 'grayscale(1) brightness(0.8)';
export const RELOAD_DIM_MS = 180;
const COVER_MAX_AGE_MS = 10_000;
const COVER_JPEG_QUALITY = 85;

let pendingCover: {readonly jpeg: Buffer; readonly dimStartedAt: number} | null = null;
let coverNextDocument = false;

export function consumeReloadCover(): number | null {
	const cover = coverNextDocument ? pendingCover : null;
	coverNextDocument = false;
	return cover === null ? null : Math.min(RELOAD_DIM_MS, Math.max(0, Date.now() - cover.dimStartedAt));
}

export function takeReloadCoverImage(): Buffer | null {
	const cover = pendingCover;
	pendingCover = null;
	return cover !== null && Date.now() - cover.dimStartedAt < COVER_MAX_AGE_MS ? cover.jpeg : null;
}

export async function dimAndCaptureForReload(window: BrowserWindow): Promise<void> {
	const image = await window.webContents.capturePage().catch(() => null);
	if (window.isDestroyed()) return;
	const dimStartedAt = Date.now();
	void window.webContents
		.executeJavaScript(
			`(() => {
				const root = document.documentElement;
				const instant = matchMedia('(prefers-reduced-motion: reduce)').matches;
				root.style.transition = instant ? 'none' : 'filter ${RELOAD_DIM_MS}ms ease-out';
				root.style.filter = '${RELOAD_DIM_FILTER}';
			})()`,
			true,
		)
		.catch(() => undefined);
	if (image === null || image.isEmpty()) return;
	pendingCover = {jpeg: image.toJPEG(COVER_JPEG_QUALITY), dimStartedAt};
	coverNextDocument = true;
}
