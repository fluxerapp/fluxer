// SPDX-License-Identifier: AGPL-3.0-or-later

import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {TEXT_PREVIEW_MAX_BYTES} from '@app/features/messaging/utils/AttachmentPreviewUtils';

const SIGNATURE_REFUSAL_STATUSES = new Set([401, 403, 404, 410]);

export class PreviewSizeLimitError extends Error {
	constructor() {
		super('Attachment preview exceeds the size limit');
		this.name = 'PreviewSizeLimitError';
	}
}

export function isAttachmentSignatureRefusal(status: number): boolean {
	return SIGNATURE_REFUSAL_STATUSES.has(status);
}

export async function readPreviewText(response: Response): Promise<string> {
	const contentLength = response.headers.get('content-length');
	if (contentLength !== null) {
		const parsedContentLength = Number(contentLength);
		if (Number.isFinite(parsedContentLength) && parsedContentLength > TEXT_PREVIEW_MAX_BYTES) {
			throw new PreviewSizeLimitError();
		}
	}
	const body = response.body;
	if (!body) {
		throw new Error('Attachment preview response has no readable body');
	}
	const reader = body.getReader();
	const chunks: Array<Uint8Array> = [];
	let totalBytes = 0;
	try {
		while (true) {
			const {done, value} = await reader.read();
			if (done) {
				break;
			}
			if (!value) {
				continue;
			}
			totalBytes += value.byteLength;
			if (totalBytes > TEXT_PREVIEW_MAX_BYTES) {
				await reader.cancel().catch(() => undefined);
				throw new PreviewSizeLimitError();
			}
			chunks.push(value);
		}
	} finally {
		reader.releaseLock();
	}
	const bytes = new Uint8Array(totalBytes);
	let offset = 0;
	for (const chunk of chunks) {
		bytes.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return new TextDecoder().decode(bytes);
}

function requestFailed(response: Response): Error {
	return new Error(`Attachment preview request failed: ${response.status} ${response.statusText}`);
}

export async function fetchTextualPreviewText(url: string, signal: AbortSignal): Promise<string> {
	const target = await AttachmentUrlRefresher.refresh(url);
	const response = await fetch(target, {signal});
	if (response.ok) {
		return readPreviewText(response);
	}
	if (!isAttachmentSignatureRefusal(response.status)) {
		throw requestFailed(response);
	}
	const refreshed = await AttachmentUrlRefresher.refresh(url, {force: true});
	if (refreshed === target) {
		throw requestFailed(response);
	}
	const retried = await fetch(refreshed, {signal});
	if (!retried.ok) {
		throw requestFailed(retried);
	}
	return readPreviewText(retried);
}
