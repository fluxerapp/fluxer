// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IStorageService} from '@app/api/infrastructure/IStorageService';

const RISK_S3_BUCKET = 'fluxer-geoip';
export const RISK_S3_KEYS = {
	feedUrls: 'blocklists/feed-urls.txt',
} as const;

export async function writeLinesToS3(storage: IStorageService, key: string, lines: Iterable<string>): Promise<number> {
	let totalBytes = 0;
	const collected: Array<string> = [];
	for (const line of lines) {
		collected.push(line);
		totalBytes += Buffer.byteLength(line, 'utf8') + 1;
	}
	const buf = Buffer.allocUnsafe(totalBytes);
	let offset = 0;
	for (const line of collected) {
		offset += buf.write(line, offset, 'utf8');
		buf[offset++] = 0x0a;
	}
	await storage.uploadObject({
		bucket: RISK_S3_BUCKET,
		key,
		body: buf,
		contentType: 'text/plain; charset=utf-8',
	});
	return collected.length;
}

export async function readLinesFromS3(storage: IStorageService, key: string): Promise<Array<string>> {
	try {
		const data = await storage.readObject(RISK_S3_BUCKET, key);
		const text = Buffer.from(data).toString('utf8');
		const lines: Array<string> = [];
		let start = 0;
		for (let i = 0; i < text.length; i++) {
			if (text.charCodeAt(i) === 0x0a) {
				if (i > start) lines.push(text.slice(start, i));
				start = i + 1;
			}
		}
		if (start < text.length) lines.push(text.slice(start));
		return lines;
	} catch {
		return [];
	}
}
