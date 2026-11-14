// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import fs from 'node:fs';

export async function hashFileSha256(filePath: string): Promise<string> {
	const hash = createHash('sha256');
	const stream = fs.createReadStream(filePath, {highWaterMark: 1024 * 1024});
	try {
		for await (const chunk of stream) {
			hash.update(chunk as Buffer);
		}
	} catch (error) {
		stream.destroy();
		throw error;
	}
	return hash.digest('hex');
}
