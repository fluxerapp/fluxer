// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {VirusScanResult} from '@pkgs/virus_scan/src/VirusScanResult';

export class DisabledVirusScanService implements IVirusScanService {
	readonly enabled = false;
	private cachedVirusHashes = new Set<string>();

	async initialize(): Promise<void> {}

	async scanFile(filePath: string): Promise<VirusScanResult> {
		const fileHash = crypto.createHash('sha256').update(filePath).digest('hex');
		return {
			isClean: true,
			fileHash,
		};
	}

	async scanBuffer(buffer: Buffer, _filename: string): Promise<VirusScanResult> {
		const fileHash = crypto.createHash('sha256').update(buffer).digest('hex');
		return {
			isClean: true,
			fileHash,
		};
	}

	async cacheVirusHash(fileHash: string): Promise<void> {
		this.cachedVirusHashes.add(fileHash);
	}
}
