/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import crypto from 'node:crypto';
import type {IVirusScanService, VirusScanResult} from './IVirusScanService';

export class DisabledVirusScanService implements IVirusScanService {
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

	async isVirusHashCached(fileHash: string): Promise<boolean> {
		return this.cachedVirusHashes.has(fileHash);
	}

	async cacheVirusHash(fileHash: string): Promise<void> {
		this.cachedVirusHashes.add(fileHash);
	}
}
