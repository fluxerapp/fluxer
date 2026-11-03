// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {Config} from '@app/api/Config';
import {hashFileSha256} from '@app/api/infrastructure/StorageObjectHelpers';
import {Logger} from '@app/api/Logger';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import {ClamAVProvider} from '@pkgs/virus_scan/src/providers/ClamAVProvider';
import type {VirusScanProviderResult} from '@pkgs/virus_scan/src/VirusScanProviderResult';
import type {VirusScanResult} from '@pkgs/virus_scan/src/VirusScanResult';
import {seconds} from 'itty-time';

function describeError(error: unknown): string {
	if (typeof error === 'string') {
		return error;
	}
	if (error instanceof Error) {
		return error.message;
	}
	return 'Unknown error';
}

export class VirusScanService implements IVirusScanService {
	readonly enabled = true;
	private readonly provider = new ClamAVProvider({host: Config.clamav.host, port: Config.clamav.port});
	private readonly failOpen = Config.clamav.failOpen;

	constructor(private cacheService: ICacheService) {}

	async scanFile(filePath: string): Promise<VirusScanResult> {
		const fileHash = await hashFileSha256(filePath);
		return this.scan(fileHash, path.basename(filePath), () => this.provider.scanFile(filePath));
	}

	private async scan(
		fileHash: string,
		filename: string,
		run: () => Promise<VirusScanProviderResult>,
	): Promise<VirusScanResult> {
		const cacheKey = `virus:${fileHash}`;
		if ((await this.cacheService.get(cacheKey)) != null) {
			return {isClean: false, threat: 'Cached virus signature', fileHash};
		}
		try {
			const scanResult = await run();
			if (scanResult.isClean) {
				return {isClean: true, fileHash};
			}
			if (!scanResult.threat) {
				throw new Error('Virus scan provider returned infected status without threat name');
			}
			await this.cacheService.set(cacheKey, 'true', seconds('7 days'));
			return {isClean: false, threat: scanResult.threat, fileHash};
		} catch (error) {
			Logger.error({error: describeError(error), filename, fileHash}, 'Virus scan failed');
			if (this.failOpen) {
				return {isClean: true, fileHash};
			}
			throw new Error(`Virus scan failed: ${describeError(error)}`);
		}
	}
}
