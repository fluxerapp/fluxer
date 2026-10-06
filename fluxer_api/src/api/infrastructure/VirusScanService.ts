// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {VirusHashCache} from '@pkgs/virus_scan/src/cache/VirusHashCache';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import {ClamAVProvider} from '@pkgs/virus_scan/src/providers/ClamAVProvider';
import type {VirusScanResult} from '@pkgs/virus_scan/src/VirusScanResult';
import {VirusScanService as SharedVirusScanService} from '@pkgs/virus_scan/src/VirusScanService';

export class VirusScanService implements IVirusScanService {
	readonly enabled = true;
	private readonly service: SharedVirusScanService;

	constructor(cacheService: ICacheService) {
		const provider = new ClamAVProvider({
			host: Config.clamav.host,
			port: Config.clamav.port,
		});
		const virusHashCache = new VirusHashCache(cacheService);
		this.service = new SharedVirusScanService({
			provider,
			virusHashCache,
			logger: Logger,
			config: {
				failOpen: Config.clamav.failOpen,
			},
		});
	}

	async initialize(): Promise<void> {
		await this.service.initialize();
	}

	async scanFile(filePath: string): Promise<VirusScanResult> {
		return this.service.scanFile(filePath);
	}

	async scanBuffer(buffer: Buffer, filename: string): Promise<VirusScanResult> {
		return this.service.scanBuffer(buffer, filename);
	}
}
