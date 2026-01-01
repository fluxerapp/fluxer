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
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {Config} from '~/Config';
import {InstanceConfigRepository} from '~/instance/InstanceConfigRepository';
import {ClamAV} from './ClamAV';
import type {ICacheService} from './ICacheService';
import type {IVirusScanService, VirusScanResult} from './IVirusScanService';

export class VirusScanService implements IVirusScanService {
	private clamav: ClamAV;
	private readonly CACHE_TTL = 60 * 60 * 24 * 7;
	private static readonly ALERT_SAMPLE_RATE = 0.05;
	private static readonly ERROR_FIELD_LIMIT = 900;
	private instanceConfigRepository: InstanceConfigRepository;

	constructor(private cacheService: ICacheService) {
		this.clamav = new ClamAV();
		this.instanceConfigRepository = new InstanceConfigRepository();
	}

	async initialize(): Promise<void> {}

	async scanFile(filePath: string): Promise<VirusScanResult> {
		const buffer = await fs.readFile(filePath);
		const filename = path.basename(filePath);
		return this.scanBuffer(buffer, filename);
	}

	async scanBuffer(buffer: Buffer, filename: string): Promise<VirusScanResult> {
		const fileHash = crypto.createHash('sha256').update(buffer).digest('hex');
		const isCachedVirus = await this.isVirusHashCached(fileHash);
		if (isCachedVirus) {
			return {
				isClean: false,
				threat: 'Cached virus signature',
				fileHash,
			};
		}
		try {
			const tempDir = os.tmpdir();
			const tempFilePath = path.join(tempDir, `scan_${Date.now()}_${filename}`);
			await fs.writeFile(tempFilePath, buffer);
			try {
				const scanResult = await this.clamav.scanFile(tempFilePath);
				if (!scanResult.isClean) {
					await this.cacheVirusHash(fileHash);
					return {
						isClean: false,
						threat: scanResult.virus || 'Virus detected',
						fileHash,
					};
				}
				return {
					isClean: true,
					fileHash,
				};
			} finally {
				try {
					await fs.unlink(tempFilePath);
				} catch (error) {
					console.warn('Failed to cleanup temp file:', tempFilePath, error);
				}
			}
		} catch (error) {
			console.error('ClamAV scan failed:', error);
			void this.reportScanFailure(error, filename, fileHash);

			if (Config.clamav.failOpen) {
				return {
					isClean: true,
					fileHash,
				};
			}

			throw new Error(`Virus scan failed: ${this.describeError(error)}`);
		}
	}

	async isVirusHashCached(fileHash: string): Promise<boolean> {
		const cacheKey = `virus:${fileHash}`;
		const cached = await this.cacheService.get(cacheKey);
		return cached != null;
	}

	async cacheVirusHash(fileHash: string): Promise<void> {
		const cacheKey = `virus:${fileHash}`;
		await this.cacheService.set(cacheKey, 'true', this.CACHE_TTL);
	}

	private describeError(error: unknown): string {
		if (typeof error === 'string') {
			return error;
		}
		if (error instanceof Error) {
			return error.message;
		}
		return 'Unknown error';
	}

	private truncateText(value: string, limit: number): string {
		if (value.length <= limit) return value;
		return `${value.slice(0, limit - 3)}...`;
	}

	private async reportScanFailure(error: unknown, filename: string, fileHash: string): Promise<void> {
		const instanceConfig = await this.instanceConfigRepository.getInstanceConfig();
		const webhookUrl = instanceConfig.systemAlertsWebhookUrl;
		if (!webhookUrl) return;

		if (Math.random() >= VirusScanService.ALERT_SAMPLE_RATE) return;

		const errorDescription = this.truncateText(this.describeError(error), VirusScanService.ERROR_FIELD_LIMIT);

		const payload = {
			username: 'Virus Scan Monitor',
			content: 'A virus scan failed to complete.',
			embeds: [
				{
					title: 'Virus scan failure detected',
					description: `Unable to scan attachment ${filename}`,
					color: 0xe53e3e,
					fields: [
						{
							name: 'Scan mode',
							value: Config.clamav.failOpen ? 'Fail-open' : 'Fail-closed',
							inline: true,
						},
						{
							name: 'File hash',
							value: fileHash,
						},
						{
							name: 'Error',
							value: errorDescription || 'Unknown error',
						},
					],
					timestamp: new Date().toISOString(),
				},
			],
		};

		try {
			const response = await fetch(webhookUrl, {
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: JSON.stringify(payload),
			});

			if (!response.ok) {
				const body = await response.text();
				console.warn('Failed to deliver virus scan alert', response.status, body);
			}
		} catch (broadcastError) {
			console.warn('Failed to deliver virus scan alert', broadcastError);
		}
	}
}
