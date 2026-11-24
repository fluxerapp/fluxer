// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';

interface MockSnowflakeServiceConfig {
	startTimestampMs?: number;
}

function waitForMillisecond(): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, 1));
}

export class MockSnowflakeService implements ISnowflakeService {
	private nextTimestampMs?: number;
	private initialized = false;
	constructor(config: MockSnowflakeServiceConfig = {}) {
		this.nextTimestampMs = config.startTimestampMs;
	}
	configure(config: MockSnowflakeServiceConfig): void {
		if (config.startTimestampMs !== undefined) {
			this.nextTimestampMs = config.startTimestampMs;
		}
	}
	async initialize(): Promise<void> {
		this.initialized = true;
	}
	async shutdown(): Promise<void> {
		this.initialized = false;
	}
	async generate(): Promise<bigint> {
		if (!this.initialized) await this.initialize();
		return this.generateId();
	}
	async generateForChannel(_channelId: string | bigint): Promise<bigint> {
		return this.generate();
	}
	private async generateId(): Promise<bigint> {
		const timestamp = this.nextTimestampMs ?? Date.now();
		this.nextTimestampMs = timestamp + 1;
		while (Date.now() < timestamp) {
			await waitForMillisecond();
		}
		return createSnowflake({timestamp, workerId: 0, sequence: 0});
	}
}
