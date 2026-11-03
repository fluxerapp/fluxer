// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';

interface MockSnowflakeServiceConfig {
	startTimestampMs?: number;
}

export class MockSnowflakeService implements ISnowflakeService {
	private startTimestampMs?: number;
	private sequence = 0;
	private initialized = false;
	constructor(config: MockSnowflakeServiceConfig = {}) {
		this.startTimestampMs = config.startTimestampMs;
	}
	configure(config: MockSnowflakeServiceConfig): void {
		if (config.startTimestampMs !== undefined) {
			this.startTimestampMs = config.startTimestampMs;
			this.sequence = 0;
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
	private generateId(): bigint {
		const timestamp = this.startTimestampMs ?? Date.now();
		const id = createSnowflake({timestamp, workerId: 0, sequence: this.sequence});
		this.sequence = (this.sequence + 1) & 4095;
		if (this.sequence === 0 || this.startTimestampMs !== undefined) this.startTimestampMs = timestamp + 1;
		return id;
	}
}
