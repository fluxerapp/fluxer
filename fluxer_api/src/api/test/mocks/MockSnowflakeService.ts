// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {vi} from 'vitest';

interface MockSnowflakeServiceConfig {
	initialCounter?: bigint;
	startTimestampMs?: number;
	workerId?: number;
	shouldFailInitialize?: boolean;
	shouldFailGenerate?: boolean;
}

export class MockSnowflakeService implements ISnowflakeService {
	private counter: bigint;
	private startTimestampMs?: number;
	private sequence: number = 0;
	private initialized: boolean = false;
	private config: MockSnowflakeServiceConfig;
	private generatedIds: Array<bigint> = [];
	readonly initializeSpy = vi.fn();
	readonly reinitializeSpy = vi.fn();
	readonly shutdownSpy = vi.fn();
	readonly generateSpy = vi.fn();
	readonly generateForChannelSpy = vi.fn();

	constructor(config: MockSnowflakeServiceConfig = {}) {
		this.config = config;
		this.counter = config.initialCounter ?? 1n;
		this.startTimestampMs = config.startTimestampMs;
	}

	configure(config: MockSnowflakeServiceConfig): void {
		this.config = {...this.config, ...config};
		if (config.initialCounter !== undefined) {
			this.counter = config.initialCounter;
		}
		if (config.startTimestampMs !== undefined) {
			this.startTimestampMs = config.startTimestampMs;
			this.sequence = 0;
		}
	}

	async initialize(): Promise<void> {
		this.initializeSpy();
		if (this.config.shouldFailInitialize) {
			throw new Error('Mock snowflake initialization failure');
		}
		this.initialized = true;
	}

	async reinitialize(): Promise<void> {
		this.reinitializeSpy();
		this.initialized = false;
		await this.initialize();
	}

	async shutdown(): Promise<void> {
		this.shutdownSpy();
		this.initialized = false;
	}

	async generate(): Promise<bigint> {
		this.generateSpy();
		if (this.config.shouldFailGenerate) {
			throw new Error('Mock snowflake generation failure');
		}
		if (!this.initialized) {
			await this.initialize();
		}
		const id = this.generateId();
		this.generatedIds.push(id);
		return id;
	}

	async generateForChannel(channelId: string | bigint): Promise<bigint> {
		this.generateForChannelSpy(channelId);
		return await this.generate();
	}

	setCounter(value: bigint): void {
		this.counter = value;
	}

	getCounter(): bigint {
		return this.counter;
	}

	getGeneratedIds(): Array<bigint> {
		return [...this.generatedIds];
	}

	isInitialized(): boolean {
		return this.initialized;
	}

	reset(): void {
		this.counter = this.config.initialCounter ?? 1n;
		this.startTimestampMs = this.config.startTimestampMs;
		this.sequence = 0;
		this.initialized = false;
		this.generatedIds = [];
		this.config = {};
		this.initializeSpy.mockClear();
		this.reinitializeSpy.mockClear();
		this.shutdownSpy.mockClear();
		this.generateSpy.mockClear();
		this.generateForChannelSpy.mockClear();
	}

	private generateId(): bigint {
		if (this.config.initialCounter !== undefined && this.config.startTimestampMs === undefined) {
			const id = this.counter;
			this.counter += 1n;
			return id;
		}
		const timestamp = this.startTimestampMs ?? Date.now();
		const id = createSnowflake({
			timestamp,
			workerId: this.config.workerId ?? 0,
			sequence: this.sequence,
		});
		this.sequence = (this.sequence + 1) & 4095;
		if (this.sequence === 0 || this.startTimestampMs !== undefined) {
			this.startTimestampMs = timestamp + 1;
		}
		return id;
	}
}
