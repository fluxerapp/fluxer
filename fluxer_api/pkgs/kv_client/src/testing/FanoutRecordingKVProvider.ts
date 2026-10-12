// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import {BatchRecordingKVProvider} from '@pkgs/kv_client/src/testing/BatchRecordingKVProvider';

export class FanoutRecordingKVProvider extends BatchRecordingKVProvider {
	inFlight = 0;
	peakInFlight = 0;
	failingKey: string | null = null;

	private insidePipeline = 0;

	override pipeline(): IKVPipeline {
		const inner = super.pipeline();
		return {
			...inner,
			exec: async () =>
				await this.trackRoundTrip(async () => {
					this.insidePipeline += 1;
					try {
						return await inner.exec();
					} finally {
						this.insidePipeline -= 1;
					}
				}),
		};
	}

	override async setex(key: string, ttlSeconds: number, value: string): Promise<void> {
		if (key === this.failingKey) {
			throw new Error('write rejected');
		}
		if (this.insidePipeline > 0) {
			await super.setex(key, ttlSeconds, value);
			return;
		}
		this.batches.push({mode: 'pipeline', keys: [key]});
		await this.trackRoundTrip(async () => {
			await super.setex(key, ttlSeconds, value);
		});
	}

	private async trackRoundTrip<T>(run: () => Promise<T>): Promise<T> {
		this.inFlight += 1;
		this.peakInFlight = Math.max(this.peakInFlight, this.inFlight);
		try {
			await new Promise((resolve) => setTimeout(resolve, 0));
			return await run();
		} finally {
			this.inFlight -= 1;
		}
	}
}
