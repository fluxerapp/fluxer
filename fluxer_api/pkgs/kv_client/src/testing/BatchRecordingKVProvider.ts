// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';

type BatchMode = 'multi' | 'pipeline';

interface RecordedBatch {
	mode: BatchMode;
	keys: Array<string>;
}

export class BatchRecordingKVProvider extends MockKVProvider {
	readonly batches: Array<RecordedBatch> = [];

	override pipeline(): IKVPipeline {
		return this.recordBatch('pipeline', super.pipeline());
	}

	override multi(): IKVPipeline {
		return this.recordBatch('multi', super.multi());
	}

	crossSlotBatches(): Array<RecordedBatch> {
		return this.batches.filter((batch) => new Set(batch.keys.map(computeHashSlot)).size > 1);
	}

	private recordBatch(mode: BatchMode, inner: IKVPipeline): IKVPipeline {
		const batch: RecordedBatch = {mode, keys: []};
		this.batches.push(batch);
		const recorded: IKVPipeline = {
			setex: (key, ttlSeconds, value) => {
				batch.keys.push(key);
				inner.setex(key, ttlSeconds, value);
				return recorded;
			},
			del: (key) => {
				batch.keys.push(key);
				inner.del(key);
				return recorded;
			},
			expire: (key, ttlSeconds) => {
				batch.keys.push(key);
				inner.expire(key, ttlSeconds);
				return recorded;
			},
			sadd: (key, ...members) => {
				batch.keys.push(key);
				inner.sadd(key, ...members);
				return recorded;
			},
			hgetall: (key) => {
				batch.keys.push(key);
				inner.hgetall(key);
				return recorded;
			},
			exec: async () => await inner.exec(),
		};
		return recorded;
	}
}
