// SPDX-License-Identifier: AGPL-3.0-or-later

type LogicalBucketMap = ReadonlyMap<string, string>;

export function parseBucketMap(value: string): LogicalBucketMap {
	const map = new Map<string, string>();
	for (const entry of value.split(',')) {
		const trimmed = entry.trim();
		if (trimmed === '') continue;
		const separator = trimmed.indexOf('=');
		if (separator <= 0 || separator === trimmed.length - 1) {
			throw new Error(`Invalid bucket map entry "${trimmed}", expected logical=physical`);
		}
		const logical = trimmed.slice(0, separator).trim();
		const physical = trimmed.slice(separator + 1).trim();
		if (map.has(logical)) {
			throw new Error(`Bucket map lists logical bucket "${logical}" twice`);
		}
		map.set(logical, physical);
	}
	return map;
}

export class StoreBucketResolver {
	private readonly logicalByPhysical = new Map<string, string>();

	constructor(
		private readonly primary: LogicalBucketMap,
		private readonly fallback: LogicalBucketMap,
	) {
		for (const map of [fallback, primary]) {
			for (const [logical, physical] of map) {
				this.logicalByPhysical.set(physical, logical);
			}
		}
	}

	logicalName(bucket: string): string | undefined {
		return this.logicalByPhysical.get(bucket);
	}

	primaryBucket(bucket: string): string {
		const logical = this.logicalName(bucket);
		return (logical && this.primary.get(logical)) ?? bucket;
	}

	fallbackBucket(bucket: string): string {
		const logical = this.logicalName(bucket);
		return (logical && this.fallback.get(logical)) ?? bucket;
	}
}
