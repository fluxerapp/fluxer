// SPDX-License-Identifier: AGPL-3.0-or-later

const HASH_SLOT_COUNT = 16384;
const MAX_CONCURRENT_SLOT_BATCHES = 16;

function extractHashTag(key: string): string {
	const start = key.indexOf('{');
	if (start === -1) {
		return key;
	}
	const end = key.indexOf('}', start + 1);
	if (end > start + 1) {
		return key.slice(start + 1, end);
	}
	return key;
}

export function computeHashSlot(key: string): number {
	const hashed = extractHashTag(key);
	let crc = 0;
	for (let index = 0; index < hashed.length; index += 1) {
		crc ^= (hashed.charCodeAt(index) & 0xff) << 8;
		for (let bit = 0; bit < 8; bit += 1) {
			crc = (crc & 0x8000) === 0 ? (crc << 1) & 0xffff : ((crc << 1) ^ 0x1021) & 0xffff;
		}
	}
	return crc % HASH_SLOT_COUNT;
}

export function splitIntoSlotBatches<T>(
	items: ReadonlyArray<T>,
	keyOf: (item: T) => string,
	clustered: boolean,
): Array<Array<T>> {
	if (items.length === 0) {
		return [];
	}
	if (!clustered) {
		return [[...items]];
	}
	const batches = new Map<number, Array<T>>();
	for (const item of items) {
		const slot = computeHashSlot(keyOf(item));
		const batch = batches.get(slot);
		if (batch) {
			batch.push(item);
		} else {
			batches.set(slot, [item]);
		}
	}
	return [...batches.values()];
}

export async function runSlotBatches<T>(batches: ReadonlyArray<T>, run: (batch: T) => Promise<void>): Promise<void> {
	if (batches.length <= MAX_CONCURRENT_SLOT_BATCHES) {
		await Promise.all(batches.map(async (batch) => await run(batch)));
		return;
	}
	let nextIndex = 0;
	const workers = Array.from({length: MAX_CONCURRENT_SLOT_BATCHES}, async () => {
		while (nextIndex < batches.length) {
			const batch = batches[nextIndex];
			nextIndex += 1;
			await run(batch);
		}
	});
	await Promise.all(workers);
}
