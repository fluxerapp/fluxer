import assert from 'node:assert/strict';

export function chunkArray<T>(items: ReadonlyArray<T>, chunkSize: number): Array<Array<T>> {
	assert(Number.isSafeInteger(chunkSize) && chunkSize > 0, 'Chunk size must be a positive safe integer');
	const chunks: Array<Array<T>> = [];
	for (let index = 0; index < items.length; index += chunkSize) {
		chunks.push(items.slice(index, index + chunkSize));
	}
	return chunks;
}
