// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ThrowCollectedFailuresRequest {
	readonly failures: ReadonlyArray<unknown>;
	readonly message: string;
}

class MissingFailureReasonError extends Error {
	constructor() {
		super('Operation failed without an error reason');
		this.name = 'MissingFailureReasonError';
	}
}

function normalizeFailureReason(failure: unknown): unknown {
	if (failure == null) {
		return new MissingFailureReasonError();
	}
	return failure;
}

function flattenFailures(failures: ReadonlyArray<unknown>): Array<unknown> {
	const flattened: Array<unknown> = [];
	for (const failure of failures) {
		const normalized = normalizeFailureReason(failure);
		if (normalized instanceof AggregateError && Array.isArray(normalized.errors)) {
			for (const nested of normalized.errors) {
				flattened.push(normalizeFailureReason(nested));
			}
			continue;
		}
		flattened.push(normalized);
	}
	return flattened;
}

export async function collectSettledFailures(operations: ReadonlyArray<PromiseLike<unknown>>): Promise<Array<unknown>> {
	const outcomes = await Promise.allSettled(operations);
	const failures: Array<unknown> = [];
	for (const outcome of outcomes) {
		if (outcome.status === 'rejected') {
			failures.push(normalizeFailureReason(outcome.reason));
		}
	}
	return flattenFailures(failures);
}

export function throwCollectedFailures(
	request: ThrowCollectedFailuresRequest & {readonly failures: readonly [unknown, ...Array<unknown>]},
): never;
export function throwCollectedFailures(request: ThrowCollectedFailuresRequest): void;
export function throwCollectedFailures(request: ThrowCollectedFailuresRequest): void {
	const flattened = flattenFailures(request.failures);
	if (flattened.length === 0) {
		return;
	}
	if (flattened.length === 1) {
		const failure = flattened[0];
		if (failure == null) {
			throw new MissingFailureReasonError();
		}
		throw failure;
	}
	throw new AggregateError(flattened, request.message);
}
