// SPDX-License-Identifier: AGPL-3.0-or-later

export type TransientUploadFieldValue = string | null | undefined;

export interface TransientUploadFieldMutation {
	readonly value: TransientUploadFieldValue;
	readonly previewUrl?: string | null;
	readonly hasCleared?: boolean;
}

function getTransientUploadFieldMutation({
	value,
	previewUrl,
	hasCleared = false,
}: TransientUploadFieldMutation): TransientUploadFieldValue {
	if (hasCleared) return null;
	if (previewUrl) return value ?? previewUrl;
	return undefined;
}

export function assignTransientUploadFieldMutation<TPayload extends object>(
	payload: TPayload,
	key: string,
	mutation: TransientUploadFieldMutation,
): void {
	const value = getTransientUploadFieldMutation(mutation);
	if (value === undefined) return;
	(payload as Record<string, TransientUploadFieldValue>)[key] = value;
}

export function omitTransientUploadFields<TValues extends object, TKey extends keyof TValues>(
	values: TValues,
	keys: ReadonlyArray<TKey>,
): Omit<TValues, TKey> {
	const next = {...values};
	for (const key of keys) {
		delete next[key];
	}
	return next;
}
