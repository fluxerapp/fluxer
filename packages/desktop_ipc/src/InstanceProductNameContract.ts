// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_INSTANCE_PRODUCT_NAME_CHANNEL = 'desktop-instance-product-name:report';

const INSTANCE_PRODUCT_NAME_MAX_LENGTH = 80;
const INSTANCE_PRODUCT_NAME_KEY_MAX_LENGTH = 2048;

export interface InstanceProductNameReport {
	readonly instanceKey: string;
	readonly productName: string;
}

function containsControlCharacter(value: string): boolean {
	for (let index = 0; index < value.length; index += 1) {
		const code = value.charCodeAt(index);
		if (code <= 0x1f || (code >= 0x7f && code <= 0x9f)) {
			return true;
		}
	}
	return false;
}

export function readInstanceProductName(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	const trimmed = value.trim();
	if (trimmed.length === 0 || trimmed.length > INSTANCE_PRODUCT_NAME_MAX_LENGTH) return null;
	return containsControlCharacter(trimmed) ? null : trimmed;
}

function readInstanceProductNameKey(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	if (value.length === 0 || value.length > INSTANCE_PRODUCT_NAME_KEY_MAX_LENGTH) return null;
	return containsControlCharacter(value) ? null : value;
}

export function readInstanceProductNameReport(value: unknown): InstanceProductNameReport | null {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) return null;
	const record = value as Record<string, unknown>;
	const instanceKey = readInstanceProductNameKey(record.instanceKey);
	const productName = readInstanceProductName(record.productName);
	if (instanceKey === null || productName === null) return null;
	return {instanceKey, productName};
}
