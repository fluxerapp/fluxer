// SPDX-License-Identifier: AGPL-3.0-or-later

export const GATEWAY_DISPATCH_BUFFER_MAX_ENTRIES = 1024;
export const GATEWAY_DISPATCH_BUFFER_MAX_BYTES = 16 * 1024 * 1024;

export function exceedsDispatchBufferCapacity(
	entryCount: number,
	retainedByteSize: number,
	deliveryByteSize: number,
): boolean {
	return (
		entryCount > GATEWAY_DISPATCH_BUFFER_MAX_ENTRIES ||
		deliveryByteSize >= GATEWAY_DISPATCH_BUFFER_MAX_BYTES ||
		retainedByteSize > GATEWAY_DISPATCH_BUFFER_MAX_BYTES
	);
}
