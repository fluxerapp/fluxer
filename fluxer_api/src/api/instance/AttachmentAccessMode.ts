// SPDX-License-Identifier: AGPL-3.0-or-later

let cachedProxyAttachmentsThroughApi: boolean | null = null;

export function isApiProxiedAttachments(): boolean {
	return cachedProxyAttachmentsThroughApi === true;
}

export function setCachedProxyAttachmentsThroughApi(value: boolean | null): void {
	cachedProxyAttachmentsThroughApi = value;
}
