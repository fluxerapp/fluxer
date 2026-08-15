// SPDX-License-Identifier: AGPL-3.0-or-later

export type GifProvider = string;

export interface GifProviderInfo {
	name: GifProvider;
	displayName: string;
	attributionRequired: boolean;
}

export interface GifProviderInfoInput {
	name?: GifProvider | null;
	provider?: GifProvider | null;
	attributionRequired?: boolean | null;
}

type KnownGifProvider = 'klipy' | 'tenor';

const GIF_PROVIDER_DISPLAY_NAMES: Record<KnownGifProvider, string> = {
	klipy: 'Klipy',
	tenor: 'Tenor',
};

const GIF_PROVIDER_DEFAULT_ATTRIBUTION: Record<KnownGifProvider, boolean> = {
	klipy: false,
	tenor: false,
};

function normalizeGifProviderName(provider: GifProvider | null | undefined): KnownGifProvider {
	return provider === 'tenor' ? 'tenor' : 'klipy';
}

export function normalizeGifProviderInfo(input: GifProviderInfoInput = {}): GifProviderInfo {
	const name = normalizeGifProviderName(input.name ?? input.provider);
	return {
		name,
		displayName: GIF_PROVIDER_DISPLAY_NAMES[name],
		attributionRequired: input.attributionRequired ?? GIF_PROVIDER_DEFAULT_ATTRIBUTION[name],
	};
}

export const DEFAULT_GIF_PROVIDER_INFO: GifProviderInfo = Object.freeze(normalizeGifProviderInfo());
