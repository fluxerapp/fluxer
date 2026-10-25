// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SetupBrandingAssetKind} from '@app/features/app/components/setup/SetupWizardClient';
import {type AssetKind, getPolicy, isExtensionAllowed} from '@fluxer/constants/src/AssetFormatPolicy';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {createContext, type ReactNode, useCallback, useContext, useMemo, useState} from 'react';

const BRANDING_ASSET_KINDS: ReadonlyArray<SetupBrandingAssetKind> = [
	'logo',
	'wordmark',
	'sign_in_backdrop',
	'icon',
	'symbol',
	'favicon',
	'favicon_dark',
];

export type BrandingAssetError = 'too_large' | 'unsupported' | 'unreadable' | 'not_saved' | 'not_loaded';

export interface BrandingAssetState {
	kind: SetupBrandingAssetKind;
	url: string | null;
	preview: string | null;
	reading: boolean;
	error: BrandingAssetError | null;
}

export function brandingAssetPolicyKind(kind: SetupBrandingAssetKind): AssetKind {
	return kind === 'sign_in_backdrop' ? 'sign_in_backdrop' : 'guild_icon';
}

export function validateBrandingFile(kind: SetupBrandingAssetKind, file: File): BrandingAssetError | null {
	const policyKind = brandingAssetPolicyKind(kind);
	const policy = getPolicy(policyKind);
	const extension = file.name.includes('.') ? (file.name.split('.').pop() ?? '') : '';
	if (!Object.values(policy.mimes).includes(file.type) && !isExtensionAllowed(policyKind, extension)) {
		return 'unsupported';
	}
	if (kind !== 'sign_in_backdrop' && file.size > policy.maxBytes) {
		return 'too_large';
	}
	return null;
}

function hexToNumber(value: string | null): number | null {
	if (!value) return null;
	const match = /^#?([0-9a-fA-F]{6})$/.exec(value.trim());
	return match ? parseInt(match[1], 16) >>> 0 : null;
}

export function brandingAssetUrl(config: InstanceConfigResponse, kind: SetupBrandingAssetKind): string | null {
	const branding = config.app_public.branding;
	switch (kind) {
		case 'icon':
			return branding.icon_url;
		case 'symbol':
			return branding.symbol_url;
		case 'logo':
			return branding.logo_url;
		case 'wordmark':
			return branding.wordmark_url;
		case 'favicon':
			return branding.favicon_url;
		case 'favicon_dark':
			return branding.favicon_dark_url;
		case 'sign_in_backdrop':
			return branding.sign_in_backdrop_url;
	}
}

function assetsFromConfig(config: InstanceConfigResponse): ReadonlyArray<BrandingAssetState> {
	return BRANDING_ASSET_KINDS.map((kind) => ({
		kind,
		url: brandingAssetUrl(config, kind),
		preview: null,
		reading: false,
		error: null,
	}));
}

interface BrandingDraftState {
	productName: string;
	setProductName: (name: string) => void;
	themeColor: number | null;
	setThemeColor: (color: number | null) => void;
	assets: ReadonlyArray<BrandingAssetState>;
	updateAsset: (kind: SetupBrandingAssetKind, patch: Partial<BrandingAssetState>) => void;
	hydrate: (config: InstanceConfigResponse) => void;
}

const BrandingDraftContext = createContext<BrandingDraftState | null>(null);

export function SetupBrandingDraftProvider({children}: {children: ReactNode}) {
	const [productName, setProductName] = useState('');
	const [themeColor, setThemeColor] = useState<number | null>(null);
	const [assets, setAssets] = useState<ReadonlyArray<BrandingAssetState>>(() =>
		BRANDING_ASSET_KINDS.map((kind) => ({kind, url: null, preview: null, reading: false, error: null})),
	);
	const updateAsset = useCallback((kind: SetupBrandingAssetKind, patch: Partial<BrandingAssetState>) => {
		setAssets((current) => current.map((asset) => (asset.kind === kind ? {...asset, ...patch} : asset)));
	}, []);

	const hydrate = useCallback((next: InstanceConfigResponse) => {
		setProductName(next.app_public.branding.product_name);
		setThemeColor(hexToNumber(next.app_public.branding.theme_color));
		setAssets(assetsFromConfig(next));
	}, []);
	const value = useMemo(
		() => ({productName, setProductName, themeColor, setThemeColor, assets, updateAsset, hydrate}),
		[productName, themeColor, assets, updateAsset, hydrate],
	);
	return <BrandingDraftContext.Provider value={value}>{children}</BrandingDraftContext.Provider>;
}

export function useSetupBrandingDraft(): BrandingDraftState {
	const draft = useContext(BrandingDraftContext);
	if (draft === null) throw new Error('Setup branding draft provider is missing');
	return draft;
}
