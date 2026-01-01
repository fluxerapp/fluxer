/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {ImageCropModal} from '~/components/modals/ImageCropModal';

export enum AssetType {
	AVATAR = 'avatar',
	GUILD_ICON = 'guild_icon',
	CHANNEL_ICON = 'channel_icon',
	GUILD_BANNER = 'guild_banner',
	PROFILE_BANNER = 'profile_banner',
	SPLASH = 'splash',
	EMBED_SPLASH = 'embed_splash',
}

interface AssetConfig {
	aspectRatio: number;
	cropShape: 'rect' | 'round';
	maxWidth: number;
	maxHeight: number;
	minWidth: number;
	minHeight: number;
	sizeLimitBytes: number;
}

const ASSET_CONFIGS: Record<AssetType, AssetConfig> = {
	[AssetType.AVATAR]: {
		aspectRatio: 1,
		cropShape: 'round',
		maxWidth: 1024,
		maxHeight: 1024,
		minWidth: 256,
		minHeight: 256,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.GUILD_ICON]: {
		aspectRatio: 1,
		cropShape: 'round',
		maxWidth: 1024,
		maxHeight: 1024,
		minWidth: 256,
		minHeight: 256,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.CHANNEL_ICON]: {
		aspectRatio: 1,
		cropShape: 'round',
		maxWidth: 1024,
		maxHeight: 1024,
		minWidth: 256,
		minHeight: 256,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.GUILD_BANNER]: {
		aspectRatio: 16 / 9,
		cropShape: 'rect',
		maxWidth: 2048,
		maxHeight: 1152,
		minWidth: 960,
		minHeight: 540,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.PROFILE_BANNER]: {
		aspectRatio: 17 / 6,
		cropShape: 'rect',
		maxWidth: 2048,
		maxHeight: 723,
		minWidth: 680,
		minHeight: 240,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.SPLASH]: {
		aspectRatio: 16 / 9,
		cropShape: 'rect',
		maxWidth: 2048,
		maxHeight: 1152,
		minWidth: 960,
		minHeight: 540,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
	[AssetType.EMBED_SPLASH]: {
		aspectRatio: 16 / 9,
		cropShape: 'rect',
		maxWidth: 2048,
		maxHeight: 1152,
		minWidth: 960,
		minHeight: 540,
		sizeLimitBytes: 10 * 1024 * 1024,
	},
};

export const getAssetConfig = (type: AssetType): AssetConfig => ASSET_CONFIGS[type];

const getTitle = (assetType: AssetType): React.ReactNode => {
	switch (assetType) {
		case AssetType.AVATAR:
			return <Trans>Crop Avatar</Trans>;
		case AssetType.GUILD_ICON:
			return <Trans>Crop Community Icon</Trans>;
		case AssetType.CHANNEL_ICON:
			return <Trans>Crop Group Icon</Trans>;
		case AssetType.GUILD_BANNER:
			return <Trans>Crop Banner</Trans>;
		case AssetType.PROFILE_BANNER:
			return <Trans>Crop Profile Banner</Trans>;
		case AssetType.SPLASH:
			return <Trans>Crop Invite Background</Trans>;
		case AssetType.EMBED_SPLASH:
			return <Trans>Crop Chat Embed Background</Trans>;
	}
};

const getDescription = (assetType: AssetType): React.ReactNode => {
	const config = getAssetConfig(assetType);

	switch (assetType) {
		case AssetType.AVATAR:
			return (
				<Trans>
					Drag to reposition your avatar and use the scroll wheel or pinch to zoom. The recommended minimum size is{' '}
					256×256 pixels.
				</Trans>
			);
		case AssetType.GUILD_ICON:
			return (
				<Trans>
					Drag to reposition your community icon and use the scroll wheel or pinch to zoom. The recommended minimum size
					is 256×256 pixels.
				</Trans>
			);
		case AssetType.CHANNEL_ICON:
			return (
				<Trans>
					Drag to reposition your group icon and use the scroll wheel or pinch to zoom. The recommended minimum size is
					256×256 pixels.
				</Trans>
			);
		case AssetType.GUILD_BANNER:
			return (
				<Trans>
					Drag to reposition your banner and use the scroll wheel or pinch to zoom. The recommended minimum size is{' '}
					{config.minWidth}×{config.minHeight} pixels (16:9).
				</Trans>
			);
		case AssetType.PROFILE_BANNER:
			return (
				<Trans>
					Drag to reposition your banner and use the scroll wheel or pinch to zoom. The recommended minimum size is{' '}
					{config.minWidth}×{config.minHeight} pixels (17:6).
				</Trans>
			);
		case AssetType.SPLASH:
			return (
				<Trans>
					Drag to reposition your invite background and use the scroll wheel or pinch to zoom. The recommended minimum{' '}
					size is {config.minWidth}×{config.minHeight} pixels (16:9).
				</Trans>
			);
		case AssetType.EMBED_SPLASH:
			return (
				<Trans>
					Drag to reposition your chat embed background and use the scroll wheel or pinch to zoom. The recommended
					minimum size is {config.minWidth}×{config.minHeight} pixels (16:9).
				</Trans>
			);
	}
};

const getSaveButtonLabel = (assetType: AssetType): React.ReactNode => {
	switch (assetType) {
		case AssetType.AVATAR:
			return <Trans>Save Avatar</Trans>;
		case AssetType.GUILD_ICON:
		case AssetType.CHANNEL_ICON:
			return <Trans>Save Icon</Trans>;
		case AssetType.GUILD_BANNER:
		case AssetType.PROFILE_BANNER:
			return <Trans>Save Banner</Trans>;
		case AssetType.SPLASH:
		case AssetType.EMBED_SPLASH:
			return <Trans>Save Background</Trans>;
	}
};

const getErrorMessage = (assetType: AssetType): string => {
	const {t} = useLingui();
	switch (assetType) {
		case AssetType.AVATAR:
			return t`Failed to crop avatar. Please try again.`;
		case AssetType.GUILD_ICON:
		case AssetType.CHANNEL_ICON:
			return t`Failed to crop icon. Please try again.`;
		case AssetType.GUILD_BANNER:
		case AssetType.PROFILE_BANNER:
			return t`Failed to crop banner. Please try again.`;
		case AssetType.SPLASH:
		case AssetType.EMBED_SPLASH:
			return t`Failed to crop background. Please try again.`;
	}
};

interface AssetCropModalProps {
	imageUrl: string;
	sourceMimeType: string;
	assetType: AssetType;
	onCropComplete: (croppedImageBlob: Blob) => void;
	onSkip?: () => void;
}

export const AssetCropModal: React.FC<AssetCropModalProps> = observer(
	({imageUrl, sourceMimeType, assetType, onCropComplete, onSkip}) => {
		const config = getAssetConfig(assetType);
		const hasFlexibleHeight =
			assetType === AssetType.GUILD_BANNER || assetType === AssetType.SPLASH || assetType === AssetType.EMBED_SPLASH;

		return (
			<ImageCropModal
				imageUrl={imageUrl}
				sourceMimeType={sourceMimeType}
				onCropComplete={onCropComplete}
				onSkip={onSkip}
				title={getTitle(assetType)}
				description={getDescription(assetType)}
				saveButtonLabel={getSaveButtonLabel(assetType)}
				errorMessage={getErrorMessage(assetType)}
				aspectRatio={config.aspectRatio}
				cropShape={config.cropShape}
				maxWidth={config.maxWidth}
				maxHeight={config.maxHeight}
				sizeLimitBytes={config.sizeLimitBytes}
				minHeightRatio={hasFlexibleHeight ? 0.5 : undefined}
				maxHeightRatio={hasFlexibleHeight ? 1 : undefined}
			/>
		);
	},
);
