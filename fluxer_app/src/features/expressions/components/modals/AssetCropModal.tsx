// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clampAspectRatio,
	getAspectRatioRange,
	type ImageDimensions,
	isOriginalImageWithinAssetBounds,
} from '@app/features/expressions/utils/AssetImageGeometry';
import {ImageCropModal} from '@app/features/messaging/components/modals/ImageCropModal';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FAILED_TO_CROP_AVATAR_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to crop avatar. Try again.',
	comment: 'Error toast shown when cropping an avatar fails.',
});
const FAILED_TO_CROP_ICON_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to crop icon. Try again.',
	comment: 'Error toast shown when cropping a community icon fails.',
});
const FAILED_TO_CROP_BANNER_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to crop banner. Try again.',
	comment: 'Error toast shown when cropping a banner fails.',
});
const FAILED_TO_CROP_BACKGROUND_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to crop background. Try again.',
	comment: 'Error toast shown when cropping a background fails.',
});
const CROP_AVATAR_TITLE_DESCRIPTOR = msg({
	message: 'Crop avatar',
	comment: 'Modal title for cropping a user avatar before upload.',
});
const CROP_COMMUNITY_ICON_TITLE_DESCRIPTOR = msg({
	message: 'Crop community icon',
	comment: 'Modal title for cropping a community icon before upload.',
});
const CROP_GROUP_ICON_TITLE_DESCRIPTOR = msg({
	message: 'Crop group icon',
	comment: 'Modal title for cropping a group DM icon before upload.',
});
const CROP_BANNER_TITLE_DESCRIPTOR = msg({
	message: 'Crop banner',
	comment: 'Modal title for cropping a community banner before upload.',
});
const CROP_PROFILE_BANNER_TITLE_DESCRIPTOR = msg({
	message: 'Crop profile banner',
	comment: 'Modal title for cropping a profile banner before upload.',
});
const CROP_INVITE_BACKGROUND_TITLE_DESCRIPTOR = msg({
	message: 'Crop invite background',
	comment: 'Modal title for cropping an invite background before upload.',
});
const CROP_CHAT_EMBED_BACKGROUND_TITLE_DESCRIPTOR = msg({
	message: 'Crop chat embed background',
	comment: 'Modal title for cropping a chat embed background before upload.',
});
const AVATAR_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your avatar and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels.',
	comment:
		'Description in the avatar crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const COMMUNITY_ICON_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your community icon and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels.',
	comment:
		'Description in the community icon crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const GROUP_ICON_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your group icon and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels.',
	comment:
		'Description in the group icon crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const BANNER_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your banner and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels (16:9).',
	comment:
		'Description in the community banner crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const PROFILE_BANNER_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your banner and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels (17:6).',
	comment:
		'Description in the profile banner crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const INVITE_BACKGROUND_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your invite background and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels (16:9).',
	comment:
		'Description in the invite background crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const CHAT_EMBED_BACKGROUND_CROP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Drag to reposition your chat embed background and use the scroll wheel or pinch to zoom. The recommended minimum size is {minimumWidth}×{minimumHeight} pixels (16:9).',
	comment:
		'Description in the chat embed background crop modal. minimumWidth and minimumHeight are pixel dimensions for the recommended image size.',
});
const SAVE_AVATAR_DESCRIPTOR = msg({
	message: 'Save avatar',
	comment: 'Primary button label in the avatar crop modal.',
});
const SAVE_ICON_DESCRIPTOR = msg({
	message: 'Save icon',
	comment: 'Primary button label in the icon crop modal.',
});
const SAVE_BANNER_DESCRIPTOR = msg({
	message: 'Save banner',
	comment: 'Primary button label in the banner crop modal.',
});
const SAVE_BACKGROUND_DESCRIPTOR = msg({
	message: 'Save background',
	comment: 'Primary button label in the background crop modal.',
});
export const AssetType = {
	AVATAR: 'avatar',
	GUILD_ICON: 'guild_icon',
	CHANNEL_ICON: 'channel_icon',
	GUILD_BANNER: 'guild_banner',
	PROFILE_BANNER: 'profile_banner',
	SPLASH: 'splash',
	EMBED_SPLASH: 'embed_splash',
} as const;

export type AssetType = ValueOf<typeof AssetType>;

interface AssetConfig {
	aspectRatio: number;
	cropShape: 'rect' | 'round';
	maxWidth: number;
	maxHeight: number;
	minWidth: number;
	minHeight: number;
	sizeLimitBytes: number;
	minHeightRatio?: number;
	maxHeightRatio?: number;
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
		minHeightRatio: 0.5,
		maxHeightRatio: 1,
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
		minHeightRatio: 0.5,
		maxHeightRatio: 1,
	},
	[AssetType.EMBED_SPLASH]: {
		aspectRatio: 16 / 9,
		cropShape: 'rect',
		maxWidth: 2048,
		maxHeight: 1152,
		minWidth: 960,
		minHeight: 540,
		sizeLimitBytes: 10 * 1024 * 1024,
		minHeightRatio: 0.5,
		maxHeightRatio: 1,
	},
};
export const getAssetConfig = (type: AssetType): AssetConfig => ASSET_CONFIGS[type];
export const getAssetAspectRatioRange = (type: AssetType) => {
	const config = getAssetConfig(type);
	return getAspectRatioRange(config.aspectRatio, config.minHeightRatio, config.maxHeightRatio);
};
export const clampAssetAspectRatio = (type: AssetType, aspectRatio: number | undefined): number | undefined =>
	clampAspectRatio(aspectRatio, getAssetAspectRatioRange(type));
const canSkipOriginalAssetImage = (type: AssetType, dimensions: ImageDimensions): boolean => {
	const config = getAssetConfig(type);
	return isOriginalImageWithinAssetBounds(
		dimensions,
		getAssetAspectRatioRange(type),
		config.maxWidth,
		config.maxHeight,
	);
};
const getTitle = (i18n: I18n, assetType: AssetType): string => {
	switch (assetType) {
		case AssetType.AVATAR:
			return i18n._(CROP_AVATAR_TITLE_DESCRIPTOR);
		case AssetType.GUILD_ICON:
			return i18n._(CROP_COMMUNITY_ICON_TITLE_DESCRIPTOR);
		case AssetType.CHANNEL_ICON:
			return i18n._(CROP_GROUP_ICON_TITLE_DESCRIPTOR);
		case AssetType.GUILD_BANNER:
			return i18n._(CROP_BANNER_TITLE_DESCRIPTOR);
		case AssetType.PROFILE_BANNER:
			return i18n._(CROP_PROFILE_BANNER_TITLE_DESCRIPTOR);
		case AssetType.SPLASH:
			return i18n._(CROP_INVITE_BACKGROUND_TITLE_DESCRIPTOR);
		case AssetType.EMBED_SPLASH:
			return i18n._(CROP_CHAT_EMBED_BACKGROUND_TITLE_DESCRIPTOR);
	}
};
const getDescription = (i18n: I18n, assetType: AssetType): string => {
	const config = getAssetConfig(assetType);
	const dimensions = {minimumWidth: config.minWidth, minimumHeight: config.minHeight};
	switch (assetType) {
		case AssetType.AVATAR:
			return i18n._(AVATAR_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.GUILD_ICON:
			return i18n._(COMMUNITY_ICON_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.CHANNEL_ICON:
			return i18n._(GROUP_ICON_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.GUILD_BANNER:
			return i18n._(BANNER_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.PROFILE_BANNER:
			return i18n._(PROFILE_BANNER_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.SPLASH:
			return i18n._(INVITE_BACKGROUND_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
		case AssetType.EMBED_SPLASH:
			return i18n._(CHAT_EMBED_BACKGROUND_CROP_DESCRIPTION_DESCRIPTOR, dimensions);
	}
};
const getSaveButtonLabel = (i18n: I18n, assetType: AssetType): string => {
	switch (assetType) {
		case AssetType.AVATAR:
			return i18n._(SAVE_AVATAR_DESCRIPTOR);
		case AssetType.GUILD_ICON:
		case AssetType.CHANNEL_ICON:
			return i18n._(SAVE_ICON_DESCRIPTOR);
		case AssetType.GUILD_BANNER:
		case AssetType.PROFILE_BANNER:
			return i18n._(SAVE_BANNER_DESCRIPTOR);
		case AssetType.SPLASH:
		case AssetType.EMBED_SPLASH:
			return i18n._(SAVE_BACKGROUND_DESCRIPTOR);
	}
};
const getErrorMessage = (i18n: I18n, assetType: AssetType): string => {
	switch (assetType) {
		case AssetType.AVATAR:
			return i18n._(FAILED_TO_CROP_AVATAR_PLEASE_TRY_AGAIN_DESCRIPTOR);
		case AssetType.GUILD_ICON:
		case AssetType.CHANNEL_ICON:
			return i18n._(FAILED_TO_CROP_ICON_PLEASE_TRY_AGAIN_DESCRIPTOR);
		case AssetType.GUILD_BANNER:
		case AssetType.PROFILE_BANNER:
			return i18n._(FAILED_TO_CROP_BANNER_PLEASE_TRY_AGAIN_DESCRIPTOR);
		case AssetType.SPLASH:
		case AssetType.EMBED_SPLASH:
			return i18n._(FAILED_TO_CROP_BACKGROUND_PLEASE_TRY_AGAIN_DESCRIPTOR);
	}
};

interface AssetCropModalProps {
	imageUrl: string;
	assetType: AssetType;
	onCropComplete: (croppedImageBlob: Blob) => void;
	onSkip?: () => void;
}

export const AssetCropModal: React.FC<AssetCropModalProps> = observer(
	({imageUrl, assetType, onCropComplete, onSkip}) => {
		const {i18n} = useLingui();
		const config = getAssetConfig(assetType);
		return (
			<ImageCropModal
				imageUrl={imageUrl}
				onCropComplete={onCropComplete}
				onSkip={onSkip}
				title={getTitle(i18n, assetType)}
				description={getDescription(i18n, assetType)}
				saveButtonLabel={getSaveButtonLabel(i18n, assetType)}
				errorMessage={getErrorMessage(i18n, assetType)}
				aspectRatio={config.aspectRatio}
				cropShape={config.cropShape}
				maxWidth={config.maxWidth}
				maxHeight={config.maxHeight}
				sizeLimitBytes={config.sizeLimitBytes}
				minHeightRatio={config.minHeightRatio}
				maxHeightRatio={config.maxHeightRatio}
				canSkipOriginal={(dimensions) => canSkipOriginalAssetImage(assetType, dimensions)}
				data-flx="expressions.asset-crop-modal.image-crop-modal"
			/>
		);
	},
);
