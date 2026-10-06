// SPDX-License-Identifier: AGPL-3.0-or-later

import {ImagePreviewField} from '@app/features/app/components/shared/ImagePreviewField';
import {
	ANIMATED_BANNER_FEATURE,
	ANIMATED_IMAGE_FORMATS,
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_FORMATS,
	WIDE_IMAGE_ASPECT_RATIO_LABEL,
} from '@app/features/app/config/I18nDisplayConstants';
import {AssetType, getAssetConfig} from '@app/features/expressions/components/modals/AssetCropModal';
import {getAcceptStringFiltered} from '@app/features/expressions/utils/AssetFormatCopy';
import {getAspectRatioFromDimensions} from '@app/features/expressions/utils/AssetImageGeometry';
import {formatImageUploadMinimumHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import type {GuildLike} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTypes';
import {useGuildImageAssetField} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/hooks/useGuildImageAssetField';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Button} from '@app/features/ui/button/Button';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';

const BANNER_DESCRIPTOR = msg({
	message: 'Banner',
	comment:
		'Button or menu action label in the guild banner upload field. Keep it concise. Keep the tone plain and specific.',
});
const ANIMATED_BANNERS_REQUIRE_THE_COMMUNITY_FEATURE_DESCRIPTOR = msg({
	message: 'Animated banners require the {animatedBannerFeature} community feature.',
	comment:
		'Description text in the guild banner upload field. Preserve {animatedBannerFeature}; it is inserted by code. Keep the tone plain and specific.',
});
const BANNER_PREVIEW_DESCRIPTOR = msg({
	message: 'Banner preview',
	comment:
		'Button or menu action label in the guild banner upload field. Keep it concise. Keep the tone plain and specific.',
});
const CHANGE_BANNER_DESCRIPTOR = msg({
	message: 'Change banner',
	comment:
		'Title of the modal where the user picks a community banner source (file upload or GIF provider). Keep it concise.',
});
export const GuildBannerUploadField: React.FC<{
	guild: GuildLike;
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	previewBannerUrl: string | null;
	setPreviewBannerUrl: React.Dispatch<React.SetStateAction<string | null>>;
	hasClearedBanner: boolean;
	setHasClearedBanner: React.Dispatch<React.SetStateAction<boolean>>;
	bannerAspectRatio: number | undefined;
	setBannerAspectRatio: (ratio: number | undefined) => void;
	computeAspectRatioFromBase64: (dataUrl: string) => Promise<number>;
}> = ({
	guild,
	form,
	canManageGuild,
	previewBannerUrl,
	setPreviewBannerUrl,
	hasClearedBanner,
	setHasClearedBanner,
	bannerAspectRatio,
	setBannerAspectRatio,
	computeAspectRatioFromBase64,
}) => {
	const {i18n} = useLingui();
	const bannerConfig = getAssetConfig(AssetType.GUILD_BANNER);
	const canUseAnimatedBanner = guild.features.has(GuildFeatures.ANIMATED_BANNER);
	const bannerFormats = canUseAnimatedBanner ? ANIMATED_IMAGE_FORMATS : STATIC_IMAGE_FORMATS;
	const bannerMinimumSize = `${bannerConfig.minWidth}×${bannerConfig.minHeight}`;
	const controller = useGuildImageAssetField({
		form,
		fieldName: 'banner',
		assetType: AssetType.GUILD_BANNER,
		canManage: canManageGuild,
		filePickerAccept: getAcceptStringFiltered('banner', canUseAnimatedBanner),
		previewUrl: previewBannerUrl,
		setPreviewUrl: setPreviewBannerUrl,
		setHasCleared: setHasClearedBanner,
		labelForMessages: i18n._(BANNER_DESCRIPTOR),
		gif: {
			mode: 'require-feature',
			isAllowed: () => canUseAnimatedBanner,
			featureMissingMessage: i18n._(ANIMATED_BANNERS_REQUIRE_THE_COMMUNITY_FEATURE_DESCRIPTOR, {
				animatedBannerFeature: ANIMATED_BANNER_FEATURE,
			}),
		},
		aspectRatio: {
			compute: computeAspectRatioFromBase64,
			set: setBannerAspectRatio,
		},
		sourceModalTitle: i18n._(CHANGE_BANNER_DESCRIPTOR),
		uploadHint: formatImageUploadMinimumHint(i18n, {
			formats: bannerFormats,
			maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
			minimumSize: bannerMinimumSize,
			aspectRatio: WIDE_IMAGE_ASPECT_RATIO_LABEL,
		}),
	});
	const showRemove = (guild.banner || previewBannerUrl) && !hasClearedBanner;
	const hasBannerImage = Boolean(previewBannerUrl || (guild.banner && !hasClearedBanner));
	const existingBannerAspectRatio = getAspectRatioFromDimensions({
		width: guild.bannerWidth ?? 0,
		height: guild.bannerHeight ?? 0,
	});
	const previewAspectRatio = previewBannerUrl
		? (bannerAspectRatio ?? bannerConfig.aspectRatio)
		: existingBannerAspectRatio;
	const imageUrl =
		previewBannerUrl ||
		(guild.banner && !hasClearedBanner
			? AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true)
			: null);
	return (
		<div data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.div">
			<div
				className={styles.iconField}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.icon-field"
			>
				<Trans>Banner</Trans>
			</div>
			<div
				className={styles.imagePreviewContainer}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.image-preview-container"
			>
				<div
					className={styles.imageUploadActions}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.image-upload-actions"
				>
					<div
						className={styles.imageUploadButtons}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.image-upload-buttons"
					>
						<Button
							variant="primary"
							small={true}
							onClick={controller.openSourcePicker}
							disabled={!canManageGuild || controller.isProcessing}
							data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.button.pick-file"
						>
							<Trans>Upload banner</Trans>
						</Button>
						{showRemove && (
							<Button
								variant="secondary"
								small={true}
								onClick={controller.clear}
								disabled={!canManageGuild || controller.isProcessing}
								data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.button.clear"
							>
								<Trans>Remove</Trans>
							</Button>
						)}
					</div>
				</div>
				<div
					className={styles.imagePreviewColumn}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.image-preview-column"
				>
					<ImagePreviewField
						imageUrl={imageUrl}
						showPlaceholder={!hasBannerImage}
						placeholderText={<Trans>No community banner</Trans>}
						altText={i18n._(BANNER_PREVIEW_DESCRIPTOR)}
						objectFit="contain"
						aspectRatio={previewAspectRatio}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.image-preview-field"
					/>
				</div>
			</div>
			{form.formState.errors.banner?.message ? (
				<p
					className={styles.errorMessage}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-banner-upload-field.error-message"
				>
					{form.formState.errors.banner.message}
				</p>
			) : null}
		</div>
	);
};
