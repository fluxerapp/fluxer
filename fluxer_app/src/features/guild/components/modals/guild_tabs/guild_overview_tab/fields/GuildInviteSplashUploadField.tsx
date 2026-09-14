// SPDX-License-Identifier: AGPL-3.0-or-later

import {ImagePreviewField} from '@app/features/app/components/shared/ImagePreviewField';
import {
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_WITH_AVIF_FORMATS,
	WIDE_IMAGE_ASPECT_RATIO_LABEL,
} from '@app/features/app/config/I18nDisplayConstants';
import {AssetType, getAssetConfig} from '@app/features/expressions/components/modals/AssetCropModal';
import {getAcceptString} from '@app/features/expressions/utils/AssetFormatCopy';
import {getAspectRatioFromDimensions} from '@app/features/expressions/utils/AssetImageGeometry';
import {formatImageUploadMinimumHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import type {GuildLike} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTypes';
import {useGuildImageAssetField} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/hooks/useGuildImageAssetField';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Button} from '@app/features/ui/button/Button';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';

const SPLASH_DESCRIPTOR = msg({
	message: 'Splash',
	comment: 'Short label in the guild invite splash upload field. Keep it concise.',
});
const SPLASH_IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Splash images cannot be animated. Use a static image.',
	comment: 'Error message in the guild invite splash upload field.',
});
const INVITE_SPLASH_PREVIEW_DESCRIPTOR = msg({
	message: 'Invite splash preview',
	comment: 'Button or menu action label in the guild invite splash upload field. Keep it concise.',
});
const CHANGE_INVITE_BACKGROUND_DESCRIPTOR = msg({
	message: 'Change invite background',
	comment:
		'Title of the modal where the user picks a community invite background source. Keep it concise. Keep the tone plain and specific.',
});
export const GuildInviteSplashUploadField: React.FC<{
	guild: GuildLike;
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	previewSplashUrl: string | null;
	setPreviewSplashUrl: React.Dispatch<React.SetStateAction<string | null>>;
	hasClearedSplash: boolean;
	setHasClearedSplash: React.Dispatch<React.SetStateAction<boolean>>;
	splashAspectRatio: number | undefined;
	setSplashAspectRatio: (ratio: number | undefined) => void;
	computeAspectRatioFromBase64: (dataUrl: string) => Promise<number>;
}> = ({
	guild,
	form,
	canManageGuild,
	previewSplashUrl,
	setPreviewSplashUrl,
	hasClearedSplash,
	setHasClearedSplash,
	splashAspectRatio,
	setSplashAspectRatio,
	computeAspectRatioFromBase64,
}) => {
	const {i18n} = useLingui();
	const splashConfig = getAssetConfig(AssetType.SPLASH);
	const splashMinimumSize = `${splashConfig.minWidth}×${splashConfig.minHeight}px`;
	const controller = useGuildImageAssetField({
		form,
		fieldName: 'splash',
		assetType: AssetType.SPLASH,
		canManage: canManageGuild,
		filePickerAccept: getAcceptString('splash'),
		previewUrl: previewSplashUrl,
		setPreviewUrl: setPreviewSplashUrl,
		setHasCleared: setHasClearedSplash,
		labelForMessages: i18n._(SPLASH_DESCRIPTOR),
		gif: {
			mode: 'disallow',
			disallowedMessage: i18n._(SPLASH_IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR),
		},
		sourceModalTitle: i18n._(CHANGE_INVITE_BACKGROUND_DESCRIPTOR),
		uploadHint: formatImageUploadMinimumHint(i18n, {
			formats: STATIC_IMAGE_WITH_AVIF_FORMATS,
			maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
			minimumSize: splashMinimumSize,
			aspectRatio: WIDE_IMAGE_ASPECT_RATIO_LABEL,
		}),
		aspectRatio: {
			compute: computeAspectRatioFromBase64,
			set: setSplashAspectRatio,
		},
	});
	const showRemove = (guild.splash || previewSplashUrl) && !hasClearedSplash;
	const hasSplashImage = Boolean(previewSplashUrl || (guild.splash && !hasClearedSplash));
	const existingSplashAspectRatio = getAspectRatioFromDimensions({
		width: guild.splashWidth ?? 0,
		height: guild.splashHeight ?? 0,
	});
	const previewAspectRatio = previewSplashUrl
		? (splashAspectRatio ?? splashConfig.aspectRatio)
		: existingSplashAspectRatio;
	const imageUrl =
		previewSplashUrl ||
		(guild.splash && !hasClearedSplash ? AvatarUtils.getGuildSplashURL({id: guild.id, splash: guild.splash}) : null);
	return (
		<div data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.div">
			<div
				className={styles.iconField}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.icon-field"
			>
				<Trans>Invite background</Trans>
			</div>
			<div
				className={styles.imagePreviewContainer}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.image-preview-container"
			>
				<div
					className={styles.imageUploadActions}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.image-upload-actions"
				>
					<div
						className={styles.imageUploadButtons}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.image-upload-buttons"
					>
						<Button
							variant="primary"
							small={true}
							onClick={controller.openSourcePicker}
							disabled={!canManageGuild || controller.isProcessing}
							data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.button.pick-file"
						>
							<Trans>Upload background</Trans>
						</Button>
						{showRemove && (
							<Button
								variant="secondary"
								small={true}
								onClick={controller.clear}
								disabled={!canManageGuild || controller.isProcessing}
								data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.button.clear"
							>
								<Trans>Remove</Trans>
							</Button>
						)}
					</div>
				</div>
				<div
					className={styles.imagePreviewColumn}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.image-preview-column"
				>
					<ImagePreviewField
						imageUrl={imageUrl}
						showPlaceholder={!hasSplashImage}
						placeholderText={<Trans>No invite background</Trans>}
						altText={i18n._(INVITE_SPLASH_PREVIEW_DESCRIPTOR)}
						objectFit="contain"
						aspectRatio={previewAspectRatio}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.image-preview-field"
					/>
				</div>
			</div>
			{form.formState.errors.splash?.message ? (
				<p
					className={styles.errorMessage}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-invite-splash-upload-field.error-message"
				>
					{form.formState.errors.splash.message}
				</p>
			) : null}
		</div>
	);
};
