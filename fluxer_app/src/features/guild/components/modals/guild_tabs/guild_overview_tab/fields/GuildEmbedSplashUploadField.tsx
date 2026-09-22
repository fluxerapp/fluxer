// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_WITH_AVIF_FORMATS,
	WIDE_IMAGE_ASPECT_RATIO_LABEL,
} from '@app/features/app/config/I18nDisplayConstants';
import {GuildInviteEmbedPreview} from '@app/features/channel/components/InviteEmbed';
import {AssetType, getAssetConfig} from '@app/features/expressions/components/modals/AssetCropModal';
import {getAcceptString} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadMinimumHintWithNote} from '@app/features/expressions/utils/AssetUploadHintCopy';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import type {GuildLike} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTypes';
import {useGuildImageAssetField} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/hooks/useGuildImageAssetField';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';

const EMBED_SPLASH_DESCRIPTOR = msg({
	message: 'Embed splash',
	comment: 'Short label in the guild embed splash upload field. Keep it concise.',
});
const EMBED_SPLASH_IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Embed splash images cannot be animated. Use a static image.',
	comment: 'Error message in the guild embed splash upload field.',
});
const CHANGE_CHAT_EMBED_BACKGROUND_DESCRIPTOR = msg({
	message: 'Change chat embed background',
	comment:
		'Title of the modal where the user picks a community chat embed background source. Keep it concise. Keep the tone plain and specific.',
});
const SHOWN_IN_INVITE_EMBEDS_IN_CHAT_DESCRIPTOR = msg({
	message: 'Shown in invite embeds in chat.',
	comment: 'Extra note in the chat embed background asset upload source modal.',
});
export const GuildEmbedSplashUploadField: React.FC<{
	guildId: string;
	guild: GuildLike;
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	previewEmbedSplashUrl: string | null;
	setPreviewEmbedSplashUrl: React.Dispatch<React.SetStateAction<string | null>>;
	hasClearedEmbedSplash: boolean;
	setHasClearedEmbedSplash: React.Dispatch<React.SetStateAction<boolean>>;
}> = ({
	guildId,
	guild,
	form,
	canManageGuild,
	previewEmbedSplashUrl,
	setPreviewEmbedSplashUrl,
	hasClearedEmbedSplash,
	setHasClearedEmbedSplash,
}) => {
	const {i18n} = useLingui();
	const embedSplashConfig = getAssetConfig(AssetType.EMBED_SPLASH);
	const embedSplashMinimumSize = `${embedSplashConfig.minWidth}×${embedSplashConfig.minHeight}px`;
	const controller = useGuildImageAssetField({
		form,
		fieldName: 'embed_splash',
		assetType: AssetType.EMBED_SPLASH,
		canManage: canManageGuild,
		filePickerAccept: getAcceptString('embed_splash'),
		previewUrl: previewEmbedSplashUrl,
		setPreviewUrl: setPreviewEmbedSplashUrl,
		setHasCleared: setHasClearedEmbedSplash,
		labelForMessages: i18n._(EMBED_SPLASH_DESCRIPTOR),
		gif: {
			mode: 'disallow',
			disallowedMessage: i18n._(EMBED_SPLASH_IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR),
		},
		sourceModalTitle: i18n._(CHANGE_CHAT_EMBED_BACKGROUND_DESCRIPTOR),
		uploadHint: formatImageUploadMinimumHintWithNote(i18n, {
			formats: STATIC_IMAGE_WITH_AVIF_FORMATS,
			maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
			minimumSize: embedSplashMinimumSize,
			aspectRatio: WIDE_IMAGE_ASPECT_RATIO_LABEL,
			note: i18n._(SHOWN_IN_INVITE_EMBEDS_IN_CHAT_DESCRIPTOR),
		}),
	});
	const showRemove = (guild.embedSplash || previewEmbedSplashUrl) && !hasClearedEmbedSplash;
	const splashURLOverride = hasClearedEmbedSplash ? null : (previewEmbedSplashUrl ?? undefined);
	const previewKey = hasClearedEmbedSplash ? 'cleared' : (previewEmbedSplashUrl ?? 'server');
	return (
		<div data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.div">
			<div
				className={styles.iconField}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.icon-field"
			>
				<Trans>Chat embed background</Trans>
			</div>
			<div
				className={styles.imagePreviewContainer}
				data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.image-preview-container"
			>
				<div
					className={styles.imageUploadActions}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.image-upload-actions"
				>
					<div
						className={styles.imageUploadButtons}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.image-upload-buttons"
					>
						<Button
							variant="primary"
							small={true}
							onClick={controller.openSourcePicker}
							disabled={!canManageGuild || controller.isProcessing}
							data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.button.pick-file"
						>
							<Trans>Upload background</Trans>
						</Button>
						{showRemove && (
							<Button
								variant="secondary"
								small={true}
								onClick={controller.clear}
								disabled={!canManageGuild || controller.isProcessing}
								data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.button.clear"
							>
								<Trans>Remove</Trans>
							</Button>
						)}
					</div>
				</div>
				<div
					className={styles.imagePreviewColumn}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.image-preview-column"
				>
					<GuildInviteEmbedPreview
						key={previewKey}
						guildId={guildId}
						splashURLOverride={splashURLOverride}
						data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.guild-invite-embed-preview"
					/>
				</div>
			</div>
			{form.formState.errors.embed_splash?.message ? (
				<p
					className={styles.errorMessage}
					data-flx="guild.guild-tabs.guild-overview-tab.fields.guild-embed-splash-upload-field.error-message"
				>
					{form.formState.errors.embed_splash.message}
				</p>
			) : null}
		</div>
	);
};
