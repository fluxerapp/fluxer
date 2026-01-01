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
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';
import {GuildInviteEmbedPreview} from '~/components/channel/InviteEmbed';
import {AssetType, getAssetConfig} from '~/components/modals/AssetCropModal';
import {Button} from '~/components/uikit/Button/Button';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import styles from '../GuildOverviewTab.module.css';
import {useGuildImageAssetField} from '../hooks/useGuildImageAssetField';
import type {GuildLike} from '../types';

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
	const {t} = useLingui();
	const embedSplashConfig = getAssetConfig(AssetType.EMBED_SPLASH);

	const controller = useGuildImageAssetField({
		form,
		fieldName: 'embed_splash',
		assetType: AssetType.EMBED_SPLASH,
		canManage: canManageGuild,
		filePickerAccept: 'image/jpeg,image/png,image/webp',
		previewUrl: previewEmbedSplashUrl,
		setPreviewUrl: setPreviewEmbedSplashUrl,
		setHasCleared: setHasClearedEmbedSplash,
		labelForMessages: t`Embed splash`,
		gif: {
			mode: 'disallow',
			disallowedMessage: t`Embed splash images cannot be animated. Please use JPEG, PNG, or WebP.`,
		},
	});

	const showRemove = (guild.embedSplash || previewEmbedSplashUrl) && !hasClearedEmbedSplash;

	const splashURLOverride = hasClearedEmbedSplash ? null : (previewEmbedSplashUrl ?? undefined);

	const previewKey = hasClearedEmbedSplash ? 'cleared' : (previewEmbedSplashUrl ?? 'server');

	return (
		<div>
			<div className={styles.iconField}>
				<Trans>Chat Embed Background</Trans>
			</div>

			<div className={styles.imagePreviewContainer}>
				<div className={styles.imageUploadActions}>
					<div className={styles.imageUploadButtons}>
						<Button
							variant="primary"
							small={true}
							onClick={controller.pickFile}
							disabled={!canManageGuild || controller.isProcessing}
						>
							<Trans>Upload Background</Trans>
						</Button>

						{showRemove && (
							<Button
								variant="secondary"
								small={true}
								onClick={controller.clear}
								disabled={!canManageGuild || controller.isProcessing}
							>
								<Trans>Remove</Trans>
							</Button>
						)}
					</div>

					<div className={styles.imageUploadDescription}>
						<Trans>
							JPEG, PNG, WebP. Max 10MB. Minimum: {embedSplashConfig.minWidth}×{embedSplashConfig.minHeight}px (16:9).
							Shown in invite embeds in chat.
						</Trans>
					</div>
				</div>

				<div className={styles.imagePreviewColumn}>
					<GuildInviteEmbedPreview key={previewKey} guildId={guildId} splashURLOverride={splashURLOverride} />
				</div>
			</div>

			{form.formState.errors.embed_splash?.message ? (
				<p className={styles.errorMessage}>{form.formState.errors.embed_splash.message}</p>
			) : null}
		</div>
	);
};
