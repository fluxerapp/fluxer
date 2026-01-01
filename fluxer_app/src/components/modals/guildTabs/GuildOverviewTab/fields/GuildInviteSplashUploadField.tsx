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
import {AssetType, getAssetConfig} from '~/components/modals/AssetCropModal';
import {ImagePreviewField} from '~/components/shared/ImagePreviewField';
import {Button} from '~/components/uikit/Button/Button';
import * as AvatarUtils from '~/utils/AvatarUtils';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import styles from '../GuildOverviewTab.module.css';
import {useGuildImageAssetField} from '../hooks/useGuildImageAssetField';
import type {GuildLike} from '../types';

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
	const {t} = useLingui();
	const splashConfig = getAssetConfig(AssetType.SPLASH);

	const controller = useGuildImageAssetField({
		form,
		fieldName: 'splash',
		assetType: AssetType.SPLASH,
		canManage: canManageGuild,
		filePickerAccept: 'image/jpeg,image/png,image/webp',
		previewUrl: previewSplashUrl,
		setPreviewUrl: setPreviewSplashUrl,
		setHasCleared: setHasClearedSplash,
		labelForMessages: t`Splash`,
		gif: {
			mode: 'disallow',
			disallowedMessage: t`Splash images cannot be animated. Please use JPEG, PNG, or WebP.`,
		},
		aspectRatio: {
			compute: computeAspectRatioFromBase64,
			set: setSplashAspectRatio,
		},
	});

	const showRemove = (guild.splash || previewSplashUrl) && !hasClearedSplash;
	const hasSplashImage = Boolean(previewSplashUrl || (guild.splash && !hasClearedSplash));

	const imageUrl =
		previewSplashUrl ||
		(guild.splash && !hasClearedSplash ? AvatarUtils.getGuildSplashURL({id: guild.id, splash: guild.splash}) : null);

	return (
		<div>
			<div className={styles.iconField}>
				<Trans>Invite Background</Trans>
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
							JPEG, PNG, WebP. Max 10MB. Minimum: {splashConfig.minWidth}×{splashConfig.minHeight}px (16:9)
						</Trans>
					</div>
				</div>

				<div className={styles.imagePreviewColumn}>
					<ImagePreviewField
						imageUrl={imageUrl}
						showPlaceholder={!hasSplashImage}
						placeholderText={<Trans>No invite background</Trans>}
						altText={t`Invite splash preview`}
						objectFit="contain"
						aspectRatio={previewSplashUrl && splashAspectRatio ? splashAspectRatio : undefined}
					/>
				</div>
			</div>

			{form.formState.errors.splash?.message ? (
				<p className={styles.errorMessage}>{form.formState.errors.splash.message}</p>
			) : null}
		</div>
	);
};
