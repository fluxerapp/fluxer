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
import {Controller} from 'react-hook-form';

import {GuildFeatures} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import {AssetType, getAssetConfig} from '~/components/modals/AssetCropModal';
import {ImagePreviewField} from '~/components/shared/ImagePreviewField';
import {Button} from '~/components/uikit/Button/Button';
import * as AvatarUtils from '~/utils/AvatarUtils';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import styles from '../GuildOverviewTab.module.css';
import {useGuildImageAssetField} from '../hooks/useGuildImageAssetField';
import type {GuildLike} from '../types';

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
	const {t} = useLingui();
	const bannerConfig = getAssetConfig(AssetType.GUILD_BANNER);
	const canUseAnimatedBanner = guild.features.has(GuildFeatures.ANIMATED_BANNER);

	const controller = useGuildImageAssetField({
		form,
		fieldName: 'banner',
		assetType: AssetType.GUILD_BANNER,
		canManage: canManageGuild,
		filePickerAccept: canUseAnimatedBanner
			? 'image/jpeg,image/png,image/gif,image/webp'
			: 'image/jpeg,image/png,image/webp',
		previewUrl: previewBannerUrl,
		setPreviewUrl: setPreviewBannerUrl,
		setHasCleared: setHasClearedBanner,
		labelForMessages: t`Banner`,
		gif: {
			mode: 'require-feature',
			isAllowed: () => canUseAnimatedBanner,
			featureMissingMessage: t`Animated banners require the ANIMATED_BANNER guild feature.`,
		},
		aspectRatio: {
			compute: computeAspectRatioFromBase64,
			set: setBannerAspectRatio,
		},
	});

	const showRemove = (guild.banner || previewBannerUrl) && !hasClearedBanner;
	const hasBannerImage = Boolean(previewBannerUrl || (guild.banner && !hasClearedBanner));

	const imageUrl =
		previewBannerUrl ||
		(guild.banner && !hasClearedBanner
			? AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true)
			: null);

	return (
		<div>
			<div className={styles.iconField}>
				<Trans>Banner</Trans>
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
							<Trans>Upload Banner</Trans>
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
						{canUseAnimatedBanner ? (
							<Trans>
								JPEG, PNG, GIF, WebP. Max 10MB. Minimum: {bannerConfig.minWidth}×{bannerConfig.minHeight} (16:9)
							</Trans>
						) : (
							<Trans>
								JPEG, PNG, WebP. Max 10MB. Minimum: {bannerConfig.minWidth}×{bannerConfig.minHeight} (16:9)
							</Trans>
						)}
					</div>

					<Controller
						name="detached_banner"
						control={form.control}
						render={({field}) => (
							<Switch
								label={t`Detached Banner`}
								description={t`When enabled, the banner appears in its own section below the community header.`}
								value={field.value ?? false}
								onChange={field.onChange}
								disabled={!canManageGuild}
							/>
						)}
					/>
				</div>

				<div className={styles.imagePreviewColumn}>
					<ImagePreviewField
						imageUrl={imageUrl}
						showPlaceholder={!hasBannerImage}
						placeholderText={<Trans>No community banner</Trans>}
						altText={t`Banner preview`}
						objectFit="contain"
						aspectRatio={previewBannerUrl && bannerAspectRatio ? bannerAspectRatio : undefined}
					/>
				</div>
			</div>

			{form.formState.errors.banner?.message ? (
				<p className={styles.errorMessage}>{form.formState.errors.banner.message}</p>
			) : null}
		</div>
	);
};
