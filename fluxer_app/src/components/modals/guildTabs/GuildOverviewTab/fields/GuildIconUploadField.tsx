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
import {useWatch} from 'react-hook-form';

import {GuildFeatures} from '~/Constants';
import {AssetType} from '~/components/modals/AssetCropModal';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Button} from '~/components/uikit/Button/Button';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import styles from '../GuildOverviewTab.module.css';
import {useGuildImageAssetField} from '../hooks/useGuildImageAssetField';
import type {GuildLike} from '../types';

export const GuildIconUploadField: React.FC<{
	guild: GuildLike;
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;

	previewIconUrl: string | null;
	setPreviewIconUrl: React.Dispatch<React.SetStateAction<string | null>>;

	hasClearedIcon: boolean;
	setHasClearedIcon: React.Dispatch<React.SetStateAction<boolean>>;
}> = ({guild, form, canManageGuild, previewIconUrl, setPreviewIconUrl, hasClearedIcon, setHasClearedIcon}) => {
	const {t} = useLingui();
	const canUseAnimatedIcon = guild.features.has(GuildFeatures.ANIMATED_ICON);
	const watchedName = useWatch({
		control: form.control,
		name: 'name',
		defaultValue: guild.name,
	});
	const iconName = watchedName ?? guild.name;

	const controller = useGuildImageAssetField({
		form,
		fieldName: 'icon',
		assetType: AssetType.GUILD_ICON,
		canManage: canManageGuild,
		filePickerAccept: canUseAnimatedIcon
			? 'image/jpeg,image/png,image/gif,image/webp'
			: 'image/jpeg,image/png,image/webp',
		previewUrl: previewIconUrl,
		setPreviewUrl: setPreviewIconUrl,
		setHasCleared: setHasClearedIcon,
		labelForMessages: t`Icon`,
		gif: {
			mode: 'require-feature',
			isAllowed: () => canUseAnimatedIcon,
			featureMissingMessage: t`Animated icons require the ANIMATED_ICON guild feature.`,
		},
	});

	const showRemove = (guild.icon || previewIconUrl) && !hasClearedIcon;

	return (
		<div>
			<div className={styles.iconField}>
				<Trans>Icon</Trans>
			</div>

			<div className={styles.iconUploadContainer}>
				{previewIconUrl ? (
					<div className={styles.iconPreview} style={{backgroundImage: `url(${previewIconUrl})`}} />
				) : (
					<div className={styles.iconPreview}>
						<GuildIcon id={guild.id} name={iconName} icon={hasClearedIcon ? null : guild.icon} sizePx={80} />
					</div>
				)}

				<div className={styles.iconUploadActions}>
					<div className={styles.iconUploadButtons}>
						<Button
							variant="primary"
							small={true}
							onClick={controller.pickFile}
							disabled={!canManageGuild || controller.isProcessing}
						>
							<Trans>Upload Icon</Trans>
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

					<div className={styles.iconUploadDescription}>
						{canUseAnimatedIcon ? (
							<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Recommended: 512×512px</Trans>
						) : (
							<Trans>JPEG, PNG, WebP. Max 10MB. Recommended: 512×512px</Trans>
						)}
					</div>
				</div>
			</div>

			{form.formState.errors.icon?.message ? (
				<p className={styles.errorMessage}>{form.formState.errors.icon.message}</p>
			) : null}
		</div>
	);
};
