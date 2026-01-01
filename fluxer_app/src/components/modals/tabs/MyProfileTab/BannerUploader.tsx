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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {AssetCropModal, AssetType} from '~/components/modals/AssetCropModal';
import {Button} from '~/components/uikit/Button/Button';
import {PlutoniumUpsell} from '~/components/uikit/PlutoniumUpsell/PlutoniumUpsell';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import styles from './BannerUploader.module.css';

export type BannerMode = 'inherit' | 'custom' | 'unset';

interface BannerUploaderProps {
	hasBanner: boolean;
	onBannerChange: (base64: string) => void;
	onBannerClear: () => void;
	disabled?: boolean;
	hasPremium: boolean;
	isPerGuildProfile: boolean;
	errorMessage?: string;
	bannerMode?: BannerMode;
	onBannerModeChange?: (mode: BannerMode) => void;
}

export const BannerUploader = observer(
	({
		hasBanner,
		onBannerChange,
		onBannerClear,
		disabled,
		hasPremium,
		isPerGuildProfile,
		errorMessage,
		bannerMode = 'inherit',
		onBannerModeChange,
	}: BannerUploaderProps) => {
		const {t} = useLingui();

		const getBannerModeOptions = React.useCallback(
			() => [
				{
					value: 'inherit' as BannerMode,
					name: t`Use global profile`,
					desc: t`Show your global profile banner in this community`,
				},
				{
					value: 'custom' as BannerMode,
					name: t`Use custom image`,
					desc: t`Upload a custom banner for this community`,
					disabled: !hasPremium,
				},
				{
					value: 'unset' as BannerMode,
					name: t`Don't show`,
					desc: t`Show accent color only, ignoring your global profile`,
				},
			],
			[t, hasPremium],
		);

		const handleBannerUpload = React.useCallback(async () => {
			try {
				const [file] = await openFilePicker({accept: 'image/*'});
				if (!file) return;

				if (file.size > 10 * 1024 * 1024) {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Banner file is too large. Please choose a file smaller than 10MB.`,
					});
					return;
				}

				const base64 = await AvatarUtils.fileToBase64(file);

				ModalActionCreators.push(
					modal(() => (
						<AssetCropModal
							imageUrl={base64}
							sourceMimeType={file.type}
							assetType={AssetType.PROFILE_BANNER}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									onBannerChange(croppedBase64);
								};
								reader.onerror = () => {
									ToastActionCreators.createToast({
										type: 'error',
										children: t`Failed to process the cropped image. Please try again.`,
									});
								};
								reader.readAsDataURL(croppedBlob);
							}}
							onSkip={() => {
								onBannerChange(base64);
							}}
						/>
					)),
				);
			} catch {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`That image is invalid. Please try another one.`,
				});
			}
		}, [onBannerChange, t]);

		const handleModeChange = React.useCallback(
			(mode: BannerMode) => {
				onBannerModeChange?.(mode);
				if (mode === 'custom') {
					handleBannerUpload();
				}
			},
			[onBannerModeChange, handleBannerUpload],
		);

		const bannerModeOptions = getBannerModeOptions();

		const radioGroupDisabled = Boolean(disabled && !(isPerGuildProfile && !hasPremium));

		if (isPerGuildProfile && onBannerModeChange) {
			return (
				<div>
					<div className={styles.label}>
						<Trans>Banner</Trans>
					</div>
					<RadioGroup
						options={bannerModeOptions}
						value={bannerMode}
						disabled={radioGroupDisabled}
						onChange={handleModeChange}
						aria-label={t`Banner mode selection`}
					/>
					{bannerMode === 'custom' && (
						<div className={styles.buttonGroup}>
							<Button variant="primary" small={true} onClick={handleBannerUpload} disabled={disabled}>
								<Trans>Change Banner</Trans>
							</Button>
							{hasBanner && (
								<Button
									variant="secondary"
									small={true}
									onClick={() => {
										onBannerModeChange('inherit');
										onBannerClear();
									}}
									disabled={disabled}
								>
									<Trans>Remove Banner</Trans>
								</Button>
							)}
						</div>
					)}
					<div className={clsx(styles.description, styles.helperSpacing)}>
						<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Minimum: 680×240px (17:6)</Trans>
					</div>
					{errorMessage && <p className={styles.errorMessage}>{errorMessage}</p>}
				</div>
			);
		}

		return (
			<div>
				<div className={styles.label}>
					<Trans>Banner</Trans>
				</div>
				{hasPremium || isPerGuildProfile ? (
					<>
						<div className={styles.buttonGroup}>
							<Button variant="primary" small={true} onClick={handleBannerUpload} disabled={disabled}>
								<Trans>Change Banner</Trans>
							</Button>
							{hasBanner && (
								<Button variant="secondary" small={true} onClick={onBannerClear} disabled={disabled}>
									<Trans>Remove Banner</Trans>
								</Button>
							)}
						</div>
						<div className={styles.description}>
							<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Minimum: 680×240px (17:6)</Trans>
						</div>
					</>
				) : (
					<PlutoniumUpsell>
						<Trans>Customize your profile with a static or animated banner image to make it stand out.</Trans>
					</PlutoniumUpsell>
				)}
				{errorMessage && <p className={styles.errorMessage}>{errorMessage}</p>}
			</div>
		);
	},
);
