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
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {MAX_BIO_LENGTH_PREMIUM} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import {PlutoniumLink} from '~/components/uikit/PlutoniumLink/PlutoniumLink';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import {AssetCropModal, AssetType} from '../../AssetCropModal';
import styles from './AvatarUploader.module.css';

export type AvatarMode = 'inherit' | 'custom' | 'unset';

interface AvatarUploaderProps {
	hasAvatar: boolean;
	onAvatarChange: (base64: string) => void;
	onAvatarClear: () => void;
	disabled?: boolean;
	hasPremium: boolean;
	isPerGuildProfile: boolean;
	errorMessage?: string;
	avatarMode?: AvatarMode;
	onAvatarModeChange?: (mode: AvatarMode) => void;
}

export const AvatarUploader = observer(
	({
		hasAvatar,
		onAvatarChange,
		onAvatarClear,
		disabled,
		hasPremium,
		isPerGuildProfile,
		errorMessage,
		avatarMode = 'inherit',
		onAvatarModeChange,
	}: AvatarUploaderProps) => {
		const {t} = useLingui();

		const getAvatarModeOptions = React.useCallback(
			() => [
				{
					value: 'inherit' as AvatarMode,
					name: t`Use global profile`,
					desc: t`Show your global profile avatar in this community`,
				},
				{
					value: 'custom' as AvatarMode,
					name: t`Use custom image`,
					desc: t`Upload a custom avatar for this community`,
					disabled: !hasPremium,
				},
				{
					value: 'unset' as AvatarMode,
					name: t`Don't show`,
					desc: t`Show default avatar, ignoring your global profile`,
				},
			],
			[t, hasPremium],
		);

		const handleAvatarUpload = React.useCallback(async () => {
			try {
				const [file] = await openFilePicker({accept: 'image/*'});
				if (!file) return;

				if (file.size > 10 * 1024 * 1024) {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Avatar file is too large. Please choose a file smaller than 10MB.`,
					});
					return;
				}

				if (file.type === 'image/gif' && !hasPremium) {
					ModalActionCreators.push(
						modal(() => (
							<ConfirmModal
								title={t`Animated Avatars Require Plutonium`}
								description={
									<>
										<p>
											<Trans>
												Animated avatars (GIFs) are a premium feature exclusively available to Plutonium subscribers.
											</Trans>
										</p>
										<p className={styles.spacedParagraph}>
											<Trans>
												With Plutonium, you can use animated GIFs for both your avatar and banner, customize your
												4-digit tag, write longer bios (up to {MAX_BIO_LENGTH_PREMIUM} characters), and unlock many
												other premium features.
											</Trans>
										</p>
									</>
								}
								primaryText={t`Get Plutonium`}
								primaryVariant="primary"
								secondaryText={t`Cancel`}
								onPrimary={() => {
									PremiumModalActionCreators.open();
								}}
							/>
						)),
					);
					return;
				}

				const base64 = await AvatarUtils.fileToBase64(file);

				ModalActionCreators.push(
					modal(() => (
						<AssetCropModal
							assetType={AssetType.AVATAR}
							imageUrl={base64}
							sourceMimeType={file.type}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									onAvatarChange(croppedBase64);
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
								onAvatarChange(base64);
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
		}, [hasPremium, onAvatarChange, t]);

		const handleModeChange = React.useCallback(
			(mode: AvatarMode) => {
				onAvatarModeChange?.(mode);
				if (mode === 'custom') {
					handleAvatarUpload();
				}
			},
			[onAvatarModeChange, handleAvatarUpload],
		);

		const avatarModeOptions = getAvatarModeOptions();

		const radioGroupDisabled = Boolean(disabled && !(isPerGuildProfile && !hasPremium));

		if (isPerGuildProfile && onAvatarModeChange) {
			return (
				<div>
					<div className={styles.label}>
						<Trans>Avatar</Trans>
					</div>
					<RadioGroup
						options={avatarModeOptions}
						value={avatarMode}
						disabled={radioGroupDisabled}
						onChange={handleModeChange}
						aria-label={t`Avatar mode selection`}
					/>
					{avatarMode === 'custom' && (
						<div className={styles.buttonGroup}>
							<Button variant="primary" small={true} onClick={handleAvatarUpload} disabled={disabled}>
								<Trans>Change Avatar</Trans>
							</Button>
							{hasAvatar && (
								<Button
									variant="secondary"
									small={true}
									onClick={() => {
										onAvatarModeChange('inherit');
										onAvatarClear();
									}}
									disabled={disabled}
								>
									<Trans>Remove Avatar</Trans>
								</Button>
							)}
						</div>
					)}
					<div className={clsx(styles.description, styles.helperSpacing)}>
						<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Recommended: 512×512px</Trans>
					</div>
					{errorMessage && <p className={styles.errorMessage}>{errorMessage}</p>}
				</div>
			);
		}

		return (
			<div>
				<div className={styles.label}>
					<Trans>Avatar</Trans>
				</div>
				<div className={styles.buttonGroup}>
					<Button variant="primary" small={true} onClick={handleAvatarUpload} disabled={disabled}>
						<Trans>Change Avatar</Trans>
					</Button>
					{hasAvatar && (
						<Button variant="secondary" small={true} onClick={onAvatarClear} disabled={disabled}>
							<Trans>Remove Avatar</Trans>
						</Button>
					)}
				</div>
				{!isPerGuildProfile && (
					<div className={styles.description}>
						{hasPremium ? (
							<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Recommended: 512×512px</Trans>
						) : (
							<Trans>
								JPEG, PNG, WebP. Max 10MB. Recommended: 512×512px. Animated avatars (GIF) require <PlutoniumLink />.
							</Trans>
						)}
					</div>
				)}
				{isPerGuildProfile && (
					<div className={styles.description}>
						<Trans>JPEG, PNG, GIF, WebP. Max 10MB. Recommended: 512×512px</Trans>
					</div>
				)}
				{errorMessage && <p className={styles.errorMessage}>{errorMessage}</p>}
			</div>
		);
	},
);
