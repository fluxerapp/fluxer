// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR,
	COMMUNITY_NAME_DESCRIPTOR,
	ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {
	getGuildIconDisplayInitials,
	getGuildInitialsFitStyle,
	getInitialsLength,
} from '@app/features/guild/utils/GuildInitialsUtils';
import {
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
	VERIFY_EMAIL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as StringUtils from '@app/lib/strings';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';
import type {UseFormReturn} from 'react-hook-form';

interface GuildCreateFormValues {
	icon?: string | null;
	name: string;
}

interface GuildCreateIconFieldOptions {
	form: UseFormReturn<GuildCreateFormValues>;
	nameValue: string;
	dataFlx: string;
	cropModalDataFlx: string;
	clearIconErrors: boolean;
	messages: {imageCouldNotBeUsed: MessageDescriptor; changeIcon: MessageDescriptor};
}

export function useGuildCreateIconField({
	form,
	nameValue,
	dataFlx,
	cropModalDataFlx,
	clearIconErrors,
	messages,
}: GuildCreateIconFieldOptions) {
	const {i18n} = useLingui();
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const initials = useMemo(() => {
		const raw = (nameValue || '').trim();
		if (!raw) return '';
		return getGuildIconDisplayInitials(StringUtils.getInitialsFromName(raw));
	}, [nameValue]);
	const initialsLength = initials ? getInitialsLength(initials) : null;
	const showIconUploadErrorModal = useCallback(
		(message: string) => {
			showGuildErrorModal({
				title: i18n._(messages.imageCouldNotBeUsed),
				message,
				dataFlx: `${dataFlx}.icon-upload-error-modal`,
			});
		},
		[dataFlx, i18n, messages],
	);
	const handleIconUpload = useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: getAcceptStringFiltered('guild_icon', false)});
			if (!file) return;
			if (file.size > 10 * 1024 * 1024) {
				showIconUploadErrorModal(
					i18n._(ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
						imageMaxSizeLabel: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
					}),
				);
				return;
			}
			const svg = isSvgFile(file);
			const animated = svg ? false : await isAnimatedFile(file);
			if (animated) {
				showIconUploadErrorModal(i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR));
				return;
			}
			const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
			ModalCommands.push(
				modal(() => (
					<AssetCropModal
						assetType={AssetType.GUILD_ICON}
						imageUrl={base64}
						onCropComplete={(croppedBlob) => {
							const reader = new FileReader();
							reader.onload = () => {
								const croppedBase64 = reader.result as string;
								form.setValue('icon', croppedBase64);
								setPreviewIconUrl(croppedBase64);
								if (clearIconErrors) form.clearErrors('icon');
							};
							reader.onerror = () => {
								showIconUploadErrorModal(i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR));
							};
							reader.readAsDataURL(croppedBlob);
						}}
						onSkip={() => {
							form.setValue('icon', base64);
							setPreviewIconUrl(base64);
							if (clearIconErrors) form.clearErrors('icon');
						}}
						data-flx={cropModalDataFlx}
					/>
				)),
			);
		} catch {
			showIconUploadErrorModal(i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
		}
	}, [clearIconErrors, cropModalDataFlx, form, i18n, showIconUploadErrorModal]);
	const handleClearIcon = useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
	}, [form]);
	const handleOpenIconUpload = useCallback(() => {
		openAssetSourceModal({
			title: i18n._(messages.changeIcon),
			uploadHint: formatImageUploadRecommendedHint(i18n, {
				formats: STATIC_IMAGE_FORMATS,
				maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUpload,
			showGifOption: false,
		});
	}, [handleIconUpload, i18n, messages]);
	return {previewIconUrl, initials, initialsLength, handleOpenIconUpload, handleClearIcon};
}

export function renderGuildCreateAccountGate(i18n: I18n, dataFlx: string): React.ReactNode {
	const currentUser = Users.currentUser;
	if (currentUser != null && !currentUser.isClaimed()) {
		return (
			<div className={styles.formContainer} data-flx={`${dataFlx}.form-container`}>
				<div className={styles.verificationNotice} data-flx={`${dataFlx}.verification-notice`}>
					<EnvelopeSimpleIcon size={remFromPx(32)} weight="fill" data-flx={`${dataFlx}.envelope-simple-icon`} />
					<p data-flx={`${dataFlx}.p`}>
						<Trans>You need to claim your account before you can create a community.</Trans>
					</p>
					<Button
						onClick={() => openClaimAccountModal({force: true})}
						data-flx={`${dataFlx}.button.open-claim-account-modal`}
					>
						<Trans>Claim your account</Trans>
					</Button>
				</div>
			</div>
		);
	}
	if (currentUser?.verified === false) {
		return (
			<div className={styles.formContainer} data-flx={`${dataFlx}.form-container--2`}>
				<div className={styles.verificationNotice} data-flx={`${dataFlx}.verification-notice--2`}>
					<EnvelopeSimpleIcon size={remFromPx(32)} weight="fill" data-flx={`${dataFlx}.envelope-simple-icon--2`} />
					<p data-flx={`${dataFlx}.p--2`}>
						<Trans>You need to verify your email address before you can create a community.</Trans>
					</p>
					<Button
						onClick={() =>
							ModalCommands.push(
								modal(
									() => <UserSettingsModal initialTab="account_security" data-flx={`${dataFlx}.user-settings-modal`} />,
									'user-settings',
								),
							)
						}
						data-flx={`${dataFlx}.button.push`}
					>
						{i18n._(VERIFY_EMAIL_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	}
	return null;
}

interface GuildCreateFieldsOptions extends ReturnType<typeof useGuildCreateIconField> {
	i18n: I18n;
	form: UseFormReturn<GuildCreateFormValues>;
	dataFlx: string;
	iconSectionDataFlx: string;
	nameInputDataFlx: string;
	showIconError: boolean;
}

export function renderGuildCreateFields({
	i18n,
	form,
	dataFlx,
	iconSectionDataFlx,
	nameInputDataFlx,
	showIconError,
	previewIconUrl,
	initials,
	initialsLength,
	handleOpenIconUpload,
	handleClearIcon,
}: GuildCreateFieldsOptions): React.ReactNode {
	const guidelinesUrl = Routes.guidelines();
	return (
		<div className={styles.iconSection} data-flx={iconSectionDataFlx}>
			<div className={styles.iconSectionInner} data-flx={`${dataFlx}.icon-section-inner`}>
				<div className={styles.iconLabel} data-flx={`${dataFlx}.icon-label`}>
					<Trans>Community icon</Trans>
				</div>
				<div className={styles.iconPreview} data-flx={`${dataFlx}.icon-preview`}>
					{previewIconUrl ? (
						<div
							className={styles.iconImage}
							style={{backgroundImage: `url(${previewIconUrl})`}}
							data-flx={`${dataFlx}.icon-image`}
						/>
					) : (
						<div
							className={styles.iconPlaceholder}
							data-initials-length={initialsLength}
							data-flx={`${dataFlx}.icon-placeholder`}
						>
							{initials ? (
								<span
									className={styles.iconInitials}
									style={getGuildInitialsFitStyle(initials)}
									data-flx={`${dataFlx}.icon-initials`}
								>
									{initials}
								</span>
							) : null}
						</div>
					)}
					<div className={styles.iconActions} data-flx={`${dataFlx}.icon-actions`}>
						<div className={styles.iconButtons} data-flx={`${dataFlx}.icon-buttons`}>
							<Button
								variant="secondary"
								small={true}
								onClick={handleOpenIconUpload}
								data-flx={`${dataFlx}.button.icon-upload`}
							>
								{previewIconUrl ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
							</Button>
							{previewIconUrl && (
								<Button
									variant="secondary"
									small={true}
									onClick={handleClearIcon}
									data-flx={`${dataFlx}.button.clear-icon`}
								>
									<Trans>Remove icon</Trans>
								</Button>
							)}
						</div>
					</div>
				</div>
				{showIconError && form.formState.errors.icon?.message && (
					<p className={styles.iconError} data-flx={`${dataFlx}.icon-error`}>
						{form.formState.errors.icon.message}
					</p>
				)}
			</div>
			<Input
				data-flx={nameInputDataFlx}
				{...form.register('name')}
				autoFocus={true}
				error={form.formState.errors.name?.message}
				label={i18n._(COMMUNITY_NAME_DESCRIPTOR)}
				minLength={1}
				maxLength={100}
				name="name"
				required={true}
				type="text"
			/>
			{guidelinesUrl && (
				<p className={styles.guidelines} data-flx={`${dataFlx}.guidelines`}>
					<Trans>
						By creating a community, you agree to follow and uphold the{' '}
						<ExternalLink
							href={guidelinesUrl}
							className={styles.guidelinesLink}
							data-flx={`${dataFlx}.guidelines-link`}
						>
							{ph({PRODUCT_NAME: RuntimeConfig.productName})} community guidelines
						</ExternalLink>
						.
					</Trans>
				</p>
			)}
		</div>
	);
}
