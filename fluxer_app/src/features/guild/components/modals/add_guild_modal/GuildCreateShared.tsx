// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {AVATAR_RECOMMENDED_SIZE_LABEL, STATIC_IMAGE_FORMATS} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {GlobalLimits} from '@app/features/app/utils/GlobalLimits';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {GuildAcronym} from '@app/features/guild/components/GuildAcronym';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR,
	COMMUNITY_NAME_DESCRIPTOR,
	ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {readBlobAsDataUrl} from '@app/features/platform/utils/BlobDataUrl';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import Users from '@app/features/user/state/Users';
import {AccountGateSlate} from '@app/features/user/utils/AccountGatePresentation';
import {canPerform} from '@app/features/user/utils/AccountGates';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {getAcceptString} from '@fluxer/constants/src/AssetFormatPolicy';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useState} from 'react';
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
	const iconName = (nameValue || '').trim();
	const showIconUploadErrorModal = useCallback(
		(message: string) => {
			showGenericErrorModal({
				...{
					title: i18n._(messages.imageCouldNotBeUsed),
					message,
					dataFlx: `${dataFlx}.icon-upload-error-modal`,
				},
				defer: true,
			});
		},
		[dataFlx, i18n, messages],
	);
	const handleIconUpload = useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: getAcceptString('guild_icon', {animatedAllowed: false})});
			if (!file) return;
			if (file.size > GlobalLimits.getAvatarMaxSize()) {
				showIconUploadErrorModal(
					i18n._(ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
						imageMaxSizeLabel: formatFileSize(i18n.locale, GlobalLimits.getAvatarMaxSize()),
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
			const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await readBlobAsDataUrl(file);
			ModalCommands.push(
				modal(() => (
					<AssetCropModal
						assetType={AssetType.GUILD_ICON}
						imageUrl={base64}
						onCropComplete={(croppedBlob) => {
							void readBlobAsDataUrl(croppedBlob).then(
								(croppedBase64) => {
									form.setValue('icon', croppedBase64);
									setPreviewIconUrl(croppedBase64);
									if (clearIconErrors) form.clearErrors('icon');
								},
								() => {
									showIconUploadErrorModal(i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR));
								},
							);
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
				maxSize: formatFileSize(i18n.locale, GlobalLimits.getAvatarMaxSize()),
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUpload,
			showGifOption: false,
		});
	}, [handleIconUpload, i18n, messages]);
	return {previewIconUrl, iconName, handleOpenIconUpload, handleClearIcon};
}

export function renderGuildCreateAccountGate(dataFlx: string): React.ReactNode {
	const gate = canPerform(Users.currentUser, TierGatedActions.GUILD_CREATE);
	if (gate.allowed) return null;
	return (
		<div className={styles.formContainer} data-flx={`${dataFlx}.form-container`}>
			<AccountGateSlate gate={gate} data-flx={`${dataFlx}.account-gate-slate`} />
		</div>
	);
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
	iconName,
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
						<div className={styles.iconPlaceholder} data-flx={`${dataFlx}.icon-placeholder`}>
							{iconName ? <GuildAcronym name={iconName} data-flx={`${dataFlx}.icon-acronym`} /> : null}
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
