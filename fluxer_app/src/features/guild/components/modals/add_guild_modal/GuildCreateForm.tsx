// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_LABEL,
	PRODUCT_NAME,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR,
	COMMUNITY_NAME_DESCRIPTOR,
	CREATE_COMMUNITY_FORM_DESCRIPTOR,
	type GuildCreateFormInputs,
	handleGuildCreationError,
	ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR,
	ModalFooterContext,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {getGuildIconDisplayInitials, getInitialsLength} from '@app/features/guild/utils/GuildInitialsUtils';
import {
	CREATE_COMMUNITY_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
	VERIFY_EMAIL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as StringUtils from '@app/lib/strings';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useContext, useEffect, useId, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const IMAGE_COULDN_T_BE_USED_DESCRIPTOR = msg({
	message: "Image couldn't be used",
	comment: 'Error modal title shown when a create-community icon upload cannot be accepted or processed.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a create-community icon source. Keep it concise. Keep the tone plain and specific.',
});

export const GuildCreateForm = observer(() => {
	const {i18n} = useLingui();
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const form = useForm<GuildCreateFormInputs>({defaultValues: {name: ''}});
	const modalFooterContext = useContext(ModalFooterContext);
	const formId = useId();
	const currentUser = Users.currentUser;
	const shouldRequireClaimedAccount = currentUser != null && !currentUser.isClaimed();
	const nameValue = form.watch('name');
	const rawInitials = useMemo(() => {
		const raw = (nameValue || '').trim();
		if (!raw) return '';
		return StringUtils.getInitialsFromName(raw);
	}, [nameValue]);
	const initials = useMemo(() => getGuildIconDisplayInitials(rawInitials), [rawInitials]);
	const initialsLength = useMemo(() => (rawInitials ? getInitialsLength(rawInitials) : null), [rawInitials]);
	const showIconUploadErrorModal = useCallback(
		(message: string) => {
			showGuildErrorModal({
				title: i18n._(IMAGE_COULDN_T_BE_USED_DESCRIPTOR),
				message,
				dataFlx: 'guild.add-guild-modal.guild-create-form.icon-upload-error-modal',
			});
		},
		[i18n],
	);
	const handleIconUpload = useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: getAcceptStringFiltered('guild_icon', false)});
			if (!file) return;
			if (file.size > 10 * 1024 * 1024) {
				showIconUploadErrorModal(
					i18n._(ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
						imageMaxSizeLabel: IMAGE_MAX_SIZE_LABEL,
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
						sourceMimeType={svg ? 'image/svg+xml' : file.type}
						onCropComplete={(croppedBlob) => {
							const reader = new FileReader();
							reader.onload = () => {
								const croppedBase64 = reader.result as string;
								form.setValue('icon', croppedBase64);
								setPreviewIconUrl(croppedBase64);
								form.clearErrors('icon');
							};
							reader.onerror = () => {
								showIconUploadErrorModal(i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR));
							};
							reader.readAsDataURL(croppedBlob);
						}}
						onSkip={() => {
							form.setValue('icon', base64);
							setPreviewIconUrl(base64);
							form.clearErrors('icon');
						}}
						data-flx="guild.add-guild-modal.handle-icon-upload.asset-crop-modal"
					/>
				)),
			);
		} catch {
			showIconUploadErrorModal(i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
		}
	}, [form, i18n, showIconUploadErrorModal]);
	const onSubmit = useCallback(async (data: GuildCreateFormInputs) => {
		try {
			const guild = await GuildCommands.create({icon: data.icon, name: data.name});
			ModalCommands.pop();
			NavigationCommands.selectChannel(guild.id, guild.system_channel_id || undefined);
		} catch (error) {
			handleGuildCreationError(error);
		}
	}, []);
	const {handleSubmit, isSubmitting} = useFormSubmit({form, onSubmit, defaultErrorField: 'name'});
	useEffect(() => {
		const isNameEmpty = !nameValue?.trim();
		modalFooterContext?.setFooterContent(
			<>
				<Button
					onClick={modalFooterContext.onBack}
					variant="secondary"
					data-flx="guild.add-guild-modal.guild-create-form.button.back"
				>
					<Trans>Back</Trans>
				</Button>
				<Button
					onClick={handleSubmit}
					submitting={isSubmitting}
					disabled={isNameEmpty}
					data-flx="guild.add-guild-modal.guild-create-form.button.submit"
				>
					{i18n._(CREATE_COMMUNITY_DESCRIPTOR)}
				</Button>
			</>,
		);
		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, nameValue]);
	const handleClearIcon = useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
	}, [form]);
	const handleOpenIconUpload = useCallback(() => {
		openAssetSourceModal({
			title: i18n._(CHANGE_ICON_DESCRIPTOR),
			uploadHint: formatImageUploadRecommendedHint(i18n, {
				formats: STATIC_IMAGE_FORMATS,
				maxSize: IMAGE_MAX_SIZE_LABEL,
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUpload,
			showGifOption: false,
		});
	}, [handleIconUpload, i18n]);
	if (shouldRequireClaimedAccount) {
		return (
			<div className={styles.formContainer} data-flx="guild.add-guild-modal.guild-create-form.form-container">
				<div
					className={styles.verificationNotice}
					data-flx="guild.add-guild-modal.guild-create-form.verification-notice"
				>
					<EnvelopeSimpleIcon
						size={remFromPx(32)}
						weight="fill"
						data-flx="guild.add-guild-modal.guild-create-form.envelope-simple-icon"
					/>
					<p data-flx="guild.add-guild-modal.guild-create-form.p">
						<Trans>You need to claim your account before you can create a community.</Trans>
					</p>
					<Button
						onClick={() => openClaimAccountModal({force: true})}
						data-flx="guild.add-guild-modal.guild-create-form.button.open-claim-account-modal"
					>
						<Trans>Claim your account</Trans>
					</Button>
				</div>
			</div>
		);
	}
	if (currentUser?.verified === false) {
		return (
			<div className={styles.formContainer} data-flx="guild.add-guild-modal.guild-create-form.form-container--2">
				<div
					className={styles.verificationNotice}
					data-flx="guild.add-guild-modal.guild-create-form.verification-notice--2"
				>
					<EnvelopeSimpleIcon
						size={remFromPx(32)}
						weight="fill"
						data-flx="guild.add-guild-modal.guild-create-form.envelope-simple-icon--2"
					/>
					<p data-flx="guild.add-guild-modal.guild-create-form.p--2">
						<Trans>You need to verify your email address before you can create a community.</Trans>
					</p>
					<Button
						onClick={() =>
							ModalCommands.push(
								modal(() => (
									<UserSettingsModal
										initialTab="account_security"
										data-flx="guild.add-guild-modal.guild-create-form.user-settings-modal"
									/>
								)),
							)
						}
						data-flx="guild.add-guild-modal.guild-create-form.button.push"
					>
						{i18n._(VERIFY_EMAIL_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.guild-create-form.form-container--3">
			<Modal.Description data-flx="guild.add-guild-modal.guild-create-form.modal-description">
				<Trans>Create a community for you and your friends to chat.</Trans>
			</Modal.Description>
			<Form
				form={form}
				onSubmit={handleSubmit}
				id={formId}
				aria-label={i18n._(CREATE_COMMUNITY_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.guild-create-form.form.submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.guild-create-form.icon-section">
					<div
						className={styles.iconSectionInner}
						data-flx="guild.add-guild-modal.guild-create-form.icon-section-inner"
					>
						<div className={styles.iconLabel} data-flx="guild.add-guild-modal.guild-create-form.icon-label">
							<Trans>Community icon</Trans>
						</div>
						<div className={styles.iconPreview} data-flx="guild.add-guild-modal.guild-create-form.icon-preview">
							{previewIconUrl ? (
								<div
									className={styles.iconImage}
									style={{backgroundImage: `url(${previewIconUrl})`}}
									data-flx="guild.add-guild-modal.guild-create-form.icon-image"
								/>
							) : (
								<div
									className={styles.iconPlaceholder}
									data-initials-length={initialsLength}
									data-flx="guild.add-guild-modal.guild-create-form.icon-placeholder"
								>
									{initials ? (
										<span
											className={styles.iconInitials}
											data-flx="guild.add-guild-modal.guild-create-form.icon-initials"
										>
											{initials}
										</span>
									) : null}
								</div>
							)}
							<div className={styles.iconActions} data-flx="guild.add-guild-modal.guild-create-form.icon-actions">
								<div className={styles.iconButtons} data-flx="guild.add-guild-modal.guild-create-form.icon-buttons">
									<Button
										variant="secondary"
										small={true}
										onClick={handleOpenIconUpload}
										data-flx="guild.add-guild-modal.guild-create-form.button.icon-upload"
									>
										{previewIconUrl ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
									</Button>
									{previewIconUrl && (
										<Button
											variant="secondary"
											small={true}
											onClick={handleClearIcon}
											data-flx="guild.add-guild-modal.guild-create-form.button.clear-icon"
										>
											<Trans>Remove icon</Trans>
										</Button>
									)}
								</div>
							</div>
						</div>
						{form.formState.errors.icon?.message && (
							<p className={styles.iconError} data-flx="guild.add-guild-modal.guild-create-form.icon-error">
								{form.formState.errors.icon.message}
							</p>
						)}
					</div>
					<Input
						data-flx="guild.add-guild-modal.guild-create-form.input.text"
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
					<p className={styles.guidelines} data-flx="guild.add-guild-modal.guild-create-form.guidelines">
						<Trans>
							By creating a community, you agree to follow and uphold the{' '}
							<ExternalLink
								href={Routes.guidelines()}
								className={styles.guidelinesLink}
								data-flx="guild.add-guild-modal.guild-create-form.guidelines-link"
							>
								{PRODUCT_NAME} community guidelines
							</ExternalLink>
							.
						</Trans>
					</p>
				</div>
			</Form>
		</div>
	);
});
