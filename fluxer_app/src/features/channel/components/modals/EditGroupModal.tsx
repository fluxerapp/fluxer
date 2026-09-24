// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import styles from '@app/features/channel/components/modals/EditGroupModal.module.css';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {
	EDIT_GROUP_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {canCropFile} from '@app/features/voice/utils/MediaCapabilities';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {assignTransientUploadFieldMutation} from '@app/lib/forms/TransientUploadFields';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Icon file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment: 'Error message in the edit group modal. Preserve {imageMaxSizeLabel}; it is inserted by code.',
});
const ICON_FILE_IS_TOO_LARGE_TITLE_DESCRIPTOR = msg({
	message: 'Icon file is too large',
	comment: 'Title of the error modal shown when the selected group icon exceeds the size limit.',
});
const UNSUPPORTED_ICON_FORMAT_DESCRIPTOR = msg({
	message: 'Unsupported icon format',
	comment: 'Title of the error modal shown when the selected group icon format is unsupported.',
});
const ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Animated icons are not supported. Use a static image.',
	comment: 'Description text in the edit group modal.',
});
const ANIMATED_ICONS_ARE_NOT_SUPPORTED_TITLE_DESCRIPTOR = msg({
	message: 'Animated icons are not supported',
	comment: 'Title of the error modal shown when an animated group icon is selected.',
});
const COULDN_T_PROCESS_IMAGE_DESCRIPTOR = msg({
	message: "Couldn't process image",
	comment: 'Title of the error modal shown when a cropped group icon cannot be processed.',
});
const INVALID_IMAGE_DESCRIPTOR = msg({
	message: 'Invalid image',
	comment: 'Title of the error modal shown when the selected group icon image cannot be used.',
});
const GROUP_NAME_MUST_NOT_EXCEED_100_CHARACTERS_DESCRIPTOR = msg({
	message: 'Group name must not exceed 100 characters',
	comment: 'Label in the edit group modal.',
});
const GROUP_NAME_DESCRIPTOR = msg({
	message: 'Group name',
	comment: 'Short label in the edit group modal. Keep it concise.',
});
const MY_GROUP_DESCRIPTOR = msg({
	message: 'My group',
	comment: 'Short label in the edit group modal. Keep it concise.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a group icon source. Keep it concise. Keep the tone plain and specific.',
});

interface FormInputs {
	icon?: string | null;
	name: string;
}

export const EditGroupModal = observer(({channelId}: {channelId: string}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const [hasClearedIcon, setHasClearedIcon] = useState(false);
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const form = useForm<FormInputs>({
		defaultValues: useMemo(() => ({name: channel?.name || ''}), [channel]),
	});
	const remoteValues: FormInputs | null = channel ? {name: channel.name || ''} : null;
	const {commitRemoteValues} = useRemoteFormReset<FormInputs>({
		form,
		identityKey: channelId,
		remoteValues,
		isDirty: form.formState.isDirty || Boolean(previewIconUrl) || hasClearedIcon,
		onApply: () => {
			setPreviewIconUrl(null);
			setHasClearedIcon(false);
		},
	});
	const handleIconUpload = useCallback(
		async (file: File | null) => {
			try {
				if (!file) return;
				if (file.size > 10 * 1024 * 1024) {
					showChannelErrorModal({
						title: i18n._(ICON_FILE_IS_TOO_LARGE_TITLE_DESCRIPTOR),
						message: i18n._(ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
							imageMaxSizeLabel: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
						}),
						dataFlx: 'channel.edit-group-modal.icon-file-too-large.generic-error-modal',
					});
					return;
				}
				const svg = isSvgFile(file);
				if (!svg && !(await canCropFile(file))) {
					showChannelErrorModal({
						title: i18n._(UNSUPPORTED_ICON_FORMAT_DESCRIPTOR),
						message: getAssetFormatErrorMessage(i18n, 'guild_icon', 'unsupported_mime'),
						dataFlx: 'channel.edit-group-modal.unsupported-icon-format.generic-error-modal',
					});
					return;
				}
				const animated = svg ? false : await isAnimatedFile(file);
				if (animated) {
					showChannelErrorModal({
						title: i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_TITLE_DESCRIPTOR),
						message: i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR),
						dataFlx: 'channel.edit-group-modal.animated-icon.generic-error-modal',
					});
					return;
				}
				const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
				ModalCommands.push(
					modal(() => (
						<AssetCropModal
							assetType={AssetType.CHANNEL_ICON}
							imageUrl={base64}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									form.setValue('icon', croppedBase64);
									setPreviewIconUrl(croppedBase64);
									setHasClearedIcon(false);
									form.clearErrors('icon');
								};
								reader.onerror = () => {
									showChannelErrorModal({
										title: i18n._(COULDN_T_PROCESS_IMAGE_DESCRIPTOR),
										message: i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR),
										dataFlx: 'channel.edit-group-modal.process-cropped-image-failed.generic-error-modal',
									});
								};
								reader.readAsDataURL(croppedBlob);
							}}
							onSkip={() => {
								form.setValue('icon', base64);
								setPreviewIconUrl(base64);
								setHasClearedIcon(false);
								form.clearErrors('icon');
							}}
							data-flx="channel.edit-group-modal.handle-icon-upload.asset-crop-modal"
						/>
					)),
				);
			} catch {
				showChannelErrorModal({
					title: i18n._(INVALID_IMAGE_DESCRIPTOR),
					message: i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR),
					dataFlx: 'channel.edit-group-modal.invalid-image.generic-error-modal',
				});
			}
		},
		[form, i18n],
	);
	const handleIconUploadClick = useCallback(async () => {
		const [file] = await openFilePicker({accept: getAcceptStringFiltered('guild_icon', false)});
		await handleIconUpload(file ?? null);
	}, [handleIconUpload]);
	const handleOpenIconUpload = useCallback(() => {
		openAssetSourceModal({
			title: i18n._(CHANGE_ICON_DESCRIPTOR),
			uploadHint: formatImageUploadRecommendedHint(i18n, {
				formats: STATIC_IMAGE_FORMATS,
				maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUploadClick,
			showGifOption: false,
		});
	}, [handleIconUploadClick, i18n]);
	const handleClearIcon = useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
		setHasClearedIcon(true);
	}, [form]);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			const updateData: {icon?: string | null; name: string} = {name: data.name};
			assignTransientUploadFieldMutation(updateData, 'icon', {
				value: data.icon,
				previewUrl: previewIconUrl,
				hasCleared: hasClearedIcon,
			});
			const newChannel = await ChannelCommands.update(channelId, updateData);
			commitRemoteValues({name: newChannel.name || data.name});
			ToastCommands.createToast({type: 'success', children: <Trans>Group updated</Trans>});
			ModalCommands.pop();
		},
		[channelId, commitRemoteValues, previewIconUrl, hasClearedIcon],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	if (!channel) {
		return null;
	}
	const iconPresentable = hasClearedIcon
		? null
		: (previewIconUrl ?? AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon}));
	const placeholderName = channel ? ChannelUtils.getDMDisplayName(channel) : '';
	return (
		<Modal.Root size="small" centered data-flx="channel.edit-group-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.edit-group-modal.form.submit">
				<Modal.Header title={i18n._(EDIT_GROUP_DESCRIPTOR)} data-flx="channel.edit-group-modal.modal-header" />
				<Modal.Content data-flx="channel.edit-group-modal.modal-content">
					<Modal.ContentLayout data-flx="channel.edit-group-modal.modal-content-layout">
						<div className={styles.iconSection} data-flx="channel.edit-group-modal.icon-section">
							<div className={styles.iconLabel} data-flx="channel.edit-group-modal.icon-label">
								<Trans>Group icon</Trans>
							</div>
							<div className={styles.iconContainer} data-flx="channel.edit-group-modal.icon-container">
								{previewIconUrl ? (
									<div
										className={styles.iconPreview}
										style={{
											backgroundImage: `url(${previewIconUrl})`,
										}}
										data-flx="channel.edit-group-modal.icon-preview"
									/>
								) : iconPresentable ? (
									<div
										className={styles.iconPreview}
										style={{
											backgroundImage: `url(${iconPresentable})`,
										}}
										data-flx="channel.edit-group-modal.icon-preview--2"
									/>
								) : (
									<div className={styles.iconPlaceholder} data-flx="channel.edit-group-modal.icon-placeholder">
										<PlusIcon
											weight="regular"
											className={styles.iconPlaceholderIcon}
											data-flx="channel.edit-group-modal.icon-placeholder-icon"
										/>
									</div>
								)}
								<div className={styles.iconActions} data-flx="channel.edit-group-modal.icon-actions">
									<div className={styles.iconButtonGroup} data-flx="channel.edit-group-modal.icon-button-group">
										<Button
											variant="secondary"
											small={true}
											onClick={handleOpenIconUpload}
											data-flx="channel.edit-group-modal.button.icon-upload-click"
										>
											{previewIconUrl || iconPresentable ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
										</Button>
										{(previewIconUrl || iconPresentable) && (
											<Button
												variant="secondary"
												small={true}
												onClick={handleClearIcon}
												data-flx="channel.edit-group-modal.button.clear-icon"
											>
												<Trans>Remove icon</Trans>
											</Button>
										)}
									</div>
								</div>
							</div>
							{form.formState.errors.icon?.message && (
								<p className={styles.iconError} data-flx="channel.edit-group-modal.icon-error">
									{form.formState.errors.icon.message}
								</p>
							)}
						</div>
						<Input
							data-flx="channel.edit-group-modal.input.text"
							{...form.register('name', {
								maxLength: {
									value: 100,
									message: i18n._(GROUP_NAME_MUST_NOT_EXCEED_100_CHARACTERS_DESCRIPTOR),
								},
							})}
							type="text"
							label={i18n._(GROUP_NAME_DESCRIPTOR)}
							placeholder={placeholderName || i18n._(MY_GROUP_DESCRIPTOR)}
							maxLength={100}
							error={form.formState.errors.name?.message}
						/>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="channel.edit-group-modal.modal-footer">
					<Button type="submit" submitting={isSubmitting} data-flx="channel.edit-group-modal.button.submit">
						<Trans>Save</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
