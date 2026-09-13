// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import styles from '@app/features/channel/components/modals/EditGroupBottomSheet.module.css';
import Channels from '@app/features/channel/state/Channels';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {
	EDIT_GROUP_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	GO_BACK_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {canCropFormat} from '@app/features/voice/utils/MediaCapabilities';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {assignTransientUploadFieldMutation} from '@app/lib/forms/TransientUploadFields';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Icon file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment:
		'Error modal body in the mobile edit group sheet when the chosen icon file exceeds the size limit. imageMaxSizeLabel is a localized size.',
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
	comment: 'Error modal body in the mobile edit group sheet when an animated icon is chosen.',
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
const EDIT_GROUP_FORM_DESCRIPTOR = msg({
	message: 'Edit group form',
	comment: 'Accessible label for the edit group form region in the mobile bottom sheet.',
});
const GROUP_NAME_DESCRIPTOR = msg({
	message: 'Group name',
	comment: 'Field label for the group name input in the mobile edit group sheet.',
});
const MY_GROUP_DESCRIPTOR = msg({
	message: 'My group',
	comment: 'Placeholder text in the group name input in the mobile edit group sheet.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a group icon source in the mobile edit group sheet. Keep it concise. Keep the tone plain and specific.',
});

interface FormInputs {
	icon?: string | null;
	name: string;
}

interface EditGroupBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
}

export const EditGroupBottomSheet: React.FC<EditGroupBottomSheetProps> = observer(({isOpen, onClose, channelId}) => {
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
						dataFlx: 'channel.edit-group-bottom-sheet.icon-file-too-large.generic-error-modal',
					});
					return;
				}
				const svg = isSvgFile(file);
				if (!svg && !(await canCropFormat(file.type))) {
					showChannelErrorModal({
						title: i18n._(UNSUPPORTED_ICON_FORMAT_DESCRIPTOR),
						message: getAssetFormatErrorMessage(i18n, 'guild_icon', 'unsupported_mime'),
						dataFlx: 'channel.edit-group-bottom-sheet.unsupported-icon-format.generic-error-modal',
					});
					return;
				}
				const animated = svg ? false : await isAnimatedFile(file);
				if (animated) {
					showChannelErrorModal({
						title: i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_TITLE_DESCRIPTOR),
						message: i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_PLEASE_USE_DESCRIPTOR),
						dataFlx: 'channel.edit-group-bottom-sheet.animated-icon.generic-error-modal',
					});
					return;
				}
				const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
				ModalCommands.push(
					modal(() => (
						<AssetCropModal
							assetType={AssetType.CHANNEL_ICON}
							imageUrl={base64}
							sourceMimeType={svg ? 'image/svg+xml' : file.type}
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
										dataFlx: 'channel.edit-group-bottom-sheet.process-cropped-image-failed.generic-error-modal',
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
							data-flx="channel.edit-group-bottom-sheet.handle-icon-upload.asset-crop-modal"
						/>
					)),
				);
			} catch {
				showChannelErrorModal({
					title: i18n._(INVALID_IMAGE_DESCRIPTOR),
					message: i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR),
					dataFlx: 'channel.edit-group-bottom-sheet.invalid-image.generic-error-modal',
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
			onClose();
		},
		[channelId, commitRemoteValues, onClose, previewIconUrl, hasClearedIcon],
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
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			disablePadding={true}
			surface="primary"
			leadingAction={
				<button
					type="button"
					onClick={onClose}
					className={styles.backButton}
					aria-label={i18n._(GO_BACK_DESCRIPTOR)}
					data-flx="channel.edit-group-bottom-sheet.back-button.close"
				>
					<ArrowLeftIcon
						className={styles.backIcon}
						weight="bold"
						data-flx="channel.edit-group-bottom-sheet.back-icon"
					/>
				</button>
			}
			title={i18n._(EDIT_GROUP_DESCRIPTOR)}
			data-flx="channel.edit-group-bottom-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="channel.edit-group-bottom-sheet.container">
				<Scroller
					className={styles.scroller}
					key="edit-group-bottom-sheet-scroller"
					data-flx="channel.edit-group-bottom-sheet.scroller"
				>
					<div className={styles.scrollContent} data-flx="channel.edit-group-bottom-sheet.scroll-content">
						<Form
							form={form}
							onSubmit={handleSubmit}
							className={styles.form}
							aria-label={i18n._(EDIT_GROUP_FORM_DESCRIPTOR)}
							data-flx="channel.edit-group-bottom-sheet.form.submit"
						>
							<div className={styles.iconSection} data-flx="channel.edit-group-bottom-sheet.icon-section">
								<div className={styles.iconLabel} data-flx="channel.edit-group-bottom-sheet.icon-label">
									<Trans>Group icon</Trans>
								</div>
								<div className={styles.iconContainer} data-flx="channel.edit-group-bottom-sheet.icon-container">
									{previewIconUrl ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${previewIconUrl})`,
											}}
											data-flx="channel.edit-group-bottom-sheet.icon-preview"
										/>
									) : iconPresentable ? (
										<div
											className={styles.iconPreview}
											style={{
												backgroundImage: `url(${iconPresentable})`,
											}}
											data-flx="channel.edit-group-bottom-sheet.icon-preview--2"
										/>
									) : (
										<div className={styles.iconPlaceholder} data-flx="channel.edit-group-bottom-sheet.icon-placeholder">
											<PlusIcon
												weight="regular"
												className={styles.iconPlaceholderIcon}
												data-flx="channel.edit-group-bottom-sheet.icon-placeholder-icon"
											/>
										</div>
									)}
									<div className={styles.iconActions} data-flx="channel.edit-group-bottom-sheet.icon-actions">
										<div
											className={styles.iconButtonGroup}
											data-flx="channel.edit-group-bottom-sheet.icon-button-group"
										>
											<Button
												variant="secondary"
												small={true}
												onClick={handleOpenIconUpload}
												data-flx="channel.edit-group-bottom-sheet.button.icon-upload-click"
											>
												{previewIconUrl || iconPresentable ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
											</Button>
											{(previewIconUrl || iconPresentable) && (
												<Button
													variant="secondary"
													small={true}
													onClick={handleClearIcon}
													data-flx="channel.edit-group-bottom-sheet.button.clear-icon"
												>
													<Trans>Remove icon</Trans>
												</Button>
											)}
										</div>
									</div>
								</div>
								{form.formState.errors.icon?.message && (
									<p className={styles.iconError} data-flx="channel.edit-group-bottom-sheet.icon-error">
										{form.formState.errors.icon.message}
									</p>
								)}
							</div>
							<Input
								data-flx="channel.edit-group-bottom-sheet.input.text"
								{...form.register('name')}
								type="text"
								label={i18n._(GROUP_NAME_DESCRIPTOR)}
								placeholder={i18n._(MY_GROUP_DESCRIPTOR)}
								minLength={1}
								maxLength={100}
								error={form.formState.errors.name?.message}
							/>
							<div className={styles.footer} data-flx="channel.edit-group-bottom-sheet.footer">
								<Button
									type="submit"
									submitting={isSubmitting}
									className={styles.fullWidth}
									data-flx="channel.edit-group-bottom-sheet.full-width.submit"
								>
									<Trans>Save</Trans>
								</Button>
							</div>
						</Form>
					</div>
				</Scroller>
			</div>
		</BottomSheet>
	);
});
