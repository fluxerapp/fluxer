// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_BYTES,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import Channels from '@app/features/channel/state/Channels';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {canCropFile} from '@app/features/voice/utils/MediaCapabilities';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {assignTransientUploadFieldMutation} from '@app/lib/forms/TransientUploadFields';
import type {MessageDescriptor} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {useCallback, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

interface FormInputs {
	icon?: string | null;
	name: string;
	nsfw: boolean;
}

interface EditGroupFormMessages {
	iconFileTooLargeTitle: MessageDescriptor;
	unsupportedIconFormatTitle: MessageDescriptor;
	animatedIconsNotSupportedTitle: MessageDescriptor;
	couldNotProcessImageTitle: MessageDescriptor;
	invalidImageTitle: MessageDescriptor;
	iconFileTooLarge: MessageDescriptor;
	animatedIconsNotSupported: MessageDescriptor;
	changeIcon: MessageDescriptor;
}

export function useEditGroupForm({
	channelId,
	onDone,
	dataFlx,
	messages,
}: {
	channelId: string;
	onDone: () => void;
	dataFlx: string;
	messages: EditGroupFormMessages;
}) {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const isOwner = channel?.ownerId === Authentication.currentUserId;
	const [hasClearedIcon, setHasClearedIcon] = useState(false);
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const form = useForm<FormInputs>({
		defaultValues: useMemo(() => ({name: channel?.name || '', nsfw: channel?.nsfw ?? false}), [channel]),
	});
	const remoteValues: FormInputs | null = channel ? {name: channel.name || '', nsfw: channel.nsfw} : null;
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
						title: i18n._(messages.iconFileTooLargeTitle),
						message: i18n._(messages.iconFileTooLarge, {
							imageMaxSizeLabel: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
						}),
						dataFlx: `${dataFlx}.icon-file-too-large.generic-error-modal`,
					});
					return;
				}
				const svg = isSvgFile(file);
				if (!svg && !(await canCropFile(file))) {
					showChannelErrorModal({
						title: i18n._(messages.unsupportedIconFormatTitle),
						message: getAssetFormatErrorMessage(i18n, 'guild_icon', 'unsupported_mime'),
						dataFlx: `${dataFlx}.unsupported-icon-format.generic-error-modal`,
					});
					return;
				}
				const animated = svg ? false : await isAnimatedFile(file);
				if (animated) {
					showChannelErrorModal({
						title: i18n._(messages.animatedIconsNotSupportedTitle),
						message: i18n._(messages.animatedIconsNotSupported),
						dataFlx: `${dataFlx}.animated-icon.generic-error-modal`,
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
										title: i18n._(messages.couldNotProcessImageTitle),
										message: i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR),
										dataFlx: `${dataFlx}.process-cropped-image-failed.generic-error-modal`,
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
							data-flx={`${dataFlx}.handle-icon-upload.asset-crop-modal`}
						/>
					)),
				);
			} catch {
				showChannelErrorModal({
					title: i18n._(messages.invalidImageTitle),
					message: i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR),
					dataFlx: `${dataFlx}.invalid-image.generic-error-modal`,
				});
			}
		},
		[dataFlx, form, i18n, messages],
	);
	const handleIconUploadClick = useCallback(async () => {
		const [file] = await openFilePicker({accept: getAcceptStringFiltered('guild_icon', false)});
		await handleIconUpload(file ?? null);
	}, [handleIconUpload]);
	const handleOpenIconUpload = useCallback(() => {
		openAssetSourceModal({
			title: i18n._(messages.changeIcon),
			uploadHint: formatImageUploadRecommendedHint(i18n, {
				formats: STATIC_IMAGE_FORMATS,
				maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUploadClick,
			showGifOption: false,
		});
	}, [handleIconUploadClick, i18n, messages]);
	const handleClearIcon = useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
		setHasClearedIcon(true);
	}, [form]);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			const updateData: {icon?: string | null; name: string; nsfw?: boolean} = {name: data.name};
			if (isOwner && data.nsfw !== channel?.nsfw) {
				updateData.nsfw = data.nsfw;
			}
			assignTransientUploadFieldMutation(updateData, 'icon', {
				value: data.icon,
				previewUrl: previewIconUrl,
				hasCleared: hasClearedIcon,
			});
			const newChannel = await ChannelCommands.update(channelId, updateData);
			commitRemoteValues({name: newChannel.name || data.name, nsfw: newChannel.nsfw ?? data.nsfw});
			ToastCommands.createToast({type: 'success', children: <Trans>Group updated</Trans>});
			onDone();
		},
		[channel, channelId, commitRemoteValues, isOwner, onDone, previewIconUrl, hasClearedIcon],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	const iconPresentable =
		hasClearedIcon || !channel
			? null
			: (previewIconUrl ?? AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon}));
	return {
		channel,
		form,
		isOwner,
		previewIconUrl,
		iconPresentable,
		handleOpenIconUpload,
		handleClearIcon,
		handleSubmit,
		isSubmitting,
	};
}
