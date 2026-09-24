// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';
import {AssetCropModal, type AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {showAnimatedAvifUnsupportedModal} from '@app/features/expressions/utils/AnimatedAvifModalUtils';
import {inspectImageFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {downloadGifAsImageFile} from '@app/features/expressions/utils/GifFileDownload';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import {
	blobToDataUrl,
	MAX_IMAGE_BYTES,
	revokeObjectUrl,
} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/utils/ImageAsset';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {canDecodeAnimatedAvif} from '@app/features/platform/utils/ImageDecoderInterop';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import type {UseFormReturn} from 'react-hook-form';

const FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_FILE_DESCRIPTOR = msg({
	message: '{label} file is too large. Choose a file smaller than {maxSizeLabel}.',
	comment:
		'Error modal body shown when a community image upload exceeds the size limit. {label} names the image field and {maxSizeLabel} is a formatted file size.',
});
const IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR = msg({
	message: '{label} images cannot be animated. Use a static image.',
	comment:
		'Error modal body shown when a community image field does not allow animation. {label} names the image field.',
});
const ANIMATED_IMAGES_ARE_NOT_SUPPORTED_FOR_THIS_ASSET_DESCRIPTOR = msg({
	message: 'Animated images are not supported for this asset.',
	comment: 'Confirmation modal body shown before using a non-croppable animated community image.',
});
const CROPPED_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Cropped image is too large. Choose a smaller area or a smaller file (max {maxSizeLabel}).',
	comment:
		'Error modal body shown when a cropped community image still exceeds the size limit. {maxSizeLabel} is a formatted file size.',
});
const IMAGE_COULDN_T_BE_USED_DESCRIPTOR = msg({
	message: "Image couldn't be used",
	comment: 'Error modal title shown when a community image upload cannot be accepted or processed.',
});

export type ImageAssetFieldName = 'icon' | 'banner' | 'splash' | 'embed_splash';
type GifMode = 'allow' | 'disallow' | 'require-feature';

export interface GifPolicy {
	mode: GifMode;
	isAllowed?: () => boolean;
	featureMissingMessage?: string;
	disallowedMessage?: string;
}

export interface AspectRatioConfig {
	compute: (dataUrl: string) => Promise<number>;
	set: (ratio: number | undefined) => void;
}

export interface UseGuildImageAssetFieldArgs {
	form: UseFormReturn<FormInputs>;
	fieldName: ImageAssetFieldName;
	assetType: AssetType;
	canManage: boolean;
	filePickerAccept: string;
	previewUrl: string | null;
	setPreviewUrl: React.Dispatch<React.SetStateAction<string | null>>;
	setHasCleared: React.Dispatch<React.SetStateAction<boolean>>;
	maxBytes?: number;
	labelForMessages: string;
	gif?: GifPolicy;
	aspectRatio?: AspectRatioConfig;
	sourceModalTitle?: string;
	uploadHint?: React.ReactNode;
}

export interface ImageAssetFieldController {
	pickFile: () => Promise<void>;
	openSourcePicker: () => void;
	handleFile: (file: File | null) => Promise<void>;
	clear: () => void;
	isProcessing: boolean;
}

interface ApplyDataUrlOptions {
	skipAspectRatio?: boolean;
}

export function useGuildImageAssetField({
	form,
	fieldName,
	assetType,
	canManage,
	filePickerAccept,
	previewUrl,
	setPreviewUrl,
	setHasCleared,
	maxBytes = MAX_IMAGE_BYTES,
	labelForMessages,
	gif,
	aspectRatio,
	sourceModalTitle,
	uploadHint,
}: UseGuildImageAssetFieldArgs): ImageAssetFieldController {
	const {i18n} = useLingui();
	const label = labelForMessages;
	const requestIdRef = useRef(0);
	const [isProcessing, setIsProcessing] = useState(false);
	useEffect(() => {
		return () => {
			revokeObjectUrl(previewUrl);
		};
	}, [previewUrl]);
	const showErrorModal = useCallback(
		(message: React.ReactNode) => {
			showGuildErrorModal({
				title: i18n._(IMAGE_COULDN_T_BE_USED_DESCRIPTOR),
				message,
				dataFlx: 'guild.guild-tabs.guild-overview-tab.use-guild-image-asset-field.error-modal',
			});
		},
		[i18n],
	);
	const setFieldValue = useCallback(
		(value: string | null) => {
			form.setValue(fieldName as keyof FormInputs, value, {shouldDirty: true, shouldValidate: true});
			form.clearErrors(fieldName as keyof FormInputs);
		},
		[form, fieldName],
	);
	const applyAspectRatio = useCallback(
		(dataUrl: string, requestId: number) => {
			if (!aspectRatio) return;
			aspectRatio
				.compute(dataUrl)
				.then((ratio) => {
					if (requestId !== requestIdRef.current) return;
					aspectRatio.set(Number.isFinite(ratio) && ratio > 0 ? ratio : undefined);
				})
				.catch(() => {
					if (requestId !== requestIdRef.current) return;
					aspectRatio.set(undefined);
				});
		},
		[aspectRatio],
	);
	const applyDataUrl = useCallback(
		async (dataUrl: string, blob: Blob, requestId: number, options?: ApplyDataUrlOptions) => {
			if (requestId !== requestIdRef.current) return;
			const nextPreviewUrl = URL.createObjectURL(blob);
			setFieldValue(dataUrl);
			setPreviewUrl(nextPreviewUrl);
			setHasCleared(false);
			if (options?.skipAspectRatio) {
				aspectRatio?.set(undefined);
				return;
			}
			applyAspectRatio(dataUrl, requestId);
		},
		[applyAspectRatio, aspectRatio, setFieldValue, setHasCleared, setPreviewUrl],
	);
	const handleFile = useCallback(
		async (file: File | null) => {
			if (!file) return;
			if (!canManage) return;
			const requestId = ++requestIdRef.current;
			if (file.size > maxBytes) {
				const maxSizeLabel = formatFileSize(i18n.locale, maxBytes);
				showErrorModal(i18n._(FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_FILE_DESCRIPTOR, {label, maxSizeLabel}));
				return;
			}
			const svg = isSvgFile(file);
			const {format, animated} = svg ? {format: 'unknown', animated: false} : await inspectImageFile(file);
			if (animated) {
				const policy = gif?.mode ?? 'disallow';
				if (policy === 'disallow') {
					showErrorModal(
						gif?.disallowedMessage ??
							i18n._(IMAGES_CANNOT_BE_ANIMATED_PLEASE_USE_DESCRIPTOR, {
								label,
							}),
					);
					return;
				}
				if (policy === 'require-feature') {
					const allowed = gif?.isAllowed?.() ?? false;
					if (!allowed) {
						showErrorModal(
							gif?.featureMissingMessage ?? i18n._(ANIMATED_IMAGES_ARE_NOT_SUPPORTED_FOR_THIS_ASSET_DESCRIPTOR),
						);
						return;
					}
				}
			}
			const isAnimatedAvif = animated && format === 'avif';
			if (isAnimatedAvif && !(await canDecodeAnimatedAvif())) {
				showAnimatedAvifUnsupportedModal({i18n});
				return;
			}
			setIsProcessing(true);
			let sourceBase64: string;
			try {
				sourceBase64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
			} catch {
				setIsProcessing(false);
				showErrorModal(i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
				return;
			}
			if (requestId !== requestIdRef.current) {
				setIsProcessing(false);
				return;
			}
			setIsProcessing(false);
			ModalCommands.push(
				modal(() => (
					<AssetCropModal
						assetType={assetType}
						imageUrl={sourceBase64}
						onCropComplete={async (croppedBlob) => {
							if (requestId !== requestIdRef.current) return;
							setIsProcessing(true);
							try {
								if (croppedBlob.size > maxBytes) {
									const maxSizeLabel = formatFileSize(i18n.locale, maxBytes);
									showErrorModal(i18n._(CROPPED_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {maxSizeLabel}));
									return;
								}
								const croppedBase64 = await blobToDataUrl(croppedBlob);
								await applyDataUrl(croppedBase64, croppedBlob, requestId);
							} catch {
								showErrorModal(i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR));
							} finally {
								if (requestId === requestIdRef.current) {
									setIsProcessing(false);
								}
							}
						}}
						onSkip={
							isAnimatedAvif
								? undefined
								: async () => {
										if (requestId !== requestIdRef.current) return;
										await applyDataUrl(sourceBase64, file, requestId);
									}
						}
						data-flx="guild.guild-tabs.guild-overview-tab.use-guild-image-asset-field.handle-file.asset-crop-modal"
					/>
				)),
			);
		},
		[
			applyAspectRatio,
			applyDataUrl,
			assetType,
			canManage,
			gif,
			label,
			maxBytes,
			setFieldValue,
			setHasCleared,
			setPreviewUrl,
			showErrorModal,
			i18n,
		],
	);
	const pickFile = useCallback(async () => {
		if (!canManage) return;
		const [file] = await openFilePicker({accept: filePickerAccept});
		await handleFile(file ?? null);
	}, [canManage, filePickerAccept, handleFile]);
	const handleSelectedGif = useCallback(
		async (selectedGif: Gif) => {
			try {
				const file = await downloadGifAsImageFile(selectedGif);
				await handleFile(file);
			} catch {
				showErrorModal(i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
			}
		},
		[handleFile, i18n, showErrorModal],
	);
	const openSourcePicker = useCallback(() => {
		if (!canManage) return;
		const gifMode = gif?.mode ?? 'disallow';
		const gifAllowed = gifMode === 'allow' || (gifMode === 'require-feature' && (gif?.isAllowed?.() ?? false));
		if (sourceModalTitle == null) {
			void pickFile();
			return;
		}
		openAssetSourceModal({
			title: sourceModalTitle,
			uploadHint,
			onPickUpload: pickFile,
			onSelectGif: (selectedGif) => void handleSelectedGif(selectedGif),
			showGifOption: gifAllowed,
		});
	}, [canManage, gif, handleSelectedGif, pickFile, sourceModalTitle, uploadHint]);
	const clear = useCallback(() => {
		requestIdRef.current += 1;
		setFieldValue(null);
		setPreviewUrl(null);
		setHasCleared(true);
		if (aspectRatio) {
			aspectRatio.set(undefined);
		}
	}, [aspectRatio, setFieldValue, setHasCleared, setPreviewUrl]);
	return {pickFile, openSourcePicker, handleFile, clear, isProcessing};
}
