// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {BACKGROUND_MEDIA_MAX_SIZE_LABEL, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {GifPickerSelectModal} from '@app/features/expressions/components/modals/GifPickerSelectModal';
import {downloadGifAsVideoOrImageFile} from '@app/features/expressions/utils/GifFileDownload';
import {
	GET_PREMIUM_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import styles from '@app/features/theme/components/modals/BackgroundImageGalleryModal.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as BackgroundImageDB from '@app/features/theme/utils/BackgroundImageDB';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings, {BLUR_BACKGROUND_ID, NONE_BACKGROUND_ID} from '@app/features/voice/state/VoiceSettings';
import {areVoiceBackgroundsAvailable} from '@app/features/voice/utils/VoiceBackgroundAvailability';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {
	ArrowsClockwiseIcon,
	CheckIcon,
	CrownIcon,
	EyeSlashIcon,
	GifIcon,
	PlusIcon,
	SparkleIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const BACKGROUND_DESCRIPTOR = msg({
	message: 'Background',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const REMOVE_BACKGROUND_DESCRIPTOR = msg({
	message: 'Remove background',
	comment:
		'Button or menu action label in the background image gallery modal. Keep it concise. Keep the tone plain and specific.',
});
const NO_BACKGROUND_DESCRIPTOR = msg({
	message: 'No background',
	comment: 'Empty-state text in the background image gallery modal.',
});
const UNLOCK_MORE_BACKGROUNDS_DESCRIPTOR = msg({
	message: 'Unlock more backgrounds with {premiumProductName}',
	comment: 'Premium upsell title in the background gallery.',
});
const BLUR_DESCRIPTOR = msg({
	message: 'Blur',
	comment: 'Short label in the background image gallery modal. Keep it concise.',
});
const REPLACE_DESCRIPTOR = msg({
	message: 'Replace',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const UPLOAD_DESCRIPTOR = msg({
	message: 'Upload',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const UNSUPPORTED_FILE_FORMAT_PLEASE_USE_DESCRIPTOR = msg({
	message: 'Unsupported file format.',
	comment: 'Error message in the background image gallery modal.',
});
const BACKGROUND_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Background image is too large. Choose a file smaller than {backgroundMediaMaxSizeLabel}.',
	comment:
		'Error message in the background image gallery modal. Preserve {backgroundMediaMaxSizeLabel}; it is inserted by code.',
});
const BACKGROUND_IMAGE_REPLACED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Background image replaced successfully.',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const BACKGROUND_IMAGE_UPLOADED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Background image uploaded successfully.',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const FAILED_TO_UPLOAD_BACKGROUND_IMAGE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to upload background image. Try again.',
	comment: 'Error message in the background image gallery modal.',
});
const YOU_VE_REACHED_THE_MAXIMUM_OF_BACKGROUNDS_REMOVE_DESCRIPTOR = msg({
	message: "You've reached the maximum of {maxBackgroundImages} backgrounds. Remove one to add a new background.",
	comment:
		'Description text in the background image gallery modal. Preserve {maxBackgroundImages}; it is inserted by code. Keep the tone plain and specific.',
});
const REPLACE_BACKGROUND_DESCRIPTOR = msg({
	message: 'Replace background?',
	comment: 'Confirmation prompt in the background image gallery modal.',
});
const BACKGROUND_IMAGE_REMOVED_DESCRIPTOR = msg({
	message: 'Background image removed.',
	comment:
		'Button or menu action label in the background image gallery modal. Keep it concise. Keep the tone plain and specific.',
});
const FAILED_TO_REMOVE_BACKGROUND_IMAGE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to remove background image. Try again.',
	comment: 'Error message in the background image gallery modal. Keep the tone plain and specific.',
});
const CHOOSE_BACKGROUND_DESCRIPTOR = msg({
	message: 'Choose background',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const BACKGROUND_SELECTION_AREA_WITH_DRAG_AND_DROP_SUPPORT_DESCRIPTOR = msg({
	message: 'Background selection area with drag and drop support',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const REPLACE_BACKGROUND_2_DESCRIPTOR = msg({
	message: 'Replace background',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const UPLOAD_CUSTOM_BACKGROUND_DESCRIPTOR = msg({
	message: 'Upload custom background',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const CUSTOM_BACKGROUND_DESCRIPTOR = msg({
	message: '{backgroundCount} / {maxBackgroundImages} custom background',
	comment:
		'Label in the background image gallery modal. Preserve {backgroundCount}, {maxBackgroundImages}; they are inserted by code.',
});
const CUSTOM_BACKGROUNDS_DESCRIPTOR = msg({
	message: '{backgroundCount} / {maxBackgroundImages} custom backgrounds',
	comment:
		'Label in the background image gallery modal. Preserve {backgroundCount}, {maxBackgroundImages}; they are inserted by code.',
});
const CHOOSE_A_GIF_DESCRIPTOR = msg({
	message: 'Choose a GIF',
	comment: 'Title of the GIF picker modal opened from the background image gallery. Keep it concise.',
});
const OR_PICK_A_GIF_FROM_PROVIDER_DESCRIPTOR = msg({
	message: 'or pick a GIF from {gifProviderName}',
	comment:
		'Link-style button under the background upload placeholder. Preserve {gifProviderName}; it is inserted by code. Keep it concise.',
});
const REPLACE_WITH_A_GIF_DESCRIPTOR = msg({
	message: 'Replace with a GIF',
	comment: 'Button or menu action label in the background image gallery modal. Keep it concise.',
});
const logger = new Logger('BackgroundImageGalleryModal');

interface BackgroundImage {
	id: string;
	createdAt: number;
	mediaKind?: 'static' | 'animated' | 'video';
}

interface BuiltInBackground {
	id: string;
	type: 'none' | 'blur' | 'upload' | 'gif';
	name: string;
	icon: React.ComponentType<IconProps>;
}

type BackgroundItemType = BuiltInBackground | BackgroundImage;

const MAX_FILE_SIZE = 10 * 1024 * 1024;
const ALLOWED_MIME_TYPES = ['image/jpeg', 'image/png', 'image/gif', 'image/webp', 'video/mp4'];
const ALLOWED_FILE_EXTENSIONS = ['jpg', 'jpeg', 'png', 'gif', 'webp', 'mp4'];

function isAllowedBackgroundFile(file: File): boolean {
	if (ALLOWED_MIME_TYPES.includes(file.type)) {
		return true;
	}
	const extension = file.name.split('.').pop()?.toLowerCase() ?? '';
	return ALLOWED_FILE_EXTENSIONS.includes(extension);
}

interface BackgroundItemProps {
	background: BackgroundItemType;
	isSelected: boolean;
	onSelect: (background: BackgroundItemType) => void;
	onContextMenu?: (event: React.MouseEvent, background: BackgroundImage) => void;
	onDelete?: (background: BackgroundImage) => void;
}

const BackgroundItem: React.FC<BackgroundItemProps> = React.memo(
	({background, isSelected, onSelect, onContextMenu, onDelete}) => {
		const {i18n} = useLingui();
		const isBuiltIn = 'type' in background;
		const Icon = isBuiltIn ? background.icon : undefined;
		const [imageUrl, setImageUrl] = useState<string | null>(null);
		const [isLoading, setIsLoading] = useState(!isBuiltIn);
		const [hasError, setHasError] = useState(false);
		const objectUrlRef = useRef<string | null>(null);
		const loadRequestIdRef = useRef(0);
		const loadImage = useCallback(() => {
			if (isBuiltIn) return;
			const requestId = ++loadRequestIdRef.current;
			setIsLoading(true);
			setHasError(false);
			BackgroundImageDB.getBackgroundImageURL(background.id)
				.then((url) => {
					if (requestId !== loadRequestIdRef.current) {
						if (url) {
							URL.revokeObjectURL(url);
						}
						return;
					}
					if (objectUrlRef.current) {
						URL.revokeObjectURL(objectUrlRef.current);
					}
					objectUrlRef.current = url;
					setImageUrl(url);
					setIsLoading(false);
				})
				.catch((error) => {
					if (requestId !== loadRequestIdRef.current) return;
					logger.error('Failed to load background image:', error);
					setHasError(true);
					setIsLoading(false);
				});
		}, [isBuiltIn, background.id]);
		useEffect(() => {
			loadImage();
			return () => {
				loadRequestIdRef.current += 1;
				if (objectUrlRef.current) {
					URL.revokeObjectURL(objectUrlRef.current);
					objectUrlRef.current = null;
				}
			};
		}, [loadImage]);
		const handleClick = useCallback(() => {
			onSelect(background);
		}, [background, onSelect]);
		const handleKeyDown = useCallback(
			(e: React.KeyboardEvent) => {
				if (isKeyboardActivationKey(e.key)) {
					e.preventDefault();
					onSelect(background);
				}
			},
			[background, onSelect],
		);
		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!isBuiltIn) {
					onContextMenu?.(e, background as BackgroundImage);
				}
			},
			[isBuiltIn, background, onContextMenu],
		);
		const handleDelete = useCallback(
			(e: React.MouseEvent) => {
				e.preventDefault();
				e.stopPropagation();
				if (!isBuiltIn) {
					onDelete?.(background as BackgroundImage);
				}
			},
			[isBuiltIn, background, onDelete],
		);
		const handleRetry = useCallback(() => {
			loadImage();
		}, [loadImage]);
		return (
			<div
				className={styles.backgroundItem}
				style={{
					borderColor: isSelected ? 'var(--brand-primary)' : 'var(--background-modifier-accent)',
				}}
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				onContextMenu={handleContextMenu}
				role="button"
				tabIndex={0}
				aria-pressed={isSelected}
				data-flx="theme.background-image-gallery-modal.background-item.background-item.click"
			>
				{isBuiltIn ? (
					<div
						className={styles.backgroundItemContent}
						data-flx="theme.background-image-gallery-modal.background-item.background-item-content"
					>
						<div
							className={styles.backgroundItemInner}
							data-flx="theme.background-image-gallery-modal.background-item.background-item-inner"
						>
							{Icon && (
								<Icon
									size={remFromPx(24)}
									weight={isSelected ? 'fill' : 'regular'}
									className={styles.backgroundItemIcon}
									data-flx="theme.background-image-gallery-modal.background-item.background-item-icon"
								/>
							)}
							<div
								className={styles.backgroundItemText}
								data-flx="theme.background-image-gallery-modal.background-item.background-item-text"
							>
								<div
									className={styles.backgroundItemName}
									data-flx="theme.background-image-gallery-modal.background-item.background-item-name"
								>
									{background.name}
								</div>
							</div>
						</div>
					</div>
				) : (
					<>
						{isLoading ? (
							<div
								className={styles.loadingContainer}
								data-flx="theme.background-image-gallery-modal.background-item.loading-container"
							>
								<div
									className={styles.spinner}
									data-flx="theme.background-image-gallery-modal.background-item.spinner"
								/>
							</div>
						) : hasError ? (
							<div
								className={styles.errorContainer}
								data-flx="theme.background-image-gallery-modal.background-item.error-container"
							>
								<WarningCircleIcon
									size={remFromPx(24)}
									weight="fill"
									className={styles.errorIcon}
									data-flx="theme.background-image-gallery-modal.background-item.error-icon"
								/>
								<div
									className={styles.errorText}
									data-flx="theme.background-image-gallery-modal.background-item.error-text"
								>
									<Trans>Failed to load</Trans>
								</div>
								<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.background-item.focus-ring">
									<button
										type="button"
										onClick={(e) => {
											e.stopPropagation();
											handleRetry();
										}}
										className={styles.errorButton}
										data-flx="theme.background-image-gallery-modal.background-item.error-button.stop-propagation"
									>
										{i18n._(TRY_AGAIN_DESCRIPTOR)}
									</button>
								</FocusRing>
							</div>
						) : imageUrl && background.mediaKind === 'video' ? (
							<video
								src={imageUrl}
								className={styles.backgroundImage}
								muted
								loop
								playsInline
								autoPlay
								data-flx="theme.background-image-gallery-modal.background-item.background-video"
							/>
						) : imageUrl ? (
							<img
								src={imageUrl}
								alt={i18n._(BACKGROUND_DESCRIPTOR)}
								className={styles.backgroundImage}
								data-flx="theme.background-image-gallery-modal.background-item.background-image"
							/>
						) : null}
						<div
							className={styles.imageOverlay}
							data-flx="theme.background-image-gallery-modal.background-item.image-overlay"
						/>
						{!isBuiltIn && onDelete && !isLoading && !hasError && (
							<Tooltip
								text={i18n._(REMOVE_BACKGROUND_DESCRIPTOR)}
								data-flx="theme.background-image-gallery-modal.background-item.tooltip"
							>
								<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.background-item.focus-ring--2">
									<button
										type="button"
										onClick={handleDelete}
										className={styles.deleteButton}
										aria-label={i18n._(REMOVE_BACKGROUND_DESCRIPTOR)}
										data-flx="theme.background-image-gallery-modal.background-item.delete-button"
									>
										<TrashIcon
											size={remFromPx(16)}
											weight="bold"
											className={styles.deleteButtonIcon}
											data-flx="theme.background-image-gallery-modal.background-item.delete-button-icon"
										/>
									</button>
								</FocusRing>
							</Tooltip>
						)}
					</>
				)}
				{isSelected && (
					<div
						className={styles.selectedBadge}
						data-flx="theme.background-image-gallery-modal.background-item.selected-badge"
					>
						<CheckIcon
							size={remFromPx(16)}
							weight="bold"
							className={styles.selectedIcon}
							data-flx="theme.background-image-gallery-modal.background-item.selected-icon"
						/>
					</div>
				)}
			</div>
		);
	},
);

BackgroundItem.displayName = 'BackgroundItem';

const BackgroundImageGalleryModal: React.FC = observer(() => {
	const {i18n} = useLingui();
	const voiceBackgroundsAvailable = areVoiceBackgroundsAvailable();
	const voiceSettings = VoiceSettings;
	const {backgroundImageId, backgroundImages = []} = voiceSettings;
	const isMountedRef = useRef(true);
	const [isDragging, setIsDragging] = useState(false);
	const dragCounterRef = useRef(0);
	const maxBackgroundImages = useMemo(() => LimitResolver.resolve({key: 'max_custom_backgrounds', fallback: 1}), []);
	const canAddMoreImages = backgroundImages.length < maxBackgroundImages;
	const backgroundCount = backgroundImages.length;
	const shouldShowReplace = maxBackgroundImages === 1 && backgroundImages.length >= 1;
	const gifProviderName = RuntimeConfig.gifProviderDisplayName;
	const builtInBackgrounds = useMemo(
		(): ReadonlyArray<BuiltInBackground> => [
			{
				id: NONE_BACKGROUND_ID,
				type: 'none',
				name: i18n._(NO_BACKGROUND_DESCRIPTOR),
				icon: EyeSlashIcon,
			},
			{
				id: BLUR_BACKGROUND_ID,
				type: 'blur',
				name: i18n._(BLUR_DESCRIPTOR),
				icon: SparkleIcon,
			},
			{
				id: 'upload',
				type: 'upload',
				name: shouldShowReplace ? i18n._(REPLACE_DESCRIPTOR) : i18n._(UPLOAD_DESCRIPTOR),
				icon: PlusIcon,
			},
			{
				id: 'gif-picker',
				type: 'gif',
				name: gifProviderName,
				icon: GifIcon,
			},
		],
		[gifProviderName, i18n.locale, shouldShowReplace],
	);
	const sortedImages = useMemo(
		() => [...backgroundImages].sort((a, b) => b.createdAt - a.createdAt),
		[backgroundImages],
	);
	useEffect(() => {
		return () => {
			isMountedRef.current = false;
		};
	}, []);
	useEffect(() => {
		if (!voiceBackgroundsAvailable) {
			ModalCommands.popByType(BackgroundImageGalleryModal);
		}
	}, [voiceBackgroundsAvailable]);
	const processFileUpload = useCallback(
		async (file: File | null) => {
			if (!file) return;
			try {
				if (!isAllowedBackgroundFile(file)) {
					showGenericErrorModal({
						title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
						message: () => i18n._(UNSUPPORTED_FILE_FORMAT_PLEASE_USE_DESCRIPTOR),
						dataFlx: 'theme.background-image-gallery-modal.unsupported-file-error-modal',
					});
					return;
				}
				if (file.size > MAX_FILE_SIZE) {
					showGenericErrorModal({
						title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
						message: () =>
							i18n._(BACKGROUND_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
								backgroundMediaMaxSizeLabel: BACKGROUND_MEDIA_MAX_SIZE_LABEL,
							}),
						dataFlx: 'theme.background-image-gallery-modal.file-too-large-error-modal',
					});
					return;
				}
				const createdAt = Date.now();
				const newImageId = `${createdAt}-${Math.random().toString(36).slice(2, 9)}`;
				const savedMedia = await BackgroundImageDB.saveBackgroundImage(newImageId, file);
				const newImage: BackgroundImage = {
					id: newImageId,
					createdAt,
					mediaKind: savedMedia.mediaKind,
				};
				if (isMountedRef.current) {
					const updatedImages = [...backgroundImages].sort((a, b) => a.createdAt - b.createdAt);
					const imageIdsToDelete: Array<string> = [];
					while (updatedImages.length >= maxBackgroundImages && updatedImages.length > 0) {
						const oldestImage = updatedImages.shift();
						if (!oldestImage) break;
						imageIdsToDelete.push(oldestImage.id);
					}
					updatedImages.push(newImage);
					VoiceSettingsCommands.update({
						backgroundImages: updatedImages,
						backgroundImageId: newImage.id,
					});
					for (const imageId of imageIdsToDelete) {
						BackgroundImageDB.deleteBackgroundImage(imageId).catch((error) => {
							logger.error('Failed to delete old background image:', error);
						});
					}
					ToastCommands.createToast({
						type: 'success',
						children:
							imageIdsToDelete.length > 0
								? i18n._(BACKGROUND_IMAGE_REPLACED_SUCCESSFULLY_DESCRIPTOR)
								: i18n._(BACKGROUND_IMAGE_UPLOADED_SUCCESSFULLY_DESCRIPTOR),
					});
					ModalCommands.popByType(BackgroundImageGalleryModal);
				}
			} catch (error) {
				logger.error('File upload failed:', error);
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(FAILED_TO_UPLOAD_BACKGROUND_IMAGE_PLEASE_TRY_AGAIN_DESCRIPTOR),
					dataFlx: 'theme.background-image-gallery-modal.upload-error-modal',
				});
			}
		},
		[backgroundImages, maxBackgroundImages],
	);
	const confirmReplaceThen = useCallback(
		(showReplaceWarning: boolean, action: () => void) => {
			if (showReplaceWarning && maxBackgroundImages === 1 && backgroundImages.length >= maxBackgroundImages) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(REPLACE_BACKGROUND_DESCRIPTOR)}
							description={
								<Trans>
									You can only have one custom background on the free tier. Picking a new one will replace your existing
									background.
								</Trans>
							}
							primaryText={i18n._(REPLACE_DESCRIPTOR)}
							primaryVariant="primary"
							onPrimary={action}
							data-flx="theme.background-image-gallery-modal.handle-upload-click.confirm-modal"
						/>
					)),
				);
				return;
			}
			if (!canAddMoreImages) {
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(YOU_VE_REACHED_THE_MAXIMUM_OF_BACKGROUNDS_REMOVE_DESCRIPTOR, {maxBackgroundImages}),
					dataFlx: 'theme.background-image-gallery-modal.max-backgrounds-error-modal',
				});
				return;
			}
			action();
		},
		[canAddMoreImages, maxBackgroundImages, backgroundImages.length],
	);
	const handleUploadClick = useCallback(
		(showReplaceWarning: boolean = false) => {
			const pickAndProcess = async () => {
				const [file] = await openFilePicker({
					accept: [...ALLOWED_MIME_TYPES, ...ALLOWED_FILE_EXTENSIONS.map((extension) => `.${extension}`)].join(','),
				});
				await processFileUpload(file ?? null);
			};
			confirmReplaceThen(showReplaceWarning, () => {
				void pickAndProcess();
			});
		},
		[confirmReplaceThen, processFileUpload],
	);
	const handlePickGifClick = useCallback(
		(showReplaceWarning: boolean = false) => {
			confirmReplaceThen(showReplaceWarning, () => {
				ModalCommands.push(
					modal(() => (
						<GifPickerSelectModal
							title={i18n._(CHOOSE_A_GIF_DESCRIPTOR)}
							onSelect={(gif) => {
								void (async () => {
									try {
										const file = await downloadGifAsVideoOrImageFile(gif);
										await processFileUpload(file);
									} catch (error) {
										logger.error('Failed to download GIF background:', error);
										showGenericErrorModal({
											title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
											message: () => i18n._(FAILED_TO_UPLOAD_BACKGROUND_IMAGE_PLEASE_TRY_AGAIN_DESCRIPTOR),
											dataFlx: 'theme.background-image-gallery-modal.gif-download-error-modal',
										});
									}
								})();
							}}
							data-flx="theme.background-image-gallery-modal.handle-pick-gif-click.gif-picker-select-modal"
						/>
					)),
				);
			});
		},
		[confirmReplaceThen, processFileUpload],
	);
	const handleBackgroundSelect = useCallback(
		(background: BackgroundItemType) => {
			if ('type' in background) {
				if (background.type === 'upload') {
					handleUploadClick(true);
					return;
				}
				if (background.type === 'gif') {
					handlePickGifClick(true);
					return;
				}
				VoiceSettingsCommands.update({
					backgroundImageId: background.id,
				});
			} else {
				VoiceSettingsCommands.update({
					backgroundImageId: background.id,
				});
			}
			ModalCommands.pop();
		},
		[handlePickGifClick, handleUploadClick],
	);
	const handleRemoveImage = useCallback(
		async (image: BackgroundImage) => {
			try {
				await BackgroundImageDB.deleteBackgroundImage(image.id);
				const updatedImages = backgroundImages.filter((img) => img.id !== image.id);
				const updates: {backgroundImages: Array<BackgroundImage>; backgroundImageId?: string} = {
					backgroundImages: updatedImages,
				};
				if (backgroundImageId === image.id) {
					updates.backgroundImageId = NONE_BACKGROUND_ID;
				}
				VoiceSettingsCommands.update(updates);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(BACKGROUND_IMAGE_REMOVED_DESCRIPTOR),
				});
			} catch (error) {
				logger.error('Failed to delete background image:', error);
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(FAILED_TO_REMOVE_BACKGROUND_IMAGE_PLEASE_TRY_AGAIN_DESCRIPTOR),
					dataFlx: 'theme.background-image-gallery-modal.remove-image-error-modal',
				});
			}
		},
		[backgroundImageId, backgroundImages],
	);
	const handleBackgroundContextMenu = useCallback(
		(event: React.MouseEvent, image: BackgroundImage) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<div data-flx="theme.background-image-gallery-modal.handle-background-context-menu.div">
					<MenuItem
						danger
						onClick={() => {
							handleRemoveImage(image);
							onClose();
						}}
						data-flx="theme.background-image-gallery-modal.handle-background-context-menu.menu-item.remove-image"
					>
						{i18n._(REMOVE_BACKGROUND_DESCRIPTOR)}
					</MenuItem>
				</div>
			));
		},
		[handleRemoveImage],
	);
	const handleDrop = useCallback(
		(e: React.DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
			setIsDragging(false);
			dragCounterRef.current = 0;
			const file = e.dataTransfer.files?.[0];
			if (!file) return;
			confirmReplaceThen(true, () => {
				void processFileUpload(file);
			});
		},
		[confirmReplaceThen, processFileUpload],
	);
	const handleDragEnter = useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
		dragCounterRef.current++;
		if (e.dataTransfer.items && e.dataTransfer.items.length > 0) {
			setIsDragging(true);
		}
	}, []);
	const handleDragLeave = useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
		dragCounterRef.current--;
		if (dragCounterRef.current === 0) {
			setIsDragging(false);
		}
	}, []);
	const handleDragOver = useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
	}, []);
	if (!voiceBackgroundsAvailable) {
		return null;
	}
	return (
		<Modal.Root size="medium" data-flx="theme.background-image-gallery-modal.modal-root">
			<Modal.Header
				title={i18n._(CHOOSE_BACKGROUND_DESCRIPTOR)}
				data-flx="theme.background-image-gallery-modal.modal-header"
			/>
			<Modal.Content data-flx="theme.background-image-gallery-modal.modal-content">
				<section
					className={styles.selectionSection}
					onDrop={handleDrop}
					onDragEnter={handleDragEnter}
					onDragLeave={handleDragLeave}
					onDragOver={handleDragOver}
					aria-label={i18n._(BACKGROUND_SELECTION_AREA_WITH_DRAG_AND_DROP_SUPPORT_DESCRIPTOR)}
					data-flx="theme.background-image-gallery-modal.selection-section"
				>
					{isDragging && (
						<div className={styles.dragOverlay} data-flx="theme.background-image-gallery-modal.drag-overlay">
							<div className={styles.dragContent} data-flx="theme.background-image-gallery-modal.drag-content">
								<PlusIcon
									size={remFromPx(48)}
									weight="bold"
									className={styles.dragIcon}
									data-flx="theme.background-image-gallery-modal.drag-icon"
								/>
								<div className={styles.dragText} data-flx="theme.background-image-gallery-modal.drag-text">
									<Trans>Drop to upload background</Trans>
								</div>
							</div>
						</div>
					)}
					{maxBackgroundImages === 1 ? (
						<div
							className={styles.freeUserContainer}
							data-flx="theme.background-image-gallery-modal.free-user-container"
						>
							{sortedImages.length > 0 ? (
								<div
									className={styles.customBackgroundWrapper}
									data-flx="theme.background-image-gallery-modal.custom-background-wrapper"
								>
									<BackgroundItem
										key={sortedImages[0].id}
										background={sortedImages[0]}
										isSelected={backgroundImageId === sortedImages[0].id}
										onSelect={handleBackgroundSelect}
										onDelete={undefined}
										data-flx="theme.background-image-gallery-modal.background-item.background-select"
									/>
									<div className={styles.actionButtons} data-flx="theme.background-image-gallery-modal.action-buttons">
										<Tooltip
											text={i18n._(REPLACE_BACKGROUND_2_DESCRIPTOR)}
											data-flx="theme.background-image-gallery-modal.tooltip"
										>
											<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.focus-ring">
												<button
													type="button"
													onClick={(e) => {
														e.stopPropagation();
														handleUploadClick(true);
													}}
													className={styles.actionButton}
													aria-label={i18n._(REPLACE_BACKGROUND_2_DESCRIPTOR)}
													data-flx="theme.background-image-gallery-modal.action-button.stop-propagation"
												>
													<ArrowsClockwiseIcon
														size={remFromPx(16)}
														weight="bold"
														className={styles.actionButtonIcon}
														data-flx="theme.background-image-gallery-modal.action-button-icon"
													/>
												</button>
											</FocusRing>
										</Tooltip>
										<Tooltip
											text={i18n._(REPLACE_WITH_A_GIF_DESCRIPTOR)}
											data-flx="theme.background-image-gallery-modal.tooltip--gif"
										>
											<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.focus-ring--gif-replace">
												<button
													type="button"
													onClick={(e) => {
														e.stopPropagation();
														handlePickGifClick(true);
													}}
													className={styles.actionButton}
													aria-label={i18n._(REPLACE_WITH_A_GIF_DESCRIPTOR)}
													data-flx="theme.background-image-gallery-modal.action-button.pick-gif-click"
												>
													<GifIcon
														size={remFromPx(16)}
														weight="bold"
														className={styles.actionButtonIcon}
														data-flx="theme.background-image-gallery-modal.action-button-icon--gif"
													/>
												</button>
											</FocusRing>
										</Tooltip>
										<Tooltip
											text={i18n._(REMOVE_BACKGROUND_DESCRIPTOR)}
											data-flx="theme.background-image-gallery-modal.tooltip--2"
										>
											<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.focus-ring--2">
												<button
													type="button"
													onClick={(e) => {
														e.stopPropagation();
														handleRemoveImage(sortedImages[0]);
													}}
													className={styles.actionButton}
													aria-label={i18n._(REMOVE_BACKGROUND_DESCRIPTOR)}
													data-flx="theme.background-image-gallery-modal.action-button.stop-propagation--2"
												>
													<TrashIcon
														size={remFromPx(16)}
														weight="bold"
														className={styles.actionButtonIcon}
														data-flx="theme.background-image-gallery-modal.action-button-icon--2"
													/>
												</button>
											</FocusRing>
										</Tooltip>
									</div>
								</div>
							) : (
								<div
									className={styles.uploadPlaceholder}
									onClick={() => handleUploadClick(false)}
									onKeyDown={(e) => {
										if (isKeyboardActivationKey(e.key)) {
											e.preventDefault();
											handleUploadClick(false);
										}
									}}
									role="button"
									tabIndex={0}
									aria-label={i18n._(UPLOAD_CUSTOM_BACKGROUND_DESCRIPTOR)}
									data-flx="theme.background-image-gallery-modal.upload-placeholder.upload-click"
								>
									<div
										className={styles.uploadPlaceholderContent}
										data-flx="theme.background-image-gallery-modal.upload-placeholder-content"
									>
										<PlusIcon
											size={remFromPx(48)}
											weight="regular"
											className={styles.uploadIcon}
											data-flx="theme.background-image-gallery-modal.upload-icon"
										/>
										<div
											className={styles.uploadTextContainer}
											data-flx="theme.background-image-gallery-modal.upload-text-container"
										>
											<div className={styles.uploadTitle} data-flx="theme.background-image-gallery-modal.upload-title">
												<Trans>Upload custom background</Trans>
											</div>
											<FocusRing offset={-2} data-flx="theme.background-image-gallery-modal.focus-ring--gif">
												<button
													type="button"
													className={styles.gifLinkButton}
													onClick={(e) => {
														e.stopPropagation();
														handlePickGifClick(false);
													}}
													data-flx="theme.background-image-gallery-modal.gif-link-button.pick-gif-click"
												>
													{i18n._(OR_PICK_A_GIF_FROM_PROVIDER_DESCRIPTOR, {
														gifProviderName: RuntimeConfig.gifProviderDisplayName,
													})}
												</button>
											</FocusRing>
										</div>
									</div>
								</div>
							)}
							<div className={styles.builtInGrid} data-flx="theme.background-image-gallery-modal.built-in-grid">
								{builtInBackgrounds
									.filter((bg) => bg.type !== 'upload' && bg.type !== 'gif')
									.map((background) => (
										<BackgroundItem
											key={background.id}
											background={background}
											isSelected={backgroundImageId === background.id}
											onSelect={handleBackgroundSelect}
											data-flx="theme.background-image-gallery-modal.background-item.background-select--2"
										/>
									))}
							</div>
						</div>
					) : (
						<div className={styles.premiumGrid} data-flx="theme.background-image-gallery-modal.premium-grid">
							{builtInBackgrounds.map((background) => (
								<BackgroundItem
									key={background.id}
									background={background}
									isSelected={backgroundImageId === background.id}
									onSelect={handleBackgroundSelect}
									data-flx="theme.background-image-gallery-modal.background-item.background-select--3"
								/>
							))}
							{sortedImages.map((image) => (
								<BackgroundItem
									key={image.id}
									background={image}
									isSelected={backgroundImageId === image.id}
									onSelect={handleBackgroundSelect}
									onContextMenu={handleBackgroundContextMenu}
									onDelete={handleRemoveImage}
									data-flx="theme.background-image-gallery-modal.background-item.background-select--4"
								/>
							))}
						</div>
					)}
					<div className={styles.statsText} data-flx="theme.background-image-gallery-modal.stats-text">
						{backgroundCount === 1
							? i18n._(CUSTOM_BACKGROUND_DESCRIPTOR, {backgroundCount, maxBackgroundImages})
							: i18n._(CUSTOM_BACKGROUNDS_DESCRIPTOR, {backgroundCount, maxBackgroundImages})}
					</div>
					<div className={styles.infoText} data-flx="theme.background-image-gallery-modal.info-text">
						<Trans>Max size: {BACKGROUND_MEDIA_MAX_SIZE_LABEL}.</Trans>
					</div>
					{maxBackgroundImages === 1 && shouldShowPremiumFeatures() && (
						<div className={styles.premiumUpsell} data-flx="theme.background-image-gallery-modal.premium-upsell">
							<div className={styles.premiumHeader} data-flx="theme.background-image-gallery-modal.premium-header">
								<CrownIcon
									weight="fill"
									size={remFromPx(18)}
									className={styles.premiumIcon}
									data-flx="theme.background-image-gallery-modal.premium-icon"
								/>
								<span className={styles.premiumTitle} data-flx="theme.background-image-gallery-modal.premium-title">
									{i18n._(UNLOCK_MORE_BACKGROUNDS_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
								</span>
							</div>
							<p className={styles.premiumDesc} data-flx="theme.background-image-gallery-modal.premium-desc">
								<Trans>
									Upgrade to store up to 15 custom backgrounds and unlock HD video quality, higher frame rates, and
									more.
								</Trans>
							</p>
							<Button
								variant="secondary"
								small={true}
								onClick={() => PremiumModalCommands.open()}
								data-flx="theme.background-image-gallery-modal.button.open"
							>
								{i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
							</Button>
						</div>
					)}
				</section>
			</Modal.Content>
		</Modal.Root>
	);
});

export default BackgroundImageGalleryModal;
