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
import {
	ArrowsClockwiseIcon,
	CheckIcon,
	CrownIcon,
	EyeSlashIcon,
	PlusIcon,
	SparkleIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import styles from '~/components/modals/BackgroundImageGalleryModal.module.css';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import UserStore from '~/stores/UserStore';
import VoiceSettingsStore, {BLUR_BACKGROUND_ID, NONE_BACKGROUND_ID} from '~/stores/VoiceSettingsStore';
import * as BackgroundImageDB from '~/utils/BackgroundImageDB';
import {openFilePicker} from '~/utils/FilePickerUtils';

interface BackgroundImage {
	id: string;
	createdAt: number;
}

interface BuiltInBackground {
	id: string;
	type: 'none' | 'blur' | 'upload';
	name: string;
	icon: React.ComponentType<any>;
	description: string;
}

type BackgroundItemType = BuiltInBackground | BackgroundImage;

const getBuiltInBackgrounds = (isReplace: boolean): ReadonlyArray<BuiltInBackground> => {
	const {t} = useLingui();
	return [
		{
			id: NONE_BACKGROUND_ID,
			type: 'none',
			name: t`No Background`,
			icon: EyeSlashIcon,
			description: t`Show your actual background`,
		},
		{
			id: BLUR_BACKGROUND_ID,
			type: 'blur',
			name: t`Blur`,
			icon: SparkleIcon,
			description: t`Blur your background`,
		},
		{
			id: 'upload',
			type: 'upload',
			name: isReplace ? t`Replace` : t`Upload`,
			icon: PlusIcon,
			description: isReplace ? t`Replace your custom background` : t`Add a custom background`,
		},
	];
};

const MAX_FILE_SIZE = 10 * 1024 * 1024;
const ALLOWED_MIME_TYPES = ['image/jpeg', 'image/png', 'image/gif', 'image/webp', 'video/mp4'];

interface BackgroundItemProps {
	background: BackgroundItemType;
	isSelected: boolean;
	onSelect: (background: BackgroundItemType) => void;
	onContextMenu?: (event: React.MouseEvent, background: BackgroundImage) => void;
	onDelete?: (background: BackgroundImage) => void;
}

const BackgroundItem: React.FC<BackgroundItemProps> = React.memo(
	({background, isSelected, onSelect, onContextMenu, onDelete}) => {
		const {t} = useLingui();
		const isBuiltIn = 'type' in background;
		const Icon = isBuiltIn ? background.icon : undefined;
		const [imageUrl, setImageUrl] = React.useState<string | null>(null);
		const [isLoading, setIsLoading] = React.useState(!isBuiltIn);
		const [hasError, setHasError] = React.useState(false);

		React.useEffect(() => {
			if (isBuiltIn) return;
			let objectUrl: string | null = null;
			setIsLoading(true);
			setHasError(false);
			BackgroundImageDB.getBackgroundImageURL(background.id)
				.then((url) => {
					objectUrl = url;
					setImageUrl(url);
					setIsLoading(false);
				})
				.catch((error) => {
					console.error('Failed to load background image:', error);
					setHasError(true);
					setIsLoading(false);
				});

			return () => {
				if (objectUrl) {
					URL.revokeObjectURL(objectUrl);
				}
			};
		}, [isBuiltIn, background.id]);

		const handleClick = React.useCallback(() => {
			onSelect(background);
		}, [background, onSelect]);

		const handleKeyDown = React.useCallback(
			(e: React.KeyboardEvent) => {
				if (e.key === 'Enter' || e.key === ' ') {
					e.preventDefault();
					onSelect(background);
				}
			},
			[background, onSelect],
		);

		const handleContextMenu = React.useCallback(
			(e: React.MouseEvent) => {
				if (!isBuiltIn) {
					onContextMenu?.(e, background as BackgroundImage);
				}
			},
			[isBuiltIn, background, onContextMenu],
		);

		const handleDelete = React.useCallback(
			(e: React.MouseEvent) => {
				e.preventDefault();
				e.stopPropagation();
				if (!isBuiltIn) {
					onDelete?.(background as BackgroundImage);
				}
			},
			[isBuiltIn, background, onDelete],
		);

		const handleRetry = React.useCallback(() => {
			setHasError(false);
			setIsLoading(true);
			BackgroundImageDB.getBackgroundImageURL(background.id)
				.then((url) => {
					setImageUrl(url);
					setIsLoading(false);
				})
				.catch((error) => {
					console.error('Failed to load background image:', error);
					setHasError(true);
					setIsLoading(false);
				});
		}, [background.id]);

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
			>
				{isBuiltIn ? (
					<div className={styles.backgroundItemContent}>
						<div className={styles.backgroundItemInner}>
							{Icon && (
								<Icon size={24} weight={isSelected ? 'fill' : 'regular'} className={styles.backgroundItemIcon} />
							)}
							<div className={styles.backgroundItemText}>
								<div className={styles.backgroundItemName}>{background.name}</div>
								<div className={styles.backgroundItemDesc}>{background.description}</div>
							</div>
						</div>
					</div>
				) : (
					<>
						{isLoading ? (
							<div className={styles.loadingContainer}>
								<div className={styles.spinner} />
							</div>
						) : hasError ? (
							<div className={styles.errorContainer}>
								<WarningCircleIcon size={24} weight="fill" className={styles.errorIcon} />
								<div className={styles.errorText}>
									<Trans>Failed to load</Trans>
								</div>
								<FocusRing offset={-2}>
									<button
										type="button"
										onClick={(e) => {
											e.stopPropagation();
											handleRetry();
										}}
										className={styles.errorButton}
									>
										<Trans>Retry</Trans>
									</button>
								</FocusRing>
							</div>
						) : imageUrl ? (
							<img src={imageUrl} alt="Background" className={styles.backgroundImage} />
						) : null}
						<div className={styles.imageOverlay} />
						{!isBuiltIn && onDelete && !isLoading && !hasError && (
							<Tooltip text={t`Remove background`}>
								<FocusRing offset={-2}>
									<button
										type="button"
										onClick={handleDelete}
										className={styles.deleteButton}
										aria-label={t`Remove background`}
									>
										<TrashIcon size={16} weight="bold" className={styles.deleteButtonIcon} />
									</button>
								</FocusRing>
							</Tooltip>
						)}
					</>
				)}

				{isSelected && (
					<div className={styles.selectedBadge}>
						<CheckIcon size={16} weight="bold" className={styles.selectedIcon} />
					</div>
				)}
			</div>
		);
	},
);

BackgroundItem.displayName = 'BackgroundItem';

const BackgroundImageGalleryModal: React.FC = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const voiceSettings = VoiceSettingsStore;
	const {backgroundImageId, backgroundImages = []} = voiceSettings;

	const isMountedRef = React.useRef(true);
	const [isDragging, setIsDragging] = React.useState(false);
	const dragCounterRef = React.useRef(0);

	const hasPremium = React.useMemo(() => user?.isPremium?.() ?? false, [user]);
	const maxBackgroundImages = hasPremium ? 15 : 1;
	const canAddMoreImages = backgroundImages.length < maxBackgroundImages;
	const backgroundCount = backgroundImages.length;

	const shouldShowReplace = !hasPremium && backgroundImages.length >= 1;
	const builtInBackgrounds = React.useMemo(() => getBuiltInBackgrounds(shouldShowReplace), [shouldShowReplace]);

	const sortedImages = React.useMemo(
		() => [...backgroundImages].sort((a, b) => b.createdAt - a.createdAt),
		[backgroundImages],
	);

	React.useEffect(() => {
		return () => {
			isMountedRef.current = false;
		};
	}, []);

	const processFileUpload = React.useCallback(
		async (file: File | null) => {
			if (!file) return;

			try {
				if (!ALLOWED_MIME_TYPES.includes(file.type)) {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Unsupported file format. Please use JPG, PNG, GIF, WebP, or MP4.`,
					});
					return;
				}

				if (file.size > MAX_FILE_SIZE) {
					ToastActionCreators.createToast({
						type: 'error',
						children: t`Background image is too large. Please choose a file smaller than 10MB.`,
					});
					return;
				}

				const newImage: BackgroundImage = {
					id: `${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
					createdAt: Date.now(),
				};

				await BackgroundImageDB.saveBackgroundImage(newImage.id, file);

				if (isMountedRef.current) {
					let updatedImages = [...backgroundImages];
					let oldImageToDelete: string | null = null;

					if (!hasPremium && backgroundImages.length >= 1) {
						const oldImage = backgroundImages[0];
						oldImageToDelete = oldImage.id;
						updatedImages = [];
					}

					updatedImages.push(newImage);

					VoiceSettingsActionCreators.update({
						backgroundImages: updatedImages,
						backgroundImageId: newImage.id,
					});

					if (oldImageToDelete) {
						BackgroundImageDB.deleteBackgroundImage(oldImageToDelete).catch((error) => {
							console.error('Failed to delete old background image:', error);
						});
					}

					ToastActionCreators.createToast({
						type: 'success',
						children: oldImageToDelete
							? t`Background image replaced successfully.`
							: t`Background image uploaded successfully.`,
					});

					ModalActionCreators.pop();
				}
			} catch (error) {
				console.error('File upload failed:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to upload background image. Please try again.`,
				});
			}
		},
		[backgroundImages, hasPremium],
	);

	const handleUploadClick = React.useCallback(
		(showReplaceWarning: boolean = false) => {
			if (!canAddMoreImages && hasPremium) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`You've reached the maximum of ${maxBackgroundImages} backgrounds. Remove one to add a new background.`,
				});
				return;
			}

			const pickAndProcess = async () => {
				const [file] = await openFilePicker({accept: ALLOWED_MIME_TYPES.join(',')});
				await processFileUpload(file ?? null);
			};

			if (showReplaceWarning && !hasPremium && backgroundImages.length >= 1) {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Replace Background?`}
							description={
								<Trans>
									You can only have one custom background on the free tier. Uploading a new one will replace your
									existing background.
								</Trans>
							}
							primaryText={t`Replace`}
							primaryVariant="primary"
							onPrimary={pickAndProcess}
						/>
					)),
				);
				return;
			}

			void pickAndProcess();
		},
		[canAddMoreImages, hasPremium, maxBackgroundImages, backgroundImages.length, processFileUpload],
	);

	const handleBackgroundSelect = React.useCallback(
		(background: BackgroundItemType) => {
			if ('type' in background) {
				if (background.type === 'upload') {
					handleUploadClick(true);
					return;
				}

				VoiceSettingsActionCreators.update({
					backgroundImageId: background.id,
				});
			} else {
				VoiceSettingsActionCreators.update({
					backgroundImageId: background.id,
				});
			}

			ModalActionCreators.pop();
		},
		[handleUploadClick],
	);

	const handleRemoveImage = React.useCallback(
		async (image: BackgroundImage) => {
			try {
				await BackgroundImageDB.deleteBackgroundImage(image.id);

				const updatedImages = backgroundImages.filter((img) => img.id !== image.id);

				const updates: any = {
					backgroundImages: updatedImages,
				};

				if (backgroundImageId === image.id) {
					updates.backgroundImageId = NONE_BACKGROUND_ID;
				}

				VoiceSettingsActionCreators.update(updates);

				ToastActionCreators.createToast({
					type: 'success',
					children: t`Background image removed.`,
				});
			} catch (error) {
				console.error('Failed to delete background image:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to remove background image. Please try again.`,
				});
			}
		},
		[backgroundImageId, backgroundImages],
	);

	const handleBackgroundContextMenu = React.useCallback(
		(event: React.MouseEvent, image: BackgroundImage) => {
			event.preventDefault();
			event.stopPropagation();

			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<div>
					<MenuItem
						danger
						onClick={() => {
							handleRemoveImage(image);
							onClose();
						}}
					>
						{t`Remove Background`}
					</MenuItem>
				</div>
			));
		},
		[handleRemoveImage],
	);

	const handleDrop = React.useCallback(
		async (e: React.DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
			setIsDragging(false);
			dragCounterRef.current = 0;

			const file = e.dataTransfer.files?.[0];
			if (!file) return;
			await processFileUpload(file);
		},
		[processFileUpload],
	);

	const handleDragEnter = React.useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
		dragCounterRef.current++;
		if (e.dataTransfer.items && e.dataTransfer.items.length > 0) {
			setIsDragging(true);
		}
	}, []);

	const handleDragLeave = React.useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
		dragCounterRef.current--;
		if (dragCounterRef.current === 0) {
			setIsDragging(false);
		}
	}, []);

	const handleDragOver = React.useCallback((e: React.DragEvent) => {
		e.preventDefault();
		e.stopPropagation();
	}, []);

	return (
		<Modal.Root size="medium">
			<Modal.Header title={t`Choose Background`} />
			<Modal.Content>
				<section
					className={styles.selectionSection}
					onDrop={handleDrop}
					onDragEnter={handleDragEnter}
					onDragLeave={handleDragLeave}
					onDragOver={handleDragOver}
					aria-label={t`Background selection area with drag and drop support`}
				>
					{isDragging && (
						<div className={styles.dragOverlay}>
							<div className={styles.dragContent}>
								<PlusIcon size={48} weight="bold" className={styles.dragIcon} />
								<div className={styles.dragText}>
									<Trans>Drop to upload background</Trans>
								</div>
							</div>
						</div>
					)}
					{!hasPremium ? (
						<div className={styles.freeUserContainer}>
							{sortedImages.length > 0 ? (
								<div className={styles.customBackgroundWrapper}>
									<BackgroundItem
										key={sortedImages[0].id}
										background={sortedImages[0]}
										isSelected={backgroundImageId === sortedImages[0].id}
										onSelect={handleBackgroundSelect}
										onDelete={undefined}
									/>
									<div className={styles.actionButtons}>
										<Tooltip text={t`Replace background`}>
											<FocusRing offset={-2}>
												<button
													type="button"
													onClick={(e) => {
														e.stopPropagation();
														handleUploadClick(true);
													}}
													className={styles.actionButton}
													aria-label={t`Replace background`}
												>
													<ArrowsClockwiseIcon size={16} weight="bold" className={styles.actionButtonIcon} />
												</button>
											</FocusRing>
										</Tooltip>
										<Tooltip text={t`Remove background`}>
											<FocusRing offset={-2}>
												<button
													type="button"
													onClick={(e) => {
														e.stopPropagation();
														handleRemoveImage(sortedImages[0]);
													}}
													className={styles.actionButton}
													aria-label={t`Remove background`}
												>
													<TrashIcon size={16} weight="bold" className={styles.actionButtonIcon} />
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
										if (e.key === 'Enter' || e.key === ' ') {
											e.preventDefault();
											handleUploadClick(false);
										}
									}}
									role="button"
									tabIndex={0}
									aria-label={t`Upload custom background`}
								>
									<div className={styles.uploadPlaceholderContent}>
										<PlusIcon size={48} weight="regular" className={styles.uploadIcon} />
										<div className={styles.uploadTextContainer}>
											<div className={styles.uploadTitle}>
												<Trans>Upload Custom Background</Trans>
											</div>
											<div className={styles.uploadHint}>
												<Trans>Click or drag and drop</Trans>
											</div>
										</div>
									</div>
								</div>
							)}
							<div className={styles.builtInGrid}>
								{builtInBackgrounds
									.filter((bg) => bg.type !== 'upload')
									.map((background) => (
										<BackgroundItem
											key={background.id}
											background={background}
											isSelected={backgroundImageId === background.id}
											onSelect={handleBackgroundSelect}
										/>
									))}
							</div>
						</div>
					) : (
						<div className={styles.premiumGrid}>
							{builtInBackgrounds.map((background) => (
								<BackgroundItem
									key={background.id}
									background={background}
									isSelected={backgroundImageId === background.id}
									onSelect={handleBackgroundSelect}
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
								/>
							))}
						</div>
					)}

					<div className={styles.statsText}>
						{backgroundCount === 1
							? t`${backgroundCount} / ${maxBackgroundImages} custom background`
							: t`${backgroundCount} / ${maxBackgroundImages} custom backgrounds`}
					</div>

					<div className={styles.infoText}>
						<Trans>Supported: JPG, PNG, GIF, WebP, MP4. Max size: 10MB.</Trans>
					</div>

					{!hasPremium && (
						<div className={styles.premiumUpsell}>
							<div className={styles.premiumHeader}>
								<CrownIcon weight="fill" size={18} className={styles.premiumIcon} />
								<span className={styles.premiumTitle}>
									<Trans>Unlock More Backgrounds with Plutonium</Trans>
								</span>
							</div>
							<p className={styles.premiumDesc}>
								<Trans>
									Upgrade to store up to 15 custom backgrounds and unlock HD video quality, higher frame rates, and
									more.
								</Trans>
							</p>
							<Button variant="secondary" small={true} onClick={() => PremiumModalActionCreators.open()}>
								<Trans>Get Plutonium</Trans>
							</Button>
						</div>
					)}
				</section>
			</Modal.Content>
		</Modal.Root>
	);
});

export default BackgroundImageGalleryModal;
