// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	ANIMATED_AVATAR_FORMATS,
	ANIMATED_IMAGE_FORMATS,
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_LABEL,
	PREMIUM_PRODUCT_NAME,
	STATIC_IMAGE_FORMATS,
} from '@app/features/app/config/I18nDisplayConstants';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {showAnimatedAvifUnsupportedModal} from '@app/features/expressions/utils/AnimatedAvifModalUtils';
import {
	getAnimatedFormatLabel,
	isAnimatedFile,
	shouldHandleAnimatedNonGifUpload,
} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptString, getAssetFormatErrorMessage} from '@app/features/expressions/utils/AssetFormatCopy';
import {
	formatImageUploadRecommendedHint,
	formatImageUploadRecommendedHintWithNote,
} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {downloadGifAsImageFile} from '@app/features/expressions/utils/GifFileDownload';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {isGif} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/utils/ImageAsset';
import {
	CANCEL_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	GET_PREMIUM_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/AvatarUploader.module.css';
import type {ProfileAssetMode} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileAssetCustomizationStateMachine';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {showUserErrorModal} from '@app/features/user/utils/UserErrorModalUtils';
import {canCropFormat} from '@app/features/voice/utils/MediaCapabilities';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const USE_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Use global profile',
	comment: 'Short label in the avatar uploader. Keep it concise.',
});
const SHOW_YOUR_GLOBAL_PROFILE_AVATAR_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Show your global profile avatar in this community',
	comment: 'Label in the avatar uploader.',
});
const USE_CUSTOM_IMAGE_DESCRIPTOR = msg({
	message: 'Use custom image',
	comment: 'Short label in the avatar uploader. Keep it concise.',
});
const UPLOAD_A_CUSTOM_AVATAR_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Upload a custom avatar for this community',
	comment: 'Button or menu action label in the avatar uploader. Keep it concise.',
});
const DON_T_SHOW_DESCRIPTOR = msg({
	message: "Don't show",
	comment: 'Short label in the avatar uploader. Keep it concise.',
});
const SHOW_DEFAULT_AVATAR_IGNORING_YOUR_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Show default avatar, ignoring your global profile',
	comment: 'Label in the avatar uploader.',
});
const AVATAR_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Avatar file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment: 'Error message in the avatar uploader. Preserve {imageMaxSizeLabel}; it is inserted by code.',
});
const ANIMATED_AVATARS_REQUIRE_DESCRIPTOR = msg({
	message: 'Animated avatars require {premiumProductName}',
	comment:
		'Label in the avatar uploader. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const ANIMATED_AVATARS_NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'Animated avatars not available',
	comment: 'Error message in the avatar uploader.',
});
const CROPPING_ANIMATED_FILES_ISN_T_SUPPORTED_YET_THE_DESCRIPTOR = msg({
	message: "Cropping animated {formatLabel} files isn't supported yet. The original upload will be used.",
	comment: 'Description text in the avatar uploader. Preserve {formatLabel}; it is inserted by code.',
});
const CROPPING_ANIMATED_FILES_WITHOUT_FORMAT_ISN_T_SUPPORTED_YET_THE_DESCRIPTOR = msg({
	message: "Cropping animated files isn't supported yet. The original upload will be used.",
	comment: 'Description text in the avatar uploader when the animated image format is unknown.',
});
const AVATAR_MODE_SELECTION_DESCRIPTOR = msg({
	message: 'Avatar mode selection',
	comment: 'Short label in the avatar uploader. Keep it concise.',
});
const COULDN_T_UPLOAD_AVATAR_DESCRIPTOR = msg({
	message: "Couldn't upload avatar",
	comment: 'Title of the error modal shown when uploading a profile avatar fails.',
});
const CHANGE_AVATAR_DESCRIPTOR = msg({
	message: 'Change avatar',
	comment: 'Title of the modal where the user picks an avatar source (file upload or GIF provider). Keep it concise.',
});
const ANIMATED_AVATARS_REQUIRE_PREMIUM_NOTE_DESCRIPTOR = msg({
	message: 'Animated avatars ({animatedAvatarFormats}) require {premiumProductName}.',
	comment:
		'Extra note in the avatar asset upload source modal. Preserve {animatedAvatarFormats} and {premiumProductName}; they are inserted by code.',
});

export type AvatarMode = ProfileAssetMode;

interface AvatarUploaderProps {
	hasAvatar: boolean;
	onAvatarChange: (base64: string) => void;
	onAvatarClear: () => void;
	disabled?: boolean;
	disableModeSelection?: boolean;
	requireAnimatedAvatarEntitlement?: boolean;
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
		disableModeSelection,
		requireAnimatedAvatarEntitlement = true,
		isPerGuildProfile,
		errorMessage,
		avatarMode = 'inherit',
		onAvatarModeChange,
	}: AvatarUploaderProps) => {
		const {i18n} = useLingui();
		const hasAnimatedAvatarEntitlement = isLimitToggleEnabled(
			{feature_animated_avatar: LimitResolver.resolve({key: 'feature_animated_avatar', fallback: 0})},
			'feature_animated_avatar',
		);
		const canUploadAnimatedAvatar = !requireAnimatedAvatarEntitlement || hasAnimatedAvatarEntitlement;
		const getAvatarModeOptions = useCallback(
			() => [
				{
					value: 'inherit' as AvatarMode,
					name: i18n._(USE_GLOBAL_PROFILE_DESCRIPTOR),
					desc: i18n._(SHOW_YOUR_GLOBAL_PROFILE_AVATAR_IN_THIS_COMMUNITY_DESCRIPTOR),
				},
				{
					value: 'custom' as AvatarMode,
					name: i18n._(USE_CUSTOM_IMAGE_DESCRIPTOR),
					desc: i18n._(UPLOAD_A_CUSTOM_AVATAR_FOR_THIS_COMMUNITY_DESCRIPTOR),
					disabled: !canUploadAnimatedAvatar,
				},
				{
					value: 'unset' as AvatarMode,
					name: i18n._(DON_T_SHOW_DESCRIPTOR),
					desc: i18n._(SHOW_DEFAULT_AVATAR_IGNORING_YOUR_GLOBAL_PROFILE_DESCRIPTOR),
				},
			],
			[canUploadAnimatedAvatar, i18n],
		);
		const processAvatarFile = useCallback(
			async (file: File) => {
				if (file.size > 10 * 1024 * 1024) {
					showUserErrorModal(
						i18n._(COULDN_T_UPLOAD_AVATAR_DESCRIPTOR),
						i18n._(AVATAR_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
							imageMaxSizeLabel: IMAGE_MAX_SIZE_LABEL,
						}),
					);
					return;
				}
				const svg = isSvgFile(file);
				if (!svg && !(await canCropFormat(file.type))) {
					showUserErrorModal(
						i18n._(COULDN_T_UPLOAD_AVATAR_DESCRIPTOR),
						getAssetFormatErrorMessage(i18n, 'avatar', 'unsupported_mime'),
					);
					return;
				}
				const animated = svg ? false : await isAnimatedFile(file);
				const isGifFile = isGif(file);
				if (animated && !canUploadAnimatedAvatar) {
					if (shouldShowPremiumFeatures()) {
						ModalCommands.push(
							modal(() => (
								<ConfirmModal
									title={i18n._(ANIMATED_AVATARS_REQUIRE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
									description={
										<>
											<p data-flx="user.my-profile-tab.avatar-uploader.handle-avatar-upload.p">
												<Trans>
													Animated avatars ({ANIMATED_AVATAR_FORMATS}) are exclusively available to{' '}
													{PREMIUM_PRODUCT_NAME} subscribers.
												</Trans>
											</p>
											<p
												className={styles.spacedParagraph}
												data-flx="user.my-profile-tab.avatar-uploader.handle-avatar-upload.spaced-paragraph"
											>
												<Trans>
													With {PREMIUM_PRODUCT_NAME}, you can use animated avatars ({ANIMATED_AVATAR_FORMATS}), profile
													banners, customize your tag, and unlock many other {PREMIUM_PRODUCT_NAME} perks.
												</Trans>
											</p>
										</>
									}
									primaryText={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
									primaryVariant="primary"
									secondaryText={i18n._(CANCEL_DESCRIPTOR)}
									onPrimary={() => {
										window.setTimeout(() => {
											PremiumModalCommands.open();
										}, 0);
									}}
									data-flx="user.my-profile-tab.avatar-uploader.handle-avatar-upload.confirm-modal"
								/>
							)),
						);
					} else {
						ModalCommands.push(
							modal(() => (
								<GenericErrorModal
									title={i18n._(ANIMATED_AVATARS_NOT_AVAILABLE_DESCRIPTOR)}
									message={
										<Trans>
											Animated avatars ({ANIMATED_AVATAR_FORMATS}) are not available on this instance. Upload a static
											image instead.
										</Trans>
									}
									data-flx="user.my-profile-tab.avatar-uploader.handle-avatar-upload.confirm-modal--2"
								/>
							)),
						);
					}
					return;
				}
				const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
				const animatedHandled = shouldHandleAnimatedNonGifUpload({
					file,
					isGif: isGifFile,
					animated,
					onAnimatedAvif: () => {
						showAnimatedAvifUnsupportedModal({i18n});
					},
					onOtherAnimated: () => {
						const formatLabel = getAnimatedFormatLabel(file);
						ToastCommands.createToast({
							type: 'info',
							children:
								formatLabel == null
									? i18n._(CROPPING_ANIMATED_FILES_WITHOUT_FORMAT_ISN_T_SUPPORTED_YET_THE_DESCRIPTOR)
									: i18n._(CROPPING_ANIMATED_FILES_ISN_T_SUPPORTED_YET_THE_DESCRIPTOR, {formatLabel}),
						});
						onAvatarChange(base64);
					},
				});
				if (animatedHandled) {
					return;
				}
				ModalCommands.push(
					modal(() => (
						<AssetCropModal
							assetType={AssetType.AVATAR}
							imageUrl={base64}
							sourceMimeType={svg ? 'image/svg+xml' : file.type}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									onAvatarChange(croppedBase64);
								};
								reader.onerror = () => {
									showUserErrorModal(
										i18n._(COULDN_T_UPLOAD_AVATAR_DESCRIPTOR),
										i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR),
									);
								};
								reader.readAsDataURL(croppedBlob);
							}}
							onSkip={() => {
								onAvatarChange(base64);
							}}
							data-flx="user.my-profile-tab.avatar-uploader.handle-avatar-upload.asset-crop-modal"
						/>
					)),
				);
			},
			[canUploadAnimatedAvatar, onAvatarChange, i18n],
		);
		const showAvatarUploadError = useCallback(() => {
			showUserErrorModal(i18n._(COULDN_T_UPLOAD_AVATAR_DESCRIPTOR), i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
		}, [i18n]);
		const handlePickAvatarFile = useCallback(async () => {
			try {
				const [file] = await openFilePicker({accept: getAcceptString('avatar')});
				if (!file) return;
				await processAvatarFile(file);
			} catch {
				showAvatarUploadError();
			}
		}, [processAvatarFile, showAvatarUploadError]);
		const handleSelectAvatarGif = useCallback(
			async (gif: Gif) => {
				try {
					await processAvatarFile(await downloadGifAsImageFile(gif));
				} catch {
					showAvatarUploadError();
				}
			},
			[processAvatarFile, showAvatarUploadError],
		);
		const handleAvatarUpload = useCallback(() => {
			const uploadHint = canUploadAnimatedAvatar
				? formatImageUploadRecommendedHint(i18n, {
						formats: ANIMATED_IMAGE_FORMATS,
						maxSize: IMAGE_MAX_SIZE_LABEL,
						recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
					})
				: shouldShowPremiumFeatures()
					? formatImageUploadRecommendedHintWithNote(i18n, {
							formats: STATIC_IMAGE_FORMATS,
							maxSize: IMAGE_MAX_SIZE_LABEL,
							recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
							note: i18n._(ANIMATED_AVATARS_REQUIRE_PREMIUM_NOTE_DESCRIPTOR, {
								animatedAvatarFormats: ANIMATED_AVATAR_FORMATS,
								premiumProductName: PREMIUM_PRODUCT_NAME,
							}),
						})
					: formatImageUploadRecommendedHint(i18n, {
							formats: STATIC_IMAGE_FORMATS,
							maxSize: IMAGE_MAX_SIZE_LABEL,
							recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
						});
			openAssetSourceModal({
				title: i18n._(CHANGE_AVATAR_DESCRIPTOR),
				uploadHint,
				onPickUpload: handlePickAvatarFile,
				onSelectGif: (gif) => void handleSelectAvatarGif(gif),
			});
		}, [canUploadAnimatedAvatar, handlePickAvatarFile, handleSelectAvatarGif, i18n]);
		const handleModeChange = useCallback(
			(mode: AvatarMode) => {
				if (mode === 'custom') {
					handleAvatarUpload();
					return;
				}
				onAvatarModeChange?.(mode);
			},
			[onAvatarModeChange, handleAvatarUpload],
		);
		const avatarModeOptions = getAvatarModeOptions();
		const radioGroupDisabled =
			disableModeSelection ?? Boolean(disabled && !(isPerGuildProfile && !canUploadAnimatedAvatar));
		if (isPerGuildProfile && onAvatarModeChange) {
			return (
				<div data-flx="user.my-profile-tab.avatar-uploader.div">
					<div className={styles.label} data-flx="user.my-profile-tab.avatar-uploader.label">
						<Trans>Avatar</Trans>
					</div>
					<RadioGroup
						options={avatarModeOptions}
						value={avatarMode}
						disabled={radioGroupDisabled}
						onChange={handleModeChange}
						aria-label={i18n._(AVATAR_MODE_SELECTION_DESCRIPTOR)}
						data-flx="user.my-profile-tab.avatar-uploader.radio-group.mode-change"
					/>
					{avatarMode === 'custom' && (
						<div
							className={clsx(styles.buttonGroup, styles.buttonGroupAfterMode)}
							data-flx="user.my-profile-tab.avatar-uploader.button-group"
						>
							<Button
								variant="primary"
								small={true}
								onClick={handleAvatarUpload}
								disabled={disabled}
								data-flx="user.my-profile-tab.avatar-uploader.button.avatar-upload"
							>
								<Trans>Change avatar</Trans>
							</Button>
							{hasAvatar && (
								<Button
									variant="secondary"
									small={true}
									onClick={() => onAvatarModeChange('inherit')}
									disabled={disabled}
									data-flx="user.my-profile-tab.avatar-uploader.button.avatar-mode-change"
								>
									<Trans>Remove avatar</Trans>
								</Button>
							)}
						</div>
					)}
					{errorMessage && (
						<p className={styles.errorMessage} data-flx="user.my-profile-tab.avatar-uploader.error-message">
							{errorMessage}
						</p>
					)}
				</div>
			);
		}
		return (
			<div data-flx="user.my-profile-tab.avatar-uploader.div--2">
				<div className={styles.label} data-flx="user.my-profile-tab.avatar-uploader.label--2">
					<Trans>Avatar</Trans>
				</div>
				<div className={styles.buttonGroup} data-flx="user.my-profile-tab.avatar-uploader.button-group--2">
					<Button
						variant="primary"
						small={true}
						onClick={handleAvatarUpload}
						disabled={disabled}
						data-flx="user.my-profile-tab.avatar-uploader.button.avatar-upload--2"
					>
						<Trans>Change avatar</Trans>
					</Button>
					{hasAvatar && (
						<Button
							variant="secondary"
							small={true}
							onClick={onAvatarClear}
							disabled={disabled}
							data-flx="user.my-profile-tab.avatar-uploader.button.avatar-clear"
						>
							<Trans>Remove avatar</Trans>
						</Button>
					)}
				</div>
				{errorMessage && (
					<p className={styles.errorMessage} data-flx="user.my-profile-tab.avatar-uploader.error-message--2">
						{errorMessage}
					</p>
				)}
			</div>
		);
	},
);
