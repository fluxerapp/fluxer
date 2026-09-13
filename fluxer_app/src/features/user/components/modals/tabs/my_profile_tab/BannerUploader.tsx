// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	ANIMATED_IMAGE_FORMATS,
	BANNER_ASPECT_RATIO_LABEL,
	BANNER_MINIMUM_SIZE_LABEL,
	IMAGE_MAX_SIZE_BYTES,
	PREMIUM_PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {getAcceptString} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadMinimumHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {downloadGifAsImageFile} from '@app/features/expressions/utils/GifFileDownload';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {
	CANCEL_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	GET_PREMIUM_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/BannerUploader.module.css';
import type {ProfileAssetMode} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileAssetCustomizationStateMachine';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {showUserErrorModal} from '@app/features/user/utils/UserErrorModalUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const USE_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Use global profile',
	comment: 'Short label in the banner uploader. Keep it concise. Keep the tone plain and specific.',
});
const SHOW_YOUR_GLOBAL_PROFILE_BANNER_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Show your global profile banner in this community',
	comment: 'Label in the banner uploader. Keep the tone plain and specific.',
});
const USE_CUSTOM_IMAGE_DESCRIPTOR = msg({
	message: 'Use custom image',
	comment: 'Short label in the banner uploader. Keep it concise. Keep the tone plain and specific.',
});
const UPLOAD_A_CUSTOM_BANNER_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Upload a custom banner for this community',
	comment: 'Button or menu action label in the banner uploader. Keep it concise. Keep the tone plain and specific.',
});
const DON_T_SHOW_DESCRIPTOR = msg({
	message: "Don't show",
	comment: 'Short label in the banner uploader. Keep it concise. Keep the tone plain and specific.',
});
const PROFILE_BANNERS_REQUIRE_PREMIUM_DESCRIPTOR = msg({
	message: 'Profile banners require {premiumProductName}.',
	comment: 'Profile banner uploader notice shown when profile banners require premium.',
});
const PROFILE_BANNERS_NOT_ENABLED_DESCRIPTOR = msg({
	message: 'Profile banners are not enabled on this instance.',
	comment: 'Profile banner uploader notice shown when the instance configuration disables profile banners.',
});
const SHOW_ACCENT_COLOR_ONLY_IGNORING_YOUR_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'Show accent color only, ignoring your global profile',
	comment: 'Label in the banner uploader. Keep the tone plain and specific.',
});
const PROFILE_BANNERS_REQUIRE_DESCRIPTOR = msg({
	message: 'Profile banners require {premiumProductName}',
	comment:
		'Label in the banner uploader. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const BANNER_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Banner file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment:
		'Error message in the banner uploader. Preserve {imageMaxSizeLabel}; it is inserted by code. Keep the tone plain and specific.',
});
const BANNER_MODE_SELECTION_DESCRIPTOR = msg({
	message: 'Banner mode selection',
	comment: 'Button or menu action label in the banner uploader. Keep it concise. Keep the tone plain and specific.',
});
const COULDN_T_UPLOAD_BANNER_DESCRIPTOR = msg({
	message: "Couldn't upload banner",
	comment: 'Title of the error modal shown when uploading a profile banner fails.',
});
const CHANGE_BANNER_DESCRIPTOR = msg({
	message: 'Change banner',
	comment: 'Title of the modal where the user picks a banner source (file upload or GIF provider). Keep it concise.',
});

export type BannerMode = ProfileAssetMode;

interface BannerUploaderProps {
	hasBanner: boolean;
	onBannerChange: (base64: string) => void;
	onBannerClear: () => void;
	disabled?: boolean;
	disableModeSelection?: boolean;
	hideUploadWhenMissingEntitlement?: boolean;
	requireBannerEntitlement?: boolean;
	isPerGuildProfile: boolean;
	errorMessage?: string;
	bannerMode?: BannerMode;
	onBannerModeChange?: (mode: BannerMode) => void;
}

export const BannerUploader = observer(
	({
		hasBanner,
		onBannerChange,
		onBannerClear,
		disabled,
		disableModeSelection,
		hideUploadWhenMissingEntitlement = false,
		requireBannerEntitlement = true,
		isPerGuildProfile,
		errorMessage,
		bannerMode = 'inherit',
		onBannerModeChange,
	}: BannerUploaderProps) => {
		const {i18n} = useLingui();
		const hasPremiumBannerEntitlement = isLimitToggleEnabled(
			{feature_animated_banner: LimitResolver.resolve({key: 'feature_animated_banner', fallback: 0})},
			'feature_animated_banner',
		);
		const canUploadBanner = !requireBannerEntitlement || hasPremiumBannerEntitlement;
		const getBannerModeOptions = useCallback(
			() => [
				{
					value: 'inherit' as BannerMode,
					name: i18n._(USE_GLOBAL_PROFILE_DESCRIPTOR),
					desc: i18n._(SHOW_YOUR_GLOBAL_PROFILE_BANNER_IN_THIS_COMMUNITY_DESCRIPTOR),
				},
				{
					value: 'custom' as BannerMode,
					name: i18n._(USE_CUSTOM_IMAGE_DESCRIPTOR),
					desc: i18n._(UPLOAD_A_CUSTOM_BANNER_FOR_THIS_COMMUNITY_DESCRIPTOR),
					disabled: !canUploadBanner,
				},
				{
					value: 'unset' as BannerMode,
					name: i18n._(DON_T_SHOW_DESCRIPTOR),
					desc: i18n._(SHOW_ACCENT_COLOR_ONLY_IGNORING_YOUR_GLOBAL_PROFILE_DESCRIPTOR),
				},
			],
			[canUploadBanner, i18n],
		);
		const showBannerPremiumUpsell = useCallback(() => {
			if (!shouldShowPremiumFeatures()) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(PROFILE_BANNERS_REQUIRE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						description={
							<Trans>
								Profile banners are a {PREMIUM_PRODUCT_NAME} feature. Get {PREMIUM_PRODUCT_NAME} to add a banner to your
								profile.
							</Trans>
						}
						primaryText={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						primaryVariant="primary"
						secondaryText={i18n._(CANCEL_DESCRIPTOR)}
						onPrimary={() => {
							window.setTimeout(() => {
								PremiumModalCommands.open();
							}, 0);
						}}
						data-flx="user.my-profile-tab.banner-uploader.show-banner-premium-upsell.confirm-modal"
					/>
				)),
			);
		}, [i18n]);
		const processBannerFile = useCallback(
			async (file: File) => {
				if (file.size > 10 * 1024 * 1024) {
					showUserErrorModal(
						i18n._(COULDN_T_UPLOAD_BANNER_DESCRIPTOR),
						i18n._(BANNER_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
							imageMaxSizeLabel: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
						}),
					);
					return;
				}
				const svg = isSvgFile(file);
				const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
				ModalCommands.push(
					modal(() => (
						<AssetCropModal
							imageUrl={base64}
							sourceMimeType={svg ? 'image/svg+xml' : file.type}
							assetType={AssetType.PROFILE_BANNER}
							onCropComplete={(croppedBlob) => {
								const reader = new FileReader();
								reader.onload = () => {
									const croppedBase64 = reader.result as string;
									onBannerChange(croppedBase64);
								};
								reader.onerror = () => {
									showUserErrorModal(
										i18n._(COULDN_T_UPLOAD_BANNER_DESCRIPTOR),
										i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR),
									);
								};
								reader.readAsDataURL(croppedBlob);
							}}
							onSkip={() => {
								onBannerChange(base64);
							}}
							data-flx="user.my-profile-tab.banner-uploader.handle-banner-upload.asset-crop-modal"
						/>
					)),
				);
			},
			[i18n, onBannerChange],
		);
		const showBannerUploadError = useCallback(() => {
			showUserErrorModal(i18n._(COULDN_T_UPLOAD_BANNER_DESCRIPTOR), i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
		}, [i18n]);
		const handlePickBannerFile = useCallback(async () => {
			try {
				const [file] = await openFilePicker({accept: getAcceptString('banner')});
				if (!file) return;
				await processBannerFile(file);
			} catch {
				showBannerUploadError();
			}
		}, [processBannerFile, showBannerUploadError]);
		const handleSelectBannerGif = useCallback(
			async (gif: Gif) => {
				try {
					await processBannerFile(await downloadGifAsImageFile(gif));
				} catch {
					showBannerUploadError();
				}
			},
			[processBannerFile, showBannerUploadError],
		);
		const handleBannerUpload = useCallback(() => {
			if (!canUploadBanner) {
				showBannerPremiumUpsell();
				return;
			}
			openAssetSourceModal({
				title: i18n._(CHANGE_BANNER_DESCRIPTOR),
				uploadHint: formatImageUploadMinimumHint(i18n, {
					formats: ANIMATED_IMAGE_FORMATS,
					maxSize: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
					minimumSize: BANNER_MINIMUM_SIZE_LABEL,
					aspectRatio: BANNER_ASPECT_RATIO_LABEL,
				}),
				onPickUpload: handlePickBannerFile,
				onSelectGif: (gif) => void handleSelectBannerGif(gif),
			});
		}, [canUploadBanner, handlePickBannerFile, handleSelectBannerGif, i18n, showBannerPremiumUpsell]);
		const handleModeChange = useCallback(
			(mode: BannerMode) => {
				if (mode === 'custom') {
					handleBannerUpload();
					return;
				}
				onBannerModeChange?.(mode);
			},
			[onBannerModeChange, handleBannerUpload],
		);
		const bannerModeOptions = getBannerModeOptions();
		const showBannerUploadAction = canUploadBanner || !hideUploadWhenMissingEntitlement;
		const radioGroupDisabled = disableModeSelection ?? Boolean(disabled && !(isPerGuildProfile && !canUploadBanner));
		if (isPerGuildProfile && onBannerModeChange) {
			return (
				<div data-flx="user.my-profile-tab.banner-uploader.div">
					<div className={styles.label} data-flx="user.my-profile-tab.banner-uploader.label">
						<Trans>Banner</Trans>
					</div>
					<RadioGroup
						options={bannerModeOptions}
						value={bannerMode}
						disabled={radioGroupDisabled}
						onChange={handleModeChange}
						aria-label={i18n._(BANNER_MODE_SELECTION_DESCRIPTOR)}
						data-flx="user.my-profile-tab.banner-uploader.radio-group.mode-change"
					/>
					{bannerMode === 'custom' && (showBannerUploadAction || hasBanner) && (
						<div
							className={clsx(styles.buttonGroup, styles.buttonGroupAfterMode)}
							data-flx="user.my-profile-tab.banner-uploader.button-group"
						>
							{showBannerUploadAction && (
								<Button
									variant="primary"
									small={true}
									onClick={handleBannerUpload}
									disabled={disabled}
									data-flx="user.my-profile-tab.banner-uploader.button.banner-upload"
								>
									<Trans>Change banner</Trans>
								</Button>
							)}
							{hasBanner && (
								<Button
									variant="secondary"
									small={true}
									onClick={() => onBannerModeChange('inherit')}
									disabled={disabled}
									data-flx="user.my-profile-tab.banner-uploader.button.banner-mode-change"
								>
									<Trans>Remove banner</Trans>
								</Button>
							)}
						</div>
					)}
					{errorMessage && (
						<p className={styles.errorMessage} data-flx="user.my-profile-tab.banner-uploader.error-message">
							{errorMessage}
						</p>
					)}
				</div>
			);
		}
		return (
			<div data-flx="user.my-profile-tab.banner-uploader.div--2">
				<div className={styles.label} data-flx="user.my-profile-tab.banner-uploader.label--2">
					<Trans>Banner</Trans>
				</div>
				{(showBannerUploadAction || hasBanner) && (
					<div className={styles.buttonGroup} data-flx="user.my-profile-tab.banner-uploader.button-group--2">
						{showBannerUploadAction && (
							<Button
								variant="primary"
								small={true}
								onClick={handleBannerUpload}
								disabled={disabled}
								data-flx="user.my-profile-tab.banner-uploader.button.banner-upload--2"
							>
								<Trans>Change banner</Trans>
							</Button>
						)}
						{hasBanner && (
							<Button
								variant="secondary"
								small={true}
								onClick={onBannerClear}
								disabled={disabled}
								data-flx="user.my-profile-tab.banner-uploader.button.banner-clear"
							>
								<Trans>Remove banner</Trans>
							</Button>
						)}
					</div>
				)}
				{!canUploadBanner && (
					<div className={styles.description} data-flx="user.my-profile-tab.banner-uploader.description--3">
						{shouldShowPremiumFeatures()
							? i18n._(PROFILE_BANNERS_REQUIRE_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})
							: i18n._(PROFILE_BANNERS_NOT_ENABLED_DESCRIPTOR)}
					</div>
				)}
				{errorMessage && (
					<p className={styles.errorMessage} data-flx="user.my-profile-tab.banner-uploader.error-message--2">
						{errorMessage}
					</p>
				)}
			</div>
		);
	},
);
