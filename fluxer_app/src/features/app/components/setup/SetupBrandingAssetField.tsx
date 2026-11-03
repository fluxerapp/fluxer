// SPDX-License-Identifier: AGPL-3.0-or-later

import {type BrandingAssetState, brandingAssetPolicyKind} from '@app/features/app/components/setup/SetupBrandingDraft';
import styles from '@app/features/app/components/setup/SetupBrandingStep.module.css';
import type {SetupBrandingAssetKind} from '@app/features/app/components/setup/SetupWizardClient';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {contrastRatio} from '@app/features/theme/utils/InstanceBrandColors';
import {Button} from '@app/features/ui/button/Button';
import {formatAssetUploadExtensions, getPolicy} from '@fluxer/constants/src/AssetFormatPolicy';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowsClockwiseIcon,
	CaretDownIcon,
	ImageIcon,
	TrashIcon,
	UploadSimpleIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

const CHOOSE_IMAGE_DESCRIPTOR = msg({
	message: 'Choose an image or drop one here',
	comment: 'Text on the empty drop area of a branding image field in the setup wizard.',
});
const CHOOSE_ASSET_DESCRIPTOR = msg({
	message: 'Choose',
	comment: 'Button that opens the file picker for an empty branding image field in the setup wizard.',
});
const READING_IMAGE_DESCRIPTOR = msg({
	message: 'Loading image',
	comment: 'Status shown while a chosen branding image is being loaded in the setup wizard.',
});
const REPLACE_ASSET_DESCRIPTOR = msg({
	message: 'Replace',
	comment: 'Button that replaces an already uploaded branding image.',
});
const REMOVE_ASSET_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Button that removes a branding image in the setup wizard.',
});
const ASSET_SIZE_DESCRIPTOR = msg({
	message: '{width} by {height} pixels',
	comment: 'Size of the chosen branding image in the setup wizard. Width and height are numbers.',
});
const ASSET_SMALL_DESCRIPTOR = msg({
	message: 'This image is smaller than recommended and may look blurry.',
	comment: 'Warning when a branding image is smaller than the recommended size in the setup wizard.',
});
const WORDMARK_LOW_ON_LIGHT_DESCRIPTOR = msg({
	message: 'This wordmark is hard to see on the light title bar.',
	comment:
		'Warning in the setup wizard when the uploaded wordmark is too light to read on the light desktop app title bar.',
});
const WORDMARK_LOW_ON_DARK_DESCRIPTOR = msg({
	message: 'This wordmark is hard to see on the dark title bar.',
	comment:
		'Warning in the setup wizard when the uploaded wordmark is too dark to read on the dark desktop app title bar.',
});
const ERROR_TOO_LARGE_DESCRIPTOR = msg({
	message: 'This file is larger than {size}. Choose a smaller one.',
	comment: 'Error when a branding image file is too large in the setup wizard. size is a file size such as 4 MB.',
});
const ERROR_UNSUPPORTED_DESCRIPTOR = msg({
	message: 'This file type is not supported. Supported types are {formats}.',
	comment:
		'Error when a branding image has an unsupported file type in the setup wizard. formats is a list such as PNG, JPEG, WebP.',
});
const ERROR_UNREADABLE_DESCRIPTOR = msg({
	message: 'That image could not be used. Try a different file.',
	comment: 'Error shown when a chosen branding image cannot be loaded in the setup wizard.',
});
const ERROR_NOT_SAVED_DESCRIPTOR = msg({
	message: 'This image was not saved. Press "Continue" to try again.',
	comment:
		'Error on a branding image field in the setup wizard when saving the image failed. "Continue" is the label of the button that saves the step.',
});
const ERROR_NOT_LOADED_DESCRIPTOR = msg({
	message: 'This image could not be loaded after saving. Press "Continue" to try again, or choose a different file.',
	comment:
		'Error on a branding image field in the setup wizard when the image was uploaded but loading it from the server failed. "Continue" is the label of the button that saves the step.',
});
const MORE_ABOUT_IMAGE_DESCRIPTOR = msg({
	message: 'More about this image',
	comment:
		'Button on a branding image field in the setup wizard on narrow screens that shows where the image appears and what shows when it is empty.',
});

interface AssetCopy {
	label: MessageDescriptor;
	hint: MessageDescriptor;
	help: MessageDescriptor;
	empty: MessageDescriptor;
	minWidth: number;
	minHeight: number;
}

export const ASSET_COPY: Record<SetupBrandingAssetKind, AssetCopy> = {
	logo: {
		label: msg({message: 'Logo', comment: 'Label for the logo branding image in the setup wizard.'}),
		hint: msg({
			message: 'Square or wide, in light colors on a transparent background. At least 256 pixels tall.',
			comment: 'Short hint under the logo field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				'The large image next to the sign-in form, which is always dark. On a phone it only shows next to the name, when there is no wordmark and the product name is not Fluxer. Use a square or wide PNG or SVG in light colors with a transparent background, at least 256 pixels tall and up to {size}.',
			comment:
				'Where the logo branding image appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message:
				'If you leave it empty, the sign-in page uses your app icon or symbol. Without those, it shows the Fluxer logo while the product name is Fluxer, and no image for any other name.',
			comment: 'What the app shows when no logo is set, in the setup wizard.',
		}),
		minWidth: 0,
		minHeight: 256,
	},
	wordmark: {
		label: msg({message: 'Wordmark', comment: 'Label for the wordmark branding image in the setup wizard.'}),
		hint: msg({
			message: 'Your name in your own lettering, on a transparent background. At least 64 pixels tall.',
			comment: 'Short hint under the wordmark field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				'Your name in your own lettering. It shows under the logo on the sign-in page, at the top of the sign-in page on phones and in the desktop app title bar. Use a wide PNG or SVG with a transparent background, at least 64 pixels tall and up to {size}, in a color that works on both light and dark title bars.',
			comment:
				'Where the wordmark branding image appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message:
				'If you leave it empty, the product name shows as plain text, or as the Fluxer wordmark while the product name is Fluxer.',
			comment: 'What the app shows when no wordmark is set, in the setup wizard.',
		}),
		minWidth: 0,
		minHeight: 64,
	},
	sign_in_backdrop: {
		label: msg({
			message: 'Sign-in background',
			comment: 'Label for the sign-in page background image in the setup wizard.',
		}),
		hint: msg({
			message: 'A landscape image, at least 1920 by 1080 pixels.',
			comment: 'Short hint under the sign-in background field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				'Fills the sign-in page behind the form, darkened so the form stays easy to read. Use a landscape JPEG, PNG, WebP or AVIF, at least 1920 by 1080 pixels and up to {size}.',
			comment:
				'Where the sign-in background image appears and the recommended format, in the setup wizard. size is a file size such as 4 MB.',
		}),
		empty: msg({
			message: 'If you leave it empty, the sign-in page has the dark starry background.',
			comment: 'What the app shows when no sign-in background is set, in the setup wizard.',
		}),
		minWidth: 1920,
		minHeight: 1080,
	},
	icon: {
		label: msg({message: 'App icon', comment: 'Label for the icon branding image in the setup wizard.'}),
		hint: msg({
			message: 'Square, at least 512 by 512 pixels.',
			comment: 'Short hint under the app icon field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				'Shows when someone adds the web app to their home screen and next to the join date on profiles. It also fills in for the logo, symbol and favicon when those are empty. Use a square PNG, at least 512 by 512 pixels and up to {size}.',
			comment:
				'Where the app icon branding image appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message: 'If you leave it empty, those places show the Fluxer icon.',
			comment: 'What the app shows when no app icon is set, in the setup wizard.',
		}),
		minWidth: 512,
		minHeight: 512,
	},
	symbol: {
		label: msg({message: 'Symbol', comment: 'Label for the symbol branding image in the setup wizard.'}),
		hint: msg({
			message: 'Square on a transparent background, still clear at 16 pixels.',
			comment: 'Short hint under the symbol field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				"A small mark next to your instance's name in the account switcher and the instance list. Use a square PNG or SVG with a transparent background that is still clear at 16 pixels, up to {size}.",
			comment:
				'Where the symbol branding image appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message: 'If you leave it empty, the lists use your app icon or logo, and a globe if both are empty.',
			comment: 'What the app shows when no symbol is set, in the setup wizard.',
		}),
		minWidth: 64,
		minHeight: 64,
	},
	favicon: {
		label: msg({message: 'Favicon', comment: 'Label for the favicon branding image in the setup wizard.'}),
		hint: msg({
			message: 'Square, at least 64 by 64 pixels.',
			comment: 'Short hint under the favicon field in the setup wizard branding step.',
		}),
		help: msg({
			message: 'The small image in the browser tab. Use a square PNG, at least 64 by 64 pixels and up to {size}.',
			comment:
				'Where the favicon appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message: 'If you leave it empty, the tab uses your app icon, or the Fluxer icon if that is empty too.',
			comment: 'What the app shows when no favicon is set, in the setup wizard.',
		}),
		minWidth: 64,
		minHeight: 64,
	},
	favicon_dark: {
		label: msg({
			message: 'Dark mode favicon',
			comment: 'Label for the favicon that browsers show while they use a dark theme, in the setup wizard.',
		}),
		hint: msg({
			message: 'Square, at least 64 by 64 pixels, for dark browser tabs.',
			comment: 'Short hint under the dark mode favicon field in the setup wizard branding step.',
		}),
		help: msg({
			message:
				'Browsers set to a dark theme show this instead of the favicon. Add one when your favicon is too dark to see on a dark tab. Use a square PNG, at least 64 by 64 pixels and up to {size}.',
			comment:
				'Where the dark mode favicon appears and the recommended format, in the setup wizard. size is a file size such as 10 MB.',
		}),
		empty: msg({
			message: 'If you leave it empty, the tab shows the same favicon in light and dark themes.',
			comment: 'What the app shows when no dark mode favicon is set, in the setup wizard.',
		}),
		minWidth: 64,
		minHeight: 64,
	},
};

const LIGHT_TITLEBAR = [0xe3, 0xe5, 0xe8] as const;
const DARK_TITLEBAR = [0x12, 0x12, 0x14] as const;
const LOW_CONTRAST = 1.6;
const LOW_CONTRAST_SHARE = 0.85;

interface WordmarkReadability {
	light: boolean;
	dark: boolean;
}

function measureWordmarkReadability(image: HTMLImageElement): WordmarkReadability | null {
	const scale = Math.min(1, 256 / Math.max(image.naturalWidth, 1));
	const width = Math.max(1, Math.round(image.naturalWidth * scale));
	const height = Math.max(1, Math.round(image.naturalHeight * scale));
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	const context = canvas.getContext('2d', {willReadFrequently: true});
	if (!context) return null;
	let pixels: Uint8ClampedArray;
	try {
		context.drawImage(image, 0, 0, width, height);
		pixels = context.getImageData(0, 0, width, height).data;
	} catch {
		return null;
	}
	let opaque = 0;
	let lowOnLight = 0;
	let lowOnDark = 0;
	for (let index = 0; index < pixels.length; index += 4) {
		if (pixels[index + 3] < 128) continue;
		const rgb = [pixels[index], pixels[index + 1], pixels[index + 2]] as const;
		opaque += 1;
		if (contrastRatio(rgb, LIGHT_TITLEBAR) < LOW_CONTRAST) lowOnLight += 1;
		if (contrastRatio(rgb, DARK_TITLEBAR) < LOW_CONTRAST) lowOnDark += 1;
	}
	if (opaque === 0) return null;
	return {light: lowOnLight / opaque >= LOW_CONTRAST_SHARE, dark: lowOnDark / opaque >= LOW_CONTRAST_SHARE};
}

function hasFiles(event: React.DragEvent<HTMLElement>): boolean {
	return Array.from(event.dataTransfer.types).includes('Files');
}

export interface TargetHandlers {
	onFocusCapture: () => void;
	onBlurCapture: (event: React.FocusEvent<HTMLElement>) => void;
	onPointerEnter: () => void;
	onPointerLeave: () => void;
}

interface BrandingAssetFieldProps {
	asset: BrandingAssetState;
	labelledBy: string | null;
	disabled: boolean;
	onUpload: (kind: SetupBrandingAssetKind, file?: File) => void;
	onClear: (kind: SetupBrandingAssetKind) => void;
	onImageError: (kind: SetupBrandingAssetKind) => void;
	targetHandlers: TargetHandlers;
}

export const BrandingAssetField = observer(
	({asset, labelledBy, disabled, onUpload, onClear, onImageError, targetHandlers}: BrandingAssetFieldProps) => {
		const {i18n} = useLingui();
		const copy = ASSET_COPY[asset.kind];
		const policy = getPolicy(brandingAssetPolicyKind(asset.kind));
		const size = formatFileSize(i18n.locale, policy.maxBytes);
		const source = asset.preview ?? asset.url;
		const [dimensions, setDimensions] = useState<{source: string; width: number; height: number} | null>(null);
		const [readability, setReadability] = useState<{source: string; result: WordmarkReadability} | null>(null);
		const [dragging, setDragging] = useState(false);
		const dragDepth = useRef(0);
		const rowRef = useRef<HTMLDivElement>(null);
		const previousSource = useRef(source);
		const shownDimensions = dimensions != null && dimensions.source === source ? dimensions : null;
		const shownReadability = readability != null && readability.source === source ? readability.result : null;
		const isVector = source?.startsWith('data:image/svg') ?? false;
		const tooSmall =
			shownDimensions != null &&
			!isVector &&
			(shownDimensions.width < copy.minWidth || shownDimensions.height < copy.minHeight);
		const labelId = labelledBy ?? `setup-branding-${asset.kind}-label`;
		const hintId = `setup-branding-${asset.kind}-hint`;
		const statusId = `setup-branding-${asset.kind}-status`;
		const helpId = `setup-branding-${asset.kind}-help`;
		const [helpOpen, setHelpOpen] = useState(false);
		const busy = disabled || asset.reading;
		let errorText: string | null = null;
		switch (asset.error) {
			case 'too_large':
				errorText = i18n._(ERROR_TOO_LARGE_DESCRIPTOR, {size});
				break;
			case 'unsupported':
				errorText = i18n._(ERROR_UNSUPPORTED_DESCRIPTOR, {
					formats: formatAssetUploadExtensions(brandingAssetPolicyKind(asset.kind)),
				});
				break;
			case 'unreadable':
				errorText = i18n._(ERROR_UNREADABLE_DESCRIPTOR);
				break;
			case 'not_saved':
				errorText = i18n._(ERROR_NOT_SAVED_DESCRIPTOR);
				break;
			case 'not_loaded':
				errorText = i18n._(ERROR_NOT_LOADED_DESCRIPTOR);
				break;
		}
		const warnings: Array<string> = [];
		if (tooSmall) warnings.push(i18n._(ASSET_SMALL_DESCRIPTOR));
		if (shownReadability?.light) warnings.push(i18n._(WORDMARK_LOW_ON_LIGHT_DESCRIPTOR));
		if (shownReadability?.dark) warnings.push(i18n._(WORDMARK_LOW_ON_DARK_DESCRIPTOR));
		useEffect(() => {
			const was = previousSource.current;
			previousSource.current = source;
			if ((was == null) === (source == null)) return;
			const active = document.activeElement;
			const focusLost = active == null || active === document.body || rowRef.current?.contains(active) === true;
			if (!focusLost) return;
			rowRef.current
				?.querySelector<HTMLButtonElement>(
					source != null
						? '[data-flx="app.self-hosted-setup-wizard-gate.asset-replace-button"]'
						: '[data-flx="app.self-hosted-setup-wizard-gate.asset-upload-button"]',
				)
				?.focus();
		}, [source]);
		const resetDrag = () => {
			dragDepth.current = 0;
			setDragging(false);
		};
		const handleDragEnter = (event: React.DragEvent<HTMLElement>) => {
			if (busy || !hasFiles(event)) return;
			event.preventDefault();
			dragDepth.current += 1;
			setDragging(true);
		};
		const handleDragOver = (event: React.DragEvent<HTMLElement>) => {
			if (busy || !hasFiles(event)) return;
			event.preventDefault();
			event.dataTransfer.dropEffect = 'copy';
		};
		const handleDragLeave = () => {
			dragDepth.current = Math.max(0, dragDepth.current - 1);
			if (dragDepth.current === 0) setDragging(false);
		};
		const handleDrop = (event: React.DragEvent<HTMLElement>) => {
			event.preventDefault();
			resetDrag();
			if (busy) return;
			const file = event.dataTransfer.files[0];
			if (file) onUpload(asset.kind, file);
		};
		const describedBy = clsx(hintId, helpId, (errorText || warnings.length > 0) && statusId);
		return (
			<div
				ref={rowRef}
				className={clsx(
					styles.assetRow,
					source == null && styles.assetRowEmpty,
					dragging && styles.assetRowDragging,
					asset.error && styles.assetRowError,
				)}
				role="group"
				aria-labelledby={labelId}
				onDragEnter={handleDragEnter}
				onDragOver={handleDragOver}
				onDragLeave={handleDragLeave}
				onDrop={handleDrop}
				data-branding-kind={asset.kind}
				data-branding-error={asset.error ?? undefined}
				data-flx="app.setup.setup-branding-step.branding-asset-field.asset-row"
				{...targetHandlers}
			>
				<div
					className={clsx(
						styles.thumbnail,
						asset.kind === 'sign_in_backdrop' && styles.thumbnailCover,
						source == null && styles.thumbnailEmpty,
					)}
					data-flx="app.setup.setup-branding-step.branding-asset-field.thumbnail"
				>
					{source != null ? (
						<img
							src={source}
							alt=""
							draggable={false}
							onLoad={(event) => {
								const image = event.currentTarget;
								setDimensions({source, width: image.naturalWidth, height: image.naturalHeight});
								if (asset.kind === 'wordmark') {
									const result = measureWordmarkReadability(image);
									setReadability(result ? {source, result} : null);
								}
							}}
							onError={() => onImageError(asset.kind)}
							data-flx="app.setup.setup-branding-step.branding-asset-field.img"
						/>
					) : (
						<ImageIcon
							size={20}
							weight="bold"
							aria-hidden="true"
							data-flx="app.setup.setup-branding-step.branding-asset-field.image-icon"
						/>
					)}
				</div>
				<div className={styles.assetBody} data-flx="app.setup.setup-branding-step.branding-asset-field.asset-body">
					{labelledBy == null && (
						<h3
							id={labelId}
							className={styles.assetLabel}
							data-flx="app.setup.setup-branding-step.branding-asset-field.asset-label"
						>
							{i18n._(copy.label)}
						</h3>
					)}
					<p
						id={hintId}
						className={styles.assetHint}
						data-flx="app.setup.setup-branding-step.branding-asset-field.asset-hint"
					>
						{i18n._(copy.hint)}
					</p>
					<div className={styles.helpToggle} data-flx="app.setup.setup-branding-step.branding-asset-field.help-toggle">
						<Button
							variant="ghost"
							small
							compact
							aria-expanded={helpOpen}
							aria-controls={helpId}
							rightIcon={
								<CaretDownIcon
									size={12}
									weight="bold"
									aria-hidden="true"
									className={styles.helpToggleIcon}
									data-flx="app.setup.setup-branding-step.branding-asset-field.help-toggle-icon"
								/>
							}
							onClick={() => setHelpOpen((open) => !open)}
							data-flx="app.setup.setup-branding-step.branding-asset-field.help-toggle-button"
						>
							{i18n._(MORE_ABOUT_IMAGE_DESCRIPTOR)}
						</Button>
					</div>
					<div
						id={helpId}
						className={clsx(styles.help, helpOpen && styles.helpOpen)}
						data-flx="app.setup.setup-branding-step.branding-asset-field.help"
					>
						<p data-flx="app.setup.setup-branding-step.branding-asset-field.help-where">{i18n._(copy.help, {size})}</p>
						{source == null && (
							<p data-flx="app.setup.setup-branding-step.branding-asset-field.help-empty">{i18n._(copy.empty)}</p>
						)}
					</div>
					{source == null && !errorText && (
						<p className={styles.assetMeta} data-flx="app.setup.setup-branding-step.branding-asset-field.asset-empty">
							{i18n._(asset.reading ? READING_IMAGE_DESCRIPTOR : CHOOSE_IMAGE_DESCRIPTOR)}
						</p>
					)}
					{shownDimensions != null && !isVector && (
						<p className={styles.assetMeta} data-flx="app.setup.setup-branding-step.branding-asset-field.asset-size">
							{i18n._(ASSET_SIZE_DESCRIPTOR, {width: shownDimensions.width, height: shownDimensions.height})}
						</p>
					)}
					{(errorText || warnings.length > 0) && (
						<div
							id={statusId}
							className={styles.assetStatus}
							data-flx="app.setup.setup-branding-step.branding-asset-field.asset-status"
						>
							{errorText && (
								<p
									className={styles.assetError}
									role="alert"
									data-flx="app.setup.setup-branding-step.branding-asset-field.asset-error"
								>
									<WarningCircleIcon
										size={16}
										weight="bold"
										aria-hidden="true"
										data-flx="app.setup.setup-branding-step.branding-asset-field.warning-circle-icon"
									/>
									{errorText}
								</p>
							)}
							{warnings.map((warning) => (
								<p
									key={warning}
									className={styles.assetWarning}
									data-flx="app.setup.setup-branding-step.branding-asset-field.asset-warning"
								>
									<WarningCircleIcon
										size={16}
										weight="bold"
										aria-hidden="true"
										data-flx="app.setup.setup-branding-step.branding-asset-field.warning-circle-icon--2"
									/>
									{warning}
								</p>
							))}
						</div>
					)}
					<div
						className={styles.assetActions}
						data-flx="app.setup.setup-branding-step.branding-asset-field.asset-actions"
					>
						{source == null ? (
							<Button
								variant="secondary"
								small
								disabled={busy}
								submitting={asset.reading}
								leftIcon={
									<UploadSimpleIcon
										size={16}
										weight="bold"
										data-flx="app.setup.setup-branding-step.branding-asset-field.upload-simple-icon"
									/>
								}
								onClick={() => onUpload(asset.kind)}
								aria-describedby={describedBy}
								data-flx="app.self-hosted-setup-wizard-gate.asset-upload-button"
							>
								{i18n._(CHOOSE_ASSET_DESCRIPTOR)}
							</Button>
						) : (
							<>
								<Button
									variant="secondary"
									small
									disabled={busy}
									submitting={asset.reading}
									leftIcon={
										<ArrowsClockwiseIcon
											size={16}
											weight="bold"
											data-flx="app.setup.setup-branding-step.branding-asset-field.arrows-clockwise-icon"
										/>
									}
									onClick={() => onUpload(asset.kind)}
									aria-describedby={describedBy}
									data-flx="app.self-hosted-setup-wizard-gate.asset-replace-button"
								>
									{i18n._(REPLACE_ASSET_DESCRIPTOR)}
								</Button>
								<Button
									variant="secondary"
									small
									disabled={busy}
									leftIcon={
										<TrashIcon
											size={16}
											weight="bold"
											data-flx="app.setup.setup-branding-step.branding-asset-field.trash-icon"
										/>
									}
									onClick={() => onClear(asset.kind)}
									aria-describedby={labelId}
									data-flx="app.self-hosted-setup-wizard-gate.asset-clear-button"
								>
									{i18n._(REMOVE_ASSET_DESCRIPTOR)}
								</Button>
							</>
						)}
					</div>
				</div>
			</div>
		);
	},
);
