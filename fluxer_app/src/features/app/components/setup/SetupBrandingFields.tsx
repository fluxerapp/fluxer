// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ASSET_COPY,
	BrandingAssetField,
	type TargetHandlers,
} from '@app/features/app/components/setup/SetupBrandingAssetField';
import {type BrandingAssetState, brandingAssetPolicyKind} from '@app/features/app/components/setup/SetupBrandingDraft';
import {
	type BrandingPreviewTab,
	type BrandingPreviewTarget,
	brandingPreviewTabsFor,
	SetupBrandingPreview,
} from '@app/features/app/components/setup/SetupBrandingPreview';
import styles from '@app/features/app/components/setup/SetupBrandingStep.module.css';
import type {SetupBrandingAssetKind} from '@app/features/app/components/setup/SetupWizardClient';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {int2hex} from '@app/features/theme/utils/ColorUtils';
import {contrastRatio, resolveThemeSurface} from '@app/features/theme/utils/InstanceBrandColors';
import {Button} from '@app/features/ui/button/Button';
import {ColorPickerField} from '@app/features/ui/components/form/ColorPickerField';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {getPolicy} from '@fluxer/constants/src/AssetFormatPolicy';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, InfoIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

export const DEFAULT_THEME_COLOR = 0x4641d9;

const THEME_COLOR_PRESETS: ReadonlyArray<{value: number; name: MessageDescriptor}> = [
	{
		value: DEFAULT_THEME_COLOR,
		name: msg({
			message: 'Indigo',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0x7c3aed,
		name: msg({
			message: 'Violet',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0xc026d3,
		name: msg({
			message: 'Fuchsia',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0xe11d48,
		name: msg({
			message: 'Rose',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0xea580c,
		name: msg({
			message: 'Orange',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0xca8a04,
		name: msg({
			message: 'Amber',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0x16a34a,
		name: msg({
			message: 'Green',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0x0d9488,
		name: msg({
			message: 'Teal',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0x0284c7,
		name: msg({
			message: 'Sky blue',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
	{
		value: 0x475569,
		name: msg({
			message: 'Slate gray',
			comment:
				'Color name. Names a suggested theme color in the setup wizard branding step and is read out by screen readers.',
		}),
	},
];

const TITLE_DESCRIPTOR = msg({
	message: 'Make it look like yours',
	comment: 'Setup wizard title for the theme color and images.',
});
const BODY_DESCRIPTOR = msg({
	message: 'Everything here is optional.',
	comment: 'Setup wizard body for the theme color and images.',
});
const THEME_COLOR_LABEL_DESCRIPTOR = msg({
	message: 'Theme color',
	comment: 'Label for the theme color picker in the setup wizard.',
});
const THEME_COLOR_HELP_DESCRIPTOR = msg({
	message:
		"Buttons, links and highlights across the app use this color, and phone browsers use it for their toolbar. Reset it to keep Fluxer's indigo.",
	comment: 'Help text under the theme color picker in the setup wizard branding step.',
});
const THEME_COLOR_PRESETS_DESCRIPTOR = msg({
	message: 'Suggested colors',
	comment: 'Accessible label for the row of suggested theme colors in the setup wizard branding step.',
});
const RESET_COLOR_DESCRIPTOR = msg({
	message: 'Reset',
	comment: 'Button that resets the theme color to the default indigo in the setup wizard branding step.',
});
const COLOR_DARK_TEXT_DESCRIPTOR = msg({
	message: 'Buttons in this color get dark text so the text stays readable.',
	comment:
		'Note under the theme color picker in the setup wizard when the chosen color is light enough that button text switches from white to dark.',
});
const COLOR_LOW_ON_DARK_DESCRIPTOR = msg({
	message: 'This color is close to the dark theme background, so buttons may be hard to see there.',
	comment: 'Warning under the theme color picker in the setup wizard when the chosen color is very dark.',
});
const COLOR_LOW_ON_LIGHT_DESCRIPTOR = msg({
	message: 'This color is close to the light theme background, so buttons may be hard to see there.',
	comment: 'Warning under the theme color picker in the setup wizard when the chosen color is very light.',
});
const LOGOS_HEADING_DESCRIPTOR = msg({
	message: 'Logos',
	comment: 'Heading above the logo and wordmark fields in the setup wizard branding step.',
});
const APP_ICONS_HEADING_DESCRIPTOR = msg({
	message: 'App icons',
	comment: 'Heading above the app icon, symbol and both favicon fields in the setup wizard branding step.',
});
function colorChannels(value: number): readonly [number, number, number] {
	return [(value >>> 16) & 0xff, (value >>> 8) & 0xff, value & 0xff];
}

function themeColorNotes(value: number): Array<{tone: 'info' | 'warning'; text: MessageDescriptor}> {
	const rgb = colorChannels(value);
	const notes: Array<{tone: 'info' | 'warning'; text: MessageDescriptor}> = [];
	if (contrastRatio(rgb, resolveThemeSurface(ThemeTypes.DARK, '--background-primary', 1)) < 1.5) {
		notes.push({tone: 'warning', text: COLOR_LOW_ON_DARK_DESCRIPTOR});
	}
	if (contrastRatio(rgb, resolveThemeSurface(ThemeTypes.LIGHT, '--background-primary', 1)) < 1.5) {
		notes.push({tone: 'warning', text: COLOR_LOW_ON_LIGHT_DESCRIPTOR});
	}
	if (contrastRatio(rgb, [20, 20, 20]) > contrastRatio(rgb, [250, 250, 250])) {
		notes.push({tone: 'info', text: COLOR_DARK_TEXT_DESCRIPTOR});
	}
	return notes;
}

interface ThemeColorSectionProps {
	themeColor: number | null;
	disabled: boolean;
	onChange: (value: number | null) => void;
	targetHandlers: TargetHandlers;
}

const ThemeColorSection = observer(({themeColor, disabled, onChange, targetHandlers}: ThemeColorSectionProps) => {
	const {i18n} = useLingui();
	const value = themeColor ?? DEFAULT_THEME_COLOR;
	const notes = useMemo(() => themeColorNotes(value), [value]);
	const presetsRef = useRef<HTMLDivElement>(null);
	const selectedIndex = THEME_COLOR_PRESETS.findIndex((preset) => preset.value === value);
	const focusIndex = selectedIndex >= 0 ? selectedIndex : 0;
	const rtl = getLocaleDirection(i18n.locale) === 'rtl';
	const selectPreset = (index: number) => {
		const preset = THEME_COLOR_PRESETS[index].value;
		onChange(preset === DEFAULT_THEME_COLOR ? null : preset);
	};
	const handlePresetKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>, index: number) => {
		let direction = getTabNavigationDirection(event.key, 'both');
		if (direction == null) return;
		if (rtl && (event.key === 'ArrowLeft' || event.key === 'ArrowRight')) {
			direction = direction === 'next' ? 'previous' : 'next';
		}
		const next = getNextTabIndex(index, THEME_COLOR_PRESETS.length, direction);
		if (next == null) return;
		event.preventDefault();
		selectPreset(next);
		presetsRef.current?.querySelectorAll<HTMLButtonElement>('[role="radio"]')[next]?.focus();
	};
	return (
		<section
			className={styles.group}
			aria-labelledby="setup-branding-theme-color"
			data-flx="app.setup.setup-branding-step.theme-color-section.group"
			{...targetHandlers}
		>
			<div className={styles.groupHeader} data-flx="app.setup.setup-branding-step.theme-color-section.group-header">
				<h2
					id="setup-branding-theme-color"
					className={styles.groupHeading}
					data-flx="app.setup.setup-branding-step.theme-color-section.heading"
				>
					{i18n._(THEME_COLOR_LABEL_DESCRIPTOR)}
				</h2>
				<Button
					variant="ghost"
					small
					compact
					disabled={disabled || themeColor === null}
					className={clsx(themeColor === null && styles.resetHidden)}
					onClick={(event: React.MouseEvent<HTMLButtonElement>) => {
						onChange(null);
						event.currentTarget
							.closest('section')
							?.querySelector<HTMLInputElement>('[data-flx="ui.form.color-picker-field.input.text"]')
							?.focus();
					}}
					data-flx="app.setup.setup-branding-step.theme-color-section.reset-button"
				>
					{i18n._(RESET_COLOR_DESCRIPTOR)}
				</Button>
			</div>
			<div
				ref={presetsRef}
				className={styles.presets}
				role="radiogroup"
				aria-label={i18n._(THEME_COLOR_PRESETS_DESCRIPTOR)}
				data-flx="app.setup.setup-branding-step.theme-color-section.presets"
			>
				{THEME_COLOR_PRESETS.map(({value: preset, name}, index) => {
					const selected = preset === value;
					const hex = int2hex(preset).toUpperCase();
					return (
						<FocusRing key={preset} offset={2} data-flx="app.setup.setup-branding-step.theme-color-section.focus-ring">
							<button
								type="button"
								role="radio"
								className={clsx(styles.preset, selected && styles.presetSelected)}
								style={{backgroundColor: hex}}
								aria-label={i18n._(name)}
								title={hex}
								aria-checked={selected}
								tabIndex={index === focusIndex ? 0 : -1}
								disabled={disabled}
								onClick={() => selectPreset(index)}
								onKeyDown={(event) => handlePresetKeyDown(event, index)}
								data-flx="app.setup.setup-branding-step.theme-color-section.preset"
							>
								{selected && (
									<CheckIcon
										size={14}
										weight="bold"
										aria-hidden="true"
										color={contrastRatio(colorChannels(preset), [255, 255, 255]) >= 3 ? '#fff' : '#000'}
										data-flx="app.setup.setup-branding-step.theme-color-section.check-icon"
									/>
								)}
							</button>
						</FocusRing>
					);
				})}
			</div>
			<ColorPickerField
				value={value}
				onChange={onChange}
				defaultValue={DEFAULT_THEME_COLOR}
				isDefaultValue={themeColor === null}
				onReset={() => onChange(null)}
				description={i18n._(THEME_COLOR_HELP_DESCRIPTOR)}
				descriptionClassName={styles.colorHelp}
				disabled={disabled}
				data-flx="app.self-hosted-setup-wizard-gate.theme-color-field"
			/>
			{notes.map((note) => (
				<p
					key={note.text.id}
					className={clsx(styles.note, note.tone === 'warning' && styles.noteWarning)}
					role={note.tone === 'warning' ? 'status' : undefined}
					data-flx="app.setup.setup-branding-step.theme-color-section.note"
				>
					{note.tone === 'warning' ? (
						<WarningCircleIcon
							size={16}
							weight="bold"
							aria-hidden="true"
							data-flx="app.setup.setup-branding-step.theme-color-section.warning-circle-icon"
						/>
					) : (
						<InfoIcon
							size={16}
							weight="bold"
							aria-hidden="true"
							data-flx="app.setup.setup-branding-step.theme-color-section.info-icon"
						/>
					)}
					<span data-flx="app.setup.setup-branding-step.theme-color-section.note-text">{i18n._(note.text)}</span>
				</p>
			))}
		</section>
	);
});

const ASSET_GROUPS: ReadonlyArray<{
	id: string;
	heading: MessageDescriptor;
	kinds: ReadonlyArray<SetupBrandingAssetKind>;
}> = [
	{id: 'logos', heading: LOGOS_HEADING_DESCRIPTOR, kinds: ['logo', 'wordmark']},
	{id: 'backdrop', heading: ASSET_COPY.sign_in_backdrop.label, kinds: ['sign_in_backdrop']},
	{id: 'app-icons', heading: APP_ICONS_HEADING_DESCRIPTOR, kinds: ['icon', 'symbol', 'favicon', 'favicon_dark']},
];

interface BrandingFieldsProps {
	formRef: React.RefObject<HTMLFormElement | null>;
	onSubmit: (event: React.FormEvent<HTMLFormElement>) => void;
	productName: string;
	themeColor: number | null;
	assets: ReadonlyArray<BrandingAssetState>;
	disabled: boolean;
	error: string | null;
	onThemeColorChange: (value: number | null) => void;
	onUploadAsset: (kind: SetupBrandingAssetKind, file?: File) => void;
	onClearAsset: (kind: SetupBrandingAssetKind) => void;
	onAssetImageError: (kind: SetupBrandingAssetKind) => void;
}

const COLOR_POPOVER_SELECTOR = '[data-flx="ui.form.color-picker-field.popover"]';

export const BrandingFields = observer(
	({
		formRef,
		onSubmit,
		productName,
		themeColor,
		assets,
		disabled,
		error,
		onThemeColorChange,
		onUploadAsset,
		onClearAsset,
		onAssetImageError,
	}: BrandingFieldsProps) => {
		const {i18n} = useLingui();
		const [tab, setTab] = useState<BrandingPreviewTab>('sign_in');
		const [focusTarget, setFocusTarget] = useState<BrandingPreviewTarget | null>(null);
		const [hoverTarget, setHoverTarget] = useState<BrandingPreviewTarget | null>(null);
		const stepRef = useRef<HTMLFormElement>(null);
		const mergedFormRef = useMergeRefs([stepRef, formRef]);
		const errorRef = useRef<HTMLParagraphElement>(null);
		const handledError = useRef<string | null>(null);
		const draft = useMemo(() => {
			const sources = Object.fromEntries(assets.map((asset) => [asset.kind, asset.preview ?? asset.url])) as Record<
				SetupBrandingAssetKind,
				string | null
			>;
			return {
				productName,
				themeColor: themeColor === null ? null : int2hex(themeColor),
				sources,
			};
		}, [assets, productName, themeColor]);
		const focusField = useCallback(
			(target: BrandingPreviewTarget) => {
				setFocusTarget(target);
				if (target === 'theme_color') return;
				setTab((current) => {
					const tabs = brandingPreviewTabsFor(target, draft);
					return tabs.includes(current) ? current : tabs[0];
				});
			},
			[draft],
		);
		const targetHandlers = useCallback(
			(target: BrandingPreviewTarget): TargetHandlers => ({
				onFocusCapture: () => focusField(target),
				onBlurCapture: (event) => {
					const next = event.relatedTarget;
					if (next instanceof Node && event.currentTarget.contains(next)) return;
					if (next instanceof Element && next.closest(COLOR_POPOVER_SELECTOR)) return;
					setFocusTarget(null);
				},
				onPointerEnter: () => setHoverTarget(target),
				onPointerLeave: () => setHoverTarget(null),
			}),
			[focusField],
		);
		const selectTarget = useCallback((target: BrandingPreviewTarget) => {
			const root = stepRef.current;
			if (!root) return;
			const control =
				target === 'theme_color'
					? root.querySelector<HTMLElement>('[data-flx="ui.form.color-picker-field.input.text"]')
					: root.querySelector<HTMLElement>(
							`[data-branding-kind="${target}"] :is([data-flx="app.self-hosted-setup-wizard-gate.asset-upload-button"], [data-flx="app.self-hosted-setup-wizard-gate.asset-replace-button"]):not(:disabled)`,
						);
			if (!control) return;
			control.focus({preventScroll: true});
			control.scrollIntoView({block: 'center', behavior: 'smooth'});
		}, []);
		useEffect(() => {
			const guard = (event: DragEvent) => {
				if (event.dataTransfer && Array.from(event.dataTransfer.types).includes('Files')) event.preventDefault();
			};
			window.addEventListener('dragover', guard);
			window.addEventListener('drop', guard);
			return () => {
				window.removeEventListener('dragover', guard);
				window.removeEventListener('drop', guard);
			};
		}, []);
		useEffect(() => {
			if (error == null) {
				handledError.current = null;
				return;
			}
			if (disabled || handledError.current === error) return;
			handledError.current = error;
			const failed = stepRef.current?.querySelector<HTMLElement>(
				':is([data-branding-error="not_saved"], [data-branding-error="not_loaded"]) button[data-flx^="app.self-hosted-setup-wizard-gate.asset-"]:not(:disabled)',
			);
			if (failed) {
				failed.focus();
				return;
			}
			errorRef.current?.scrollIntoView({block: 'nearest'});
		}, [error, disabled]);
		const highlight = focusTarget ?? hoverTarget;
		let caption: React.ReactNode = null;
		if (highlight != null && highlight !== 'theme_color') {
			const copy = ASSET_COPY[highlight];
			const size = formatFileSize(i18n.locale, getPolicy(brandingAssetPolicyKind(highlight)).maxBytes);
			caption = (
				<>
					<p data-flx="app.setup.setup-branding-step.caption.p">{i18n._(copy.help, {size})}</p>
					{draft.sources[highlight] == null && (
						<p data-flx="app.setup.setup-branding-step.caption.p--2">{i18n._(copy.empty)}</p>
					)}
				</>
			);
		}
		const assetByKind = new Map(assets.map((asset) => [asset.kind, asset]));
		return (
			<form
				ref={mergedFormRef}
				noValidate
				onSubmit={onSubmit}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.branding-step"
			>
				<div className={styles.header} data-flx="app.setup.setup-branding-step.branding-step.header">
					<h1
						className={styles.title}
						tabIndex={-1}
						data-step-focus="true"
						data-flx="app.setup.setup-branding-step.branding-step.title"
					>
						{i18n._(TITLE_DESCRIPTOR)}
					</h1>
					<p className={styles.body} data-flx="app.setup.setup-branding-step.branding-step.body">
						{i18n._(BODY_DESCRIPTOR)}
					</p>
					{error && (
						<p
							ref={errorRef}
							className={styles.stepError}
							role="alert"
							data-flx="app.setup.setup-branding-step.branding-step.step-error"
						>
							<WarningCircleIcon
								size={16}
								weight="bold"
								aria-hidden="true"
								data-flx="app.setup.setup-branding-step.branding-step.step-error-icon"
							/>
							{error}
						</p>
					)}
				</div>
				<div className={styles.fieldsPane} data-flx="app.setup.setup-branding-step.branding-step.fields-pane">
					<Scroller contentClassName={styles.fields} data-flx="app.setup.setup-branding-step.branding-step.scroller">
						<ThemeColorSection
							themeColor={themeColor}
							disabled={disabled}
							onChange={onThemeColorChange}
							targetHandlers={targetHandlers('theme_color')}
							data-flx="app.setup.setup-branding-step.branding-step.theme-color-section"
						/>
						{ASSET_GROUPS.map((group) => {
							const headingId = `setup-branding-group-${group.id}`;
							const solo = group.kinds.length === 1;
							return (
								<section
									key={group.id}
									className={styles.group}
									aria-labelledby={headingId}
									data-flx="app.setup.setup-branding-step.branding-step.group"
								>
									<h2
										id={headingId}
										className={styles.groupHeading}
										data-flx="app.setup.setup-branding-step.branding-step.group-heading"
									>
										{i18n._(group.heading)}
									</h2>
									{group.kinds.map((kind) => {
										const asset = assetByKind.get(kind);
										if (!asset) return null;
										return (
											<BrandingAssetField
												key={kind}
												asset={asset}
												labelledBy={solo ? headingId : null}
												disabled={disabled}
												onUpload={onUploadAsset}
												onClear={onClearAsset}
												onImageError={onAssetImageError}
												targetHandlers={targetHandlers(kind)}
												data-flx="app.setup.setup-branding-step.branding-step.branding-asset-field"
											/>
										);
									})}
								</section>
							);
						})}
					</Scroller>
				</div>
				<div className={styles.previewPane} data-flx="app.setup.setup-branding-step.branding-step.preview-pane">
					<SetupBrandingPreview
						draft={draft}
						tab={tab}
						highlight={highlight}
						caption={caption}
						onTabChange={setTab}
						onTargetSelect={selectTarget}
						data-flx="app.setup.setup-branding-step.branding-step.setup-branding-preview"
					/>
				</div>
			</form>
		);
	},
);
