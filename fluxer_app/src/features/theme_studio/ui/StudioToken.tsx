// SPDX-License-Identifier: AGPL-3.0-or-later

import {ColorPickerPopover} from '@app/features/app/components/floating/ColorPickerPopover';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ThemeVariableKind} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeConstants';
import {
	cssColorStringToHex,
	stripCssColorPriority,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowCounterClockwiseIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {Button as AriaButton, Dialog, DialogTrigger, Popover} from 'react-aria-components';
import styles from './StudioToken.module.css';

const EDIT_COLOR_FOR_DESCRIPTOR = msg({
	message: 'Edit color for {tokenLabel}',
	comment: 'Accessible label for a Theme Studio color-token swatch button.',
});
const COLOR_PICKER_FOR_DESCRIPTOR = msg({
	message: 'Color picker for {tokenLabel}',
	comment: 'Accessible label for a Theme Studio color picker popover.',
});
const RESET_TO_DEFAULT_DESCRIPTOR = msg({
	message: 'Reset {tokenLabel} to default',
	comment: 'Accessible label for resetting a Theme Studio token value to its default.',
});
const VALUE_FOR_DESCRIPTOR = msg({
	message: 'Value for {tokenLabel}',
	comment: 'Accessible label for editing a Theme Studio token value.',
});
const HEX_VALUE_FOR_DESCRIPTOR = msg({
	message: 'Hex value for {tokenLabel}',
	comment: 'Accessible label for editing a Theme Studio color token hex value.',
});

interface StudioTokenColorProps {
	variableName: string;
	label: string;
	currentValue: string;
	defaultValue: string;
	overridden: boolean;
	onChange: (hex: string | null) => void;
}

export const StudioTokenColor: React.FC<StudioTokenColorProps> = ({
	variableName,
	label,
	currentValue,
	defaultValue,
	overridden,
	onChange,
}) => {
	const {i18n} = useLingui();
	const [open, setOpen] = useState(false);
	const colorValue = currentValue || defaultValue;
	const swatchCss = stripCssColorPriority(colorValue) || 'transparent';
	const initialHex = cssColorStringToHex(colorValue) ?? '#000000';
	const handlePickerChange = useCallback(
		(hex: string) => {
			onChange(hex.toLowerCase());
		},
		[onChange],
	);
	const handleReset = useCallback(() => {
		onChange(null);
		setOpen(false);
	}, [onChange]);
	return (
		<div
			className={clsx(styles.row, overridden && styles.overridden)}
			data-flx="theme-studio.ui.studio-token.studio-token-color.row"
		>
			<DialogTrigger
				isOpen={open}
				onOpenChange={setOpen}
				data-flx="theme-studio.ui.studio-token.studio-token-color.dialog-trigger"
			>
				<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-color.focus-ring">
					<AriaButton
						className={styles.swatchButton}
						style={{backgroundColor: swatchCss}}
						aria-label={i18n._(EDIT_COLOR_FOR_DESCRIPTOR, {tokenLabel: label})}
						data-flx="theme-studio.ui.studio-token.studio-token-color.swatch-button"
					/>
				</FocusRing>
				<Popover
					placement="bottom start"
					offset={8}
					className={styles.pickerPopover}
					data-flx="theme-studio.ui.studio-token.studio-token-color.picker-popover"
				>
					<Dialog
						aria-label={i18n._(COLOR_PICKER_FOR_DESCRIPTOR, {tokenLabel: label})}
						data-flx="theme-studio.ui.studio-token.studio-token-color.dialog"
					>
						<ColorPickerPopover
							color={initialHex}
							onChange={handlePickerChange}
							onReset={handleReset}
							data-flx="theme-studio.ui.studio-token.studio-token-color.color-picker-popover.picker-change"
						/>
					</Dialog>
				</Popover>
			</DialogTrigger>
			<span className={styles.label} data-flx="theme-studio.ui.studio-token.studio-token-color.label">
				<span className={styles.labelText} data-flx="theme-studio.ui.studio-token.studio-token-color.label-text">
					{label}
				</span>
				<span className={styles.variableName} data-flx="theme-studio.ui.studio-token.studio-token-color.variable-name">
					{variableName}
				</span>
			</span>
			<HexInput
				label={label}
				value={colorValue}
				placeholder={defaultValue}
				onCommit={onChange}
				data-flx="theme-studio.ui.studio-token.studio-token-color.hex-input"
			/>
			{overridden ? (
				<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-color.focus-ring--2">
					<button
						type="button"
						className={styles.resetButton}
						aria-label={i18n._(RESET_TO_DEFAULT_DESCRIPTOR, {tokenLabel: label})}
						onClick={() => onChange(null)}
						data-flx="theme-studio.ui.studio-token.studio-token-color.reset-button.change"
					>
						<ArrowCounterClockwiseIcon
							size={remFromPx(13)}
							weight="bold"
							data-flx="theme-studio.ui.studio-token.studio-token-color.arrow-counter-clockwise-icon"
						/>
					</button>
				</FocusRing>
			) : (
				<span className={styles.resetSpacer} data-flx="theme-studio.ui.studio-token.studio-token-color.reset-spacer" />
			)}
		</div>
	);
};

interface StudioTokenFontProps {
	variableName: string;
	label: string;
	currentValue: string;
	defaultValue: string;
	overridden: boolean;
	onChange: (value: string | null) => void;
}

export const StudioTokenFont: React.FC<StudioTokenFontProps> = ({
	variableName,
	label,
	currentValue,
	defaultValue,
	overridden,
	onChange,
}) => {
	const {i18n} = useLingui();
	const [draft, setDraft] = useState(currentValue);
	useEffect(() => {
		setDraft(currentValue);
	}, [currentValue]);
	const commit = useCallback(() => {
		const trimmed = draft.trim();
		if (trimmed === currentValue) return;
		onChange(trimmed.length === 0 ? null : trimmed);
	}, [draft, currentValue, onChange]);
	const previewFamily = currentValue || defaultValue || 'inherit';
	return (
		<div
			className={clsx(styles.row, overridden && styles.overridden)}
			data-flx="theme-studio.ui.studio-token.studio-token-font.row"
		>
			<span
				className={clsx(styles.swatchButton, styles.fontSwatch)}
				style={{fontFamily: previewFamily}}
				aria-hidden
				data-flx="theme-studio.ui.studio-token.studio-token-font.swatch-button"
			>
				Aa
			</span>
			<span className={styles.label} data-flx="theme-studio.ui.studio-token.studio-token-font.label">
				<span className={styles.labelText} data-flx="theme-studio.ui.studio-token.studio-token-font.label-text">
					{label}
				</span>
				<span className={styles.variableName} data-flx="theme-studio.ui.studio-token.studio-token-font.variable-name">
					{variableName}
				</span>
			</span>
			<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-font.focus-ring">
				<input
					type="text"
					className={clsx(styles.valueInput, styles.fontInput)}
					value={draft}
					placeholder={defaultValue}
					spellCheck={false}
					onChange={(event) => setDraft(event.target.value)}
					onBlur={commit}
					onKeyDown={(event) => {
						if (event.key === 'Enter') {
							event.preventDefault();
							commit();
							event.currentTarget.blur();
						}
					}}
					aria-label={i18n._(VALUE_FOR_DESCRIPTOR, {tokenLabel: label})}
					data-flx="theme-studio.ui.studio-token.studio-token-font.value-input.set-draft.text"
				/>
			</FocusRing>
			{overridden ? (
				<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-font.focus-ring--2">
					<button
						type="button"
						className={styles.resetButton}
						aria-label={i18n._(RESET_TO_DEFAULT_DESCRIPTOR, {tokenLabel: label})}
						onClick={() => onChange(null)}
						data-flx="theme-studio.ui.studio-token.studio-token-font.reset-button.change"
					>
						<ArrowCounterClockwiseIcon
							size={remFromPx(13)}
							weight="bold"
							data-flx="theme-studio.ui.studio-token.studio-token-font.arrow-counter-clockwise-icon"
						/>
					</button>
				</FocusRing>
			) : (
				<span className={styles.resetSpacer} data-flx="theme-studio.ui.studio-token.studio-token-font.reset-spacer" />
			)}
		</div>
	);
};

interface StudioTokenValueProps {
	variableName: string;
	label: string;
	kind: ThemeVariableKind;
	currentValue: string;
	defaultValue: string;
	overridden: boolean;
	onChange: (value: string | null) => void;
}

function getKindMarker(kind: ThemeVariableKind): string {
	switch (kind) {
		case 'dimension':
			return 'px';
		case 'number':
			return '#';
		case 'shadow':
			return 'sh';
		case 'transition':
			return 'ms';
		case 'other':
			return '{}';
		default:
			return 'var';
	}
}

export const StudioTokenValue: React.FC<StudioTokenValueProps> = ({
	variableName,
	label,
	kind,
	currentValue,
	defaultValue,
	overridden,
	onChange,
}) => {
	const {i18n} = useLingui();
	const [draft, setDraft] = useState(currentValue);
	useEffect(() => {
		setDraft(currentValue);
	}, [currentValue]);
	const commit = useCallback(() => {
		const trimmed = draft.trim();
		if (trimmed === currentValue) return;
		onChange(trimmed.length === 0 ? null : trimmed);
	}, [draft, currentValue, onChange]);
	return (
		<div
			className={clsx(styles.row, overridden && styles.overridden)}
			data-flx="theme-studio.ui.studio-token.studio-token-value.row"
		>
			<span
				className={clsx(styles.swatchButton, styles.valueSwatch)}
				aria-hidden
				data-flx="theme-studio.ui.studio-token.studio-token-value.swatch-button"
			>
				{getKindMarker(kind)}
			</span>
			<span className={styles.label} data-flx="theme-studio.ui.studio-token.studio-token-value.label">
				<span className={styles.labelText} data-flx="theme-studio.ui.studio-token.studio-token-value.label-text">
					{label}
				</span>
				<span className={styles.variableName} data-flx="theme-studio.ui.studio-token.studio-token-value.variable-name">
					{variableName}
				</span>
			</span>
			<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-value.focus-ring">
				<input
					type="text"
					className={clsx(styles.valueInput, styles.genericInput)}
					value={draft}
					placeholder={defaultValue}
					spellCheck={false}
					onChange={(event) => setDraft(event.target.value)}
					onBlur={commit}
					onKeyDown={(event) => {
						if (event.key === 'Enter') {
							event.preventDefault();
							commit();
							event.currentTarget.blur();
						}
					}}
					aria-label={i18n._(VALUE_FOR_DESCRIPTOR, {tokenLabel: label})}
					data-flx="theme-studio.ui.studio-token.studio-token-value.value-input.set-draft.text"
				/>
			</FocusRing>
			{overridden ? (
				<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.studio-token-value.focus-ring--2">
					<button
						type="button"
						className={styles.resetButton}
						aria-label={i18n._(RESET_TO_DEFAULT_DESCRIPTOR, {tokenLabel: label})}
						onClick={() => onChange(null)}
						data-flx="theme-studio.ui.studio-token.studio-token-value.reset-button.change"
					>
						<ArrowCounterClockwiseIcon
							size={remFromPx(13)}
							weight="bold"
							data-flx="theme-studio.ui.studio-token.studio-token-value.arrow-counter-clockwise-icon"
						/>
					</button>
				</FocusRing>
			) : (
				<span className={styles.resetSpacer} data-flx="theme-studio.ui.studio-token.studio-token-value.reset-spacer" />
			)}
		</div>
	);
};

interface HexInputProps {
	label: string;
	value: string;
	placeholder: string;
	onCommit: (hex: string | null) => void;
}

const HexInput: React.FC<HexInputProps> = ({label, value, placeholder, onCommit}) => {
	const {i18n} = useLingui();
	const initial = cssColorStringToHex(value) ?? value;
	const [draft, setDraft] = useState(initial);
	useEffect(() => {
		setDraft(cssColorStringToHex(value) ?? value);
	}, [value]);
	const placeholderHex = cssColorStringToHex(placeholder) ?? placeholder;
	const commit = useCallback(() => {
		const trimmed = draft.trim();
		if (trimmed.length === 0) {
			onCommit(null);
			return;
		}
		const nextHex = cssColorStringToHex(trimmed);
		if (nextHex !== null) {
			const currentHex = cssColorStringToHex(value);
			if (currentHex !== null && nextHex.toLowerCase() === currentHex.toLowerCase()) {
				setDraft(nextHex);
				return;
			}
			onCommit(nextHex.toLowerCase());
		} else {
			setDraft(initial);
		}
	}, [draft, initial, onCommit, value]);
	return (
		<FocusRing offset={-2} data-flx="theme-studio.ui.studio-token.hex-input.focus-ring">
			<input
				type="text"
				className={styles.valueInput}
				value={draft}
				placeholder={placeholderHex}
				spellCheck={false}
				onChange={(event) => setDraft(event.target.value)}
				onBlur={commit}
				onKeyDown={(event) => {
					if (event.key === 'Enter') {
						event.preventDefault();
						commit();
						event.currentTarget.blur();
					}
				}}
				aria-label={i18n._(HEX_VALUE_FOR_DESCRIPTOR, {tokenLabel: label})}
				data-flx="theme-studio.ui.studio-token.hex-input.value-input.set-draft.text"
			/>
		</FocusRing>
	);
};
