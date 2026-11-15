// SPDX-License-Identifier: AGPL-3.0-or-later

import {ColorPickerPopover} from '@app/features/app/components/floating/ColorPickerPopover';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {ThemeVariableKind} from '@app/features/theme/variables/ThemeVariableManifest';
import styles from '@app/features/theme_settings/components/TokenRow.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
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

const EDIT_COLOR_FOR_DESCRIPTOR = msg({
	message: 'Edit color for {tokenLabel}',
	comment: 'Accessible label for the color swatch button of a color in Settings > Themes > "Colors and fonts".',
});
const COLOR_PICKER_FOR_DESCRIPTOR = msg({
	message: 'Color picker for {tokenLabel}',
	comment: 'Accessible label for the color picker popover of a color in Settings > Themes.',
});
const RESET_TO_DEFAULT_DESCRIPTOR = msg({
	message: 'Reset {tokenLabel} to default',
	comment: 'Accessible label for resetting a color or font in Settings > Themes to its default.',
});
const VALUE_FOR_DESCRIPTOR = msg({
	message: 'Value for {tokenLabel}',
	comment: 'Accessible label for the text field of a font or other value in Settings > Themes.',
});
const HEX_VALUE_FOR_DESCRIPTOR = msg({
	message: 'Hex value for {tokenLabel}',
	comment: 'Accessible label for the hex text field of a color in Settings > Themes.',
});

interface TokenRowProps {
	variableName: string;
	label: string;
	kind: ThemeVariableKind;
	currentValue: string;
	defaultValue: string;
	onChange: (value: string | null) => void;
}

function useDraft(value: string): [string, (next: string) => void] {
	const [draft, setDraft] = useState(value);
	useEffect(() => {
		setDraft(value);
	}, [value]);
	return [draft, setDraft];
}

function commitOnEnter(commit: () => void) {
	return (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (event.key !== 'Enter') return;
		event.preventDefault();
		commit();
		event.currentTarget.blur();
	};
}

const ColorControl: React.FC<TokenRowProps> = ({label, currentValue, defaultValue, onChange}) => {
	const {i18n} = useLingui();
	const [open, setOpen] = useState(false);
	const colorValue = currentValue || defaultValue;
	const swatchCss = stripCssColorPriority(colorValue) || 'transparent';
	const [draft, setDraft] = useDraft(cssColorStringToHex(colorValue) ?? colorValue);
	const commit = useCallback(() => {
		const trimmed = draft.trim();
		if (trimmed.length === 0) {
			onChange(null);
			return;
		}
		const nextHex = cssColorStringToHex(trimmed);
		if (nextHex === null) {
			setDraft(cssColorStringToHex(colorValue) ?? colorValue);
			return;
		}
		const currentHex = cssColorStringToHex(colorValue);
		if (currentHex !== null && nextHex.toLowerCase() === currentHex.toLowerCase()) {
			setDraft(nextHex);
			return;
		}
		onChange(nextHex.toLowerCase());
	}, [colorValue, draft, onChange, setDraft]);
	return (
		<FocusRing within offset={-2} data-flx="theme-settings.token-row.color-control.focus-ring">
			<div
				className={clsx(styles.field, surfaceStyles.surface)}
				data-flx="theme-settings.token-row.color-control.field"
			>
				<input
					type="text"
					className={clsx(styles.input, styles.colorInput)}
					value={draft}
					placeholder={cssColorStringToHex(defaultValue) ?? defaultValue}
					spellCheck={false}
					onChange={(event) => setDraft(event.target.value)}
					onBlur={commit}
					onKeyDown={commitOnEnter(commit)}
					aria-label={i18n._(HEX_VALUE_FOR_DESCRIPTOR, {tokenLabel: label})}
					data-flx="theme-settings.token-row.color-control.input.set-draft.text"
				/>
				<div className={styles.divider} data-flx="theme-settings.token-row.color-control.divider" />
				<DialogTrigger
					isOpen={open}
					onOpenChange={setOpen}
					data-flx="theme-settings.token-row.color-control.dialog-trigger"
				>
					<AriaButton
						className={styles.swatch}
						style={{backgroundColor: swatchCss}}
						aria-label={i18n._(EDIT_COLOR_FOR_DESCRIPTOR, {tokenLabel: label})}
						data-flx="theme-settings.token-row.color-control.swatch"
					/>
					<Popover
						placement="bottom end"
						offset={8}
						className={styles.popover}
						data-flx="theme-settings.token-row.color-control.popover"
					>
						<Dialog
							aria-label={i18n._(COLOR_PICKER_FOR_DESCRIPTOR, {tokenLabel: label})}
							data-flx="theme-settings.token-row.color-control.dialog"
						>
							<ColorPickerPopover
								color={cssColorStringToHex(colorValue) ?? '#000000'}
								onChange={(hex) => onChange(hex.toLowerCase())}
								onReset={() => {
									onChange(null);
									setOpen(false);
								}}
								data-flx="theme-settings.token-row.color-control.color-picker-popover.change"
							/>
						</Dialog>
					</Popover>
				</DialogTrigger>
			</div>
		</FocusRing>
	);
};

const TextControl: React.FC<TokenRowProps> = ({kind, label, currentValue, defaultValue, onChange}) => {
	const {i18n} = useLingui();
	const [draft, setDraft] = useDraft(currentValue);
	const commit = useCallback(() => {
		const trimmed = draft.trim();
		if (trimmed === currentValue) return;
		onChange(trimmed.length === 0 ? null : trimmed);
	}, [currentValue, draft, onChange]);
	const isFont = kind === 'font';
	return (
		<FocusRing within offset={-2} data-flx="theme-settings.token-row.text-control.focus-ring">
			<div
				className={clsx(styles.field, styles.textField, surfaceStyles.surface)}
				data-flx="theme-settings.token-row.text-control.field"
			>
				{isFont ? (
					<span
						className={styles.fontPreview}
						style={{fontFamily: currentValue || defaultValue || 'inherit'}}
						aria-hidden
						data-flx="theme-settings.token-row.text-control.font-preview"
					>
						Aa
					</span>
				) : null}
				<input
					type="text"
					className={styles.input}
					value={draft}
					placeholder={defaultValue}
					spellCheck={false}
					onChange={(event) => setDraft(event.target.value)}
					onBlur={commit}
					onKeyDown={commitOnEnter(commit)}
					aria-label={i18n._(VALUE_FOR_DESCRIPTOR, {tokenLabel: label})}
					data-flx="theme-settings.token-row.text-control.input.set-draft.text"
				/>
			</div>
		</FocusRing>
	);
};

export const TokenRow: React.FC<TokenRowProps> = (props) => {
	const {i18n} = useLingui();
	const {variableName, label, kind, currentValue, onChange} = props;
	const overridden = currentValue.length > 0;
	return (
		<div className={styles.row} data-flx="theme-settings.token-row.row">
			<div className={styles.text} data-flx="theme-settings.token-row.text">
				<span className={styles.label} data-flx="theme-settings.token-row.label">
					{label}
				</span>
				<span className={styles.variableName} data-flx="theme-settings.token-row.variable-name">
					{variableName}
				</span>
			</div>
			<div className={styles.controls} data-flx="theme-settings.token-row.controls">
				{kind === 'color' ? (
					<ColorControl data-flx="theme-settings.token-row.color-control" {...props} />
				) : (
					<TextControl data-flx="theme-settings.token-row.text-control" {...props} />
				)}
				{overridden ? (
					<FocusRing offset={-2} data-flx="theme-settings.token-row.focus-ring">
						<button
							type="button"
							className={styles.resetButton}
							aria-label={i18n._(RESET_TO_DEFAULT_DESCRIPTOR, {tokenLabel: label})}
							onClick={() => onChange(null)}
							data-flx="theme-settings.token-row.reset-button.change"
						>
							<ArrowCounterClockwiseIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="theme-settings.token-row.arrow-counter-clockwise-icon"
							/>
						</button>
					</FocusRing>
				) : (
					<span className={styles.resetSpacer} data-flx="theme-settings.token-row.reset-spacer" />
				)}
			</div>
		</div>
	);
};
