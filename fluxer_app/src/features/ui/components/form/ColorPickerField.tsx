// SPDX-License-Identifier: AGPL-3.0-or-later

import {ColorPickerPopover} from '@app/features/app/components/floating/ColorPickerPopover';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/components/form/ColorPickerField.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EyedropperIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import Color from 'colorjs.io';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {Button, Dialog, DialogTrigger, Popover} from 'react-aria-components';

const COLOR_VALUE_DESCRIPTOR = msg({
	message: 'Color value',
	comment: 'Accessible label for the current color input value.',
});
const OPEN_COLOR_PICKER_DESCRIPTOR = msg({
	message: 'Open color picker',
	comment: 'Accessible label for the button that opens the color picker.',
});
const COLOR_PICKER_DESCRIPTOR = msg({
	message: 'Color picker',
	comment: 'Accessible label for the color picker dialog.',
});

function clampByte(n: number) {
	return Math.max(0, Math.min(255, Math.round(n)));
}

function rgbToHex(r: number, g: number, b: number) {
	return `#${clampByte(r).toString(16).padStart(2, '0')}${clampByte(g).toString(16).padStart(2, '0')}${clampByte(b).toString(16).padStart(2, '0')}`.toUpperCase();
}

function hexToNumber(hex: string): number {
	const clean = hex.replace('#', '');
	return parseInt(clean.slice(0, 6), 16) >>> 0;
}

function numberToHex(n: number): string {
	return `#${(n >>> 0).toString(16).padStart(6, '0').slice(-6)}`.toUpperCase();
}

function expandShortHex(h: string) {
	if (h.length === 4 || h.length === 5) {
		const chars = h.slice(1).split('');
		const expanded = chars.map((c) => c + c).join('');
		return `#${expanded}`;
	}
	return h;
}

function parseColor(input: string): {hex: string; num: number} | null {
	const raw = (input || '').trim();
	if (raw.startsWith('#')) {
		let h = raw.toUpperCase();
		h = expandShortHex(h);
		if (h.length === 9) h = h.slice(0, 7);
		if (/^#[0-9A-F]{6}$/.test(h)) return {hex: h, num: hexToNumber(h)};
		return null;
	}
	{
		const ctx = document.createElement('canvas').getContext('2d');
		if (ctx) {
			ctx.fillStyle = '#000';
			ctx.fillStyle = raw as string;
			const parsedRaw = String(ctx.fillStyle);
			ctx.fillStyle = '#123456';
			ctx.fillStyle = raw as string;
			const secondRaw = String(ctx.fillStyle);
			const looksValid = parsedRaw !== '#000000' || secondRaw !== '#123456';
			if (looksValid) {
				const m = /^rgba?\((\d+),\s*(\d+),\s*(\d+)/i.exec(parsedRaw);
				if (m) {
					const r = parseInt(m[1], 10);
					const g = parseInt(m[2], 10);
					const b = parseInt(m[3], 10);
					const hex = rgbToHex(r, g, b);
					return {hex, num: hexToNumber(hex)};
				}
				if (/^#[0-9A-Fa-f]{6}$/.test(parsedRaw)) {
					const hex = parsedRaw.toUpperCase();
					return {hex, num: hexToNumber(hex)};
				}
			}
		}
	}
	return null;
}

function bestIconColorFor(bgColorCss: string): 'black' | 'white' {
	if (bgColorCss === 'var(--text-chat)') {
		const isLightTheme = document.documentElement.classList.contains('theme-light');
		return isLightTheme ? 'white' : 'black';
	}
	try {
		const bgColor = new Color(bgColorCss);
		const contrastWithWhite = Math.abs(bgColor.contrast('#FFFFFF', 'WCAG21'));
		const contrastWithBlack = Math.abs(bgColor.contrast('#000000', 'WCAG21'));
		return contrastWithWhite >= contrastWithBlack ? 'white' : 'black';
	} catch {
		return 'white';
	}
}

interface ColorPickerFieldProps {
	label?: string;
	description?: string;
	value: number;
	onChange: (value: number) => void;
	disabled?: boolean;
	className?: string;
	defaultValue?: number;
	isDefaultValue?: boolean;
	onReset?: () => void;
	hideHelperText?: boolean;
	descriptionClassName?: string;
}

export const ColorPickerField: React.FC<ColorPickerFieldProps> = observer((props) => {
	const {i18n} = useLingui();
	const {
		label,
		description,
		value,
		onChange,
		disabled,
		className,
		defaultValue,
		isDefaultValue = false,
		onReset,
		hideHelperText,
		descriptionClassName,
	} = props;
	const containerRef = useRef<HTMLFieldSetElement>(null);
	const inputRef = useRef<HTMLInputElement>(null);
	const isShowingDefault = isDefaultValue && defaultValue !== undefined;
	const getEffectiveValue = useCallback(() => {
		return isShowingDefault ? defaultValue : value;
	}, [value, defaultValue, isShowingDefault]);
	const [inputValue, setInputValue] = useState(() => numberToHex(getEffectiveValue()));
	const [showError, setShowError] = useState(false);
	const [popoutOpen, setPopoutOpen] = useState(false);
	useEffect(() => {
		if (!popoutOpen) {
			const effectiveValue = getEffectiveValue();
			setInputValue(numberToHex(effectiveValue));
		}
	}, [getEffectiveValue, popoutOpen]);
	const commitFromText = useCallback(() => {
		const parsed = parseColor(inputValue);
		const effectiveValue = getEffectiveValue();
		if (!parsed) {
			setShowError(true);
			setInputValue(numberToHex(effectiveValue));
			return;
		}
		if (parsed.num !== effectiveValue) {
			onChange(parsed.num);
		}
		setInputValue(parsed.hex);
		setShowError(false);
	}, [inputValue, getEffectiveValue, onChange]);
	const handleInputBlur = useCallback(() => {
		commitFromText();
	}, [commitFromText]);
	const handleInputKeyDown: React.KeyboardEventHandler<HTMLInputElement> = useCallback(
		(e) => {
			if (e.key === 'Enter') {
				e.preventDefault();
				commitFromText();
				containerRef.current?.querySelector<HTMLButtonElement>('button[data-role="swatch"]')?.focus();
			}
		},
		[commitFromText],
	);
	const handleInputChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
		setInputValue(e.target.value);
		setShowError(false);
	}, []);
	const handleColorChange = useCallback(
		(colorHex: string) => {
			const parsed = parseColor(colorHex);
			if (parsed) {
				onChange(parsed.num);
				setInputValue(parsed.hex);
				setShowError(false);
			}
		},
		[onChange],
	);
	const handleReset = useCallback(() => {
		if (onReset) {
			onReset();
		} else {
			onChange(0);
		}
		const resetHex = defaultValue !== undefined ? numberToHex(defaultValue) : '#000000';
		setInputValue(resetHex);
		setShowError(false);
		setPopoutOpen(false);
	}, [onChange, onReset, defaultValue]);
	const effectiveValue = getEffectiveValue();
	const logicalHex = numberToHex(effectiveValue);
	const swatchBackgroundCss =
		isShowingDefault || defaultValue !== undefined || value !== 0 ? logicalHex : 'var(--text-chat)';
	const iconOnSwatch = bestIconColorFor(swatchBackgroundCss);
	return (
		<FocusRing within={true} offset={-2} enabled={!disabled} data-flx="ui.form.color-picker-field.focus-ring">
			<fieldset
				ref={containerRef}
				className={clsx(styles.fieldset, className)}
				data-flx="ui.form.color-picker-field.fieldset"
			>
				{label && (
					<div className={styles.labelContainer} data-flx="ui.form.color-picker-field.label-container">
						<legend className={styles.label} data-flx="ui.form.color-picker-field.label">
							{label}
						</legend>
					</div>
				)}
				<div className={styles.inputContainer} data-flx="ui.form.color-picker-field.input-container">
					<div
						className={clsx(styles.inputWrapper, surfaceStyles.surface)}
						data-flx="ui.form.color-picker-field.input-wrapper"
					>
						<input
							ref={inputRef}
							type="text"
							data-flx="ui.form.color-picker-field.input.text"
							{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
							value={inputValue}
							onChange={handleInputChange}
							onBlur={handleInputBlur}
							onKeyDown={handleInputKeyDown}
							placeholder="#000000, rgb(...), red"
							maxLength={64}
							disabled={disabled}
							className={clsx(styles.input, showError && styles.inputError)}
							aria-label={i18n._(COLOR_VALUE_DESCRIPTOR)}
							aria-invalid={showError}
						/>
						<div className={styles.divider} data-flx="ui.form.color-picker-field.divider" />
						<DialogTrigger
							isOpen={popoutOpen}
							onOpenChange={setPopoutOpen}
							data-flx="ui.form.color-picker-field.dialog-trigger"
						>
							<Button
								data-role="swatch"
								className={styles.swatchButton}
								style={{backgroundColor: swatchBackgroundCss}}
								aria-label={i18n._(OPEN_COLOR_PICKER_DESCRIPTOR)}
								isDisabled={disabled}
								data-flx="ui.form.color-picker-field.swatch"
							>
								<EyedropperIcon
									size={remFromPx(18)}
									weight="fill"
									style={{color: iconOnSwatch === 'white' ? '#FFFFFF' : '#000000'}}
									className={styles.swatchIcon}
									data-flx="ui.form.color-picker-field.swatch-icon"
								/>
							</Button>
							<Popover
								placement="bottom start"
								offset={8}
								className={styles.popover}
								data-flx="ui.form.color-picker-field.popover"
							>
								<Dialog
									className={styles.dialog}
									aria-label={i18n._(COLOR_PICKER_DESCRIPTOR)}
									data-flx="ui.form.color-picker-field.dialog"
								>
									<ColorPickerPopover
										color={numberToHex(effectiveValue)}
										onChange={handleColorChange}
										onReset={handleReset}
										data-flx="ui.form.color-picker-field.color-picker-popover.color-change"
									/>
								</Dialog>
							</Popover>
						</DialogTrigger>
					</div>
					{(description || !hideHelperText) && (
						<p
							className={clsx(styles.description, descriptionClassName)}
							data-flx="ui.form.color-picker-field.description"
						>
							{description ?? <Trans>Type a color (hex, rgb(), hsl, or name), or use the picker.</Trans>}
						</p>
					)}
					{showError && (
						<p className={styles.errorText} data-flx="ui.form.color-picker-field.error-text">
							<Trans>That doesn't look like a valid color. Try hex, rgb(), hsl(), or a CSS color name.</Trans>
						</p>
					)}
				</div>
			</fieldset>
		</FocusRing>
	);
});
