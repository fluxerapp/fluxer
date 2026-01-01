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
import {EyedropperIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import Color from 'colorjs.io';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Button, Dialog, DialogTrigger, Popover} from 'react-aria-components';
import {ColorPickerPopout} from '~/components/popouts/ColorPickerPopout';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './ColorPickerField.module.css';
import surfaceStyles from './FormSurface.module.css';

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
			(ctx as any).fillStyle = raw as any;
			const parsedRaw = String(ctx.fillStyle);

			ctx.fillStyle = '#123456';
			(ctx as any).fillStyle = raw as any;
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
	hideHelperText?: boolean;
	descriptionClassName?: string;
}

export const ColorPickerField: React.FC<ColorPickerFieldProps> = observer((props) => {
	const {t} = useLingui();
	const {label, description, value, onChange, disabled, className, defaultValue, hideHelperText, descriptionClassName} =
		props;

	const containerRef = React.useRef<HTMLFieldSetElement>(null);
	const inputRef = React.useRef<HTMLInputElement>(null);

	const getEffectiveValue = React.useCallback(() => {
		return value === 0 && defaultValue !== undefined ? defaultValue : value;
	}, [value, defaultValue]);

	const [inputValue, setInputValue] = React.useState(() => numberToHex(getEffectiveValue()));
	const [showError, setShowError] = React.useState(false);
	const [popoutOpen, setPopoutOpen] = React.useState(false);

	React.useEffect(() => {
		if (!popoutOpen) {
			const effectiveValue = getEffectiveValue();
			setInputValue(numberToHex(effectiveValue));
		}
	}, [getEffectiveValue, popoutOpen]);

	const commitFromText = React.useCallback(() => {
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

	const handleInputBlur = React.useCallback(() => {
		commitFromText();
	}, [commitFromText]);

	const handleInputKeyDown: React.KeyboardEventHandler<HTMLInputElement> = React.useCallback(
		(e) => {
			if (e.key === 'Enter') {
				e.preventDefault();
				commitFromText();
				containerRef.current?.querySelector<HTMLButtonElement>('button[data-role="swatch"]')?.focus();
			}
		},
		[commitFromText],
	);

	const handleInputChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
		setInputValue(e.target.value);
		setShowError(false);
	}, []);

	const handleColorChange = React.useCallback(
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

	const handleReset = React.useCallback(() => {
		onChange(0);
		const resetHex = defaultValue !== undefined ? numberToHex(defaultValue) : '#000000';
		setInputValue(resetHex);
		setShowError(false);
		setPopoutOpen(false);
	}, [onChange, defaultValue]);

	const effectiveValue = getEffectiveValue();
	const logicalHex = numberToHex(effectiveValue);
	const swatchBackgroundCss =
		value === 0 && defaultValue !== undefined ? logicalHex : value === 0 ? 'var(--text-chat)' : logicalHex;

	const iconOnSwatch = bestIconColorFor(swatchBackgroundCss);

	return (
		<FocusRing within={true} offset={-2} enabled={!disabled}>
			<fieldset ref={containerRef} className={clsx(styles.fieldset, className)}>
				{label && (
					<div className={styles.labelContainer}>
						<legend className={styles.label}>{label}</legend>
					</div>
				)}

				<div className={styles.inputContainer}>
					<div className={clsx(styles.inputWrapper, surfaceStyles.surface)}>
						<input
							ref={inputRef}
							type="text"
							value={inputValue}
							onChange={handleInputChange}
							onBlur={handleInputBlur}
							onKeyDown={handleInputKeyDown}
							placeholder="#000000, rgb(...), red"
							maxLength={64}
							disabled={disabled}
							className={clsx(styles.input, showError && styles.inputError)}
							aria-label={t`Color value`}
							aria-invalid={showError}
						/>
						<div className={styles.divider} />
						<DialogTrigger isOpen={popoutOpen} onOpenChange={setPopoutOpen}>
							<Button
								data-role="swatch"
								className={styles.swatchButton}
								style={{backgroundColor: swatchBackgroundCss}}
								aria-label={t`Open color picker`}
								isDisabled={disabled}
							>
								<EyedropperIcon
									size={18}
									weight="fill"
									style={{color: iconOnSwatch === 'white' ? '#FFFFFF' : '#000000'}}
									className={styles.swatchIcon}
								/>
							</Button>
							<Popover placement="bottom start" offset={8} className={styles.popover}>
								<Dialog className={styles.dialog} aria-label={t`Color picker`}>
									<ColorPickerPopout
										color={numberToHex(effectiveValue)}
										onChange={handleColorChange}
										onReset={handleReset}
									/>
								</Dialog>
							</Popover>
						</DialogTrigger>
					</div>

					{(description || !hideHelperText) && (
						<p className={clsx(styles.description, descriptionClassName)}>
							{description ?? <Trans>Type a color (hex, rgb(), hsl, or name) — or use the picker.</Trans>}
						</p>
					)}

					{showError && (
						<p className={styles.errorText}>
							<Trans>That doesn't look like a valid color. Try hex, rgb(), hsl(), or a CSS color name.</Trans>
						</p>
					)}
				</div>
			</fieldset>
		</FocusRing>
	);
});
