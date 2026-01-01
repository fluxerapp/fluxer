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

import {CheckIcon} from '@phosphor-icons/react';
import * as CheckboxPrimitive from '@radix-ui/react-checkbox';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import styles from './Checkbox.module.css';

const LINK_SELECTOR = 'a[href], [role="link"]';

const isLinkTarget = (target: EventTarget | null): target is Element =>
	target instanceof Element && Boolean(target.closest(LINK_SELECTOR));

const CheckboxTypes = {
	BOX: 'box',
	ROUND: 'round',
} as const;

type CheckboxType = (typeof CheckboxTypes)[keyof typeof CheckboxTypes];

interface CheckboxLinkShortcut {
	key: string;
	label: string;
	hint: string;
	action: () => void;
}

interface CheckboxBaseProps {
	checked?: boolean;
	disabled?: boolean;
	readOnly?: boolean;
	inverted?: boolean;
	type?: CheckboxType;
	className?: string;
	noFocus?: boolean;
	size?: number | 'small';
	variant?: 'default' | 'menu';
	linkShortcuts?: ReadonlyArray<CheckboxLinkShortcut>;
	onChange?: (checked: boolean) => void;
	onFocus?: (event: React.FocusEvent<HTMLButtonElement>) => void;
	onBlur?: (event: React.FocusEvent<HTMLButtonElement>) => void;
	'aria-label'?: string;
	'aria-describedby'?: string;
	'aria-hidden'?: boolean;
}

type CheckboxWithLabelProps = CheckboxBaseProps & {
	children: React.ReactNode;
};

type CheckboxAccessibleProps = CheckboxBaseProps & {
	children?: undefined;
	'aria-label': string;
};

type CheckboxHiddenProps = CheckboxBaseProps & {
	children?: undefined;
	'aria-hidden': true;
};

type CheckboxProps = CheckboxWithLabelProps | CheckboxAccessibleProps | CheckboxHiddenProps;

export const Checkbox: React.FC<CheckboxProps> = observer(
	({
		checked = false,
		disabled = false,
		readOnly = false,
		inverted = false,
		type = CheckboxTypes.BOX,
		className,
		children,
		noFocus = false,
		size = 24,
		variant = 'default',
		onChange,
		onFocus,
		onBlur,
		linkShortcuts,
		'aria-label': ariaLabel,
		'aria-describedby': ariaDescribedBy,
		'aria-hidden': ariaHidden,
	}) => {
		const rootRef = React.useRef<React.ElementRef<typeof CheckboxPrimitive.Root>>(null);
		const checkboxRef = React.useRef<HTMLLabelElement>(null);
		const labelRef = React.useRef<HTMLDivElement>(null);

		const actualSize = size === 'small' ? 18 : size;
		const checkIconSize = Math.floor(actualSize * 0.75);
		const baseId = React.useId();
		const checkboxId = `${baseId}-checkbox-input`;

		const handleChange = React.useCallback(
			(isSelected: boolean) => {
				if (!disabled && !readOnly) {
					onChange?.(isSelected);
				}
			},
			[disabled, onChange, readOnly],
		);

		const handleFocus = React.useCallback(
			(event: React.FocusEvent<HTMLButtonElement>) => {
				setIsCheckboxFocused(true);
				if (!disabled && !readOnly && !noFocus) {
					onFocus?.(event);
				}
			},
			[disabled, noFocus, onFocus, readOnly],
		);

		const handleBlur = React.useCallback(
			(event: React.FocusEvent<HTMLButtonElement>) => {
				setIsCheckboxFocused(false);
				onBlur?.(event);
			},
			[onBlur],
		);

		const labelInteractive = !disabled && !readOnly && Boolean(children);
		const [labelContainsLink, setLabelContainsLink] = React.useState(false);

		React.useLayoutEffect(() => {
			const container = labelRef.current;
			const hasLink = Boolean(container?.querySelector(LINK_SELECTOR));
			setLabelContainsLink((previous) => (previous === hasLink ? previous : hasLink));
		}, [children]);

		const labelHandlesFocus = labelInteractive && !labelContainsLink;
		const labelFocusEnabled = labelHandlesFocus && !noFocus;
		const labelTabIndex = labelHandlesFocus ? 0 : -1;
		const [isCheckboxFocused, setIsCheckboxFocused] = React.useState(false);

		const keyboardModeEnabled = KeyboardModeStore.keyboardModeEnabled;
		const showLinkShortcuts =
			keyboardModeEnabled && isCheckboxFocused && Array.isArray(linkShortcuts) && linkShortcuts.length > 0;
		const shortcutHintId = showLinkShortcuts ? `${checkboxId}-link-shortcuts` : undefined;
		const describedByIds = [ariaDescribedBy, shortcutHintId].filter(Boolean).join(' ') || undefined;

		const handleLabelClick = React.useCallback(
			(event: React.MouseEvent<HTMLDivElement>) => {
				if (!labelInteractive) return;
				if (isLinkTarget(event.target)) {
					event.stopPropagation();
					return;
				}
				event.preventDefault();
				event.stopPropagation();
				handleChange(!checked);
				rootRef.current?.focus();
			},
			[checked, handleChange, labelInteractive],
		);

		const handleLabelKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (!labelInteractive || isLinkTarget(event.target)) return;
				if (event.key === 'Enter' || event.key === ' ') {
					event.preventDefault();
					handleChange(!checked);
					rootRef.current?.focus();
				}
			},
			[checked, handleChange, labelInteractive],
		);

		const handleShortcutKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLButtonElement>) => {
				if (!showLinkShortcuts) return;
				const targetKey = event.key.toLowerCase();
				const shortcut = linkShortcuts?.find((entry) => entry.key.toLowerCase() === targetKey);
				if (!shortcut) return;
				event.preventDefault();
				event.stopPropagation();
				shortcut.action();
			},
			[linkShortcuts, showLinkShortcuts],
		);

		const focusRingEnabled = !noFocus && !ariaHidden;

		if (ariaHidden) {
			return (
				<span
					className={clsx(
						styles.checkboxWrapper,
						disabled && (variant === 'menu' ? styles.menuDisabled : styles.disabled),
						className,
					)}
					style={{height: actualSize}}
					aria-hidden={true}
				>
					<span
						className={clsx(
							styles.checkbox,
							type === CheckboxTypes.ROUND ? styles.round : styles.box,
							checked && styles.checked,
							inverted && !checked && styles.inverted,
							checked && inverted && styles.checkedInverted,
							variant === 'menu' && checked && styles.menuChecked,
						)}
						style={{width: actualSize, height: actualSize}}
					>
						<span className={styles.checkboxIndicator}>
							{checked && (
								<CheckIcon
									size={checkIconSize}
									weight="bold"
									color={inverted ? 'var(--brand-primary)' : '#ffffff'}
									className={clsx(styles.checkIcon, inverted && styles.invertedIcon)}
								/>
							)}
						</span>
					</span>
				</span>
			);
		}

		return (
			<FocusRing focusTarget={rootRef} ringTarget={checkboxRef} offset={4} enabled={focusRingEnabled}>
				<label
					ref={checkboxRef}
					className={clsx(
						styles.checkboxWrapper,
						disabled && (variant === 'menu' ? styles.menuDisabled : styles.disabled),
						className,
					)}
					htmlFor={checkboxId}
					style={{height: actualSize}}
				>
					<CheckboxPrimitive.Root
						ref={rootRef}
						checked={checked}
						disabled={disabled}
						onCheckedChange={handleChange}
						onFocus={handleFocus}
						onBlur={handleBlur}
						onKeyDown={handleShortcutKeyDown}
						className={clsx(
							styles.checkbox,
							type === CheckboxTypes.ROUND ? styles.round : styles.box,
							checked && styles.checked,
							inverted && !checked && styles.inverted,
							checked && inverted && styles.checkedInverted,
							variant === 'menu' && checked && styles.menuChecked,
						)}
						style={{width: actualSize, height: actualSize}}
						aria-label={ariaLabel}
						aria-describedby={describedByIds}
						tabIndex={0}
						id={checkboxId}
					>
						<CheckboxPrimitive.Indicator className={styles.checkboxIndicator}>
							{checked && (
								<CheckIcon
									size={checkIconSize}
									weight="bold"
									color={inverted ? 'var(--brand-primary)' : '#ffffff'}
									className={clsx(styles.checkIcon, inverted && styles.invertedIcon)}
								/>
							)}
						</CheckboxPrimitive.Indicator>
					</CheckboxPrimitive.Root>
					{children && (
						<FocusRing
							focusTarget={labelRef}
							ringTarget={labelRef}
							offset={-2}
							ringClassName={styles.labelFocusRing}
							enabled={labelFocusEnabled}
						>
							<div
								ref={labelRef}
								className={clsx(styles.label, labelInteractive && styles.labelInteractive)}
								tabIndex={labelTabIndex}
								onClick={handleLabelClick}
								onKeyDown={labelHandlesFocus ? handleLabelKeyDown : undefined}
								role="checkbox"
								aria-checked={checked}
								aria-disabled={disabled || readOnly}
							>
								<div className={styles.labelText}>{children}</div>
							</div>
						</FocusRing>
					)}
					{showLinkShortcuts && shortcutHintId && (
						<div id={shortcutHintId} className={styles.keyboardShortcutPortal} role="status" aria-live="polite">
							{linkShortcuts!.map((shortcut) => (
								<span key={shortcut.key} className={styles.keyboardShortcut}>
									<span className={styles.keyboardShortcutKey} aria-hidden="true">
										{shortcut.key.toUpperCase()}
									</span>
									<span className={styles.keyboardShortcutLabel} aria-hidden="true">
										{shortcut.label}
									</span>
									<span className={styles.keyboardShortcutHint}>{shortcut.hint}</span>
								</span>
							))}
						</div>
					)}
				</label>
			</FocusRing>
		);
	},
);
