// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/checkbox/Checkbox.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {CheckIcon} from '@phosphor-icons/react';
import * as CheckboxPrimitive from '@radix-ui/react-checkbox';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const LINK_SELECTOR = 'a[href], [role="link"]';
const isLinkTarget = (target: EventTarget | null): target is Element =>
	target instanceof Element && Boolean(target.closest(LINK_SELECTOR));
const CheckboxTypes = {
	BOX: 'box',
	ROUND: 'round',
} as const;

type CheckboxType = ValueOf<typeof CheckboxTypes>;

type CheckboxWrapperStyle = React.CSSProperties & {
	'--checkbox-size': string;
};

interface CheckboxBaseProps {
	checked?: boolean;
	disabled?: boolean;
	readOnly?: boolean;
	type?: CheckboxType;
	className?: string;
	noFocus?: boolean;
	size?: number | 'small';
	variant?: 'default' | 'menu';
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
		type = CheckboxTypes.BOX,
		className,
		children,
		noFocus = false,
		size = 24,
		variant = 'default',
		onChange,
		onFocus,
		onBlur,
		'aria-label': ariaLabel,
		'aria-describedby': ariaDescribedBy,
		'aria-hidden': ariaHidden,
	}) => {
		const rootRef = useRef<React.ComponentRef<typeof CheckboxPrimitive.Root>>(null);
		const checkboxRef = useRef<HTMLLabelElement>(null);
		const labelRef = useRef<HTMLDivElement>(null);
		const actualSize = size === 'small' ? 18 : size;
		const checkIconSize = Math.floor(actualSize * 0.75);
		const actualSizeRem = remFromPx(actualSize);
		const checkIconSizeRem = remFromPx(checkIconSize);
		const baseId = useId();
		const checkboxId = `${baseId}-checkbox-input`;
		const wrapperStyle = useMemo(
			(): CheckboxWrapperStyle => ({minHeight: actualSizeRem, '--checkbox-size': actualSizeRem}),
			[actualSizeRem],
		);
		const handleChange = useCallback(
			(isSelected: boolean) => {
				if (!disabled && !readOnly) {
					onChange?.(isSelected);
				}
			},
			[disabled, onChange, readOnly],
		);
		const handleFocus = useCallback(
			(event: React.FocusEvent<HTMLButtonElement>) => {
				if (!disabled && !readOnly && !noFocus) {
					onFocus?.(event);
				}
			},
			[disabled, noFocus, onFocus, readOnly],
		);
		const labelInteractive = !disabled && !readOnly && Boolean(children);
		const [labelContainsLink, setLabelContainsLink] = useState(false);
		useEffect(() => {
			const container = labelRef.current;
			const hasLink = Boolean(container?.querySelector(LINK_SELECTOR));
			setLabelContainsLink((previous) => (previous === hasLink ? previous : hasLink));
		}, [children]);
		const labelHandlesFocus = labelInteractive && !labelContainsLink;
		const labelFocusEnabled = labelHandlesFocus && !noFocus;
		const labelTabIndex = labelHandlesFocus ? 0 : -1;

		const handleLabelClick = useCallback(
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
		const handleLabelKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (!labelInteractive || isLinkTarget(event.target)) return;
				if (isKeyboardActivationKey(event.key)) {
					event.preventDefault();
					handleChange(!checked);
					rootRef.current?.focus();
				}
			},
			[checked, handleChange, labelInteractive],
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
					style={wrapperStyle}
					aria-hidden={true}
					data-flx="ui.checkbox.checkbox.checkbox-wrapper"
				>
					<span
						className={clsx(
							styles.checkbox,
							type === CheckboxTypes.ROUND ? styles.round : styles.box,
							checked && styles.checked,
							variant === 'menu' && checked && styles.menuChecked,
						)}
						style={{width: actualSizeRem, height: actualSizeRem}}
						data-flx="ui.checkbox.checkbox.checkbox"
					>
						<span className={styles.checkboxIndicator} data-flx="ui.checkbox.checkbox.checkbox-indicator">
							{checked && (
								<CheckIcon
									size={checkIconSizeRem}
									weight="bold"
									color={'#ffffff'}
									className={styles.checkIcon}
									data-flx="ui.checkbox.checkbox.check-icon"
								/>
							)}
						</span>
					</span>
				</span>
			);
		}
		return (
			<FocusRing
				focusTarget={rootRef}
				ringTarget={checkboxRef}
				offset={4}
				enabled={focusRingEnabled}
				data-flx="ui.checkbox.checkbox.focus-ring"
			>
				<label
					ref={checkboxRef}
					className={clsx(
						styles.checkboxWrapper,
						disabled && (variant === 'menu' ? styles.menuDisabled : styles.disabled),
						className,
					)}
					htmlFor={checkboxId}
					style={wrapperStyle}
					data-flx="ui.checkbox.checkbox.checkbox-wrapper--2"
				>
					<CheckboxPrimitive.Root
						ref={rootRef}
						checked={checked}
						disabled={disabled}
						onCheckedChange={handleChange}
						onFocus={handleFocus}
						onBlur={onBlur}
						className={clsx(
							styles.checkbox,
							type === CheckboxTypes.ROUND ? styles.round : styles.box,
							checked && styles.checked,
							variant === 'menu' && checked && styles.menuChecked,
						)}
						style={{width: actualSizeRem, height: actualSizeRem}}
						aria-label={ariaLabel}
						aria-describedby={ariaDescribedBy}
						tabIndex={0}
						id={checkboxId}
						data-flx="ui.checkbox.checkbox.checkbox.shortcut-key-down"
					>
						<CheckboxPrimitive.Indicator
							className={styles.checkboxIndicator}
							data-flx="ui.checkbox.checkbox.checkbox-indicator--2"
						>
							{checked && (
								<CheckIcon
									size={checkIconSizeRem}
									weight="bold"
									color={'#ffffff'}
									className={styles.checkIcon}
									data-flx="ui.checkbox.checkbox.check-icon--2"
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
							data-flx="ui.checkbox.checkbox.focus-ring--2"
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
								data-flx="ui.checkbox.checkbox.label"
							>
								<div className={styles.labelText} data-flx="ui.checkbox.checkbox.label-text">
									{children}
								</div>
							</div>
						</FocusRing>
					)}
				</label>
			</FocusRing>
		);
	},
);
