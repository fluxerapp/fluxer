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

import {useLingui} from '@lingui/react/macro';
import {EyeIcon, EyeSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import lodash from 'lodash';
import React, {useState} from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {TextareaAutosizeProps} from '~/lib/TextareaAutosize';
import {TextareaAutosize} from '~/lib/TextareaAutosize';
import scrollerStyles from '~/styles/Scroller.module.css';
import surfaceStyles from './FormSurface.module.css';
import styles from './Input.module.css';

type FieldSetProps = React.HTMLProps<HTMLFieldSetElement> & {
	children: React.ReactNode;
	error?: string;
	footer?: React.ReactNode;
	label?: string;
	labelRight?: React.ReactNode;
	htmlFor?: string;
};

const FieldSet = React.forwardRef<HTMLFieldSetElement, FieldSetProps>(
	({label, labelRight, children, error, footer, htmlFor}, ref) => (
		<fieldset ref={ref} className={styles.fieldset}>
			{label && (
				<div className={styles.labelContainer}>
					<label htmlFor={htmlFor} className={styles.label}>
						{label}
					</label>
					{labelRight}
				</div>
			)}
			<div className={styles.inputGroup}>
				{children}
				{error && <span className={styles.errorText}>{error}</span>}
			</div>
			{footer}
		</fieldset>
	),
);

FieldSet.displayName = 'FieldSet';

const assignRef = <T,>(ref: React.Ref<T> | undefined, value: T | null): void => {
	if (typeof ref === 'function') {
		ref(value);
	} else if (ref && typeof ref === 'object') {
		(ref as React.MutableRefObject<T | null>).current = value;
	}
};

export interface RenderInputArgs {
	inputProps: React.InputHTMLAttributes<HTMLInputElement>;
	inputClassName: string;
	ref: React.Ref<HTMLInputElement>;
	defaultInput: React.ReactNode;
}

type InputProps = React.InputHTMLAttributes<HTMLInputElement> & {
	error?: string;
	footer?: React.ReactNode;
	label?: string;
	labelRight?: React.ReactNode;
	leftElement?: React.ReactNode;
	rightElement?: React.ReactNode;
	leftIcon?: React.ReactNode;
	rightIcon?: React.ReactNode;
	renderInput?: (args: RenderInputArgs) => React.ReactNode;
};

export const Input = React.forwardRef<HTMLInputElement, InputProps>(
	(
		{
			error,
			footer,
			label,
			labelRight,
			type,
			leftElement,
			rightElement,
			leftIcon,
			rightIcon,
			className,
			renderInput,
			disabled,
			readOnly,
			...props
		},
		forwardedRef,
	) => {
		const {t} = useLingui();
		const [showPassword, setShowPassword] = useState(false);
		const isPasswordType = type === 'password';
		const resolveInputType = (): string | undefined => {
			if (!isPasswordType) return type;
			return showPassword ? 'text' : 'password';
		};
		const inputType = resolveInputType();
		const hasRightElement = isPasswordType || rightElement || rightIcon;
		const hasLeftElement = !!leftElement;
		const hasLeftIcon = !!leftIcon;
		const inputRef = React.useRef<HTMLInputElement | null>(null);
		const inputWrapperRef = React.useRef<HTMLDivElement | null>(null);

		const setInputRefs = React.useCallback(
			(node: HTMLInputElement | null) => {
				inputRef.current = node;
				assignRef(forwardedRef, node);
			},
			[forwardedRef],
		);

		const ariaInvalid = !!error;
		const hasControlledValue = props.value !== undefined;
		const shouldForceReadOnly = hasControlledValue && typeof props.onChange !== 'function';
		const normalizedReadOnly = readOnly ?? shouldForceReadOnly;
		const inputClassName = clsx(
			surfaceStyles.surface,
			styles.input,
			styles.minHeight,
			hasRightElement && styles.hasRightElement,
			(hasLeftIcon || hasLeftElement) && styles.hasLeftIcon,
			hasLeftElement && styles.hasLeftElement,
			error ? styles.error : styles.focusable,
			className,
		);

		const inputProps: React.InputHTMLAttributes<HTMLInputElement> = {
			...props,
			disabled,
			readOnly: normalizedReadOnly,
			type: inputType,
			'aria-invalid': ariaInvalid || undefined,
		};

		const defaultInput = <input {...inputProps} className={inputClassName} ref={setInputRefs} />;

		const renderedInput = renderInput
			? renderInput({
					inputProps,
					inputClassName,
					ref: setInputRefs,
					defaultInput,
				})
			: defaultInput;

		const inputContent = (
			<div ref={inputWrapperRef} className={styles.inputWrapper}>
				{leftElement && <div className={styles.leftElement}>{leftElement}</div>}
				{leftIcon && !leftElement && <div className={styles.leftIcon}>{leftIcon}</div>}
				{renderedInput}
				{isPasswordType && (
					<button
						type="button"
						className={styles.passwordToggle}
						onClick={() => setShowPassword(!showPassword)}
						aria-label={showPassword ? t`Hide password` : t`Show password`}
					>
						{showPassword ? <EyeSlashIcon size={18} weight="fill" /> : <EyeIcon size={18} weight="fill" />}
					</button>
				)}
				{!isPasswordType && rightIcon && <div className={styles.rightIcon}>{rightIcon}</div>}
				{!isPasswordType && rightElement && <div className={styles.rightElement}>{rightElement}</div>}
			</div>
		);

		const focusDecoratedInput = (
			<FocusRing focusTarget={inputRef} ringTarget={inputWrapperRef} offset={-2} enabled={!disabled}>
				{inputContent}
			</FocusRing>
		);

		if (!label) {
			return (
				<div className={styles.inputContainer}>
					{focusDecoratedInput}
					{error && <span className={styles.errorText}>{error}</span>}
					{footer}
				</div>
			);
		}

		return (
			<FieldSet
				error={error}
				footer={footer}
				label={label}
				labelRight={labelRight}
				htmlFor={props.id as string | undefined}
			>
				{focusDecoratedInput}
			</FieldSet>
		);
	},
);

Input.displayName = 'Input';

const BaseTextarea = React.forwardRef<HTMLTextAreaElement, TextareaAutosizeProps>(({className, ...rest}, ref) => (
	<TextareaAutosize
		{...lodash.omit(rest, 'style')}
		className={clsx(surfaceStyles.surface, styles.input, scrollerStyles.scroller, className)}
		ref={ref}
	/>
));

BaseTextarea.displayName = 'BaseTextarea';

type TextareaProps = React.TextareaHTMLAttributes<HTMLTextAreaElement> & {
	error?: string;
	footer?: React.ReactNode;
	label: string;
	minRows?: number;
	maxRows?: number;
	showCharacterCount?: boolean;
	actionButton?: React.ReactNode;
	innerActionButton?: React.ReactNode;
	characterCountTooltip?: (remaining: number, total: number, current: number) => React.ReactNode;
};

export const Textarea = React.forwardRef<HTMLTextAreaElement, TextareaProps>(
	(
		{
			error,
			footer,
			label,
			minRows = 2,
			maxRows = 10,
			showCharacterCount,
			maxLength,
			value,
			actionButton,
			innerActionButton,
			characterCountTooltip,
			disabled,
			id,
			...props
		},
		forwardedRef,
	) => {
		const currentValue = value || '';
		const currentLength = String(currentValue).length;
		const textareaRef = React.useRef<HTMLTextAreaElement | null>(null);
		const textareaWrapperRef = React.useRef<HTMLDivElement | null>(null);

		const setTextareaRefs = React.useCallback(
			(node: HTMLTextAreaElement | null) => {
				textareaRef.current = node;
				assignRef(forwardedRef, node);
			},
			[forwardedRef],
		);

		const sanitizedProps = lodash.omit(props, 'style');
		const textareaProps = {
			...sanitizedProps,
			id,
			'aria-invalid': !!error,
			maxRows,
			minRows,
			maxLength,
			value,
			disabled,
		};

		const characterCounter = showCharacterCount && maxLength && (
			<span className={styles.characterCount}>
				{currentLength}/{maxLength}
			</span>
		);

		const labelRight = (
			<div className={styles.labelContainer} style={{gap: '0.5rem'}}>
				{!innerActionButton && characterCounter}
				{actionButton}
			</div>
		);

		const textareaWithActions = innerActionButton ? (
			<div
				ref={textareaWrapperRef}
				className={clsx(styles.textareaWrapper, surfaceStyles.surface, error ? styles.error : styles.focusable)}
			>
				<TextareaAutosize
					{...textareaProps}
					className={clsx(scrollerStyles.scroller, scrollerStyles.scrollerTextarea, styles.textarea)}
					ref={setTextareaRefs}
				/>
				<div className={styles.textareaActions}>
					{innerActionButton}
					{showCharacterCount && maxLength && characterCountTooltip && (
						<div className={styles.characterCountContainer}>
							{characterCountTooltip(maxLength - currentLength, maxLength, currentLength)}
						</div>
					)}
				</div>
			</div>
		) : null;

		const simpleTextarea = !innerActionButton ? (
			<BaseTextarea
				{...textareaProps}
				className={clsx(error ? styles.error : styles.focusable)}
				ref={setTextareaRefs}
			/>
		) : null;

		const ringTarget = innerActionButton ? textareaWrapperRef : textareaRef;
		const control = (innerActionButton ? textareaWithActions : simpleTextarea)!;

		return (
			<FieldSet error={error} footer={footer} label={label} labelRight={labelRight} htmlFor={id}>
				<FocusRing focusTarget={textareaRef} ringTarget={ringTarget} offset={-2} enabled={!disabled}>
					{control}
				</FocusRing>
			</FieldSet>
		);
	},
);

Textarea.displayName = 'Textarea';
