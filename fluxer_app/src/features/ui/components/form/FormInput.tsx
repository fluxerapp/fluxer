// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import type {TextareaAutosizeProps} from '@app/features/platform/utils/AutoResizingTextarea';
import {TextareaAutosize} from '@app/features/platform/utils/AutoResizingTextarea';
import {
	type InputWithPasswordManagerIgnoreAttributes,
	PASSWORD_MANAGER_IGNORE_ATTRIBUTES,
	shouldApplyPasswordManagerIgnoreAttributes,
	type TextareaWithPasswordManagerIgnoreAttributes,
} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import scrollerStyles from '@app/features/theme/styles/Scroller.module.css';
import {CharacterCountAnnouncer} from '@app/features/ui/character_counter/CharacterCountAnnouncer';
import styles from '@app/features/ui/components/form/FormInput.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {EyeIcon, EyeSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React, {useCallback, useId, useMemo, useRef, useState} from 'react';

const HIDE_PASSWORD_DESCRIPTOR = msg({
	message: 'Hide password',
	comment: 'Accessible label for the button that hides a password field.',
});
const SHOW_PASSWORD_DESCRIPTOR = msg({
	message: 'Show password',
	comment: 'Accessible label for the button that reveals a password field.',
});
const omitStyle = <T extends {style?: unknown}>(obj: T): Omit<T, 'style'> => {
	const {style: _style, ...rest} = obj;
	return rest;
};

type FieldSetProps = Omit<React.HTMLProps<HTMLFieldSetElement>, 'label'> & {
	children: React.ReactNode;
	error?: string;
	errorId?: string;
	footer?: React.ReactNode;
	label?: React.ReactNode;
	labelRight?: React.ReactNode;
	htmlFor?: string;
};

export const FieldSet = React.forwardRef<HTMLFieldSetElement, FieldSetProps>(
	({label, labelRight, children, error, errorId, footer, htmlFor, className}, ref) => (
		<fieldset ref={ref} className={clsx(styles.fieldset, className)} data-flx="ui.form.input.field-set.fieldset">
			{label && (
				<div className={styles.labelContainer} data-flx="ui.form.input.field-set.label-container">
					<label htmlFor={htmlFor} className={styles.label} data-flx="ui.form.input.field-set.label">
						{label}
					</label>
					{labelRight}
				</div>
			)}
			<div className={styles.inputGroup} data-flx="ui.form.input.field-set.input-group">
				{children}
				{error && (
					<span id={errorId} className={styles.errorText} data-flx="ui.form.input.field-set.error-text">
						{error}
					</span>
				)}
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
	inputProps: InputWithPasswordManagerIgnoreAttributes;
	inputClassName: string;
	ref: React.Ref<HTMLInputElement>;
	defaultInput: React.ReactNode;
}

type InputProps = React.InputHTMLAttributes<HTMLInputElement> & {
	error?: string;
	footer?: React.ReactNode;
	label?: React.ReactNode;
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
		const {i18n} = useLingui();
		const disableAutofocus = shouldDisableAutofocusOnMobile();
		const [showPassword, setShowPassword] = useState(false);
		const isPasswordType = type === 'password';
		const shouldIgnorePasswordManagers = useMemo(
			() => shouldApplyPasswordManagerIgnoreAttributes(type, props.autoComplete),
			[type, props.autoComplete],
		);
		const resolveInputType = useCallback((): string | undefined => {
			if (!isPasswordType) return type;
			return showPassword ? 'text' : 'password';
		}, [isPasswordType, showPassword, type]);
		const inputType = useMemo(() => resolveInputType(), [resolveInputType]);
		const hasRightElement = useMemo(
			() => isPasswordType || rightElement || rightIcon,
			[isPasswordType, rightElement, rightIcon],
		);
		const hasLeftElement = useMemo(() => !!leftElement, [leftElement]);
		const hasLeftIcon = useMemo(() => !!leftIcon, [leftIcon]);
		const inputRef = useRef<HTMLInputElement | null>(null);
		const inputWrapperRef = useRef<HTMLDivElement | null>(null);
		const generatedInputId = useId();
		const inputId = props.id ?? generatedInputId;
		const errorId = error ? `${inputId}-error` : undefined;
		const hasAccessibleName = Boolean(label) || Boolean(props['aria-label']) || Boolean(props['aria-labelledby']);
		const placeholderAriaLabel =
			!hasAccessibleName && typeof props.placeholder === 'string' && props.placeholder.trim().length > 0
				? props.placeholder
				: undefined;
		const describedBy = [props['aria-describedby'], errorId].filter(Boolean).join(' ') || undefined;
		const setInputRefs = useCallback(
			(node: HTMLInputElement | null) => {
				inputRef.current = node;
				assignRef(forwardedRef, node);
			},
			[forwardedRef],
		);
		const ariaInvalid = !!error;
		const hasControlledValue = props.value !== undefined;
		const shouldForceReadOnly = useMemo(
			() => hasControlledValue && typeof props.onChange !== 'function',
			[hasControlledValue, props.onChange],
		);
		const normalizedReadOnly = readOnly ?? shouldForceReadOnly;
		const inputClassName = useMemo(
			() =>
				clsx(
					surfaceStyles.surface,
					styles.input,
					styles.minHeight,
					hasRightElement && styles.hasRightElement,
					(hasLeftIcon || hasLeftElement) && styles.hasLeftIcon,
					hasLeftElement && styles.hasLeftElement,
					error ? styles.error : styles.focusable,
					className,
				),
			[hasRightElement, hasLeftIcon, hasLeftElement, error, className],
		);
		const inputProps: InputWithPasswordManagerIgnoreAttributes = useMemo(
			() => ({
				...props,
				...(shouldIgnorePasswordManagers ? PASSWORD_MANAGER_IGNORE_ATTRIBUTES : {}),
				autoFocus: disableAutofocus ? false : props.autoFocus,
				disabled,
				id: inputId,
				readOnly: normalizedReadOnly,
				type: inputType,
				'aria-label': props['aria-label'] ?? placeholderAriaLabel,
				'aria-describedby': describedBy,
				'aria-invalid': ariaInvalid || undefined,
			}),
			[
				props,
				shouldIgnorePasswordManagers,
				disableAutofocus,
				disabled,
				inputId,
				normalizedReadOnly,
				inputType,
				placeholderAriaLabel,
				describedBy,
				ariaInvalid,
			],
		);
		const defaultInput = useMemo(
			() => (
				<input
					data-flx="ui.form.input.default-input.input"
					{...inputProps}
					className={inputClassName}
					ref={setInputRefs}
				/>
			),
			[inputProps, inputClassName, setInputRefs],
		);
		const renderedInput = useMemo(
			() =>
				renderInput
					? renderInput({
							inputProps,
							inputClassName,
							ref: setInputRefs,
							defaultInput,
						})
					: defaultInput,
			[renderInput, inputProps, inputClassName, setInputRefs, defaultInput],
		);
		const handlePasswordToggle = useCallback(() => {
			setShowPassword(!showPassword);
		}, [showPassword]);
		const passwordToggleLabel = useMemo(
			() => (showPassword ? i18n._(HIDE_PASSWORD_DESCRIPTOR) : i18n._(SHOW_PASSWORD_DESCRIPTOR)),
			[showPassword, i18n.locale],
		);
		const inputContent = useMemo(
			() => (
				<div ref={inputWrapperRef} className={styles.inputWrapper} data-flx="ui.form.input.input-content.input-wrapper">
					{leftElement && (
						<div className={styles.leftElement} data-flx="ui.form.input.input-content.left-element">
							{leftElement}
						</div>
					)}
					{leftIcon && !leftElement && (
						<div className={styles.leftIcon} data-flx="ui.form.input.input-content.left-icon">
							{leftIcon}
						</div>
					)}
					{renderedInput}
					{isPasswordType && (
						<button
							type="button"
							className={styles.passwordToggle}
							onClick={handlePasswordToggle}
							aria-label={passwordToggleLabel}
							data-flx="ui.form.input.input-content.password-toggle.button"
						>
							{showPassword ? (
								<EyeSlashIcon size={18} weight="fill" data-flx="ui.form.input.input-content.eye-slash-icon" />
							) : (
								<EyeIcon size={18} weight="fill" data-flx="ui.form.input.input-content.eye-icon" />
							)}
						</button>
					)}
					{!isPasswordType && rightIcon && (
						<div className={styles.rightIcon} data-flx="ui.form.input.input-content.right-icon">
							{rightIcon}
						</div>
					)}
					{!isPasswordType && rightElement && (
						<div className={styles.rightElement} data-flx="ui.form.input.input-content.right-element">
							{rightElement}
						</div>
					)}
				</div>
			),
			[
				inputWrapperRef,
				leftElement,
				leftIcon,
				renderedInput,
				isPasswordType,
				handlePasswordToggle,
				passwordToggleLabel,
				showPassword,
				rightIcon,
				rightElement,
			],
		);
		const focusDecoratedInput = useMemo(
			() => (
				<FocusRing
					focusTarget={inputRef}
					ringTarget={inputWrapperRef}
					offset={-2}
					enabled={!disabled}
					data-flx="ui.form.input.focus-decorated-input.focus-ring"
				>
					{inputContent}
				</FocusRing>
			),
			[inputRef, inputWrapperRef, disabled, inputContent],
		);
		if (!label) {
			return (
				<div className={styles.inputContainer} data-flx="ui.form.input.input-container">
					{focusDecoratedInput}
					{error && (
						<span id={errorId} className={styles.errorText} data-flx="ui.form.input.error-text">
							{error}
						</span>
					)}
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
				htmlFor={inputId}
				errorId={errorId}
				data-flx="ui.form.input.field-set"
			>
				{focusDecoratedInput}
			</FieldSet>
		);
	},
);

Input.displayName = 'Input';

const BaseTextarea = React.forwardRef<HTMLTextAreaElement, TextareaAutosizeProps>(({className, ...rest}, ref) => (
	<TextareaAutosize
		data-flx="ui.form.input.base-textarea.input"
		{...omitStyle(rest)}
		className={clsx(surfaceStyles.surface, styles.input, scrollerStyles.scroller, className)}
		ref={ref}
	/>
));

BaseTextarea.displayName = 'BaseTextarea';

type TextareaProps = React.TextareaHTMLAttributes<HTMLTextAreaElement> & {
	error?: string;
	footer?: React.ReactNode;
	label: React.ReactNode;
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
		const disableAutofocus = shouldDisableAutofocusOnMobile();
		const currentValue = useMemo(() => value || '', [value]);
		const currentLength = useMemo(() => String(currentValue).length, [currentValue]);
		const textareaRef = useRef<HTMLTextAreaElement | null>(null);
		const textareaWrapperRef = useRef<HTMLDivElement | null>(null);
		const generatedTextareaId = useId();
		const textareaId = id ?? generatedTextareaId;
		const errorId = error ? `${textareaId}-error` : undefined;
		const hasAccessibleName = Boolean(label) || Boolean(props['aria-label']) || Boolean(props['aria-labelledby']);
		const placeholderAriaLabel =
			!hasAccessibleName && typeof props.placeholder === 'string' && props.placeholder.trim().length > 0
				? props.placeholder
				: undefined;
		const describedBy = [props['aria-describedby'], errorId].filter(Boolean).join(' ') || undefined;
		const setTextareaRefs = useCallback(
			(node: HTMLTextAreaElement | null) => {
				textareaRef.current = node;
				assignRef(forwardedRef, node);
			},
			[forwardedRef],
		);
		const sanitizedProps = useMemo(
			(): TextareaWithPasswordManagerIgnoreAttributes => ({
				...omitStyle(props),
				...PASSWORD_MANAGER_IGNORE_ATTRIBUTES,
				autoFocus: disableAutofocus ? false : props.autoFocus,
			}),
			[props, disableAutofocus],
		);
		const textareaProps = useMemo(
			() => ({
				...sanitizedProps,
				id: textareaId,
				'aria-label': sanitizedProps['aria-label'] ?? placeholderAriaLabel,
				'aria-describedby': describedBy,
				'aria-invalid': !!error,
				maxRows,
				minRows,
				maxLength,
				value,
				disabled,
			}),
			[
				sanitizedProps,
				textareaId,
				placeholderAriaLabel,
				describedBy,
				error,
				maxRows,
				minRows,
				maxLength,
				value,
				disabled,
			],
		);
		const characterCounter = useMemo(
			() =>
				showCharacterCount &&
				maxLength && (
					<>
						<span
							className={styles.characterCount}
							aria-hidden="true"
							data-flx="ui.form.input.character-counter.character-count"
						>
							{currentLength}/{maxLength}
						</span>
						<CharacterCountAnnouncer
							currentLength={currentLength}
							maxLength={maxLength}
							data-flx="ui.form.form-input.character-counter.character-count-announcer"
						/>
					</>
				),
			[showCharacterCount, maxLength, currentLength],
		);
		const labelRight = useMemo(
			() => (
				<div className={styles.labelContainerWithGap} data-flx="ui.form.input.label-right.label-container-with-gap">
					{!innerActionButton && characterCounter}
					{actionButton}
				</div>
			),
			[innerActionButton, characterCounter, actionButton],
		);
		const textareaWithActions = useMemo(
			() =>
				innerActionButton ? (
					<div
						ref={textareaWrapperRef}
						className={clsx(styles.textareaWrapper, surfaceStyles.surface, error ? styles.error : styles.focusable)}
						data-flx="ui.form.input.textarea-with-actions.textarea-wrapper"
					>
						<TextareaAutosize
							data-flx="ui.form.input.textarea-with-actions.textarea"
							{...textareaProps}
							className={clsx(scrollerStyles.scroller, scrollerStyles.scrollerTextarea, styles.textarea)}
							ref={setTextareaRefs}
						/>
						<div className={styles.textareaActions} data-flx="ui.form.input.textarea-with-actions.textarea-actions">
							{innerActionButton}
							{showCharacterCount && maxLength && (
								<div
									className={styles.characterCountContainer}
									data-flx="ui.form.input.textarea-with-actions.character-count-container"
								>
									{characterCountTooltip ? (
										characterCountTooltip(maxLength - currentLength, maxLength, currentLength)
									) : (
										<>
											<span
												className={styles.characterCount}
												aria-hidden="true"
												data-flx="ui.form.input.textarea-with-actions.character-count"
											>
												{currentLength}/{maxLength}
											</span>
											<CharacterCountAnnouncer
												currentLength={currentLength}
												maxLength={maxLength}
												data-flx="ui.form.form-input.textarea-with-actions.character-count-announcer"
											/>
										</>
									)}
								</div>
							)}
						</div>
					</div>
				) : null,
			[
				innerActionButton,
				textareaWrapperRef,
				error,
				textareaProps,
				setTextareaRefs,
				showCharacterCount,
				maxLength,
				characterCountTooltip,
				currentLength,
			],
		);
		const simpleTextarea = useMemo(
			() =>
				!innerActionButton ? (
					<BaseTextarea
						data-flx="ui.form.input.simple-textarea.error"
						{...textareaProps}
						className={clsx(error ? styles.error : styles.focusable)}
						ref={setTextareaRefs}
					/>
				) : null,
			[innerActionButton, textareaProps, error, setTextareaRefs],
		);
		const ringTarget = useMemo(
			() => (innerActionButton ? textareaWrapperRef : textareaRef),
			[innerActionButton, textareaWrapperRef, textareaRef],
		);
		const control = useMemo(
			() => (innerActionButton ? textareaWithActions : simpleTextarea)!,
			[innerActionButton, textareaWithActions, simpleTextarea],
		);
		return (
			<FieldSet
				error={error}
				errorId={errorId}
				footer={footer}
				label={label}
				labelRight={labelRight}
				htmlFor={textareaId}
				data-flx="ui.form.input.textarea.field-set"
			>
				<FocusRing
					focusTarget={textareaRef}
					ringTarget={ringTarget}
					offset={-2}
					enabled={!disabled}
					data-flx="ui.form.input.textarea.focus-ring"
				>
					{control}
				</FocusRing>
			</FieldSet>
		);
	},
);

Textarea.displayName = 'Textarea';
