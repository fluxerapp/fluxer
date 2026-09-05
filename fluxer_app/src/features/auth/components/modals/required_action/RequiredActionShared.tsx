// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {EXAMPLE_EMAIL, SUPPORT_EMAIL, SUPPORT_EMAIL_MAILTO} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/auth/components/modals/RequiredActionModal.module.css';
import {
	A_NEW_VERIFICATION_CODE_HAS_BEEN_SENT_DESCRIPTOR,
	ALT_ROUTES_HUMAN_REVIEW_DESCRIPTOR,
	ALT_ROUTES_TITLE_DESCRIPTOR,
	CAPTCHA_REQUIRED_DESCRIPTOR,
	CODE_DID_NOT_WORK_DESCRIPTOR,
	CODE_EXPIRED_DESCRIPTOR,
	CODE_NOT_REQUESTED_DESCRIPTOR,
	EMAIL_ALREADY_IN_USE_DESCRIPTOR,
	EMAIL_CHANGE_UNAVAILABLE_DESCRIPTOR,
	EMAIL_MUST_BE_DIFFERENT_DESCRIPTOR,
	ENTER_VALID_EMAIL_DESCRIPTOR,
	ENTER_VALID_PHONE_DESCRIPTOR,
	ESCAPE_BUTTON_DESCRIPTOR,
	FAILED_TO_RESEND_VERIFICATION_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR,
	NEW_EMAIL_DESCRIPTOR,
	PHONE_ALREADY_USED_DESCRIPTOR,
	PHONE_CANNOT_BE_USED_DESCRIPTOR,
	PHONE_NOT_ELIGIBLE_DESCRIPTOR,
	SMS_UNAVAILABLE_DESCRIPTOR,
	SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR,
	SUPPORT_LINK_LABEL_DESCRIPTOR,
	TOO_MANY_ATTEMPTS_DESCRIPTOR,
	VERIFICATION_SESSION_EXPIRED_DESCRIPTOR,
} from '@app/features/auth/components/modals/required_action/RequiredActionDescriptors';
import {buildPhoneGateEscapeHint} from '@app/features/auth/components/modals/required_action/RequiredActionEscapeCopy';
import type {
	CodeFormInputs,
	NewEmailFormInputs,
	SubmitCallback,
} from '@app/features/auth/components/modals/required_action/RequiredActionTypes';
import {isAbortError} from '@app/features/auth/state/SudoPrompt';
import {
	CANCEL_DESCRIPTOR,
	GO_BACK_DESCRIPTOR,
	SIGN_OUT_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	VERIFICATION_CODE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {failureCode, failureRetryAfter, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback} from 'react';
import type {FieldValues, Path, UseFormReturn} from 'react-hook-form';

interface ValidationFaultWithCode {
	path: string;
	message: string;
	code?: string;
}

export type RequiredActionErrorContext =
	| 'email-address'
	| 'email-code'
	| 'phone-number'
	| 'phone-code'
	| 'resend'
	| 'general';

const EMAIL_VALIDATION_CODES = new Set<string>([
	ValidationErrorCodes.EMAIL_IS_REQUIRED,
	ValidationErrorCodes.EMAIL_LENGTH_INVALID,
	ValidationErrorCodes.INVALID_EMAIL_FORMAT,
	ValidationErrorCodes.INVALID_EMAIL_LOCAL_PART,
	ValidationErrorCodes.INVALID_EMAIL_ADDRESS,
]);
const CODE_SESSION_VALIDATION_CODES = new Set<string>([
	ValidationErrorCodes.EMAIL_TOKEN_EXPIRED,
	ValidationErrorCodes.INVALID_OR_EXPIRED_TICKET,
	ValidationErrorCodes.INVALID_EMAIL_TOKEN,
	ValidationErrorCodes.INVALID_PROOF_TOKEN,
	ValidationErrorCodes.ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST,
	ValidationErrorCodes.ORIGINAL_VERIFICATION_NOT_REQUIRED,
	ValidationErrorCodes.TICKET_ALREADY_COMPLETED,
]);

function getStatus(error: unknown): number | undefined {
	if (!error || typeof error !== 'object' || !('status' in error)) return undefined;
	const status = (error as {status?: unknown}).status;
	return typeof status === 'number' ? status : undefined;
}

function getFirstValidationCode(error: unknown): string | undefined {
	const validationErrors = failureValidationErrors(error) as ReadonlyArray<ValidationFaultWithCode> | undefined;
	const first = validationErrors?.[0];
	if (!first) return undefined;
	return first.code ?? first.message;
}

function resolveValidationErrorMessage(i18n: I18n, code: string, context: RequiredActionErrorContext): string | null {
	if (EMAIL_VALIDATION_CODES.has(code)) {
		return i18n._(ENTER_VALID_EMAIL_DESCRIPTOR);
	}
	switch (code) {
		case ValidationErrorCodes.EMAIL_ALREADY_IN_USE:
			return i18n._(EMAIL_ALREADY_IN_USE_DESCRIPTOR);
		case ValidationErrorCodes.NEW_EMAIL_MUST_BE_DIFFERENT:
			return i18n._(EMAIL_MUST_BE_DIFFERENT_DESCRIPTOR);
		case ValidationErrorCodes.INVALID_VERIFICATION_CODE:
			return i18n._(CODE_DID_NOT_WORK_DESCRIPTOR);
		case ValidationErrorCodes.VERIFICATION_CODE_EXPIRED:
			return i18n._(CODE_EXPIRED_DESCRIPTOR);
		case ValidationErrorCodes.VERIFICATION_CODE_NOT_ISSUED:
		case ValidationErrorCodes.NO_NEW_EMAIL_REQUESTED:
			return i18n._(CODE_NOT_REQUESTED_DESCRIPTOR);
		case ValidationErrorCodes.PHONE_NUMBER_INVALID_FORMAT:
			return i18n._(ENTER_VALID_PHONE_DESCRIPTOR);
		case ValidationErrorCodes.MUST_HAVE_EMAIL_TO_CHANGE_IT:
		case ValidationErrorCodes.NO_ORIGINAL_EMAIL_ON_RECORD:
		case ValidationErrorCodes.ORIGINAL_EMAIL_ALREADY_VERIFIED:
			return i18n._(EMAIL_CHANGE_UNAVAILABLE_DESCRIPTOR);
		default:
			if (CODE_SESSION_VALIDATION_CODES.has(code)) {
				return i18n._(VERIFICATION_SESSION_EXPIRED_DESCRIPTOR);
			}
			if (context === 'email-code' || context === 'phone-code') {
				return i18n._(CODE_DID_NOT_WORK_DESCRIPTOR);
			}
			return null;
	}
}

export function resolveRequiredActionErrorMessage(
	i18n: I18n,
	error: unknown,
	context: RequiredActionErrorContext,
): string {
	const apiCode = failureCode(error);
	const validationCode = apiCode === APIErrorCodes.INVALID_FORM_BODY ? getFirstValidationCode(error) : undefined;
	if (validationCode) {
		const validationMessage = resolveValidationErrorMessage(i18n, validationCode, context);
		if (validationMessage) return validationMessage;
	}
	const status = getStatus(error);
	const retryAfter = failureRetryAfter(error);
	const isRateLimited =
		status === 429 ||
		apiCode === APIErrorCodes.RATE_LIMITED ||
		apiCode === APIErrorCodes.PHONE_RATE_LIMIT_EXCEEDED ||
		typeof retryAfter === 'number';
	if (isRateLimited) {
		return i18n._(TOO_MANY_ATTEMPTS_DESCRIPTOR);
	}
	switch (apiCode) {
		case APIErrorCodes.INVALID_PHONE_NUMBER:
			return context === 'phone-number'
				? i18n._(ENTER_VALID_PHONE_DESCRIPTOR)
				: i18n._(PHONE_CANNOT_BE_USED_DESCRIPTOR);
		case APIErrorCodes.INVALID_PHONE_VERIFICATION_CODE:
			return i18n._(CODE_DID_NOT_WORK_DESCRIPTOR);
		case APIErrorCodes.PHONE_ALREADY_USED:
			return i18n._(PHONE_ALREADY_USED_DESCRIPTOR);
		case APIErrorCodes.SMS_VERIFICATION_UNAVAILABLE:
			return i18n._(SMS_UNAVAILABLE_DESCRIPTOR);
		case APIErrorCodes.PHONE_ADD_NOT_ELIGIBLE:
		case APIErrorCodes.BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED:
		case APIErrorCodes.PHONE_VERIFICATION_REQUIRED:
			return i18n._(PHONE_NOT_ELIGIBLE_DESCRIPTOR);
		case APIErrorCodes.CAPTCHA_REQUIRED:
		case APIErrorCodes.INVALID_CAPTCHA:
			return i18n._(CAPTCHA_REQUIRED_DESCRIPTOR);
		case APIErrorCodes.ACCESS_DENIED:
		case APIErrorCodes.FORBIDDEN:
			return i18n._(EMAIL_CHANGE_UNAVAILABLE_DESCRIPTOR);
		default:
			return i18n._(SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR);
	}
}

function clearServerErrors<T extends FieldValues>(form: UseFormReturn<T>): void {
	const errors = form.formState.errors;
	const errorFields = Object.keys(errors) as Array<Path<T>>;
	errorFields.forEach((field) => {
		const error = errors[field];
		if (error && 'type' in error && error.type === 'server') {
			form.clearErrors(field);
		}
	});
}

export const useRequiredActionFormSubmit = <T extends FieldValues>(
	form: UseFormReturn<T>,
	onSubmit: (data: T) => Promise<void> | void,
	defaultErrorField: Path<T>,
	errorContext: RequiredActionErrorContext,
) => {
	const {i18n} = useLingui();
	const handleSubmit = useCallback(
		async (data: T) => {
			try {
				await onSubmit(data);
			} catch (error) {
				if (isAbortError(error)) return;
				form.setError(defaultErrorField, {
					type: 'server',
					message: resolveRequiredActionErrorMessage(i18n, error, errorContext),
				});
				throw error;
			}
		},
		[defaultErrorField, errorContext, form, i18n, onSubmit],
	);
	const submitWithErrorClearing = useCallback(async () => {
		clearServerErrors(form);
		await form
			.handleSubmit(handleSubmit)()
			.catch(() => undefined);
	}, [form, handleSubmit]);
	return {
		handleSubmit: submitWithErrorClearing,
		isSubmitting: form.formState.isSubmitting,
	};
};

interface StepShellProps {
	title: string;
	description?: React.ReactNode;
	children?: React.ReactNode;
	notice?: React.ReactNode;
}

export const StepShell: React.FC<StepShellProps> = ({title, description, children, notice}) => (
	<div className={styles.stepShell} data-flx="auth.required-action-modal.step-shell">
		<div className={styles.stepHeader} data-flx="auth.required-action-modal.step-header">
			<h2 className={styles.stepTitle} data-flx="auth.required-action-modal.step-title">
				{title}
			</h2>
			{description ? (
				<Modal.Description className={styles.stepDescription} data-flx="auth.required-action-modal.step-description">
					{description}
				</Modal.Description>
			) : null}
		</div>
		{notice ? (
			<div className={styles.inlineNotice} data-flx="auth.required-action-modal.inline-notice">
				{notice}
			</div>
		) : null}
		{children}
	</div>
);

interface ValueBlockProps {
	label: string;
	value: React.ReactNode;
}

export const ValueBlock: React.FC<ValueBlockProps> = ({label, value}) => (
	<div className={styles.valueBlock} data-flx="auth.required-action-modal.value-block">
		<div className={styles.valueLabel} data-flx="auth.required-action-modal.value-label">
			{label}
		</div>
		<div className={styles.valueText} data-flx="auth.required-action-modal.value-text">
			{value}
		</div>
	</div>
);
export const SupportLinkLine: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<ExternalLink
			href={SUPPORT_EMAIL_MAILTO}
			className={styles.supportLink}
			data-flx="auth.required-action-modal.support-link-line.external-link"
		>
			{i18n._(SUPPORT_LINK_LABEL_DESCRIPTOR)} ({SUPPORT_EMAIL})
		</ExternalLink>
	);
};

interface RequiredActionAltRoutesProps {
	escapeAction: {
		guildCount: number;
		submitting: boolean;
		onPress: () => void;
	} | null;
}

export const RequiredActionAltRoutes: React.FC<RequiredActionAltRoutesProps> = ({escapeAction}) => {
	const {i18n} = useLingui();
	return (
		<div data-flx="auth.required-action-modal.alt-routes">
			<p
				className={styles.altRoutesTitle}
				data-step-focus="true"
				tabIndex={-1}
				data-flx="auth.required-action-modal.alt-routes.title"
			>
				{i18n._(ALT_ROUTES_TITLE_DESCRIPTOR)}
			</p>
			<p className={styles.altRoutesBody} data-flx="auth.required-action-modal.alt-routes.body">
				{i18n._(ALT_ROUTES_HUMAN_REVIEW_DESCRIPTOR)}
			</p>
			<div className={styles.supportBlock} data-flx="auth.required-action-modal.alt-routes.support-block">
				<SupportLinkLine data-flx="auth.required-action.required-action-shared.required-action-alt-routes.support-link-line" />
			</div>
			{escapeAction ? (
				<>
					<div className={styles.altRoutesDivider} data-flx="auth.required-action-modal.alt-routes.divider" />
					<p data-flx="auth.required-action-modal.alt-routes.escape-hint">
						{buildPhoneGateEscapeHint(i18n, escapeAction.guildCount)}
					</p>
					<Button
						variant="secondary"
						className={styles.altRoutesAction}
						submitting={escapeAction.submitting}
						onClick={escapeAction.onPress}
						data-flx="auth.required-action-modal.alt-routes.button.set-aside"
					>
						{i18n._(ESCAPE_BUTTON_DESCRIPTOR)}
					</Button>
				</>
			) : null}
		</div>
	);
};

interface NewEmailAddressFormProps {
	form: UseFormReturn<NewEmailFormInputs>;
	onSubmit: SubmitCallback;
	title: string;
	description: string;
}

export const NewEmailAddressForm: React.FC<NewEmailAddressFormProps> = ({form, onSubmit, title, description}) => {
	const {i18n} = useLingui();
	return (
		<Form form={form} onSubmit={onSubmit} data-flx="auth.required-action-modal.new-email-address-form.form.submit">
			<StepShell
				title={title}
				description={description}
				data-flx="auth.required-action.required-action-shared.new-email-address-form.step-shell"
			>
				<Input
					data-step-focus="true"
					data-flx="auth.required-action-modal.new-email-address-form.input.email"
					{...form.register('newEmail')}
					autoComplete="email"
					autoFocus={true}
					error={form.formState.errors.newEmail?.message}
					label={i18n._(NEW_EMAIL_DESCRIPTOR)}
					placeholder={EXAMPLE_EMAIL}
					required={true}
					type="email"
				/>
			</StepShell>
		</Form>
	);
};

interface VerificationCodeFormProps {
	form: UseFormReturn<CodeFormInputs>;
	onSubmit: SubmitCallback;
	title: string;
	description: string;
	children?: React.ReactNode;
}

export const VerificationCodeForm: React.FC<VerificationCodeFormProps> = ({
	form,
	onSubmit,
	title,
	description,
	children,
}) => {
	const {i18n} = useLingui();
	return (
		<Form form={form} onSubmit={onSubmit} data-flx="auth.required-action-modal.verification-code-form.form.submit">
			<StepShell
				title={title}
				description={description}
				data-flx="auth.required-action.required-action-shared.verification-code-form.step-shell"
			>
				<Input
					data-step-focus="true"
					data-flx="auth.required-action-modal.verification-code-form.input"
					{...form.register('code')}
					autoComplete="one-time-code"
					autoFocus={true}
					error={form.formState.errors.code?.message}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					required={true}
				/>
				{children}
			</StepShell>
		</Form>
	);
};

interface SignOutFooterRowProps {
	mock: boolean;
	isLoggingOut: boolean;
	onDismiss: () => void;
	onLogout: () => Promise<void>;
}

export const SignOutFooterRow: React.FC<SignOutFooterRowProps> = ({mock, isLoggingOut, onDismiss, onLogout}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.signOutRow} data-flx="auth.required-action-modal.sign-out-row">
			<button
				type="button"
				className={styles.signOutButton}
				onClick={() => void onLogout()}
				disabled={isLoggingOut}
				data-flx="auth.required-action-modal.sign-out-row.button.logout"
			>
				{i18n._(SIGN_OUT_DESCRIPTOR)}
			</button>
			{mock ? (
				<>
					<span aria-hidden="true" data-flx="auth.required-action.required-action-shared.sign-out-footer-row.span">
						·
					</span>
					<button
						type="button"
						className={styles.signOutButton}
						onClick={onDismiss}
						disabled={isLoggingOut}
						data-flx="auth.required-action-modal.sign-out-row.button.dismiss"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</button>
				</>
			) : null}
		</div>
	);
};

interface InlineLinkProps {
	onClick: () => void;
	disabled?: boolean;
	submitting?: boolean;
	children: React.ReactNode;
	'data-flx'?: string;
}

export const InlineLink: React.FC<InlineLinkProps> = ({onClick, disabled, submitting, children, ...props}) => (
	<button
		type="button"
		className={styles.inlineLink}
		onClick={onClick}
		disabled={disabled || submitting}
		data-flx={props['data-flx']}
	>
		{children}
	</button>
);

interface BackButtonProps {
	onClick: () => void;
	disabled?: boolean;
}

export const BackButton: React.FC<BackButtonProps> = ({onClick, disabled}) => {
	const {i18n} = useLingui();
	return (
		<Button variant="secondary" onClick={onClick} disabled={disabled} data-flx="auth.required-action-modal.back-button">
			{i18n._(GO_BACK_DESCRIPTOR)}
		</Button>
	);
};
export const RequiredActionBackdrop: React.FC<{mock: boolean}> = ({mock}) => (
	<div
		style={{
			position: 'absolute',
			inset: 0,
			backgroundColor: 'hsl(0deg 0% 0%)',
			opacity: 0.95,
			backdropFilter: 'blur(12px)',
			WebkitBackdropFilter: 'blur(12px)',
			pointerEvents: mock ? 'auto' : 'none',
		}}
		data-flx="auth.required-action-modal.required-action-backdrop.div"
	/>
);
export const useResendEmailChangeCode = () => {
	const {i18n} = useLingui();
	return useCallback(
		async (
			ticket: string | null,
			isResending: boolean,
			setIsResending: React.Dispatch<React.SetStateAction<boolean>>,
			resend: (ticket: string) => Promise<void>,
		) => {
			if (!ticket || isResending) {
				return;
			}
			setIsResending(true);
			try {
				await resend(ticket);
				ToastCommands.success(i18n._(A_NEW_VERIFICATION_CODE_HAS_BEEN_SENT_DESCRIPTOR));
			} catch (error) {
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () =>
						resolveRequiredActionErrorMessage(i18n, error, 'resend') ||
						i18n._(FAILED_TO_RESEND_VERIFICATION_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
					dataFlx: 'auth.required-action-shared.resend-email-change-code-error-modal',
				});
			} finally {
				setIsResending(false);
			}
		},
		[i18n],
	);
};
