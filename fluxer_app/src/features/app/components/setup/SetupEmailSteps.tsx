// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {testSmtpConfig, type updateInstanceConfig} from '@app/features/app/components/setup/SetupWizardClient';
import {Note, StepError, StepHeader, TextButton} from '@app/features/app/components/setup/SetupWizardSteps';
import {Input} from '@app/features/ui/components/form/FormInput';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import * as FormUtils from '@app/lib/forms';
import {resolveSmtpTlsMode, type SmtpTlsMode, SmtpTlsModes} from '@fluxer/constants/src/SmtpConstants';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {createContext, type ReactNode, type RefObject, useCallback, useContext, useMemo, useState} from 'react';

const DEFAULT_SMTP_PORT = '587';
const MIN_SMTP_PORT = 1;
const MAX_SMTP_PORT = 65535;
type SmtpTlsChoice = SmtpTlsMode | 'auto';

const EMAIL_SERVICE_TITLE_DESCRIPTOR = msg({
	message: 'Connect your email service',
	comment: 'Setup wizard title for entering SMTP details.',
});
const EMAIL_SERVICE_BODY_DESCRIPTOR = msg({
	message: 'Enter the SMTP details from your email provider.',
	comment: 'Setup wizard body for entering SMTP details. SMTP must not be translated.',
});
const SMTP_SERVER_LABEL_DESCRIPTOR = msg({
	message: 'SMTP server',
	comment: 'Label for the SMTP server address field. SMTP must not be translated.',
});
const SMTP_PORT_LABEL_DESCRIPTOR = msg({
	message: 'Port',
	comment: 'Label for the SMTP port field.',
});
const SMTP_PORT_ERROR_DESCRIPTOR = msg({
	message: 'Enter a port between 1 and 65535.',
	comment: 'Error under the SMTP port field.',
});
const SMTP_USERNAME_LABEL_DESCRIPTOR = msg({
	message: 'Username',
	comment: 'Label for the SMTP username field.',
});
const SMTP_PASSWORD_LABEL_DESCRIPTOR = msg({
	message: 'Password',
	comment: 'Label for the SMTP password field.',
});
const SMTP_CREDENTIALS_OPTIONAL_DESCRIPTOR = msg({
	message:
		'Leave the username and password empty if your mail server accepts mail from this server without signing in.',
	comment:
		'Note under the SMTP username and password fields in the setup wizard. SMTP relays on a private network often need no sign-in.',
});
const SMTP_TLS_LABEL_DESCRIPTOR = msg({
	message: 'Encryption',
	comment: 'Label for the choice of how the SMTP connection is encrypted.',
});
const SMTP_TLS_AUTO_DESCRIPTOR = msg({
	message: 'Automatic, based on the port',
	comment: 'SMTP encryption option that picks the mode from the port number.',
});
const SMTP_TLS_IMPLICIT_DESCRIPTOR = msg({
	message: 'TLS, usually port 465',
	comment: 'SMTP encryption option where the connection is encrypted from the start. TLS must not be translated.',
});
const SMTP_TLS_STARTTLS_DESCRIPTOR = msg({
	message: 'STARTTLS, usually port 587',
	comment: 'SMTP encryption option that always upgrades with STARTTLS. STARTTLS must not be translated.',
});
const SMTP_TLS_OPPORTUNISTIC_DESCRIPTOR = msg({
	message: 'STARTTLS when the server offers it',
	comment: 'SMTP encryption option that upgrades only when the mail server offers STARTTLS.',
});
const SMTP_TLS_NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'SMTP encryption option that never encrypts the connection.',
});
const SMTP_FAILED_DESCRIPTOR = msg({
	message: 'Fluxer couldn\'t connect to {host}. The server said: "{error}"',
	comment: 'Alert when the SMTP check fails. host is the mail server address and error is its reply.',
});
const SMTP_FAILED_NO_REPLY_DESCRIPTOR = msg({
	message: "Fluxer couldn't connect to {host}.",
	comment: 'Alert when the SMTP check fails and the mail server sent no reply. host is the mail server address.',
});
const SMTP_SAVE_ANYWAY_DESCRIPTOR = msg({
	message: 'Save anyway',
	comment: 'Text button that keeps SMTP details even though the connection check failed.',
});
const SMTP_LATER_DESCRIPTOR = msg({
	message: 'Set up email later',
	comment: 'Text button that skips connecting an email service during setup.',
});

const EMAIL_SENDER_TITLE_DESCRIPTOR = msg({
	message: 'Who should emails come from?',
	comment: 'Setup wizard title for the sender address and name of outgoing email.',
});
const FROM_ADDRESS_LABEL_DESCRIPTOR = msg({
	message: 'From address',
	comment: 'Label for the email address outgoing email is sent from.',
});
const FROM_ADDRESS_ERROR_DESCRIPTOR = msg({
	message: 'Enter an email address.',
	comment: 'Error under the from address field in the setup wizard.',
});
const FROM_NAME_LABEL_DESCRIPTOR = msg({
	message: 'From name',
	comment: 'Label for the sender name shown on outgoing email.',
});

function smtpDraftFromConfig(config: InstanceConfigResponse, current: SmtpDraft): SmtpDraft {
	const email = config.integrations.email;
	return {
		host: email.smtp.host ?? current.host,
		port: email.smtp.port ? String(email.smtp.port) : current.port,
		tlsMode: email.smtp.tls_mode ?? current.tlsMode,
		username: email.smtp.username ?? current.username,
		password: current.password,
		fromEmail: email.from_email ?? current.fromEmail,
		fromName: email.from_name ?? current.fromName,
	};
}

const EMPTY_SMTP_DRAFT: SmtpDraft = {
	host: '',
	port: DEFAULT_SMTP_PORT,
	tlsMode: 'auto',
	username: '',
	password: '',
	fromEmail: '',
	fromName: '',
};

interface EmailDraftState {
	draft: SmtpDraft;
	failure: string | null;
	updateDraft: (patch: Partial<SmtpDraft>) => void;
	hydrate: (config: InstanceConfigResponse) => void;
	setFailure: (failure: string | null) => void;
}

const EmailDraftContext = createContext<EmailDraftState | null>(null);

export function SetupEmailDraftProvider({children}: {children: ReactNode}) {
	const [draft, setDraft] = useState<SmtpDraft>(EMPTY_SMTP_DRAFT);
	const [failure, setFailure] = useState<string | null>(null);
	const updateDraft = useCallback((patch: Partial<SmtpDraft>) => {
		setDraft((current) => ({...current, ...patch}));
	}, []);
	const hydrate = useCallback((config: InstanceConfigResponse) => {
		setDraft((current) => smtpDraftFromConfig(config, current));
	}, []);
	const state = useMemo(
		() => ({draft, failure, updateDraft, hydrate, setFailure}),
		[draft, failure, updateDraft, hydrate],
	);
	return <EmailDraftContext.Provider value={state}>{children}</EmailDraftContext.Provider>;
}

export function useSetupEmailDraft(): EmailDraftState {
	const draft = useContext(EmailDraftContext);
	if (draft === null) throw new Error('Setup email draft provider is missing');
	return draft;
}

interface SmtpDraft {
	host: string;
	port: string;
	tlsMode: SmtpTlsChoice;
	username: string;
	password: string;
	fromEmail: string;
	fromName: string;
}

export function parseSmtpPort(value: string): number | null {
	if (!/^\d+$/.test(value.trim())) return null;
	const parsed = Number.parseInt(value, 10);
	return parsed >= MIN_SMTP_PORT && parsed <= MAX_SMTP_PORT ? parsed : null;
}

export const EmailServiceStep = observer(
	({
		formRef,
		error,
		disabled,
		onSubmit,
		onAdvance,
		onError,
		onLater,
	}: {
		formRef: RefObject<HTMLFormElement | null>;
		error: string | null;
		disabled: boolean;
		onSubmit: (submit: () => Promise<void>) => Promise<void>;
		onAdvance: () => void;
		onError: (error: string) => void;
		onLater: () => void;
	}) => {
		const {i18n} = useLingui();
		const {draft, failure, updateDraft, setFailure} = useSetupEmailDraft();
		const smtpPort = parseSmtpPort(draft.port);
		const smtpTlsMode = draft.tlsMode === 'auto' ? null : draft.tlsMode;
		const onChange = (patch: Partial<SmtpDraft>) => {
			updateDraft(patch);
			setFailure(null);
		};
		const testSmtp = async () => {
			if (smtpPort === null) return;
			try {
				const result = await testSmtpConfig({
					host: draft.host.trim(),
					port: smtpPort,
					username: draft.username.trim() || null,
					password: draft.password.trim() || null,
					tls_mode: smtpTlsMode ?? resolveSmtpTlsMode(null, smtpPort),
				});
				if (result.ok) {
					setFailure(null);
					onAdvance();
					return;
				}
				setFailure(result.error ?? '');
			} catch (error) {
				onError(FormUtils.extractErrorMessage(i18n, error));
			}
		};

		const portInvalid = draft.port.trim().length > 0 && parseSmtpPort(draft.port) === null;
		const tlsOptions: ReadonlyArray<RadioOption<SmtpTlsChoice>> = [
			{value: 'auto', name: i18n._(SMTP_TLS_AUTO_DESCRIPTOR)},
			{value: SmtpTlsModes.IMPLICIT, name: i18n._(SMTP_TLS_IMPLICIT_DESCRIPTOR)},
			{value: SmtpTlsModes.STARTTLS, name: i18n._(SMTP_TLS_STARTTLS_DESCRIPTOR)},
			{value: SmtpTlsModes.OPPORTUNISTIC, name: i18n._(SMTP_TLS_OPPORTUNISTIC_DESCRIPTOR)},
			{value: SmtpTlsModes.NONE, name: i18n._(SMTP_TLS_NONE_DESCRIPTOR)},
		];
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(testSmtp);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.email-service-step"
			>
				<StepHeader
					title={i18n._(EMAIL_SERVICE_TITLE_DESCRIPTOR)}
					body={i18n._(EMAIL_SERVICE_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.email-service-step.step-header"
				/>
				<div className={styles.fieldRow} data-flx="app.self-hosted-setup-wizard-gate.smtp-server-row">
					<Input
						label={i18n._(SMTP_SERVER_LABEL_DESCRIPTOR)}
						value={draft.host}
						placeholder="smtp.example.com"
						autoComplete="off"
						autoCapitalize="none"
						spellCheck={false}
						disabled={disabled}
						onChange={(event) => onChange({host: event.target.value})}
						data-flx="app.self-hosted-setup-wizard-gate.smtp-host-input"
					/>
					<Input
						label={i18n._(SMTP_PORT_LABEL_DESCRIPTOR)}
						value={draft.port}
						inputMode="numeric"
						autoComplete="off"
						disabled={disabled}
						error={portInvalid ? i18n._(SMTP_PORT_ERROR_DESCRIPTOR) : undefined}
						onChange={(event) => onChange({port: event.target.value})}
						data-flx="app.self-hosted-setup-wizard-gate.smtp-port-input"
					/>
				</div>
				<div
					className={styles.fieldLabel}
					id="setup-smtp-tls-label"
					data-flx="app.self-hosted-setup-wizard-gate.smtp-tls-label"
				>
					{i18n._(SMTP_TLS_LABEL_DESCRIPTOR)}
				</div>
				<RadioGroup
					options={tlsOptions}
					value={draft.tlsMode}
					onChange={(tlsMode) => onChange({tlsMode})}
					disabled={disabled}
					aria-label={i18n._(SMTP_TLS_LABEL_DESCRIPTOR)}
					data-flx="app.self-hosted-setup-wizard-gate.smtp-tls-radio-group"
				/>
				<Input
					label={i18n._(SMTP_USERNAME_LABEL_DESCRIPTOR)}
					value={draft.username}
					autoComplete="off"
					autoCapitalize="none"
					spellCheck={false}
					disabled={disabled}
					onChange={(event) => onChange({username: event.target.value})}
					data-flx="app.self-hosted-setup-wizard-gate.smtp-username-input"
				/>
				<Input
					label={i18n._(SMTP_PASSWORD_LABEL_DESCRIPTOR)}
					type="password"
					value={draft.password}
					autoComplete="off"
					disabled={disabled}
					onChange={(event) => onChange({password: event.target.value})}
					data-flx="app.self-hosted-setup-wizard-gate.smtp-password-input"
				/>
				<Note data-flx="app.setup.setup-wizard-steps.email-service-step.credentials-note">
					{i18n._(SMTP_CREDENTIALS_OPTIONAL_DESCRIPTOR)}
				</Note>
				{failure !== null && (
					<div className={styles.alertBlock} data-flx="app.self-hosted-setup-wizard-gate.smtp-failure">
						<p className={styles.alert} role="alert" data-flx="app.self-hosted-setup-wizard-gate.smtp-failure-text">
							{failure
								? i18n._(SMTP_FAILED_DESCRIPTOR, {host: draft.host.trim(), error: failure})
								: i18n._(SMTP_FAILED_NO_REPLY_DESCRIPTOR, {host: draft.host.trim()})}
						</p>
						<TextButton
							onClick={() => {
								setFailure(null);
								onAdvance();
							}}
							disabled={disabled}
							data-flx="app.setup.setup-wizard-steps.email-service-step.text-button.save-anyway"
						>
							{i18n._(SMTP_SAVE_ANYWAY_DESCRIPTOR)}
						</TextButton>
					</div>
				)}
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.email-service-step.step-error" />
				<TextButton
					onClick={onLater}
					disabled={disabled}
					data-flx="app.setup.setup-wizard-steps.email-service-step.text-button.later"
				>
					{i18n._(SMTP_LATER_DESCRIPTOR)}
				</TextButton>
			</form>
		);
	},
);

function isValidEmailAddress(value: string): boolean {
	return /^[^\s@]+@[^\s@]+$/.test(value.trim());
}

export const EmailSenderStep = observer(
	({
		formRef,
		showErrors,
		disabled,
		error,
		onSubmit,
		onSave,
		onAdvance,
		onShowErrors,
	}: {
		formRef: RefObject<HTMLFormElement | null>;
		showErrors: boolean;
		disabled: boolean;
		error: string | null;
		onSubmit: (submit: () => Promise<void>) => Promise<void>;
		onSave: (patch: Parameters<typeof updateInstanceConfig>[0]) => Promise<boolean>;
		onAdvance: () => void;
		onShowErrors: (show: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const {draft, updateDraft: onChange} = useSetupEmailDraft();
		const smtpPort = parseSmtpPort(draft.port);
		const smtpTlsMode = draft.tlsMode === 'auto' ? null : draft.tlsMode;
		const saveSender = async () => {
			onShowErrors(true);
			const host = draft.host.trim();
			if (!host || !isValidEmailAddress(draft.fromEmail) || smtpPort === null) return;
			const saved = await onSave({
				integrations: {
					email: {
						enabled: true,
						provider: 'smtp',
						from_email: draft.fromEmail.trim(),
						from_name: draft.fromName.trim() || null,
						smtp: {
							host,
							port: smtpPort,
							username: draft.username.trim(),
							...(draft.password ? {password: draft.password} : {}),
							tls_mode: smtpTlsMode ?? resolveSmtpTlsMode(null, smtpPort),
						},
					},
				},
			});
			if (saved) onAdvance();
		};

		const fromInvalid = showErrors && !isValidEmailAddress(draft.fromEmail);
		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(saveSender);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.email-sender-step"
			>
				<StepHeader
					title={i18n._(EMAIL_SENDER_TITLE_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.email-sender-step.step-header"
				/>
				<Input
					label={i18n._(FROM_ADDRESS_LABEL_DESCRIPTOR)}
					type="email"
					value={draft.fromEmail}
					placeholder="noreply@example.com"
					autoComplete="email"
					disabled={disabled}
					error={fromInvalid ? i18n._(FROM_ADDRESS_ERROR_DESCRIPTOR) : undefined}
					onChange={(event) => onChange({fromEmail: event.target.value})}
					data-flx="app.self-hosted-setup-wizard-gate.from-email-input"
				/>
				<Input
					label={i18n._(FROM_NAME_LABEL_DESCRIPTOR)}
					value={draft.fromName}
					maxLength={120}
					autoComplete="off"
					disabled={disabled}
					onChange={(event) => onChange({fromName: event.target.value})}
					data-flx="app.self-hosted-setup-wizard-gate.from-name-input"
				/>
				<StepError error={error} data-flx="app.setup.setup-wizard-steps.email-sender-step.step-error" />
			</form>
		);
	},
);
