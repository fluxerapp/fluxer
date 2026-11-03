// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {checkSetupToken} from '@app/features/app/components/setup/SetupWizardClient';
import {StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {failureRetryAfterSeconds, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as FormUtils from '@app/lib/forms';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SECONDS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {createContext, type ReactNode, type RefObject, useContext, useMemo, useState} from 'react';

const MAX_SETUP_TOKEN_LENGTH = 256;
const MIN_LOCKOUT_MINUTES = 1;
const SETUP_TOKEN_COMMAND = 'rexctl setup token';

const SETUP_TOKEN_TITLE_DESCRIPTOR = msg({
	message: 'Enter your setup token',
	comment: 'Setup wizard title for the screen that asks for the setup token.',
});
const SETUP_TOKEN_BODY_DESCRIPTOR = msg({
	message: "The setup token shows that you're the person who installed this instance.",
	comment: 'Setup wizard body explaining what the setup token is for.',
});
const SETUP_TOKEN_HELP_DESCRIPTOR = msg({
	message:
		'If you installed Fluxer with rexctl, it printed the token when it finished. Run this on your server to show it again.',
	comment:
		'Help on the setup token screen, followed by a terminal command. rexctl is a name that must not be translated.',
});
const SETUP_TOKEN_LOST_DESCRIPTOR = msg({
	message:
		'If you run Fluxer without rexctl, set FLUXER_SETUP_TOKEN to a value of at least 32 characters that you choose, restart Fluxer, and enter that value here.',
	comment:
		'Help on the setup token screen for someone who installed Fluxer without rexctl. FLUXER_SETUP_TOKEN and rexctl must not be translated.',
});
const SETUP_TOKEN_LABEL_DESCRIPTOR = msg({
	message: 'Setup token',
	comment: 'Label for the setup token field in the setup wizard.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Button on the setup token screen that copies the terminal command.',
});
const SETUP_TOKEN_COPY_COMMAND_DESCRIPTOR = msg({
	message: 'Copy the command',
	comment: 'Accessible label for the button that copies the terminal command on the setup token screen.',
});

const SETUP_TOKEN_INVALID_DESCRIPTOR = msg({
	message: "That setup token doesn't match. Check that you copied all of it.",
	comment: 'Error under the setup token field when the token is wrong.',
});
const SETUP_TOKEN_LOCKED_OUT_DESCRIPTOR = msg({
	message:
		'{minutes, plural, one {Too many wrong setup tokens were tried from your network. Try again in # minute.} other {Too many wrong setup tokens were tried from your network. Try again in # minutes.}}',
	comment:
		'Alert on the setup token screen after too many wrong setup tokens came from the same network. minutes is how long until they can try again.',
});
interface SetupTokenDraft {
	value: string;
	setValue: (value: string) => void;
	error: string | null;
	setError: (error: string | null) => void;
}

const SetupTokenDraftContext = createContext<SetupTokenDraft | null>(null);

export function SetupTokenDraftProvider({children}: {children: ReactNode}) {
	const [value, setValue] = useState('');
	const [error, setError] = useState<string | null>(null);
	const draft = useMemo(() => ({value, setValue, error, setError}), [value, error]);
	return <SetupTokenDraftContext.Provider value={draft}>{children}</SetupTokenDraftContext.Provider>;
}

export function useSetupTokenDraft(): SetupTokenDraft {
	const draft = useContext(SetupTokenDraftContext);
	if (draft === null) throw new Error('Setup token draft provider is missing');
	return draft;
}

export const setupTokenViolation = (error: unknown) =>
	failureValidationErrors(error)?.find((violation) => violation.path === 'setup_token');

export const SetupTokenStep = observer(
	({
		formRef,
		notice,
		onSubmit,
		onAdvance,
	}: {
		formRef: RefObject<HTMLFormElement | null>;
		notice: string | null;
		onSubmit: (submit: () => Promise<void>) => Promise<void>;
		onAdvance: () => void;
	}) => {
		const {i18n} = useLingui();
		const {
			value: setupToken,
			setValue: setSetupToken,
			error: setupTokenError,
			setError: setSetupTokenError,
		} = useSetupTokenDraft();
		const submitSetupToken = async () => {
			const token = setupToken.trim();
			try {
				await checkSetupToken(token);
				setSetupTokenError(null);
				onAdvance();
			} catch (error) {
				const violation = setupTokenViolation(error);
				if (violation?.code === ValidationErrorCodes.SETUP_TOKEN_INVALID) {
					setSetupTokenError(i18n._(SETUP_TOKEN_INVALID_DESCRIPTOR));
					return;
				}
				if (error instanceof HttpError && error.status === HttpStatus.TOO_MANY_REQUESTS) {
					const minutes = Math.max(
						MIN_LOCKOUT_MINUTES,
						Math.ceil((failureRetryAfterSeconds(error) ?? SECONDS_PER_MINUTE) / SECONDS_PER_MINUTE),
					);
					setSetupTokenError(i18n._(SETUP_TOKEN_LOCKED_OUT_DESCRIPTOR, {minutes}));
					return;
				}
				setSetupTokenError(violation?.message ?? FormUtils.extractErrorMessage(i18n, error));
			}
		};

		return (
			<form
				ref={formRef}
				noValidate
				onSubmit={(event) => {
					event.preventDefault();
					void onSubmit(submitSetupToken);
				}}
				className={styles.step}
				data-flx="app.self-hosted-setup-wizard-gate.setup-token-step"
			>
				<StepHeader
					title={i18n._(SETUP_TOKEN_TITLE_DESCRIPTOR)}
					body={i18n._(SETUP_TOKEN_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.setup-token-step.step-header"
				/>
				{notice && (
					<p className={styles.alert} role="alert" data-flx="app.self-hosted-setup-wizard-gate.setup-token-notice">
						{notice}
					</p>
				)}
				<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.setup-token-help">
					{i18n._(SETUP_TOKEN_HELP_DESCRIPTOR)}
				</p>
				<div className={styles.codeRow} data-flx="app.self-hosted-setup-wizard-gate.setup-token-command">
					<code
						dir="ltr"
						className={styles.codeBlock}
						data-flx="app.self-hosted-setup-wizard-gate.setup-token-command-text"
					>
						{SETUP_TOKEN_COMMAND}
					</code>
					<Button
						variant="secondary"
						small
						aria-label={i18n._(SETUP_TOKEN_COPY_COMMAND_DESCRIPTOR)}
						onClick={() => TextCopyCommands.copy(i18n, SETUP_TOKEN_COMMAND)}
						data-flx="app.self-hosted-setup-wizard-gate.setup-token-copy"
					>
						{i18n._(COPY_DESCRIPTOR)}
					</Button>
				</div>
				<p className={styles.body} data-flx="app.self-hosted-setup-wizard-gate.setup-token-lost">
					{i18n._(SETUP_TOKEN_LOST_DESCRIPTOR)}
				</p>
				<Input
					label={i18n._(SETUP_TOKEN_LABEL_DESCRIPTOR)}
					type="text"
					value={setupToken}
					onChange={(event) => {
						setSetupToken(event.target.value);
						setSetupTokenError(null);
					}}
					autoComplete="off"
					autoCapitalize="none"
					autoCorrect="off"
					spellCheck={false}
					maxLength={MAX_SETUP_TOKEN_LENGTH}
					className={styles.monospaceInput}
					error={setupTokenError ?? undefined}
					data-flx="app.self-hosted-setup-wizard-gate.setup-token-input"
				/>
			</form>
		);
	},
);
