// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {AuthShell, resolveLayoutAuthBrand, useAuthLayoutVariant} from '@app/features/app/components/layout/AuthShell';
import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {
	AccountStep,
	CREATE_ACCOUNT_DESCRIPTOR,
	SetupAccountDraft,
	type SetupAccountValues,
} from '@app/features/app/components/setup/SetupAccountStep';
import {
	AttachmentsStep,
	SetupAttachmentsDraftProvider,
	useSetupAttachmentsDraft,
} from '@app/features/app/components/setup/SetupAttachmentsStep';
import {SetupBrandingDraftProvider, useSetupBrandingDraft} from '@app/features/app/components/setup/SetupBrandingDraft';
import {BrandingStep} from '@app/features/app/components/setup/SetupBrandingStep';
import {
	EmailSenderStep,
	EmailServiceStep,
	parseSmtpPort,
	SetupEmailDraftProvider,
	useSetupEmailDraft,
} from '@app/features/app/components/setup/SetupEmailSteps';
import {
	isEmailAnswer,
	SetupIdentityDraft,
	type SetupIdentityValues,
} from '@app/features/app/components/setup/SetupIdentityDraft';
import {
	EmailDeliveryStep,
	SharedUsernamesStep,
	SignInMethodStep,
} from '@app/features/app/components/setup/SetupIdentitySteps';
import {NameStep} from '@app/features/app/components/setup/SetupNameStep';
import {
	CommunitiesStep,
	DirectMessagesStep,
	type FinishDraft,
	PersonalNotesStep,
	PushRelayStep,
	RegistrationStep,
	SetupPolicyDraftProvider,
	useSetupPolicyDraft,
} from '@app/features/app/components/setup/SetupPolicySteps';
import {RecoveryKitStep, SetupRecoveryKitDraft} from '@app/features/app/components/setup/SetupRecoveryKitStep';
import {ReviewStep} from '@app/features/app/components/setup/SetupReviewStep';
import {type ServiceKeyDraft, ServicesStep} from '@app/features/app/components/setup/SetupServicesStep';
import {
	SetupTokenDraftProvider,
	SetupTokenStep,
	useSetupTokenDraft,
} from '@app/features/app/components/setup/SetupTokenStep';
import {
	classifySetupUnauthorized,
	fetchInstanceConfig,
	updateInstanceConfig,
} from '@app/features/app/components/setup/SetupWizardClient';
import {
	countedStepsBeforeAccount,
	type EmailDeliveryAnswer,
	firstStepAfterAccount,
	isBeforeAccountStep,
	isWizardStep,
	nextStep,
	previousStep,
	resolveStep,
	visibleSteps,
	type WizardConditions,
	type WizardStep,
	wizardProgress,
} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {
	DoneStep,
	SignInLockedStep,
	SignInToFinishStep,
	TextButton,
	WelcomeStep,
} from '@app/features/app/components/setup/SetupWizardSteps';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import {type MissingField, SubmitTooltip} from '@app/features/auth/flow/SubmitTooltip';
import Authentication from '@app/features/auth/state/Authentication';
import {AuthRegisterDraftContext} from '@app/features/auth/state/AuthRegisterDraftContext';
import {
	BACK_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
	SIGN_OUT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import SessionManager from '@app/features/platform/state/AuthSession';
import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import {ArrowLeftIcon} from '@app/features/ui/icons/DirectionalIcons';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import * as RecoveryKitCommands from '@app/features/user/commands/RecoveryKitCommands';
import RecoveryKitStatus from '@app/features/user/state/RecoveryKitStatus';
import * as FormUtils from '@app/lib/forms';
import {RegistrationModes} from '@fluxer/constants/src/AccountConfigConstants';
import {
	AccountIdentityLockReasons,
	type AccountIdentityMode,
	AccountIdentityModes,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useEffectEvent, useId, useMemo, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

const logger = new Logger('SelfHostedSetupWizardGate');

const DRAFT_STORAGE_KEY = 'fluxer.setup.draft';
const userStorageKey = (userId: string) => `fluxer.setup.${userId}`;

const SETUP_WIZARD_DESCRIPTOR = msg({
	message: 'Instance setup',
	comment: 'Accessible label for the group that holds the setup wizard screens.',
});
const PROGRESS_LABEL_DESCRIPTOR = msg({
	message: 'Setup progress',
	comment: 'Accessible label for the progress bar at the top of the setup wizard.',
});
const STEP_COUNT_DESCRIPTOR = msg({
	message: 'Step {current} of {total}',
	comment: 'Setup wizard progress next to the Back button. current and total are numbers.',
});
const GET_STARTED_DESCRIPTOR = msg({
	message: 'Get started',
	comment: 'Button on the first setup wizard screen.',
});
const FINISH_DESCRIPTOR = msg({
	message: 'Finish setup',
	comment: 'Button on the review screen that saves every choice and finishes setup.',
});
const BACK_TO_REVIEW_DESCRIPTOR = msg({
	message: 'Back to review',
	comment: 'Button that saves an answer and returns to the setup wizard review screen.',
});
const LOADING_DESCRIPTOR = msg({
	message: 'Loading your setup',
	comment: 'Accessible status while the setup wizard loads the saved answers.',
});
const SETUP_TOKEN_REJECTED_DESCRIPTOR = msg({
	message: 'Your setup token stopped working. Enter it again.',
	comment: 'Alert on the setup token screen when the token was refused while creating the account.',
});
const LOAD_ERROR_DESCRIPTOR = msg({
	message: 'Could not load the instance configuration. Try reloading the page.',
	comment: 'Error shown when the setup wizard fails to load the instance configuration.',
});
const NOT_ADMIN_DESCRIPTOR = msg({
	message: 'This account is not an admin of this instance. Sign in with an admin account to finish setup.',
	comment:
		'Error shown in the setup wizard when the signed-in account lacks admin access to the unconfigured instance. A sign out button follows.',
});
const ORIGIN_MISMATCH_DESCRIPTOR = msg({
	message:
		'The API is on a different origin than this page, so setup requests are sent without your session. Check the public origin and port this instance is configured with, then reload.',
	comment: 'Error shown when the setup wizard cannot load because the API origin differs from the page origin.',
});

interface PreAccountDraft extends SetupIdentityValues, SetupAccountValues {
	stepId: WizardStep;
}

interface PostAccountDraft {
	stepId: WizardStep;
	finishDraft: FinishDraft;
	emailAnswer: EmailDeliveryAnswer;
	stepsBeforeAccount: number;
}

function readStored(key: string): Record<string, unknown> | null {
	try {
		const raw = getProtectedSessionStorage()?.getItem(key);
		if (!raw) return null;
		const value: unknown = JSON.parse(raw);
		return isRecord(value) ? value : null;
	} catch (error) {
		logger.warn('Failed to read saved setup progress', error);
		return null;
	}
}

function writeStored(key: string, value: unknown): void {
	try {
		getProtectedSessionStorage()?.setItem(key, JSON.stringify(value));
	} catch (error) {
		logger.warn('Failed to save setup progress', error);
	}
}

function removeStored(key: string): void {
	try {
		getProtectedSessionStorage()?.removeItem(key);
	} catch (error) {
		logger.warn('Failed to remove saved setup progress', error);
	}
}

function finishDraftFromConfig(config: InstanceConfigResponse, stored: unknown): FinishDraft {
	const draft: FinishDraft = {
		registrationMode: config.registration.mode,
		singleCommunityEnabled: config.policy.single_community_enabled,
		singleCommunityName: config.app_public.branding.product_name,
		directMessagesDisabled: config.policy.direct_messages_disabled,
		personalNotesEnabled: config.policy.personal_notes_enabled,
		pushRelayConsentAccepted: config.push_relay.relay_consent_accepted,
	};
	if (!isRecord(stored)) return draft;
	if (
		stored.registrationMode === RegistrationModes.OPEN ||
		stored.registrationMode === RegistrationModes.APPROVAL ||
		stored.registrationMode === RegistrationModes.CLOSED
	) {
		draft.registrationMode = stored.registrationMode;
	}
	if (typeof stored.singleCommunityEnabled === 'boolean') {
		draft.singleCommunityEnabled = stored.singleCommunityEnabled;
	}
	if (typeof stored.singleCommunityName === 'string') {
		draft.singleCommunityName = stored.singleCommunityName;
	}
	if (typeof stored.directMessagesDisabled === 'boolean') {
		draft.directMessagesDisabled = stored.directMessagesDisabled;
	}
	if (typeof stored.personalNotesEnabled === 'boolean') {
		draft.personalNotesEnabled = stored.personalNotesEnabled;
	}
	if (typeof stored.pushRelayConsentAccepted === 'boolean') {
		draft.pushRelayConsentAccepted = stored.pushRelayConsentAccepted;
	}
	return draft;
}

const SetupWizardController = observer(() => {
	const {i18n} = useLingui();
	const authStoreAuthenticated = Authentication.isAuthenticated;
	const accountWorkAdmitted = !AccountScopedWork.isSuspended;
	const currentUserId = Authentication.currentUserId;
	const accountFormId = useId();
	const restoredDraft = useMemo(() => readStored(DRAFT_STORAGE_KEY), []);

	const [forceUnauthenticatedSetup, setForceUnauthenticatedSetup] = useState(false);
	const isAuthenticated = authStoreAuthenticated && !forceUnauthenticatedSetup;

	const [step, setStep] = useState<WizardStep>(() => {
		const stored = restoredDraft?.stepId;
		if (!isWizardStep(stored) || !isBeforeAccountStep(stored)) return 'welcome';
		const tokenNeeded = RuntimeConfig.appPublic.setup.setup_token_required === true;
		return tokenNeeded && stored !== 'welcome' ? 'setup_token' : stored;
	});
	const [direction, setDirection] = useState(0);
	const [returnToReview, setReturnToReview] = useState(false);
	const [finished, setFinished] = useState(false);
	const [busy, setBusy] = useState(false);
	const [stepError, setStepError] = useState<string | null>(null);
	const [showErrors, setShowErrors] = useState(false);

	const [config, setConfig] = useState<InstanceConfigResponse | null>(null);
	const [loadError, setLoadError] = useState<MessageDescriptor | null>(null);

	const {value: setupToken, setValue: setSetupToken, setError: setSetupTokenError} = useSetupTokenDraft();
	const setupTokenFormRef = useRef<HTMLFormElement | null>(null);
	const [setupTokenRejected, setSetupTokenRejected] = useState(false);
	const [identityDraft] = useState(() => new SetupIdentityDraft(restoredDraft));
	const {signInMethod, tagStyle, emailAnswer, setEmailAnswer} = identityDraft;
	const signInMethodFormRef = useRef<HTMLFormElement | null>(null);
	const sharedUsernamesFormRef = useRef<HTMLFormElement | null>(null);
	const [accountDraft] = useState(() => new SetupAccountDraft(restoredDraft));
	const {values: accountValues} = accountDraft;
	const [stepsBeforeAccount, setStepsBeforeAccount] = useState<number | null>(null);
	const [submitSlot, setSubmitSlot] = useState<HTMLElement | null>(null);
	const wizardRef = useRef<HTMLDivElement | null>(null);
	const communitiesFormRef = useRef<HTMLFormElement | null>(null);

	const [recoveryKitDraft] = useState(() => new SetupRecoveryKitDraft());
	const {draft: smtpDraft, hydrate: hydrateEmailDraft, setFailure: setSmtpFailure} = useSetupEmailDraft();
	const emailServiceFormRef = useRef<HTMLFormElement | null>(null);
	const emailSenderFormRef = useRef<HTMLFormElement | null>(null);
	const {productName, themeColor, assets, hydrate: hydrateBrandingDraft} = useSetupBrandingDraft();
	const nameFormRef = useRef<HTMLFormElement | null>(null);
	const brandingFormRef = useRef<HTMLFormElement | null>(null);
	const {draft: finishDraft, setDraft: setFinishDraft} = useSetupPolicyDraft();
	const serviceDraftCache = useRef<ServiceKeyDraft | null>(null);
	const servicesFormRef = useRef<HTMLFormElement | null>(null);
	const {enabled: attachmentsEnabled, setEnabled: setAttachmentsEnabled} = useSetupAttachmentsDraft();
	const attachmentsFormRef = useRef<HTMLFormElement | null>(null);

	const identityLocked = RuntimeConfig.appPublic.setup.account_identity_locked === true;
	const accountIdentity: AccountIdentityMode =
		config?.account_identity.mode ?? (identityLocked ? RuntimeConfig.accountIdentity : signInMethod);
	const conditions: WizardConditions = {
		isAuthenticated,
		accountIdentity,
		identityLocked,
		adminExists:
			identityLocked &&
			RuntimeConfig.appPublic.setup.account_identity_lock_reason === AccountIdentityLockReasons.ADMIN_EXISTS,
		setupTokenRequired: RuntimeConfig.appPublic.setup.setup_token_required === true,
		setupTokenRejected,
		emailsEnabled: RuntimeConfig.emailsEnabled,
		emailAnswer,
		finished,
	};
	const awaitingConfig = isAuthenticated && config === null && !finished;
	const shownStep = awaitingConfig ? step : resolveStep(step, conditions);
	if (shownStep !== step) {
		setStep(shownStep);
	}

	useEffect(() => {
		if (finished) {
			removeStored(DRAFT_STORAGE_KEY);
			if (currentUserId) removeStored(userStorageKey(currentUserId));
			return;
		}
		if (!isAuthenticated) {
			const draft: PreAccountDraft = {
				stepId: step,
				signInMethod,
				tagStyle,
				emailAnswer,
				username: accountValues.username,
				email: accountValues.email,
			};
			writeStored(DRAFT_STORAGE_KEY, draft);
			return;
		}
		if (!config || !currentUserId) return;
		const draft: PostAccountDraft = {
			stepId: step,
			finishDraft,
			emailAnswer,
			stepsBeforeAccount: stepsBeforeAccount ?? 0,
		};
		writeStored(userStorageKey(currentUserId), draft);
		removeStored(DRAFT_STORAGE_KEY);
	}, [
		finished,
		isAuthenticated,
		config,
		currentUserId,
		step,
		signInMethod,
		tagStyle,
		emailAnswer,
		accountValues,
		finishDraft,
		stepsBeforeAccount,
	]);

	useEffect(() => {
		if (authStoreAuthenticated || !forceUnauthenticatedSetup) return;
		setForceUnauthenticatedSetup(false);
	}, [authStoreAuthenticated, forceUnauthenticatedSetup]);

	const resetSetupSession = useCallback(async () => {
		setForceUnauthenticatedSetup(true);
		accountDraft.resetSession();
		setConfig(null);
		setLoadError(null);
		setStepError(null);
		setBusy(false);
		setStep('welcome');
		setDirection(0);
		try {
			await SessionManager.logout();
		} catch (error) {
			logger.warn('Failed to fully clear the setup session', error);
			Authentication.handleSessionStart({token: null});
			SessionManager.setUserId(null);
		}
		Authentication.handleLogout({skipRedirect: true});
	}, [accountDraft]);

	const signOutOfSetup = useCallback(async () => {
		await resetSetupSession();
		if (RuntimeConfig.appPublic.setup.setup_token_required !== true) RouterUtils.replaceWith(Routes.LOGIN);
	}, [resetSetupSession]);

	const conditionsRef = useRef(conditions);
	conditionsRef.current = conditions;

	const hydrateFromConfig = useCallback(
		(next: InstanceConfigResponse, userId: string | null) => {
			const stored = userId ? readStored(userStorageKey(userId)) : null;
			const storedAnswer = isEmailAnswer(stored?.emailAnswer) ? stored.emailAnswer : null;
			const answer = storedAnswer ?? emailAnswer;
			setConfig(next);
			hydrateBrandingDraft(next);
			hydrateEmailDraft(next);
			setAttachmentsEnabled(next.media.attachment_decay.enabled ?? next.media.attachment_decay.effective.enabled);
			setFinishDraft(finishDraftFromConfig(next, stored?.finishDraft));
			setEmailAnswer(answer);
			const after: WizardConditions = {
				...conditionsRef.current,
				isAuthenticated: true,
				finished: false,
				accountIdentity: next.account_identity.mode,
				emailAnswer: answer,
			};
			setStepsBeforeAccount((current) => {
				if (typeof stored?.stepsBeforeAccount === 'number' && stored.stepsBeforeAccount > 0) {
					return stored.stepsBeforeAccount;
				}
				return (
					current ??
					countedStepsBeforeAccount({
						...after,
						setupTokenRequired: true,
						identityLocked: false,
						emailsEnabled: false,
					})
				);
			});
			const storedStep =
				isWizardStep(stored?.stepId) && visibleSteps(after).includes(stored.stepId)
					? stored.stepId
					: firstStepAfterAccount(after);
			const resumeStep =
				storedStep === 'email_sender' && !next.integrations.email.smtp.host?.trim() ? 'email_service' : storedStep;
			setStep(resumeStep);
			setDirection(1);
			setBusy(false);
		},
		[emailAnswer, hydrateBrandingDraft, hydrateEmailDraft, setAttachmentsEnabled, setEmailAnswer, setFinishDraft],
	);

	useEffect(() => {
		if (!isAuthenticated || config || !accountWorkAdmitted || finished) return;
		let cancelled = false;
		setLoadError(null);
		void (async () => {
			try {
				const next = await fetchInstanceConfig();
				if (cancelled) return;
				hydrateFromConfig(next, Authentication.currentUserId);
			} catch (error) {
				if (cancelled || isAccountTransitionAbortError(error)) return;
				const cause =
					error instanceof HttpError && error.status === 401 ? await classifySetupUnauthorized() : 'unknown';
				if (cancelled) return;
				if (cause === 'stale_session') {
					logger.warn('The setup session token was rejected. Clearing the stale local setup session.');
					await resetSetupSession();
					return;
				}
				if (error instanceof HttpError && error.status === 403) {
					setLoadError(NOT_ADMIN_DESCRIPTOR);
					return;
				}
				logger.error('Failed to load instance configuration', error);
				setLoadError(cause === 'origin_mismatch' ? ORIGIN_MISMATCH_DESCRIPTOR : LOAD_ERROR_DESCRIPTOR);
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [isAuthenticated, config, accountWorkAdmitted, finished, hydrateFromConfig, resetSetupSession]);

	const hasRecoveryKit = currentUserId ? (RecoveryKitStatus.get(currentUserId)?.hasRecoveryKit ?? false) : false;

	useEffect(() => {
		if (step !== 'recovery_kit' || !isAuthenticated) return;
		void RecoveryKitCommands.fetchRecoveryKitStatus().catch((error) => {
			logger.warn('Failed to load recovery kit status', error);
		});
	}, [step, isAuthenticated]);

	const goTo = useCallback((target: WizardStep, nextDirection: number) => {
		setStep(target);
		setDirection(nextDirection);
		setStepError(null);
		setShowErrors(false);
	}, []);

	const advance = useCallback(() => {
		if (returnToReview && step !== 'email_service') {
			setReturnToReview(false);
			goTo('review', 1);
			return;
		}
		goTo(nextStep(step, conditionsRef.current), 1);
	}, [goTo, returnToReview, step]);

	const back = previousStep(step, conditions);
	const goBack = useCallback(() => {
		if (back === null) return;
		setReturnToReview(false);
		goTo(back, -1);
	}, [back, goTo]);

	const handleSetupTokenRejected = useCallback(() => {
		setSetupTokenRejected(true);
		setSetupToken('');
		setSetupTokenError(null);
		setStepError(i18n._(SETUP_TOKEN_REJECTED_DESCRIPTOR));
		setStep('setup_token');
		setDirection(-1);
	}, [i18n, setSetupToken, setSetupTokenError]);

	const describeError = useCallback((error: unknown) => FormUtils.extractErrorMessage(i18n, error), [i18n]);

	const saveConfig = async (patch: Parameters<typeof updateInstanceConfig>[0]): Promise<boolean> => {
		try {
			setConfig(await updateInstanceConfig(patch));
			return true;
		} catch (error) {
			logger.error('Failed to save a setup step', error);
			setStepError(describeError(error));
			return false;
		}
	};

	const smtpPort = parseSmtpPort(smtpDraft.port);

	const finish = async () => {
		if (!config) return;
		setStepError(null);
		try {
			const next = await updateInstanceConfig({
				registration: {mode: finishDraft.registrationMode},
				policy: {
					single_community_enabled: finishDraft.singleCommunityEnabled,
					...(finishDraft.singleCommunityEnabled
						? {single_community_name: finishDraft.singleCommunityName.trim()}
						: {}),
					...(finishDraft.directMessagesDisabled === config.policy.direct_messages_disabled
						? {}
						: {direct_messages_disabled: finishDraft.directMessagesDisabled}),
					...(finishDraft.personalNotesEnabled === config.policy.personal_notes_enabled
						? {}
						: {personal_notes_enabled: finishDraft.personalNotesEnabled}),
				},
				...(finishDraft.pushRelayConsentAccepted === config.push_relay.relay_consent_accepted
					? {}
					: {push_relay: {relay_consent_accepted: finishDraft.pushRelayConsentAccepted}}),
				app_public: {setup: {configured: true}},
			});
			setConfig(next);
			setFinished(true);
			goTo('done', 1);
		} catch (error) {
			logger.error('Failed to complete instance setup', error);
			setStepError(describeError(error));
		}
	};

	let primaryDisabled: boolean;
	switch (step) {
		case 'setup_token':
			primaryDisabled = setupToken.trim().length === 0;
			break;
		case 'recovery_kit':
			primaryDisabled = !recoveryKitDraft.stored;
			break;
		case 'email_service':
		case 'email_sender':
			primaryDisabled = !smtpDraft.host.trim() || smtpPort === null;
			break;
		default:
			primaryDisabled = false;
			break;
	}

	const runStep = async (submit: () => Promise<void> | void) => {
		if (busy || primaryDisabled) return;
		setBusy(true);
		setStepError(null);
		try {
			const pending = submit();
			if (pending !== undefined) await pending;
		} finally {
			setBusy(false);
		}
	};

	const runPrimary = async () => {
		if (step === 'sign_in_method') {
			signInMethodFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'shared_usernames') {
			sharedUsernamesFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'setup_token') {
			setupTokenFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'name') {
			nameFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'gif_youtube') {
			servicesFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'attachments') {
			attachmentsFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'email_service') {
			emailServiceFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'email_sender') {
			emailSenderFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'communities') {
			communitiesFormRef.current?.requestSubmit();
			return;
		}
		if (step === 'look') {
			brandingFormRef.current?.requestSubmit();
			return;
		}
		await runStep(() => {
			switch (step) {
				case 'review':
					return finish();
				case 'sign_in_to_finish':
					RouterUtils.replaceWith(Routes.LOGIN);
					return;
				default:
					advance();
					return;
			}
		});
	};

	const switchEmailToLater = () => {
		setEmailAnswer('later');
		setSmtpFailure(null);
		setReturnToReview(false);
		goTo(returnToReview ? 'review' : 'name', 1);
	};

	const reviewChange = (target: WizardStep) => {
		setReturnToReview(true);
		goTo(target, -1);
	};

	const renderAccountSubmit = (state: {disabled: boolean; submitting: boolean; missingFields: Array<MissingField>}) => {
		if (!submitSlot) return null;
		const submitting = state.submitting || awaitingConfig;
		return createPortal(
			<SubmitTooltip
				missingFields={submitting ? [] : state.missingFields}
				data-flx="app.setup.self-hosted-setup-wizard-gate.render-account-submit.submit-tooltip"
			>
				<Button
					type="submit"
					form={accountFormId}
					disabled={state.disabled && !submitting}
					submitting={submitting}
					className={styles.primaryButton}
					data-flx="app.self-hosted-setup-wizard-gate.create-account-button"
				>
					{i18n._(CREATE_ACCOUNT_DESCRIPTOR)}
				</Button>
			</SubmitTooltip>,
			submitSlot,
		);
	};

	const openInstance = () => {
		if (!config) return;
		removeStored(DRAFT_STORAGE_KEY);
		if (currentUserId) removeStored(userStorageKey(currentUserId));
		RuntimeConfig.applyAdminInstanceConfig(config);
	};

	useAuthLayoutVariant();

	useEffect(() => {
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key !== 'Tab') return;
			KeyboardMode.enterKeyboardMode(false);
			FocusRingManager.setRingsEnabled(true);
		};
		const handlePointer = () => {
			KeyboardMode.exitKeyboardMode();
			FocusRingManager.setRingsEnabled(false);
		};
		window.addEventListener('keydown', handleKeyDown, true);
		window.addEventListener('pointerdown', handlePointer, true);
		window.addEventListener('mousedown', handlePointer, true);
		return () => {
			window.removeEventListener('keydown', handleKeyDown, true);
			window.removeEventListener('pointerdown', handlePointer, true);
			window.removeEventListener('mousedown', handlePointer, true);
		};
	}, []);

	const isAccountStep = step === 'account';
	const isDone = step === 'done';
	const loadingSession = awaitingConfig && !isAccountStep && !loadError;
	const progress = loadingSession ? null : wizardProgress(step, conditions, stepsBeforeAccount ?? 0);
	let primaryLabel: string;
	if (step === 'welcome') {
		primaryLabel = i18n._(GET_STARTED_DESCRIPTOR);
	} else if (step === 'review') {
		primaryLabel = i18n._(FINISH_DESCRIPTOR);
	} else if (step === 'sign_in_to_finish') {
		primaryLabel = i18n._(SIGN_IN_DESCRIPTOR);
	} else if (returnToReview && step !== 'email_service') {
		primaryLabel = i18n._(BACK_TO_REVIEW_DESCRIPTOR);
	} else {
		primaryLabel = i18n._(CONTINUE_DESCRIPTOR);
	}
	const showBack = back !== null && !isDone && !awaitingConfig;
	const showFooter = !loadError && !isDone && !loadingSession;

	const handleEnter = useEffectEvent((event: KeyboardEvent) => {
		if (event.key !== 'Enter' || event.shiftKey || event.isComposing || event.defaultPrevented) return;
		if (isAccountStep || isDone || !showFooter) return;
		const target = event.target;
		if (!(target instanceof HTMLElement) || !wizardRef.current?.contains(target)) return;
		if (target.closest('textarea, a, [role="combobox"], [aria-expanded="true"]')) return;
		if (target.tagName === 'BUTTON' && target.getAttribute('role') !== 'radio') return;
		event.preventDefault();
		void runPrimary();
	});
	useEffect(() => {
		window.addEventListener('keydown', handleEnter);
		return () => window.removeEventListener('keydown', handleEnter);
	}, []);

	const renderStep = (): React.ReactNode => {
		switch (step) {
			case 'welcome':
				return <WelcomeStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.welcome-step" />;
			case 'setup_token':
				return (
					<SetupTokenStep
						formRef={setupTokenFormRef}
						notice={setupTokenRejected ? stepError : null}
						onSubmit={runStep}
						onAdvance={advance}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.setup-token-step.set-setup-token"
					/>
				);
			case 'sign_in_method':
				return (
					<SignInMethodStep
						draft={identityDraft}
						formRef={signInMethodFormRef}
						setupToken={setupToken}
						onSubmit={runStep}
						onAdvance={advance}
						onError={setStepError}
						onSetupTokenRejected={handleSetupTokenRejected}
						disabled={busy}
						error={stepError}
						onClearError={() => setStepError(null)}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.sign-in-method-step.set-sign-in-method"
					/>
				);
			case 'sign_in_locked':
				return (
					<SignInLockedStep
						mode={RuntimeConfig.accountIdentity}
						reason={RuntimeConfig.appPublic.setup.account_identity_lock_reason}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.sign-in-locked-step"
					/>
				);
			case 'sign_in_to_finish':
				return (
					<SignInToFinishStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.sign-in-to-finish-step" />
				);
			case 'shared_usernames':
				return (
					<SharedUsernamesStep
						draft={identityDraft}
						formRef={sharedUsernamesFormRef}
						setupToken={setupToken}
						onSubmit={runStep}
						onAdvance={advance}
						onError={setStepError}
						onSetupTokenRejected={handleSetupTokenRejected}
						disabled={busy}
						error={stepError}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.shared-usernames-step.set-tag-style"
					/>
				);
			case 'email_delivery':
				return (
					<EmailDeliveryStep
						draft={identityDraft}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.email-delivery-step.set-email-answer"
					/>
				);
			case 'account':
				return (
					<AccountStep
						usernameSignIn={accountIdentity === AccountIdentityModes.USERNAME}
						emailLater={
							accountIdentity === AccountIdentityModes.EMAIL && !RuntimeConfig.emailsEnabled && emailAnswer === 'later'
						}
						theme={ThemeTypes.DARK}
						setupToken={setupToken.trim() || undefined}
						formId={accountFormId}
						renderSubmit={renderAccountSubmit}
						onSetupTokenRejected={handleSetupTokenRejected}
						onPasswordRegistered={(password) => {
							accountDraft.passwordRef.current = password;
							setStepsBeforeAccount(countedStepsBeforeAccount(conditionsRef.current));
						}}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.account-step"
					/>
				);
			case 'recovery_kit':
				return (
					<RecoveryKitStep
						draft={recoveryKitDraft}
						currentUserId={currentUserId}
						hasRecoveryKit={hasRecoveryKit}
						adminPasswordRef={accountDraft.passwordRef}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.recovery-kit-step"
					/>
				);
			case 'email_service':
				return (
					<EmailServiceStep
						formRef={emailServiceFormRef}
						error={stepError}
						disabled={busy}
						onSubmit={runStep}
						onAdvance={advance}
						onError={setStepError}
						onLater={switchEmailToLater}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.email-service-step.set-smtp-draft"
					/>
				);
			case 'email_sender':
				return (
					<EmailSenderStep
						formRef={emailSenderFormRef}
						showErrors={showErrors}
						disabled={busy}
						error={stepError}
						onSubmit={runStep}
						onSave={saveConfig}
						onAdvance={advance}
						onShowErrors={setShowErrors}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.email-sender-step.set-smtp-draft"
					/>
				);
			case 'name':
				return (
					<NameStep
						config={config}
						formRef={nameFormRef}
						showErrors={showErrors}
						disabled={busy}
						error={stepError}
						onSubmit={runStep}
						onSave={saveConfig}
						onAdvance={advance}
						onShowErrors={setShowErrors}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.name-step.set-product-name"
					/>
				);
			case 'look':
				return (
					<BrandingStep
						config={config}
						formRef={brandingFormRef}
						disabled={busy}
						error={stepError}
						onSubmit={runStep}
						onSave={saveConfig}
						onConfigUpdate={setConfig}
						onAdvance={advance}
						onError={setStepError}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.branding-step"
					/>
				);
			case 'communities':
				return (
					<CommunitiesStep
						formRef={communitiesFormRef}
						showErrors={showErrors}
						onShowErrors={setShowErrors}
						onSubmit={runStep}
						onAdvance={advance}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.communities-step"
					/>
				);
			case 'direct_messages':
				return (
					<DirectMessagesStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.direct-messages-step.set-finish-draft" />
				);
			case 'personal_notes':
				return (
					<PersonalNotesStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.personal-notes-step.set-finish-draft" />
				);
			case 'registration':
				return (
					<RegistrationStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.registration-step.set-finish-draft" />
				);
			case 'push_relay':
				return (
					<PushRelayStep data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.push-relay-step.set-finish-draft" />
				);
			case 'gif_youtube':
				return (
					<ServicesStep
						draftCache={serviceDraftCache}
						formRef={servicesFormRef}
						klipyStored={config?.integrations.gif.klipy_api_key_set ?? false}
						youtubeStored={config?.integrations.youtube.api_key_set ?? false}
						disabled={busy}
						error={stepError}
						onSubmit={runStep}
						onSave={saveConfig}
						onAdvance={advance}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.services-step.set-service-keys"
					/>
				);
			case 'attachments':
				return (
					<AttachmentsStep
						formRef={attachmentsFormRef}
						error={stepError}
						onSubmit={runStep}
						onSave={saveConfig}
						onAdvance={advance}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.attachments-step.set-attachments-enabled"
					/>
				);
			case 'review':
				return (
					<ReviewStep
						config={config}
						finishDraft={finishDraft}
						productName={productName}
						themeColor={themeColor}
						assets={assets}
						attachmentsEnabled={attachmentsEnabled}
						accountUsername={accountValues.username}
						onChange={reviewChange}
						onEmailChange={() => {
							setEmailAnswer('now');
							reviewChange('email_service');
						}}
						error={stepError}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.review-step"
					/>
				);
			case 'done':
				return (
					<DoneStep
						name={productName.trim() || RuntimeConfig.productName}
						registrationMode={finishDraft.registrationMode}
						origin={window.location.origin}
						adminUrl={RuntimeConfig.appPublic.setup.admin_url}
						onOpen={openInstance}
						data-flx="app.setup.self-hosted-setup-wizard-gate.render-step.done-step"
					/>
				);
		}
	};

	const isMobile = isMobileExperienceEnabled();
	const backdropUrl = resolveLayoutAuthBrand(RuntimeConfig.getSnapshotOrNull())?.backdropUrl ?? null;
	const isWideStep = step === 'look';
	const progressPercent = progress ? Math.round((progress.current / progress.total) * 100) : 0;
	const stepCountText = progress
		? i18n._(STEP_COUNT_DESCRIPTOR, {current: progress.current, total: progress.total})
		: '';

	const progressBar = (
		<div
			className={styles.progressTrack}
			role="progressbar"
			aria-label={i18n._(PROGRESS_LABEL_DESCRIPTOR)}
			aria-valuemin={0}
			aria-valuemax={100}
			aria-valuenow={isDone ? 100 : progressPercent}
			aria-valuetext={stepCountText || undefined}
			data-flx="app.self-hosted-setup-wizard-gate.progress"
		>
			<div
				className={styles.progressFill}
				style={{width: `${isDone ? 100 : progressPercent}%`}}
				data-flx="app.self-hosted-setup-wizard-gate.progress-fill"
			/>
		</div>
	);

	let stepContent: React.ReactNode;
	if (loadError) {
		stepContent = (
			<div className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.load-error-wrap">
				<p className={styles.alert} role="alert" data-flx="app.self-hosted-setup-wizard-gate.load-error">
					{i18n._(loadError)}
				</p>
				{loadError === NOT_ADMIN_DESCRIPTOR && (
					<Button
						variant="secondary"
						onClick={signOutOfSetup}
						data-flx="app.self-hosted-setup-wizard-gate.load-error-sign-out"
					>
						{i18n._(SIGN_OUT_DESCRIPTOR)}
					</Button>
				)}
			</div>
		);
	} else if (loadingSession) {
		stepContent = (
			<div className={styles.centeredStep} role="status" data-flx="app.self-hosted-setup-wizard-gate.loading">
				<Spinner size="large" data-flx="app.self-hosted-setup-wizard-gate.loading-spinner" />
				<span className={styles.visuallyHidden} data-flx="app.setup.self-hosted-setup-wizard-gate.visually-hidden">
					{i18n._(LOADING_DESCRIPTOR)}
				</span>
			</div>
		);
	} else {
		stepContent = (
			<SteppedCarousel
				step={step}
				steps={visibleSteps(conditions).includes(step) ? visibleSteps(conditions) : [step]}
				direction={direction}
				focusOnStepChange
				ariaLabel={i18n._(SETUP_WIZARD_DESCRIPTOR)}
				data-flx="app.self-hosted-setup-wizard-gate.carousel"
			>
				{renderStep()}
			</SteppedCarousel>
		);
	}
	const content = (
		<div className={styles.content} data-flx="app.self-hosted-setup-wizard-gate.content-inner">
			{stepContent}
		</div>
	);

	let footer: React.ReactNode = null;
	if (showFooter) {
		footer = (
			<div className={styles.footer} data-flx="app.self-hosted-setup-wizard-gate.footer">
				<div className={styles.footerStart} data-flx="app.self-hosted-setup-wizard-gate.footer-start">
					{showBack && (
						<>
							<span className={styles.backWide} data-flx="app.setup.self-hosted-setup-wizard-gate.back-wide">
								<Button
									variant="secondary"
									disabled={busy}
									leftIcon={
										<ArrowLeftIcon
											size={18}
											weight="bold"
											data-flx="app.setup.self-hosted-setup-wizard-gate.arrow-left-icon"
										/>
									}
									onClick={goBack}
									data-flx="app.self-hosted-setup-wizard-gate.back-button"
								>
									{i18n._(BACK_DESCRIPTOR)}
								</Button>
							</span>
							<span className={styles.backNarrow} data-flx="app.setup.self-hosted-setup-wizard-gate.back-narrow">
								<TextButton
									onClick={goBack}
									disabled={busy}
									data-flx="app.setup.self-hosted-setup-wizard-gate.text-button.go-back"
								>
									{i18n._(BACK_DESCRIPTOR)}
								</TextButton>
							</span>
						</>
					)}
					<span className={styles.stepCount} aria-live="polite" data-flx="app.self-hosted-setup-wizard-gate.step-count">
						{stepCountText}
					</span>
				</div>
				<div className={styles.footerPrimary} data-flx="app.self-hosted-setup-wizard-gate.footer-primary">
					{isAccountStep ? (
						<span
							ref={setSubmitSlot}
							className={styles.submitSlot}
							data-flx="app.setup.self-hosted-setup-wizard-gate.submit-slot"
						/>
					) : (
						<Button
							disabled={primaryDisabled}
							submitting={busy}
							onClick={() => void runPrimary()}
							className={styles.primaryButton}
							data-flx="app.self-hosted-setup-wizard-gate.next-button"
						>
							{primaryLabel}
						</Button>
					)}
				</div>
			</div>
		);
	}

	let wizardBody: React.ReactNode;
	if (isMobile) {
		wizardBody = (
			<div ref={wizardRef} className={styles.mobileWizard} data-flx="app.self-hosted-setup-wizard-gate.mobile-wizard">
				{progressBar}
				<div className={styles.mobileBody} data-flx="app.self-hosted-setup-wizard-gate.mobile-body">
					{content}
				</div>
				{footer && (
					<div className={styles.mobileFooter} data-flx="app.self-hosted-setup-wizard-gate.mobile-footer">
						{footer}
					</div>
				)}
			</div>
		);
	} else {
		wizardBody = (
			<AuthCardContainer
				cardClassName={clsx(styles.card, isWideStep && styles.cardWide)}
				contentClassName={styles.cardBody}
				data-flx="app.self-hosted-setup-wizard-gate.card"
			>
				<div ref={wizardRef} className={styles.cardInner} data-flx="app.setup.self-hosted-setup-wizard-gate.card-inner">
					{progressBar}
					<Scroller
						className={clsx(styles.contentScroller, isWideStep && styles.wideScroller)}
						contentClassName={isWideStep ? styles.wideFrame : styles.contentFrame}
						key="setup-wizard-content-scroller"
						data-flx="app.self-hosted-setup-wizard-gate.content"
					>
						{content}
					</Scroller>
					{footer}
				</div>
			</AuthCardContainer>
		);
	}
	return (
		<AuthRegisterDraftContext.Provider value={accountDraft}>
			<AuthShell mobile={isMobile} backdropUrl={backdropUrl} data-flx="app.self-hosted-setup-wizard-gate.auth-shell">
				{wizardBody}
			</AuthShell>
		</AuthRegisterDraftContext.Provider>
	);
});

export const SelfHostedSetupWizardGate = () => (
	<SetupBrandingDraftProvider data-flx="app.setup.self-hosted-setup-wizard-gate.setup-branding-draft-provider">
		<SetupPolicyDraftProvider data-flx="app.setup.self-hosted-setup-wizard-gate.setup-policy-draft-provider">
			<SetupEmailDraftProvider data-flx="app.setup.self-hosted-setup-wizard-gate.setup-email-draft-provider">
				<SetupAttachmentsDraftProvider data-flx="app.setup.self-hosted-setup-wizard-gate.setup-attachments-draft-provider">
					<SetupTokenDraftProvider data-flx="app.setup.self-hosted-setup-wizard-gate.setup-token-draft-provider">
						<SetupWizardController data-flx="app.setup.self-hosted-setup-wizard-gate.setup-wizard-controller" />
					</SetupTokenDraftProvider>
				</SetupAttachmentsDraftProvider>
			</SetupEmailDraftProvider>
		</SetupPolicyDraftProvider>
	</SetupBrandingDraftProvider>
);
