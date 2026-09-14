// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME, SUPPORT_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {
	type RequiredActionFlow,
	resolveRequiredActionFlow,
	type VerificationChannel,
} from '@app/features/auth/components/modals/RequiredActionFlow';
import styles from '@app/features/auth/components/modals/RequiredActionModal.module.css';
import {
	ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR,
	CHOOSE_METHOD_DESCRIPTION_DESCRIPTOR,
	CHOOSE_METHOD_TITLE_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	ESCAPE_FAILED_NOTHING_CHANGED_DESCRIPTOR,
	ESCAPE_FAILED_PARTIAL_DESCRIPTOR,
	ESCAPE_SUCCESS_EMAIL_REMAINS_TOAST_DESCRIPTOR,
	ESCAPE_SUCCESS_TOAST_DESCRIPTOR,
	ESCAPE_UNAVAILABLE_DESCRIPTOR,
	GET_NEW_CODE_DESCRIPTOR,
	NEXT_DESCRIPTOR,
	RESEND_EMAIL_DESCRIPTOR,
	SEND_CODE_DESCRIPTOR,
	START_DESCRIPTOR,
	STEP_INTRO_BOUNCED_EMAIL_DESCRIPTION_DESCRIPTOR,
	STEP_INTRO_EMAIL_AND_PHONE_DESCRIPTION_DESCRIPTOR,
	STEP_INTRO_EMAIL_OR_PHONE_DESCRIPTION_DESCRIPTOR,
	STEP_INTRO_GENERIC_DESCRIPTION_DESCRIPTOR,
	STEP_INTRO_PHONE_DESCRIPTION_DESCRIPTOR,
	STEP_INTRO_TITLE_DESCRIPTOR,
	UPDATE_EMAIL_DESCRIPTOR,
	USE_EMAIL_DESCRIPTOR,
	USE_PHONE_DESCRIPTOR,
	VERIFY_PHONE_DESCRIPTOR,
} from '@app/features/auth/components/modals/required_action/RequiredActionDescriptors';
import {
	EmailCheckStep,
	EmailCodeStep,
	EmailHelpStep,
	NewEmailStep,
	useEmailVerification,
} from '@app/features/auth/components/modals/required_action/RequiredActionEmail';
import {buildPhoneGateEscapeConfirmCopy} from '@app/features/auth/components/modals/required_action/RequiredActionEscapeCopy';
import {
	InboundPhoneInstructionStep,
	InboundPhoneStartStep,
	PhoneCodeStep,
	PhoneNumberStep,
	usePhoneVerification,
} from '@app/features/auth/components/modals/required_action/RequiredActionPhone';
import {
	BackButton,
	RequiredActionAltRoutes,
	RequiredActionBackdrop,
	resolveRequiredActionErrorMessage,
	SignOutFooterRow,
	StepShell,
} from '@app/features/auth/components/modals/required_action/RequiredActionShared';
import type {
	ActiveInboundChallenge,
	EmailScreen,
	PhoneScreen,
} from '@app/features/auth/components/modals/required_action/RequiredActionTypes';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {CANCEL_DESCRIPTOR, VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {PhoneGateEscapePreviewResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

type RequiredActionViewKind =
	| 'intro'
	| 'choose-method'
	| 'email-check'
	| 'email-help'
	| 'email-recovery-new'
	| 'email-recovery-code'
	| 'bounced-email-new'
	| 'bounced-email-code'
	| 'phone-inbound-start'
	| 'phone-inbound-prepare'
	| 'phone-inbound-send'
	| 'phone-inbound-wait'
	| 'phone-number'
	| 'phone-code';

interface RequiredActionView {
	kind: RequiredActionViewKind;
}

const REQUIRED_ACTION_VIEW_ORDER: ReadonlyArray<RequiredActionViewKind> = [
	'intro',
	'choose-method',
	'email-check',
	'email-help',
	'email-recovery-new',
	'email-recovery-code',
	'bounced-email-new',
	'bounced-email-code',
	'phone-inbound-start',
	'phone-inbound-prepare',
	'phone-inbound-send',
	'phone-inbound-wait',
	'phone-number',
	'phone-code',
];

function emailScreenToView(screen: EmailScreen): RequiredActionView {
	switch (screen.kind) {
		case 'email-instructions':
			return {kind: 'email-check'};
		case 'email-recovery-new':
			return {kind: 'email-recovery-new'};
		case 'email-recovery-code':
			return {kind: 'email-recovery-code'};
		case 'bounced-email-new':
			return {kind: 'bounced-email-new'};
		case 'bounced-email-code':
			return {kind: 'bounced-email-code'};
	}
}

function phoneScreenToView(screen: PhoneScreen): RequiredActionView {
	switch (screen.kind) {
		case 'phone-inbound-start':
			return {kind: 'phone-inbound-start'};
		case 'phone-inbound-challenge':
			return {kind: 'phone-inbound-prepare'};
		case 'phone-number':
			return {kind: 'phone-number'};
		case 'phone-code':
			return {kind: 'phone-code'};
	}
}

function isEmailView(kind: RequiredActionViewKind): boolean {
	return (
		kind === 'email-check' ||
		kind === 'email-help' ||
		kind === 'email-recovery-new' ||
		kind === 'email-recovery-code' ||
		kind === 'bounced-email-new' ||
		kind === 'bounced-email-code'
	);
}

function isPhoneView(kind: RequiredActionViewKind): boolean {
	return (
		kind === 'phone-inbound-start' ||
		kind === 'phone-inbound-prepare' ||
		kind === 'phone-inbound-send' ||
		kind === 'phone-inbound-wait' ||
		kind === 'phone-number' ||
		kind === 'phone-code'
	);
}

function isPhoneInboundView(kind: RequiredActionViewKind): boolean {
	return (
		kind === 'phone-inbound-start' ||
		kind === 'phone-inbound-prepare' ||
		kind === 'phone-inbound-send' ||
		kind === 'phone-inbound-wait'
	);
}

function getIntroDescription(flow: RequiredActionFlow | null, isEmailBounced: boolean): MessageDescriptor {
	if (isEmailBounced) return STEP_INTRO_BOUNCED_EMAIL_DESCRIPTION_DESCRIPTOR;
	if (flow?.mode === 'email_and_phone') return STEP_INTRO_EMAIL_AND_PHONE_DESCRIPTION_DESCRIPTOR;
	if (flow?.mode === 'email_or_phone') return STEP_INTRO_EMAIL_OR_PHONE_DESCRIPTION_DESCRIPTOR;
	if (flow?.mode === 'phone') return STEP_INTRO_PHONE_DESCRIPTION_DESCRIPTOR;
	return STEP_INTRO_GENERIC_DESCRIPTION_DESCRIPTOR;
}

const RequiredActionModal: React.FC<{mock?: boolean}> = observer(({mock = false}) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const liveRequiredActionFlow = useMemo(
		() => resolveRequiredActionFlow(user?.requiredActions),
		[user?.requiredActions],
	);
	const [selectedVerificationType, setSelectedVerificationType] = useState<VerificationChannel>(
		DeveloperOptions.mockRequiredActionsSelectedTab,
	);
	const [isLoggingOut, setIsLoggingOut] = useState(false);
	const [renderedFlow, setRenderedFlow] = useState<RequiredActionFlow | null>(liveRequiredActionFlow);
	const [view, setView] = useState<RequiredActionView>({kind: 'intro'});
	const [history, setHistory] = useState<Array<RequiredActionView>>([]);
	const [carouselDirection, setCarouselDirection] = useState(1);
	const [actionError, setActionError] = useState<string | null>(null);
	const [escapePreview, setEscapePreview] = useState<PhoneGateEscapePreviewResponse | null>(null);
	const [escapeSubmitting, setEscapeSubmitting] = useState(false);
	const viewRef = useRef(view);
	const isEmailBounced = !mock && !!user?.emailBounced;
	const mockFlow = mock ? buildMockRequiredActionFlow() : null;
	const effectiveFlow = mockFlow ?? liveRequiredActionFlow ?? renderedFlow;
	const flowKey = effectiveFlow?.key;
	const defaultVerificationType = effectiveFlow?.defaultTab;
	useEffect(() => {
		viewRef.current = view;
	}, [view]);
	useEffect(() => {
		if (!mock && liveRequiredActionFlow) {
			setRenderedFlow(liveRequiredActionFlow);
		}
	}, [liveRequiredActionFlow, mock]);
	useEffect(() => {
		if (mock) {
			setSelectedVerificationType(DeveloperOptions.mockRequiredActionsSelectedTab);
		} else if (defaultVerificationType) {
			setSelectedVerificationType(defaultVerificationType);
		}
		setView({kind: 'intro'});
		setHistory([]);
		setActionError(null);
		setCarouselDirection(1);
	}, [mock, flowKey, defaultVerificationType]);
	const hasEmailVerification = !!effectiveFlow?.email;
	const hasPhoneVerification = !!effectiveFlow?.phone;
	const showMethodChoice = hasEmailVerification && hasPhoneVerification && effectiveFlow?.mode === 'email_or_phone';
	const requiresInboundPhone = effectiveFlow?.phone?.requiresInboundPhone ?? false;
	const canSelfServeEmailRecovery = !mock && !isEmailBounced && user?.verified !== true;
	const canUsePhoneRecovery = hasPhoneVerification;
	useEffect(() => {
		if (mock) {
			setEscapePreview({available: true, guilds: [], owned_guilds: []});
			return;
		}
		if (!hasPhoneVerification) {
			setEscapePreview(null);
			return;
		}
		let cancelled = false;
		void UserCommands.getPhoneGateEscapePreview()
			.then((preview) => {
				if (!cancelled) setEscapePreview(preview);
			})
			.catch(() => {
				if (!cancelled) setEscapePreview(null);
			});
		return () => {
			cancelled = true;
		};
	}, [mock, hasPhoneVerification, flowKey]);
	const goTo = useCallback((nextView: RequiredActionView) => {
		setCarouselDirection(1);
		setActionError(null);
		setHistory((current) => [...current, viewRef.current]);
		setView(nextView);
	}, []);
	const goBack = useCallback(() => {
		setCarouselDirection(-1);
		setActionError(null);
		setHistory((current) => {
			const previous = current.at(-1);
			if (!previous) return current;
			setView(previous);
			return current.slice(0, -1);
		});
	}, []);
	const goBackToFallback = useCallback((fallbackView: RequiredActionView) => {
		setCarouselDirection(-1);
		setActionError(null);
		setHistory((current) => {
			const previous = current.at(-1);
			setView(previous ?? fallbackView);
			return previous ? current.slice(0, -1) : current;
		});
	}, []);
	const goToPhoneCodeFromInbound = useCallback(() => {
		setCarouselDirection(1);
		setActionError(null);
		setHistory((current) => {
			const retainedHistory = current.filter((entry) => !isPhoneInboundView(entry.kind));
			if (retainedHistory.at(-1)?.kind === 'phone-number') {
				return retainedHistory;
			}
			return [...retainedHistory, {kind: 'phone-number'}];
		});
		setView({kind: 'phone-code'});
	}, []);
	const switchToPhonePath = useCallback(() => {
		setSelectedVerificationType('phone');
	}, []);
	const phoneVerification = usePhoneVerification({
		mock,
		requiresInboundPhone,
		resetKey: flowKey,
	});
	const emailVerification = useEmailVerification({
		mock,
		isEmailBounced,
		resetKey: flowKey,
		onSwitchToPhoneTab: switchToPhonePath,
	});
	const startPath = useCallback(
		(channel: VerificationChannel) => {
			setSelectedVerificationType(channel);
			goTo(
				channel === 'email' ? emailScreenToView(emailVerification.screen) : phoneScreenToView(phoneVerification.screen),
			);
		},
		[emailVerification.screen, goTo, phoneVerification.screen],
	);
	const handleIntroNext = useCallback(() => {
		if (!effectiveFlow) return;
		if (showMethodChoice) {
			goTo({kind: 'choose-method'});
			return;
		}
		const channel = hasEmailVerification ? 'email' : hasPhoneVerification ? 'phone' : selectedVerificationType;
		startPath(channel);
	}, [
		effectiveFlow,
		goTo,
		hasEmailVerification,
		hasPhoneVerification,
		selectedVerificationType,
		showMethodChoice,
		startPath,
	]);
	useEffect(() => {
		if (!isEmailView(viewRef.current.kind)) return;
		const nextView = emailScreenToView(emailVerification.screen);
		if (viewRef.current.kind !== nextView.kind) {
			goTo(nextView);
		}
	}, [emailVerification.screen.kind, goTo]);
	useEffect(() => {
		if (!isPhoneView(viewRef.current.kind)) return;
		if (phoneVerification.screen.kind === 'phone-inbound-challenge') {
			if (
				viewRef.current.kind === 'phone-inbound-prepare' ||
				viewRef.current.kind === 'phone-inbound-send' ||
				viewRef.current.kind === 'phone-inbound-wait'
			) {
				return;
			}
		}
		const nextView = phoneScreenToView(phoneVerification.screen);
		if (viewRef.current.kind !== nextView.kind) {
			goTo(nextView);
		}
	}, [phoneVerification.screen.kind, goTo]);
	const handleModalClose = useCallback(() => {
		if (mock) {
			ModalCommands.pop();
		}
	}, [mock]);
	const handleLogout = useCallback(async () => {
		if (isLoggingOut) return;
		setIsLoggingOut(true);
		try {
			await AuthenticationCommands.logout();
		} finally {
			setIsLoggingOut(false);
		}
	}, [isLoggingOut]);
	const runPhoneAction = useCallback(
		async <Result,>(
			action: () => Promise<Result>,
			context: 'phone-number' | 'phone-code' | 'general' = 'general',
		): Promise<Result | null> => {
			setActionError(null);
			try {
				return await action();
			} catch (error) {
				setActionError(resolveRequiredActionErrorMessage(i18n, error, context));
				return null;
			}
		},
		[i18n],
	);
	const handleEscape = useCallback(async () => {
		const hadGuildsToLeave = (escapePreview?.guilds.length ?? 0) > 0;
		setActionError(null);
		setEscapeSubmitting(true);
		try {
			const updated = await UserCommands.executePhoneGateEscape();
			Users.handleUserUpdate(updated, {clearMissingOptionalFields: true});
			const remaining = updated.required_actions;
			const phoneRemains = remaining.some((action) => action.includes('PHONE'));
			if (phoneRemains) {
				setActionError(i18n._(ESCAPE_UNAVAILABLE_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL}));
				return;
			}
			ToastCommands.success(
				remaining.length === 0
					? i18n._(ESCAPE_SUCCESS_TOAST_DESCRIPTOR)
					: i18n._(ESCAPE_SUCCESS_EMAIL_REMAINS_TOAST_DESCRIPTOR),
			);
		} catch (error) {
			if (failureCode(error) === APIErrorCodes.PHONE_GATE_ESCAPE_UNAVAILABLE) {
				setActionError(i18n._(ESCAPE_UNAVAILABLE_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL}));
			} else if (hadGuildsToLeave) {
				setActionError(i18n._(ESCAPE_FAILED_PARTIAL_DESCRIPTOR));
			} else {
				setActionError(i18n._(ESCAPE_FAILED_NOTHING_CHANGED_DESCRIPTOR));
			}
			void UserCommands.getPhoneGateEscapePreview()
				.then(setEscapePreview)
				.catch(() => undefined);
		} finally {
			setEscapeSubmitting(false);
		}
	}, [escapePreview, i18n]);
	const handleEscapePress = useCallback(() => {
		if (!escapePreview?.available) return;
		const copy = buildPhoneGateEscapeConfirmCopy(i18n, {
			guildNames: escapePreview.guilds.map((guild) => guild.name),
			ownedGuildNames: escapePreview.owned_guilds.map((guild) => guild.name),
			emailStepRemains: hasEmailVerification,
		});
		ModalCommands.push(
			ModalCommands.modal(() => (
				<ConfirmModal
					title={copy.title}
					description={
						<div className={styles.escapeConfirmLines} data-flx="auth.required-action-modal.escape-confirm.lines">
							{copy.bodyLines.map((line) => (
								<p key={line} data-flx="auth.required-action-modal.escape-confirm.line">
									{line}
								</p>
							))}
						</div>
					}
					primaryText={copy.primaryText}
					primaryVariant={copy.primaryVariant}
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={handleEscape}
					data-flx="auth.required-action-modal.escape-confirm.confirm-modal"
				/>
			)),
		);
	}, [escapePreview, hasEmailVerification, handleEscape, i18n]);
	const escapeAction = escapePreview?.available
		? {guildCount: escapePreview.guilds.length, submitting: escapeSubmitting, onPress: handleEscapePress}
		: null;
	const activeInboundChallenge: ActiveInboundChallenge | null =
		phoneVerification.screen.kind === 'phone-inbound-challenge'
			? {
					code: phoneVerification.screen.code,
					ourNumber: phoneVerification.screen.ourNumber,
					reason: phoneVerification.screen.reason,
				}
			: null;
	const renderView = (): React.ReactNode => {
		switch (view.kind) {
			case 'intro': {
				const descriptionDescriptor = getIntroDescription(effectiveFlow, isEmailBounced);
				const description =
					descriptionDescriptor === STEP_INTRO_GENERIC_DESCRIPTION_DESCRIPTOR
						? i18n._(descriptionDescriptor, {productName: PRODUCT_NAME})
						: i18n._(descriptionDescriptor);
				return (
					<StepShell
						title={i18n._(STEP_INTRO_TITLE_DESCRIPTOR)}
						description={description}
						notice={
							hasPhoneVerification ? (
								<RequiredActionAltRoutes
									escapeAction={escapeAction}
									data-flx="auth.required-action-modal.render-view.required-action-alt-routes"
								/>
							) : undefined
						}
						data-flx="auth.required-action-modal.render-view.step-shell"
					/>
				);
			}
			case 'choose-method':
				return (
					<StepShell
						title={i18n._(CHOOSE_METHOD_TITLE_DESCRIPTOR)}
						description={i18n._(CHOOSE_METHOD_DESCRIPTION_DESCRIPTOR)}
						data-flx="auth.required-action-modal.render-view.step-shell--2"
					>
						<div className={styles.choiceGroup} data-flx="auth.required-action-modal.choice-group">
							<Button
								fitContainer
								onClick={() => startPath('email')}
								data-flx="auth.required-action-modal.button.use-email"
							>
								{i18n._(USE_EMAIL_DESCRIPTOR)}
							</Button>
							<Button
								fitContainer
								variant="secondary"
								onClick={() => startPath('phone')}
								data-flx="auth.required-action-modal.button.use-phone"
							>
								{i18n._(USE_PHONE_DESCRIPTOR)}
							</Button>
						</div>
					</StepShell>
				);
			case 'email-check':
				return (
					<EmailCheckStep
						userEmail={user?.email}
						onNeedAnotherWay={() => goTo({kind: 'email-help'})}
						data-flx="auth.required-action-modal.render-view.email-check-step"
					/>
				);
			case 'email-help':
				return (
					<EmailHelpStep
						canSelfServeEmailRecovery={canSelfServeEmailRecovery}
						canUsePhoneRecovery={canUsePhoneRecovery}
						onUseDifferentEmail={() => {
							emailVerification.onStartEmailRecovery();
							goTo({kind: 'email-recovery-new'});
						}}
						onUsePhone={() => startPath('phone')}
						data-flx="auth.required-action-modal.render-view.email-help-step"
					/>
				);
			case 'email-recovery-new':
				return (
					<NewEmailStep
						form={emailVerification.emailRecoveryForm}
						onSubmit={emailVerification.onEmailRecoverySubmit}
						isBouncedEmail={false}
						data-flx="auth.required-action-modal.render-view.new-email-step.email-recovery-submit"
					/>
				);
			case 'email-recovery-code':
				return (
					<EmailCodeStep
						form={emailVerification.emailRecoveryCodeForm}
						onSubmit={emailVerification.onEmailRecoveryCodeSubmit}
						recipient={
							emailVerification.screen.kind === 'email-recovery-code' ? emailVerification.screen.recipient : null
						}
						onResendCode={emailVerification.onResendEmailRecoveryCode}
						isResendingCode={emailVerification.isResendingEmailRecoveryCode}
						isSubmitting={emailVerification.isEmailRecoveryCodeSubmitting}
						data-flx="auth.required-action-modal.render-view.email-code-step.email-recovery-code-submit"
					/>
				);
			case 'bounced-email-new':
				return (
					<NewEmailStep
						form={emailVerification.bouncedEmailForm}
						onSubmit={emailVerification.onBouncedEmailSubmit}
						isBouncedEmail={true}
						data-flx="auth.required-action-modal.render-view.new-email-step.bounced-email-submit"
					/>
				);
			case 'bounced-email-code':
				return (
					<EmailCodeStep
						form={emailVerification.bouncedEmailCodeForm}
						onSubmit={emailVerification.onBouncedEmailCodeSubmit}
						recipient={
							emailVerification.screen.kind === 'bounced-email-code' ? emailVerification.screen.recipient : null
						}
						onResendCode={emailVerification.onResendBouncedEmailCode}
						isResendingCode={emailVerification.isResendingBouncedEmailCode}
						isSubmitting={emailVerification.isBouncedEmailCodeSubmitting}
						data-flx="auth.required-action-modal.render-view.email-code-step.bounced-email-code-submit"
					/>
				);
			case 'phone-inbound-start':
				return (
					<InboundPhoneStartStep
						requiresInboundPhone={requiresInboundPhone}
						data-flx="auth.required-action-modal.render-view.inbound-phone-start-step"
					/>
				);
			case 'phone-inbound-prepare':
				return activeInboundChallenge ? (
					<InboundPhoneInstructionStep
						challenge={activeInboundChallenge}
						kind="prepare"
						data-flx="auth.required-action-modal.render-view.inbound-phone-instruction-step"
					/>
				) : (
					<InboundPhoneStartStep
						requiresInboundPhone={requiresInboundPhone}
						data-flx="auth.required-action-modal.render-view.inbound-phone-start-step--2"
					/>
				);
			case 'phone-inbound-send':
				return activeInboundChallenge ? (
					<InboundPhoneInstructionStep
						challenge={activeInboundChallenge}
						kind="send"
						data-flx="auth.required-action-modal.render-view.inbound-phone-instruction-step--2"
					/>
				) : (
					<InboundPhoneStartStep
						requiresInboundPhone={requiresInboundPhone}
						data-flx="auth.required-action-modal.render-view.inbound-phone-start-step--3"
					/>
				);
			case 'phone-inbound-wait':
				return activeInboundChallenge ? (
					<InboundPhoneInstructionStep
						challenge={activeInboundChallenge}
						kind="wait"
						data-flx="auth.required-action-modal.render-view.inbound-phone-instruction-step--3"
					/>
				) : (
					<InboundPhoneStartStep
						requiresInboundPhone={requiresInboundPhone}
						data-flx="auth.required-action-modal.render-view.inbound-phone-start-step--4"
					/>
				);
			case 'phone-number':
				return (
					<PhoneNumberStep
						form={phoneVerification.phoneForm}
						onSubmit={phoneVerification.onPhoneFormSubmit}
						selectedCountry={phoneVerification.selectedCountry}
						formattedPhone={phoneVerification.formattedPhone}
						onCountryChange={phoneVerification.onCountryChange}
						onPhoneInput={phoneVerification.onPhoneInput}
						data-flx="auth.required-action-modal.render-view.phone-number-step.phone-form-submit"
					/>
				);
			case 'phone-code':
				return (
					<PhoneCodeStep
						form={phoneVerification.codeForm}
						onSubmit={phoneVerification.onCodeFormSubmit}
						recipient={phoneVerification.phoneCodeRecipient}
						data-flx="auth.required-action-modal.render-view.phone-code-step.code-form-submit"
					/>
				);
		}
	};
	const renderFooterActions = (): React.ReactNode => {
		const canGoBack = history.length > 0;
		switch (view.kind) {
			case 'intro':
				return effectiveFlow ? (
					<Button onClick={handleIntroNext} data-flx="auth.required-action-modal.footer.button.next">
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				) : null;
			case 'choose-method':
				return canGoBack ? (
					<BackButton
						onClick={goBack}
						data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back"
					/>
				) : null;
			case 'email-check':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--2"
							/>
						) : null}
						<Button
							onClick={emailVerification.onResendEmail}
							submitting={emailVerification.isEmailResending}
							data-flx="auth.required-action-modal.footer.button.resend-email"
						>
							{i18n._(RESEND_EMAIL_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'email-help':
				return canGoBack ? (
					<BackButton
						onClick={goBack}
						data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--3"
					/>
				) : null;
			case 'email-recovery-new':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								disabled={emailVerification.isEmailRecoverySubmitting}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--4"
							/>
						) : null}
						<Button
							onClick={emailVerification.onEmailRecoverySubmit}
							submitting={emailVerification.isEmailRecoverySubmitting}
							data-flx="auth.required-action-modal.footer.button.send-email-recovery-code"
						>
							{i18n._(SEND_CODE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'email-recovery-code':
				return (
					<>
						<BackButton
							onClick={() => {
								emailVerification.onUseDifferentRecoveryEmail();
								goBackToFallback({kind: 'email-recovery-new'});
							}}
							disabled={emailVerification.isEmailRecoveryCodeSubmitting}
							data-flx="auth.required-action-modal.render-footer-actions.back-button.use-different-recovery-email"
						/>
						<Button
							onClick={emailVerification.onEmailRecoveryCodeSubmit}
							submitting={emailVerification.isEmailRecoveryCodeSubmitting}
							data-flx="auth.required-action-modal.footer.button.update-email-recovery"
						>
							{i18n._(UPDATE_EMAIL_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'bounced-email-new':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								disabled={emailVerification.isBouncedEmailSubmitting}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--5"
							/>
						) : null}
						<Button
							onClick={emailVerification.onBouncedEmailSubmit}
							submitting={emailVerification.isBouncedEmailSubmitting}
							data-flx="auth.required-action-modal.footer.button.send-bounced-code"
						>
							{i18n._(SEND_CODE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'bounced-email-code':
				return (
					<>
						<BackButton
							onClick={() => {
								emailVerification.onUseDifferentBouncedEmail();
								goBackToFallback({kind: 'bounced-email-new'});
							}}
							disabled={emailVerification.isBouncedEmailCodeSubmitting}
							data-flx="auth.required-action-modal.render-footer-actions.back-button.use-different-bounced-email"
						/>
						<Button
							onClick={emailVerification.onBouncedEmailCodeSubmit}
							submitting={emailVerification.isBouncedEmailCodeSubmitting}
							data-flx="auth.required-action-modal.footer.button.update-bounced-email"
						>
							{i18n._(UPDATE_EMAIL_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-inbound-start':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								disabled={phoneVerification.isStartingInbound}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--6"
							/>
						) : null}
						<Button
							onClick={() => void runPhoneAction(phoneVerification.onStartInbound, 'general')}
							submitting={phoneVerification.isStartingInbound}
							data-flx="auth.required-action-modal.footer.button.start-inbound"
						>
							{i18n._(START_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-inbound-prepare':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--7"
							/>
						) : null}
						<Button
							onClick={() => goTo({kind: 'phone-inbound-send'})}
							data-flx="auth.required-action-modal.footer.button.inbound-next"
						>
							{i18n._(NEXT_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-inbound-send':
				return (
					<>
						<BackButton
							onClick={goBack}
							data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--8"
						/>
						<Button
							onClick={() => goTo({kind: 'phone-inbound-wait'})}
							data-flx="auth.required-action-modal.footer.button.inbound-wait"
						>
							{i18n._(CONTINUE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-inbound-wait':
				return (
					<>
						<BackButton
							onClick={goBack}
							disabled={phoneVerification.isStartingInbound}
							data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--9"
						/>
						<Button
							onClick={() =>
								void runPhoneAction(phoneVerification.onRefreshInboundChallenge, 'general').then((result) => {
									if (result === 'inbound-challenge') {
										goBackToFallback({kind: 'phone-inbound-send'});
									} else if (result === 'phone-code') {
										goToPhoneCodeFromInbound();
									}
								})
							}
							submitting={phoneVerification.isStartingInbound}
							data-flx="auth.required-action-modal.footer.button.refresh-inbound"
						>
							{i18n._(GET_NEW_CODE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-number':
				return (
					<>
						{canGoBack ? (
							<BackButton
								onClick={goBack}
								disabled={phoneVerification.isPhoneSubmitting}
								data-flx="auth.required-action-modal.render-footer-actions.back-button.go-back--10"
							/>
						) : null}
						<Button
							onClick={phoneVerification.onPhoneFormSubmit}
							submitting={phoneVerification.isPhoneSubmitting}
							data-flx="auth.required-action-modal.footer.button.verify-phone"
						>
							{i18n._(VERIFY_PHONE_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'phone-code':
				return (
					<>
						<BackButton
							onClick={() => {
								phoneVerification.onBackToPhone();
								goBackToFallback({kind: 'phone-number'});
							}}
							disabled={phoneVerification.isCodeSubmitting}
							data-flx="auth.required-action-modal.render-footer-actions.back-button.back-to-phone"
						/>
						<Button
							onClick={phoneVerification.onCodeFormSubmit}
							submitting={phoneVerification.isCodeSubmitting}
							data-flx="auth.required-action-modal.footer.button.verify-phone-code"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</>
				);
		}
	};
	const footerActions = renderFooterActions();
	return (
		<Modal.Root
			size="small"
			centered
			onClose={handleModalClose}
			backdropSlot={
				<RequiredActionBackdrop mock={mock} data-flx="auth.required-action-modal.required-action-backdrop" />
			}
			data-flx="auth.required-action-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR)}
				hideCloseButton
				data-flx="auth.required-action-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.required-action-modal.modal-content">
				<Modal.ContentLayout className={styles.container} data-flx="auth.required-action-modal.container">
					<SteppedCarousel
						step={view.kind}
						steps={REQUIRED_ACTION_VIEW_ORDER}
						direction={carouselDirection}
						focusOnStepChange
						ariaLabel={i18n._(ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR)}
						ariaLive="polite"
						data-flx="auth.required-action-modal.carousel"
					>
						{renderView()}
					</SteppedCarousel>
					{actionError ? (
						<div className={styles.errorNotice} data-flx="auth.required-action-modal.error-notice">
							{actionError}
						</div>
					) : null}
					{hasPhoneVerification && view.kind !== 'intro' ? (
						<div className={styles.supportBlock} data-flx="auth.required-action-modal.alt-routes-outside">
							<RequiredActionAltRoutes
								escapeAction={escapeAction}
								data-flx="auth.required-action-modal.required-action-alt-routes"
							/>
						</div>
					) : null}
					<SignOutFooterRow
						mock={mock}
						isLoggingOut={isLoggingOut}
						onDismiss={handleModalClose}
						onLogout={handleLogout}
						data-flx="auth.required-action-modal.sign-out-footer-row"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			{footerActions ? (
				<Modal.Footer data-flx="auth.required-action-modal.modal-footer">{footerActions}</Modal.Footer>
			) : null}
		</Modal.Root>
	);
});

function buildMockRequiredActionFlow(): RequiredActionFlow {
	const mode = DeveloperOptions.mockRequiredActionsMode;
	const reverify = DeveloperOptions.mockRequiredActionsReverify;
	const channelPlan = {
		actions: [],
		reverify,
		clearsAll: true,
		remainingActionsAfterCompletion: [],
		requiresInboundPhone: false,
	} as const;
	return {
		actions: [],
		key: `mock:${mode}:${reverify}`,
		mode,
		defaultTab: mode === 'phone' ? 'phone' : 'email',
		email: mode === 'phone' ? null : {channel: 'email', ...channelPlan},
		phone: mode === 'email' ? null : {channel: 'phone', ...channelPlan},
		reverify,
		requiresInboundPhone: false,
	};
}

export default RequiredActionModal;
