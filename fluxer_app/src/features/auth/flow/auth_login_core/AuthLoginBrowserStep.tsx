// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {resolveSnapshotInstanceDomain} from '@app/features/auth/AccountDisplayUtils';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import styles from '@app/features/auth/flow/auth_login_core/AuthLoginBrowserStep.module.css';
import {
	DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR,
	DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR,
} from '@app/features/auth/flow/browser_handoff/BrowserHandoffDescriptors';
import {
	BrowserLoginHandoffAction,
	type BrowserLoginHandoffPendingAction,
	useBrowserLoginHandoff,
} from '@app/features/auth/flow/browser_handoff/useBrowserLoginHandoff';
import {
	type InstanceInfo,
	instanceDomainHost,
	resolveInstanceLabel,
} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {useKnownInstances} from '@app/features/auth/flow/instance_selector/useKnownInstances';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {
	BACK_DESCRIPTOR,
	CHANGE_INSTANCE_DESCRIPTOR,
	COPIED_DESCRIPTOR,
	COPY_CODE_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import {Spinner, SpinnerSize} from '@app/features/ui/components/Spinner';
import {SteppedCarousel, SteppedCarouselActions} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {flxElementClassName} from '@app/lib/react';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowClockwiseIcon,
	ArrowLeftIcon,
	ArrowSquareOutIcon,
	CheckCircleIcon,
	ClipboardIcon,
	GlobeIcon,
	PasswordIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {type ReactElement, type ReactNode, useCallback, useEffect, useMemo, useState} from 'react';

const OPEN_BROWSER_DESCRIPTOR = msg({
	message: 'Open browser',
	comment: 'Button label that opens the browser sign-in handoff page.',
});
const SHOW_CODE_DESCRIPTOR = msg({
	message: 'Show code',
	comment: 'Button label that opens the manual browser handoff code fallback.',
});
const GENERATE_NEW_CODE_DESCRIPTOR = msg({
	message: 'Generate new code',
	comment: 'Button label that replaces an expired browser sign-in code with a fresh one.',
});
const BROWSER_CODE_EXPIRED_DESCRIPTOR = msg({
	message: 'This code expired.',
	comment: 'Status shown under the browser sign-in code once its countdown reaches zero.',
});
const BROWSER_WAIT_STOPPED_DESCRIPTOR = msg({
	message: "Fluxer stopped checking whether the browser approved this sign-in. It didn't lose your code.",
	comment: 'Status shown when browser sign-in status polling gives up after repeated failures.',
});
const BROWSER_HANDOFF_UNSUPPORTED_INSTANCE_DESCRIPTOR = msg({
	message: '{instanceName} does not publish a web address, so sign-in cannot be handed to a browser.',
	comment:
		'Explanation shown when the selected instance advertises no web app endpoint. Instance name is interpolated.',
});

const logger = new Logger('AuthLoginBrowserStep');

export function resolveShouldOfferBrowserStep(): boolean {
	return isDesktop();
}

export const BrowserHandoffStep = Object.freeze({
	READY: 'ready',
	WAITING: 'waiting',
	MANUAL: 'manual',
} as const);

export type BrowserHandoffStep = (typeof BrowserHandoffStep)[keyof typeof BrowserHandoffStep];

const BROWSER_STEPS: ReadonlyArray<BrowserHandoffStep> = Object.freeze([
	BrowserHandoffStep.READY,
	BrowserHandoffStep.WAITING,
	BrowserHandoffStep.MANUAL,
]);

type ManualReturnStep = typeof BrowserHandoffStep.READY | typeof BrowserHandoffStep.WAITING;

interface BrowserInstanceIdentity {
	readonly domain: string;
	readonly isOfficial: boolean;
	readonly name: string;
}

function findKnownInstanceName(domain: string, knownInstances: ReadonlyArray<InstanceInfo>): string | null {
	for (const instance of knownInstances) {
		if (instance.domain === domain) {
			return instance.name;
		}
	}
	return null;
}

interface ResolveBrowserInstanceIdentityRequest {
	readonly snapshot: RuntimeConfigSnapshot;
	readonly knownInstances: ReadonlyArray<InstanceInfo>;
}

function resolveBrowserInstanceIdentity({
	snapshot,
	knownInstances,
}: ResolveBrowserInstanceIdentityRequest): BrowserInstanceIdentity {
	const apiEndpoint = snapshot.apiEndpoint ?? '';
	const domain = resolveSnapshotInstanceDomain(snapshot) ?? apiEndpoint;
	const isOfficial = isOfficialInstanceHost(apiEndpoint);
	if (isOfficial) {
		return {domain, isOfficial, name: OFFICIAL_INSTANCE_NAME};
	}
	const productLabel = resolveInstanceLabel(snapshot.appPublic?.branding?.product_name, domain);
	if (productLabel !== instanceDomainHost(domain)) {
		return {domain, isOfficial, name: productLabel};
	}
	return {domain, isOfficial, name: resolveInstanceLabel(findKnownInstanceName(domain, knownInstances), domain)};
}

function resolveManualReturnStep(step: BrowserHandoffStep): ManualReturnStep {
	if (step === BrowserHandoffStep.WAITING) {
		return BrowserHandoffStep.WAITING;
	}
	return BrowserHandoffStep.READY;
}

function renderCopyCodeIcon(copied: boolean): ReactNode {
	if (copied) {
		return (
			<CheckCircleIcon
				size={remFromPx(16)}
				weight="bold"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.render-copy-code-icon.check-circle-icon"
			/>
		);
	}
	return (
		<ClipboardIcon
			size={remFromPx(16)}
			weight="bold"
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.render-copy-code-icon.clipboard-icon"
		/>
	);
}

function resolveCopyCodeLabel(i18n: I18n, copied: boolean): string {
	if (copied) {
		return i18n._(COPIED_DESCRIPTOR);
	}
	return i18n._(COPY_CODE_DESCRIPTOR);
}

interface BrowserStepActionsProps {
	readonly activeBackAction: () => void;
	readonly canStart: boolean;
	readonly copied: boolean;
	readonly copyCode: () => void;
	readonly hasCode: boolean;
	readonly hasStoppedWaiting: boolean;
	readonly isExpired: boolean;
	readonly isGenerating: boolean;
	readonly onOpenBrowser: () => void;
	readonly onRegenerateCode: () => void;
	readonly onResumeWaiting: () => void;
	readonly onShowCode: () => void;
	readonly pendingAction: BrowserLoginHandoffPendingAction;
	readonly showBackButton: boolean;
	readonly step: BrowserHandoffStep;
}

function BrowserStepActions({
	activeBackAction,
	canStart,
	copied,
	copyCode,
	hasCode,
	hasStoppedWaiting,
	isExpired,
	isGenerating,
	onOpenBrowser,
	onRegenerateCode,
	onResumeWaiting,
	onShowCode,
	pendingAction,
	showBackButton,
	step,
}: BrowserStepActionsProps): ReactElement {
	const {i18n} = useLingui();
	const handoffButtonState = (action: BrowserLoginHandoffAction): {disabled: boolean; submitting: boolean} => ({
		disabled: !canStart || (isGenerating && pendingAction !== action),
		submitting: isGenerating && pendingAction === action,
	});
	const regenerateButton = (
		<Button
			onClick={onRegenerateCode}
			{...handoffButtonState(BrowserLoginHandoffAction.SHOW_CODE)}
			leftIcon={
				<ArrowClockwiseIcon
					size={remFromPx(16)}
					weight="bold"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions.arrow-clockwise-icon"
				/>
			}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.generate-new-code"
		>
			{i18n._(GENERATE_NEW_CODE_DESCRIPTOR)}
		</Button>
	);
	const showCodeButton = (
		<Button
			variant={ButtonVariant.SECONDARY}
			onClick={onShowCode}
			{...handoffButtonState(BrowserLoginHandoffAction.SHOW_CODE)}
			leftIcon={
				<PasswordIcon
					size={remFromPx(16)}
					weight="bold"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions.password-icon"
				/>
			}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.show-code"
		>
			{i18n._(SHOW_CODE_DESCRIPTOR)}
		</Button>
	);
	const resumeButton = hasStoppedWaiting ? (
		<Button
			onClick={onResumeWaiting}
			leftIcon={
				<ArrowClockwiseIcon
					size={remFromPx(16)}
					weight="bold"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions.resume-icon"
				/>
			}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.resume-waiting"
		>
			{i18n._(TRY_AGAIN_DESCRIPTOR)}
		</Button>
	) : null;
	let primaryActions: ReactNode;
	if (step === BrowserHandoffStep.MANUAL) {
		primaryActions = (
			<>
				{isExpired ? (
					regenerateButton
				) : (
					<Button
						onClick={copyCode}
						disabled={!hasCode}
						leftIcon={renderCopyCodeIcon(copied)}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.copy-code"
					>
						{resolveCopyCodeLabel(i18n, copied)}
					</Button>
				)}
				{resumeButton}
			</>
		);
	} else if (step === BrowserHandoffStep.WAITING) {
		primaryActions = (
			<>
				{showCodeButton}
				{resumeButton}
			</>
		);
	} else {
		primaryActions = (
			<>
				{showCodeButton}
				<Button
					onClick={onOpenBrowser}
					{...handoffButtonState(BrowserLoginHandoffAction.OPEN_BROWSER)}
					leftIcon={
						<ArrowSquareOutIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions.arrow-square-out-icon"
						/>
					}
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.open-browser"
				>
					{i18n._(OPEN_BROWSER_DESCRIPTOR)}
				</Button>
			</>
		);
	}
	const renderBackAction = (): ReactNode => {
		if (!showBackButton) {
			return null;
		}
		return (
			<Button
				type="button"
				variant={ButtonVariant.SECONDARY}
				leftIcon={
					<ArrowLeftIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.render-back-action.arrow-left-icon"
					/>
				}
				onClick={activeBackAction}
				disabled={isGenerating}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.back"
			>
				{i18n._(BACK_DESCRIPTOR)}
			</Button>
		);
	};
	return (
		<SteppedCarouselActions
			className={styles.browserFooter}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions.browser-footer"
		>
			{renderBackAction()}
			{primaryActions}
		</SteppedCarouselActions>
	);
}

interface BrowserStepContentProps {
	readonly canStart: boolean;
	readonly displayCode: string;
	readonly hasStoppedWaiting: boolean;
	readonly instanceName: string;
	readonly isExpired: boolean;
	readonly remainingSeconds: number | null;
	readonly step: BrowserHandoffStep;
}

function renderManualCodeTimer(i18n: I18n, isExpired: boolean, remainingSeconds: number | null): ReactNode {
	if (remainingSeconds == null) {
		return null;
	}
	if (isExpired) {
		return (
			<flx-auth-login-browser-step-timer
				className={flxElementClassName(styles.browserTimerExpired)}
				role="status"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-timer-expired"
			>
				{i18n._(BROWSER_CODE_EXPIRED_DESCRIPTOR)}
			</flx-auth-login-browser-step-timer>
		);
	}
	return (
		<flx-auth-login-browser-step-timer
			className={flxElementClassName(styles.browserTimer)}
			aria-live="off"
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-timer"
		>
			<Trans>Expires in {remainingSeconds}s</Trans>
		</flx-auth-login-browser-step-timer>
	);
}

function BrowserStepContent({
	canStart,
	displayCode,
	hasStoppedWaiting,
	instanceName,
	isExpired,
	remainingSeconds,
	step,
}: BrowserStepContentProps): ReactElement {
	const {i18n} = useLingui();
	if (step === BrowserHandoffStep.READY) {
		return (
			<flx-auth-login-browser-step-status-panel
				className={flxElementClassName(styles.browserStatusPanel)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-status-panel"
			>
				{canStart ? (
					<ArrowSquareOutIcon
						size={remFromPx(28)}
						weight="regular"
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.arrow-square-out-icon"
					/>
				) : (
					<WarningCircleIcon
						size={remFromPx(28)}
						weight="regular"
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.warning-circle-icon"
					/>
				)}
				<flx-auth-login-browser-step-status-text
					className={flxElementClassName(styles.browserStatusText)}
					role="status"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-status-text"
				>
					{canStart ? (
						<Trans>Open your browser to finish signing in.</Trans>
					) : (
						i18n._(BROWSER_HANDOFF_UNSUPPORTED_INSTANCE_DESCRIPTOR, {instanceName})
					)}
				</flx-auth-login-browser-step-status-text>
			</flx-auth-login-browser-step-status-panel>
		);
	}
	if (step === BrowserHandoffStep.WAITING) {
		return (
			<flx-auth-login-browser-step-status-panel
				className={flxElementClassName(styles.browserStatusPanel)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-status-panel--2"
			>
				{hasStoppedWaiting ? (
					<WarningCircleIcon
						size={remFromPx(28)}
						weight="regular"
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.stopped-waiting-icon"
					/>
				) : (
					<Spinner
						size={SpinnerSize.MEDIUM}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.spinner"
					/>
				)}
				<flx-auth-login-browser-step-status-text
					className={flxElementClassName(styles.browserStatusText)}
					role="status"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-status-text--2"
				>
					{hasStoppedWaiting ? i18n._(BROWSER_WAIT_STOPPED_DESCRIPTOR) : <Trans>Waiting for browser approval.</Trans>}
				</flx-auth-login-browser-step-status-text>
			</flx-auth-login-browser-step-status-panel>
		);
	}
	return (
		<flx-auth-login-browser-step-manual-panel
			className={flxElementClassName(styles.browserManualPanel)}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-manual-panel"
		>
			<flx-auth-login-browser-step-manual-code
				className={flxElementClassName(styles.browserManualCode)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content.browser-manual-code"
			>
				{displayCode}
			</flx-auth-login-browser-step-manual-code>
			{renderManualCodeTimer(i18n, isExpired, remainingSeconds)}
		</flx-auth-login-browser-step-manual-panel>
	);
}

interface AuthLoginBrowserStepProps {
	readonly extraTopContent?: ReactNode;
	readonly prefillIdentifier?: string | null;
	readonly showTitle: boolean;
	readonly title: ReactNode;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly onBack: () => void;
	readonly onBackActionChange?: (action: (() => void) | null) => void;
	readonly onChangeInstance: (() => void) | null;
	readonly onSuccess: (payload: LoginSuccessPayload) => Promise<void> | void;
	readonly showBackButton?: boolean;
}

export const AuthLoginBrowserStep = observer(function AuthLoginBrowserStep({
	extraTopContent,
	prefillIdentifier,
	showTitle,
	title,
	runtimeSnapshot,
	onBack,
	onBackActionChange,
	onChangeInstance,
	onSuccess,
	showBackButton = true,
}: AuthLoginBrowserStepProps) {
	const {i18n} = useLingui();
	const [step, setStep] = useState<BrowserHandoffStep>(BrowserHandoffStep.READY);
	const [manualReturnStep, setManualReturnStep] = useState<ManualReturnStep>(BrowserHandoffStep.READY);
	const knownInstances = useKnownInstances();
	const instanceIdentity = resolveBrowserInstanceIdentity({snapshot: runtimeSnapshot, knownInstances});
	const handoffMessages = useMemo(
		() => ({
			desktopHandoffUnavailable: DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR,
			expired: DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR,
		}),
		[],
	);
	const handleExpired = useCallback(() => {
		setManualReturnStep(BrowserHandoffStep.READY);
		setStep(BrowserHandoffStep.READY);
	}, []);
	const {
		canStart,
		copied,
		copyCode,
		displayCode,
		error,
		expireCurrentSession,
		hasCode,
		hasStoppedWaiting,
		isCurrentSessionExpired,
		isExpired,
		isGenerating,
		openBrowser,
		pendingAction,
		regenerateCode,
		remainingSeconds,
		resumeWaiting,
		showManualCode,
	} = useBrowserLoginHandoff({
		runtimeSnapshot,
		prefillIdentifier,
		messages: handoffMessages,
		onExpired: handleExpired,
		onSuccess,
	});
	const handleOpenBrowser = useCallback(() => {
		openBrowser()
			.then((opened) => {
				if (opened) {
					setStep(BrowserHandoffStep.WAITING);
				}
			})
			.catch((caught) => {
				logger.error('Failed to open the browser sign-in page', caught);
			});
	}, [openBrowser]);
	const handleShowCode = useCallback(() => {
		const returnStep = resolveManualReturnStep(step);
		showManualCode()
			.then((shown) => {
				if (shown) {
					setManualReturnStep(returnStep);
					setStep(BrowserHandoffStep.MANUAL);
				}
			})
			.catch((caught) => {
				logger.error('Failed to reveal the browser sign-in code', caught);
			});
	}, [showManualCode, step]);
	const handleRegenerateCode = useCallback(() => {
		regenerateCode()
			.then((generated) => {
				if (!generated) {
					setStep(BrowserHandoffStep.READY);
				}
			})
			.catch((caught) => {
				logger.error('Failed to regenerate the browser sign-in code', caught);
			});
	}, [regenerateCode]);
	const handleBackFromManualCode = useCallback(() => {
		if (isCurrentSessionExpired()) {
			expireCurrentSession();
			return;
		}
		setStep(manualReturnStep);
	}, [expireCurrentSession, isCurrentSessionExpired, manualReturnStep]);
	const activeBackAction = step === BrowserHandoffStep.MANUAL ? handleBackFromManualCode : onBack;
	useEffect(() => {
		if (onBackActionChange == null) {
			return;
		}
		onBackActionChange(activeBackAction);
		return () => onBackActionChange(null);
	}, [activeBackAction, onBackActionChange]);
	return (
		<flx-auth-login-browser-step
			className="flx-element"
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.flx-element"
		>
			{extraTopContent}
			{showTitle ? (
				<h1 className={loginStyles.title} data-flx="auth.flow.auth-login-core.auth-login-browser-step.h1">
					{title}
				</h1>
			) : null}
			<flx-auth-login-browser-step-pane
				className={flxElementClassName(styles.browserPane)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-pane"
			>
				<flx-auth-login-browser-step-instance-row
					className={flxElementClassName(styles.browserInstanceRow)}
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-row"
				>
					<flx-auth-login-browser-step-instance-identity
						className={flxElementClassName(styles.browserInstanceIdentity)}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-identity"
					>
						<span
							className={styles.browserInstanceLogo}
							data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-logo"
						>
							{instanceIdentity.isOfficial ? (
								<FluxerLogoAsset
									role="img"
									aria-label={OFFICIAL_INSTANCE_NAME}
									data-flx="auth.flow.auth-login-core.auth-login-browser-step.img"
								/>
							) : (
								<GlobeIcon
									size={remFromPx(20)}
									weight="regular"
									data-flx="auth.flow.auth-login-core.auth-login-browser-step.globe-icon"
								/>
							)}
						</span>
						<span
							className={styles.browserInstanceMeta}
							data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-meta"
						>
							<span
								className={styles.browserInstanceNameRow}
								data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-name-row"
							>
								<span
									className={styles.browserInstanceName}
									data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-name"
								>
									{instanceIdentity.name}
								</span>
								{instanceIdentity.isOfficial ? (
									<span
										className={styles.browserInstanceOfficialBadge}
										data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-official-badge"
									>
										<VerifiedConnectionIcon
											size={16}
											data-flx="auth.flow.auth-login-core.auth-login-browser-step.verified-connection-icon"
										/>
									</span>
								) : null}
							</span>
							<span
								className={styles.browserInstanceDomain}
								data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-instance-domain"
							>
								{instanceIdentity.domain}
							</span>
						</span>
					</flx-auth-login-browser-step-instance-identity>
					{onChangeInstance == null ? null : (
						<Button
							type="button"
							variant={ButtonVariant.SECONDARY}
							small
							leftIcon={
								<ArrowLeftIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="auth.flow.auth-login-core.auth-login-browser-step.arrow-left-icon"
								/>
							}
							onClick={onChangeInstance}
							data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.change-instance"
						>
							{i18n._(CHANGE_INSTANCE_DESCRIPTOR)}
						</Button>
					)}
				</flx-auth-login-browser-step-instance-row>
				{error != null && error.length > 0 ? (
					<div
						className={loginStyles.loginNotice}
						role="alert"
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.alert"
					>
						{error}
					</div>
				) : null}
				<SteppedCarousel
					step={step}
					steps={BROWSER_STEPS}
					focusOnStepChange
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.carousel"
				>
					<BrowserStepContent
						canStart={canStart}
						displayCode={displayCode}
						hasStoppedWaiting={hasStoppedWaiting}
						instanceName={instanceIdentity.name}
						isExpired={isExpired}
						remainingSeconds={remainingSeconds}
						step={step}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-content"
					/>
					<BrowserStepActions
						activeBackAction={activeBackAction}
						canStart={canStart}
						copied={copied}
						copyCode={copyCode}
						hasCode={hasCode}
						hasStoppedWaiting={hasStoppedWaiting}
						isExpired={isExpired}
						isGenerating={isGenerating}
						onOpenBrowser={handleOpenBrowser}
						onRegenerateCode={handleRegenerateCode}
						onResumeWaiting={resumeWaiting}
						onShowCode={handleShowCode}
						pendingAction={pendingAction}
						showBackButton={showBackButton}
						step={step}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-step-actions"
					/>
				</SteppedCarousel>
			</flx-auth-login-browser-step-pane>
		</flx-auth-login-browser-step>
	);
});
