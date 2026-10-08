// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {resolveSnapshotInstanceDomain} from '@app/features/auth/AccountDisplayUtils';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import styles from '@app/features/auth/flow/auth_login_core/AuthLoginBrowserStep.module.css';
import {
	BrowserLoginHandoffAction,
	type BrowserLoginHandoffController,
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
	CANCEL_DESCRIPTOR,
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
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {SteppedCarousel, SteppedCarouselActions} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {flxElementClassName} from '@app/lib/react';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {DesktopHandoffReturnMethod} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowClockwiseIcon,
	ArrowLeftIcon,
	ArrowSquareOutIcon,
	CheckCircleIcon,
	ClipboardIcon,
	GlobeIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {type ReactElement, type ReactNode, useCallback, useEffect, useRef, useState} from 'react';

const OPEN_BROWSER_AGAIN_DESCRIPTOR = msg({
	message: 'Open browser again',
	comment: 'Button in the desktop app that reopens the browser sign-in page while the app waits for the browser.',
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
const BROWSER_HANDOFF_FAILED_DESCRIPTOR = msg({
	message: "Couldn't start signing in with your browser.",
	comment: 'Shown in the desktop app when the browser sign-in request could not be created.',
});

const logger = new Logger('AuthLoginBrowserStep');

export function resolveShouldOfferBrowserStep(): boolean {
	return isDesktop();
}

export const BrowserHandoffStep = Object.freeze({
	WAITING: 'waiting',
	OTHER_DEVICE: 'other_device',
	PROBLEM: 'problem',
} as const);

export type BrowserHandoffStep = (typeof BrowserHandoffStep)[keyof typeof BrowserHandoffStep];

const BROWSER_STEPS: ReadonlyArray<BrowserHandoffStep> = Object.freeze([
	BrowserHandoffStep.WAITING,
	BrowserHandoffStep.OTHER_DEVICE,
	BrowserHandoffStep.PROBLEM,
]);

type RetryStep = typeof BrowserHandoffStep.WAITING | typeof BrowserHandoffStep.OTHER_DEVICE;

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

function resolveSignInAddress(webAppEndpoint: string): string {
	try {
		const url = new URL(webAppEndpoint);
		return `${url.host}${url.pathname.replace(/\/+$/u, '')}/login?handoff=1`;
	} catch {
		return `${webAppEndpoint}/login?handoff=1`;
	}
}

function HandoffCode({code}: {code: string}): ReactElement {
	return (
		<flx-auth-login-browser-step-code
			className={flxElementClassName(styles.browserCode)}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.code"
		>
			{code}
		</flx-auth-login-browser-step-code>
	);
}

interface WaitingContentProps {
	readonly handoff: BrowserLoginHandoffController;
	readonly instanceName: string;
	readonly onOtherDevice: () => void;
}

function WaitingContent({handoff, instanceName, onOtherDevice}: WaitingContentProps): ReactElement {
	const {i18n} = useLingui();
	const showCode = handoff.returnMethod === DesktopHandoffReturnMethod.CODE && handoff.hasCode;
	return (
		<flx-auth-login-browser-step-status-panel
			className={flxElementClassName(styles.browserStatusPanel)}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.waiting-panel"
		>
			{handoff.hasStoppedWaiting ? (
				<WarningCircleIcon
					size={remFromPx(28)}
					weight="regular"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.stopped-waiting-icon"
				/>
			) : (
				<Spinner size={SpinnerSize.MEDIUM} data-flx="auth.flow.auth-login-core.auth-login-browser-step.spinner" />
			)}
			<flx-auth-login-browser-step-status-title
				className={flxElementClassName(styles.browserStatusTitle)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.waiting-title"
			>
				<Trans comment="Title in the desktop app while it waits for the user to sign in on the browser page it opened.">
					Continue in your browser
				</Trans>
			</flx-auth-login-browser-step-status-title>
			<flx-auth-login-browser-step-status-text
				className={flxElementClassName(styles.browserStatusText)}
				role="status"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.waiting-text"
			>
				{handoff.hasStoppedWaiting ? (
					i18n._(BROWSER_WAIT_STOPPED_DESCRIPTOR)
				) : showCode ? (
					<Trans comment="Shown in the desktop app above the code when the browser cannot return the sign-in automatically. instanceName is the server name, such as Fluxer.">
						Sign in to {instanceName} in your browser, and check that it shows this code:
					</Trans>
				) : (
					<Trans comment="Shown in the desktop app while it waits for the browser. instanceName is the server name, such as Fluxer.">
						Sign in to {instanceName} in your browser and confirm. This app finishes signing in on its own.
					</Trans>
				)}
			</flx-auth-login-browser-step-status-text>
			{showCode && !handoff.hasStoppedWaiting ? <HandoffCode code={handoff.displayCode} /> : null}
			<FocusRing offset={-2} data-flx="auth.flow.auth-login-core.auth-login-browser-step.focus-ring.other-device">
				<button
					type="button"
					className={styles.browserTextButton}
					onClick={onOtherDevice}
					disabled={!handoff.canStart}
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.other-device"
				>
					<Trans comment="Text button in the desktop app that shows a code to type into a browser on a different device.">
						Signing in on another device?
					</Trans>
				</button>
			</FocusRing>
		</flx-auth-login-browser-step-status-panel>
	);
}

function OtherDeviceContent({handoff, signInAddress}: {handoff: BrowserLoginHandoffController; signInAddress: string}) {
	const {i18n} = useLingui();
	const remainingSeconds = handoff.remainingSeconds;
	return (
		<flx-auth-login-browser-step-manual-panel
			className={flxElementClassName(styles.browserManualPanel)}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.other-device-panel"
		>
			<flx-auth-login-browser-step-status-title
				className={flxElementClassName(styles.browserStatusTitle)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.other-device-title"
			>
				<Trans comment="Title in the desktop app above the code the user types into a browser on another device.">
					Enter this code in your browser
				</Trans>
			</flx-auth-login-browser-step-status-title>
			<flx-auth-login-browser-step-status-text
				className={flxElementClassName(styles.browserStatusText)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.other-device-text"
			>
				<Trans comment="One line explanation in the desktop app above the code. signInAddress is the web address of the sign-in page, such as web.fluxer.app/login?handoff=1.">
					On your other device, sign in at {signInAddress} and enter this code when asked.
				</Trans>
			</flx-auth-login-browser-step-status-text>
			{handoff.hasCode ? (
				<HandoffCode code={handoff.displayCode} />
			) : (
				<Spinner size={SpinnerSize.MEDIUM} data-flx="auth.flow.auth-login-core.auth-login-browser-step.code-spinner" />
			)}
			<flx-auth-login-browser-step-timer
				className={flxElementClassName(styles.browserTimer)}
				aria-live="off"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.timer"
			>
				{remainingSeconds == null ? ' ' : <Trans>Expires in {remainingSeconds}s</Trans>}
			</flx-auth-login-browser-step-timer>
			{handoff.hasStoppedWaiting ? (
				<flx-auth-login-browser-step-status-text
					className={flxElementClassName(styles.browserStatusText)}
					role="status"
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.other-device-stopped"
				>
					{i18n._(BROWSER_WAIT_STOPPED_DESCRIPTOR)}
				</flx-auth-login-browser-step-status-text>
			) : null}
		</flx-auth-login-browser-step-manual-panel>
	);
}

function ProblemContent({message}: {message: string}): ReactElement {
	return (
		<flx-auth-login-browser-step-status-panel
			className={flxElementClassName(styles.browserStatusPanel)}
			data-flx="auth.flow.auth-login-core.auth-login-browser-step.problem-panel"
		>
			<WarningCircleIcon
				size={remFromPx(28)}
				weight="regular"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.problem-icon"
			/>
			<flx-auth-login-browser-step-status-text
				className={flxElementClassName(styles.browserStatusText)}
				role="alert"
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.problem-text"
			>
				{message}
			</flx-auth-login-browser-step-status-text>
		</flx-auth-login-browser-step-status-panel>
	);
}

function ButtonIcon({icon}: {icon: 'open' | 'retry' | 'back' | 'copy' | 'copied'}): ReactElement {
	const props = {size: remFromPx(16), weight: 'bold' as const};
	switch (icon) {
		case 'open':
			return <ArrowSquareOutIcon {...props} data-flx="auth.flow.auth-login-core.auth-login-browser-step.icon.open" />;
		case 'retry':
			return <ArrowClockwiseIcon {...props} data-flx="auth.flow.auth-login-core.auth-login-browser-step.icon.retry" />;
		case 'back':
			return <ArrowLeftIcon {...props} data-flx="auth.flow.auth-login-core.auth-login-browser-step.icon.back" />;
		case 'copy':
			return <ClipboardIcon {...props} data-flx="auth.flow.auth-login-core.auth-login-browser-step.icon.copy" />;
		case 'copied':
			return <CheckCircleIcon {...props} data-flx="auth.flow.auth-login-core.auth-login-browser-step.icon.copied" />;
	}
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
	const [step, setStep] = useState<BrowserHandoffStep>(BrowserHandoffStep.WAITING);
	const [retryStep, setRetryStep] = useState<RetryStep>(BrowserHandoffStep.WAITING);
	const knownInstances = useKnownInstances();
	const instanceIdentity = resolveBrowserInstanceIdentity({snapshot: runtimeSnapshot, knownInstances});
	const handleEnded = useCallback(() => {
		setStep(BrowserHandoffStep.PROBLEM);
	}, []);
	const handoff = useBrowserLoginHandoff({
		runtimeSnapshot,
		prefillIdentifier,
		onEnded: handleEnded,
		onSuccess,
	});
	const {canStart, openBrowser, regenerateCode, reset, showManualCode} = handoff;
	const showProblem = useCallback((from: RetryStep) => {
		setRetryStep(from);
		setStep(BrowserHandoffStep.PROBLEM);
	}, []);
	const handleOpenBrowser = useCallback(() => {
		setStep(BrowserHandoffStep.WAITING);
		openBrowser()
			.then((opened) => {
				if (!opened) {
					showProblem(BrowserHandoffStep.WAITING);
				}
			})
			.catch((caught) => {
				logger.error('Failed to open the browser sign-in page', caught);
			});
	}, [openBrowser, showProblem]);
	const handleOtherDevice = useCallback(() => {
		setRetryStep(BrowserHandoffStep.OTHER_DEVICE);
		setStep(BrowserHandoffStep.OTHER_DEVICE);
		showManualCode()
			.then((shown) => {
				if (!shown) {
					showProblem(BrowserHandoffStep.OTHER_DEVICE);
				}
			})
			.catch((caught) => {
				logger.error('Failed to show the browser sign-in code', caught);
			});
	}, [showManualCode, showProblem]);
	const handleRetry = useCallback(() => {
		if (retryStep === BrowserHandoffStep.OTHER_DEVICE) {
			setStep(BrowserHandoffStep.OTHER_DEVICE);
			regenerateCode()
				.then((generated) => {
					if (!generated) {
						showProblem(BrowserHandoffStep.OTHER_DEVICE);
					}
				})
				.catch((caught) => {
					logger.error('Failed to create a new browser sign-in code', caught);
				});
			return;
		}
		handleOpenBrowser();
	}, [handleOpenBrowser, regenerateCode, retryStep, showProblem]);
	const handleCancel = useCallback(() => {
		reset();
		onBack();
	}, [onBack, reset]);
	const handleBackToWaiting = useCallback(() => {
		setRetryStep(BrowserHandoffStep.WAITING);
		setStep(BrowserHandoffStep.WAITING);
	}, []);
	const startedRef = useRef(false);
	useEffect(() => {
		if (startedRef.current) {
			return;
		}
		startedRef.current = true;
		if (canStart) {
			handleOpenBrowser();
		} else {
			setStep(BrowserHandoffStep.PROBLEM);
		}
	}, [canStart, handleOpenBrowser]);
	const activeBackAction = step === BrowserHandoffStep.OTHER_DEVICE ? handleBackToWaiting : handleCancel;
	useEffect(() => {
		if (onBackActionChange == null) {
			return;
		}
		onBackActionChange(activeBackAction);
		return () => onBackActionChange(null);
	}, [activeBackAction, onBackActionChange]);
	const problemMessage = canStart
		? (handoff.error ?? i18n._(BROWSER_HANDOFF_FAILED_DESCRIPTOR))
		: i18n._(BROWSER_HANDOFF_UNSUPPORTED_INSTANCE_DESCRIPTOR, {instanceName: instanceIdentity.name});
	const isOpening = handoff.isGenerating && handoff.pendingAction === BrowserLoginHandoffAction.OPEN_BROWSER;
	const isMakingCode = handoff.isGenerating && handoff.pendingAction === BrowserLoginHandoffAction.SHOW_CODE;
	let content: ReactNode;
	let actions: ReactNode;
	if (step === BrowserHandoffStep.OTHER_DEVICE) {
		content = (
			<OtherDeviceContent
				handoff={handoff}
				signInAddress={resolveSignInAddress(runtimeSnapshot.webAppEndpoint)}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.other-device-content"
			/>
		);
		actions = (
			<>
				{showBackButton ? (
					<Button
						variant={ButtonVariant.SECONDARY}
						leftIcon={<ButtonIcon icon="back" />}
						onClick={handleBackToWaiting}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.back-to-waiting"
					>
						{i18n._(BACK_DESCRIPTOR)}
					</Button>
				) : null}
				{handoff.hasStoppedWaiting ? (
					<Button
						leftIcon={<ButtonIcon icon="retry" />}
						onClick={handoff.resumeWaiting}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.resume-waiting"
					>
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				) : (
					<Button
						leftIcon={<ButtonIcon icon={handoff.copied ? 'copied' : 'copy'} />}
						onClick={handoff.copyCode}
						disabled={!handoff.hasCode}
						submitting={isMakingCode}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.copy-code"
					>
						{i18n._(handoff.copied ? COPIED_DESCRIPTOR : COPY_CODE_DESCRIPTOR)}
					</Button>
				)}
			</>
		);
	} else if (step === BrowserHandoffStep.PROBLEM) {
		content = (
			<ProblemContent
				message={problemMessage}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.problem-content"
			/>
		);
		actions = (
			<>
				{showBackButton ? (
					<Button
						variant={ButtonVariant.SECONDARY}
						leftIcon={<ButtonIcon icon="back" />}
						onClick={handleCancel}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.back"
					>
						{i18n._(BACK_DESCRIPTOR)}
					</Button>
				) : null}
				{canStart ? (
					<Button
						leftIcon={<ButtonIcon icon="retry" />}
						onClick={handleRetry}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.retry"
					>
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				) : null}
			</>
		);
	} else {
		content = (
			<WaitingContent
				handoff={handoff}
				instanceName={instanceIdentity.name}
				onOtherDevice={handleOtherDevice}
				data-flx="auth.flow.auth-login-core.auth-login-browser-step.waiting-content"
			/>
		);
		actions = (
			<>
				<Button
					variant={ButtonVariant.SECONDARY}
					onClick={handleCancel}
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.cancel"
				>
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				{handoff.hasStoppedWaiting ? (
					<Button
						leftIcon={<ButtonIcon icon="retry" />}
						onClick={handoff.resumeWaiting}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.resume-waiting"
					>
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				) : (
					<Button
						leftIcon={<ButtonIcon icon="open" />}
						onClick={handleOpenBrowser}
						submitting={isOpening}
						disabled={!canStart}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.button.open-browser"
					>
						{i18n._(OPEN_BROWSER_AGAIN_DESCRIPTOR)}
					</Button>
				)}
			</>
		);
	}
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
				<SteppedCarousel
					step={step}
					steps={BROWSER_STEPS}
					focusOnStepChange
					data-flx="auth.flow.auth-login-core.auth-login-browser-step.carousel"
				>
					{content}
					<SteppedCarouselActions
						className={styles.browserFooter}
						data-flx="auth.flow.auth-login-core.auth-login-browser-step.browser-footer"
					>
						{actions}
					</SteppedCarouselActions>
				</SteppedCarousel>
			</flx-auth-login-browser-step-pane>
		</flx-auth-login-browser-step>
	);
});
