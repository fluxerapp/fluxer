// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {
	fetchInstanceConfig,
	type SetupBrandingAssetKind,
	testSmtpConfig,
	updateInstanceConfig,
	uploadBrandingAsset,
} from '@app/features/app/components/setup/SetupWizardClient';
import {
	createSetupWizardSnapshot,
	selectSetupWizardModel,
	transitionSetupWizardSnapshot,
	type WizardStep,
} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {
	AdminAccountStep,
	AdminIntroStep,
	type BrandingAssetState,
	BrandingStep,
	CommunityStep,
	FinishStep,
	IntegrationStep,
	type IntegrationStepKind,
	LoadingStep,
	type MediaExpiryDraft,
	MediaExpiryStep,
	type PremiumMode,
	PremiumStep,
	type RegistrationMode,
	RegistrationStep,
	type ServiceAvailability,
	type ServiceIntegrationDraft,
	type ServiceSelection,
	ServicesStep,
	ThemeStep,
	WelcomeStep,
} from '@app/features/app/components/setup/SetupWizardSteps';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import {AuthRegisterDraftContext, type AuthRegisterFormDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {getAcceptString} from '@app/features/expressions/utils/AssetFormatCopy';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import SessionManager from '@app/features/platform/state/AuthSession';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Theme from '@app/features/theme/state/Theme';
import {Button} from '@app/features/ui/button/Button';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {fileToBase64} from '@app/features/user/utils/AvatarUtils';
import * as FormUtils from '@app/lib/forms';
import {type ThemeType, ThemeTypes} from '@fluxer/constants/src/UserConstants';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, CheckIcon, WrenchIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion, type Transition, useReducedMotion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('SelfHostedSetupWizardGate');

const SETUP_WIZARD_DESCRIPTOR = msg({
	message: 'Self-host setup',
	comment: 'Accessible label for the self-host setup wizard carousel.',
});
const HEADER_DESCRIPTOR = msg({
	message: 'Set up {productName}',
	comment: 'Title for the self-host setup wizard modal.',
});
const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Button that moves to the previous setup wizard step.',
});
const NEXT_DESCRIPTOR = msg({
	message: 'Next',
	comment: 'Button that moves to the next setup wizard step.',
});
const FINISH_DESCRIPTOR = msg({
	message: 'Finish setup',
	comment: 'Button that saves the configuration and completes the setup wizard.',
});
const LOADING_DESCRIPTOR = msg({
	message: 'Loading instance configuration',
	comment: 'Accessible status while the setup wizard loads the instance configuration.',
});
const LOAD_ERROR_DESCRIPTOR = msg({
	message: 'Could not load the instance configuration. Try reloading the page.',
	comment: 'Error shown when the setup wizard fails to load the instance configuration.',
});
const ASSET_UPLOAD_ERROR_DESCRIPTOR = msg({
	message: 'That image could not be used. Try a different file.',
	comment: 'Error shown when a branding image fails to upload in the setup wizard.',
});

const footerRevealTransition: Transition = {
	type: 'spring',
	stiffness: 460,
	damping: 40,
	mass: 0.7,
};
const footerButtonTransition: Transition = {
	type: 'spring',
	stiffness: 520,
	damping: 42,
	mass: 0.7,
};
const instantTransition: Transition = {
	duration: 0,
};
const STEP_NAVIGATION_LOCK_MS = 320;

const BRANDING_ASSET_KINDS: ReadonlyArray<SetupBrandingAssetKind> = ['icon', 'symbol', 'logo', 'wordmark', 'favicon'];

function hexToNumber(value: string | null): number {
	if (!value) return 0;
	const match = /^#?([0-9a-fA-F]{6})$/.exec(value.trim());
	if (!match) return 0;
	return parseInt(match[1], 16) >>> 0;
}

function numberToHexColor(value: number): string {
	return `#${(value >>> 0).toString(16).padStart(6, '0').slice(-6)}`;
}

function brandingAssetUrl(config: InstanceConfigResponse, kind: SetupBrandingAssetKind): string | null {
	const branding = config.app_public.branding;
	switch (kind) {
		case 'icon':
			return branding.icon_url;
		case 'symbol':
			return branding.symbol_url;
		case 'logo':
			return branding.logo_url;
		case 'wordmark':
			return branding.wordmark_url;
		case 'favicon':
			return branding.favicon_url;
	}
}

const DEFAULT_INTEGRATION_DRAFT: ServiceIntegrationDraft = {
	gifMode: 'later',
	klipyApiKey: '',
	youtubeMode: 'later',
	youtubeApiKey: '',
	captchaMode: 'later',
	captchaProvider: 'hcaptcha',
	hcaptchaSiteKey: '',
	hcaptchaSecretKey: '',
	turnstileSiteKey: '',
	turnstileSecretKey: '',
	emailMode: 'later',
	emailEnabled: true,
	emailFromEmail: '',
	emailFromName: '',
	smtpHost: '',
	smtpPort: '587',
	smtpUsername: '',
	smtpPassword: '',
	smtpSecure: true,
	blueskyMode: 'later',
	blueskyEnabled: true,
	blueskyClientName: '',
	blueskyClientUri: '',
	blueskyLogoUri: '',
	blueskyTosUri: '',
	blueskyPolicyUri: '',
	blueskyKeyId: '',
	blueskyPrivateKey: '',
};

const DEFAULT_MEDIA_EXPIRY_DRAFT: MediaExpiryDraft = {
	enabled: false,
	minSizeMb: '5',
	maxSizeMb: '500',
	maxEligibleSizeMb: '500',
	minLifetimeDays: '14',
	maxLifetimeDays: '1095',
	curve: '0.5',
	renewThresholdDays: '30',
	renewWindowDays: '30',
};

function wizardStepToIntegrationKind(step: WizardStep): IntegrationStepKind | null {
	switch (step) {
		case 'integration_gif':
			return 'gif';
		case 'integration_youtube':
			return 'youtube';
		case 'integration_captcha':
			return 'captcha';
		case 'integration_email':
			return 'email';
		case 'integration_bluesky':
			return 'bluesky';
		default:
			return null;
	}
}

function parsePort(value: string): number | null {
	const parsed = Number.parseInt(value, 10);
	return Number.isInteger(parsed) && parsed >= 1 && parsed <= 65535 ? parsed : null;
}

function parsePositiveNumber(value: string): number | null {
	const parsed = Number.parseFloat(value);
	return Number.isFinite(parsed) && parsed > 0 ? parsed : null;
}

function parsePositiveInteger(value: string): number | null {
	const parsed = Number.parseInt(value, 10);
	return Number.isInteger(parsed) && parsed >= 1 ? parsed : null;
}

function parseDecayCurve(value: string): number | null {
	const parsed = Number.parseFloat(value);
	return Number.isFinite(parsed) && parsed >= 0 && parsed <= 1 ? parsed : null;
}

function parseMediaExpiryDraft(draft: MediaExpiryDraft) {
	const minSizeMb = parsePositiveNumber(draft.minSizeMb);
	const maxSizeMb = parsePositiveNumber(draft.maxSizeMb);
	const maxEligibleSizeMb = parsePositiveNumber(draft.maxEligibleSizeMb);
	const minLifetimeDays = parsePositiveInteger(draft.minLifetimeDays);
	const maxLifetimeDays = parsePositiveInteger(draft.maxLifetimeDays);
	const curve = parseDecayCurve(draft.curve);
	const renewThresholdDays = parsePositiveInteger(draft.renewThresholdDays);
	const renewWindowDays = parsePositiveInteger(draft.renewWindowDays);
	if (
		minSizeMb === null ||
		maxSizeMb === null ||
		maxEligibleSizeMb === null ||
		minLifetimeDays === null ||
		maxLifetimeDays === null ||
		curve === null ||
		renewThresholdDays === null ||
		renewWindowDays === null ||
		maxSizeMb <= minSizeMb ||
		maxEligibleSizeMb < maxSizeMb ||
		maxLifetimeDays < minLifetimeDays
	) {
		return null;
	}
	return {
		minSizeMb,
		maxSizeMb,
		maxEligibleSizeMb,
		minLifetimeDays,
		maxLifetimeDays,
		curve,
		renewThresholdDays,
		renewWindowDays,
	};
}

function isMediaExpiryStepValid(draft: MediaExpiryDraft): boolean {
	return !draft.enabled || parseMediaExpiryDraft(draft) !== null;
}

function isIntegrationStepValid(kind: IntegrationStepKind, draft: ServiceIntegrationDraft): boolean {
	switch (kind) {
		case 'gif':
			if (draft.gifMode === 'later') return true;
			return draft.klipyApiKey.trim().length > 0;
		case 'youtube':
			return draft.youtubeMode === 'later' || draft.youtubeApiKey.trim().length > 0;
		case 'captcha':
			if (draft.captchaMode === 'later') return true;
			return draft.captchaProvider === 'hcaptcha'
				? draft.hcaptchaSiteKey.trim().length > 0 && draft.hcaptchaSecretKey.trim().length > 0
				: draft.turnstileSiteKey.trim().length > 0 && draft.turnstileSecretKey.trim().length > 0;
		case 'email':
			if (draft.emailMode === 'later' || !draft.emailEnabled) return true;
			return (
				draft.emailFromEmail.trim().length > 0 &&
				draft.emailFromName.trim().length > 0 &&
				draft.smtpHost.trim().length > 0 &&
				parsePort(draft.smtpPort) !== null &&
				draft.smtpUsername.trim().length > 0 &&
				draft.smtpPassword.trim().length > 0
			);
		case 'bluesky':
			if (draft.blueskyMode === 'later' || !draft.blueskyEnabled) return true;
			return (
				draft.blueskyClientName.trim().length > 0 &&
				draft.blueskyClientUri.trim().length > 0 &&
				draft.blueskyKeyId.trim().length > 0 &&
				draft.blueskyPrivateKey.trim().length > 0
			);
	}
}

function buildIntegrationsPatch(draft: ServiceIntegrationDraft) {
	const integrations: {
		gif?: {
			provider: 'klipy';
			klipy_api_key?: string;
		};
		youtube?: {
			api_key: string;
		};
		captcha?: {
			provider: 'hcaptcha' | 'turnstile';
			hcaptcha_site_key?: string;
			hcaptcha_secret_key?: string;
			turnstile_site_key?: string;
			turnstile_secret_key?: string;
		};
		email?: {
			enabled: boolean;
			provider: 'smtp';
			from_email: string;
			from_name: string;
			smtp: {
				host: string;
				port: number;
				username: string;
				password: string;
				secure: boolean;
			};
		};
		bluesky?: {
			enabled: boolean;
			client_name: string;
			client_uri: string;
			logo_uri?: string;
			tos_uri?: string;
			policy_uri?: string;
			keys?: Array<{kid: string; private_key: string}>;
		};
	} = {};
	if (draft.gifMode === 'configure') {
		integrations.gif = {provider: 'klipy', klipy_api_key: draft.klipyApiKey.trim()};
	}
	if (draft.youtubeMode === 'configure') {
		integrations.youtube = {api_key: draft.youtubeApiKey.trim()};
	}
	if (draft.captchaMode === 'configure') {
		integrations.captcha =
			draft.captchaProvider === 'hcaptcha'
				? {
						provider: 'hcaptcha',
						hcaptcha_site_key: draft.hcaptchaSiteKey.trim(),
						hcaptcha_secret_key: draft.hcaptchaSecretKey.trim(),
					}
				: {
						provider: 'turnstile',
						turnstile_site_key: draft.turnstileSiteKey.trim(),
						turnstile_secret_key: draft.turnstileSecretKey.trim(),
					};
	}
	if (draft.emailMode === 'configure') {
		integrations.email = {
			enabled: draft.emailEnabled,
			provider: 'smtp',
			from_email: draft.emailFromEmail.trim(),
			from_name: draft.emailFromName.trim(),
			smtp: {
				host: draft.smtpHost.trim(),
				port: parsePort(draft.smtpPort) ?? 587,
				username: draft.smtpUsername.trim(),
				password: draft.smtpPassword.trim(),
				secure: draft.smtpSecure,
			},
		};
	}
	if (draft.blueskyMode === 'configure') {
		integrations.bluesky = {
			enabled: draft.blueskyEnabled,
			client_name: draft.blueskyClientName.trim(),
			client_uri: draft.blueskyClientUri.trim(),
			logo_uri: draft.blueskyLogoUri.trim() || undefined,
			tos_uri: draft.blueskyTosUri.trim() || undefined,
			policy_uri: draft.blueskyPolicyUri.trim() || undefined,
			keys:
				draft.blueskyKeyId.trim() && draft.blueskyPrivateKey.trim()
					? [{kid: draft.blueskyKeyId.trim(), private_key: draft.blueskyPrivateKey.trim()}]
					: undefined,
		};
	}
	return Object.keys(integrations).length > 0 ? integrations : undefined;
}

function buildMediaPatch(draft: MediaExpiryDraft) {
	if (!draft.enabled) {
		return {
			attachment_decay: {
				enabled: false,
			},
		};
	}
	const parsed = parseMediaExpiryDraft(draft);
	return {
		attachment_decay: {
			enabled: true,
			min_size_mb: parsed?.minSizeMb ?? Number.parseFloat(DEFAULT_MEDIA_EXPIRY_DRAFT.minSizeMb),
			max_size_mb: parsed?.maxSizeMb ?? Number.parseFloat(DEFAULT_MEDIA_EXPIRY_DRAFT.maxSizeMb),
			max_eligible_size_mb:
				parsed?.maxEligibleSizeMb ?? Number.parseFloat(DEFAULT_MEDIA_EXPIRY_DRAFT.maxEligibleSizeMb),
			min_lifetime_days: parsed?.minLifetimeDays ?? Number.parseInt(DEFAULT_MEDIA_EXPIRY_DRAFT.minLifetimeDays, 10),
			max_lifetime_days: parsed?.maxLifetimeDays ?? Number.parseInt(DEFAULT_MEDIA_EXPIRY_DRAFT.maxLifetimeDays, 10),
			curve: parsed?.curve ?? Number.parseFloat(DEFAULT_MEDIA_EXPIRY_DRAFT.curve),
			renew_threshold_days:
				parsed?.renewThresholdDays ?? Number.parseInt(DEFAULT_MEDIA_EXPIRY_DRAFT.renewThresholdDays, 10),
			renew_window_days: parsed?.renewWindowDays ?? Number.parseInt(DEFAULT_MEDIA_EXPIRY_DRAFT.renewWindowDays, 10),
		},
	};
}

export const SelfHostedSetupWizardGate = observer(() => {
	const {i18n} = useLingui();
	const shouldReduceMotion = useReducedMotion();
	const authStoreAuthenticated = Authentication.isAuthenticated;
	const themeHydrated = Theme.isHydrated;
	const fallbackProductName = RuntimeConfig.productName;
	const welcomeInitialFocusRef = useRef<HTMLElement | null>(null);
	const registerFormDraftsRef = useRef<Map<string, AuthRegisterFormDraft>>(new Map());
	const stepNavigationLockedRef = useRef(false);
	const stepNavigationUnlockTimerRef = useRef<number | null>(null);

	const [config, setConfig] = useState<InstanceConfigResponse | null>(null);
	const [loadError, setLoadError] = useState(false);
	const [submitting, setSubmitting] = useState(false);
	const [submitError, setSubmitError] = useState<string | null>(null);
	const [stepNavigationLocked, setStepNavigationLocked] = useState(false);
	const [wizardSnapshot, setWizardSnapshot] = useState(createSetupWizardSnapshot);
	const [setupTheme, setSetupTheme] = useState<ThemeType>(ThemeTypes.SYSTEM);
	const [forceUnauthenticatedSetup, setForceUnauthenticatedSetup] = useState(false);
	const [integrationDraft, setIntegrationDraft] = useState<ServiceIntegrationDraft>(() => ({
		...DEFAULT_INTEGRATION_DRAFT,
	}));
	const [mediaExpiryDraft, setMediaExpiryDraft] = useState<MediaExpiryDraft>(() => ({
		...DEFAULT_MEDIA_EXPIRY_DRAFT,
	}));
	const [smtpTesting, setSmtpTesting] = useState(false);
	const [smtpTestResult, setSmtpTestResult] = useState<string | null>(null);

	const [productName, setProductName] = useState('');
	const [themeColor, setThemeColor] = useState(0);
	const [registrationMode, setRegistrationMode] = useState<RegistrationMode>('open');
	const [singleCommunityEnabled, setSingleCommunityEnabled] = useState(false);
	const [singleCommunityName, setSingleCommunityName] = useState('');
	const [directMessagesDisabled, setDirectMessagesDisabled] = useState(false);
	const [serviceSelection, setServiceSelection] = useState<ServiceSelection>({
		gif: false,
		youtube: false,
		bluesky: false,
	});
	const [premiumMode, setPremiumMode] = useState<PremiumMode>('mirror');
	const [assets, setAssets] = useState<ReadonlyArray<BrandingAssetState>>(() =>
		BRANDING_ASSET_KINDS.map((kind) => ({kind, url: null, preview: null})),
	);

	const getRegisterFormDraft = useCallback((draftKey: string): AuthRegisterFormDraft | undefined => {
		const draft = registerFormDraftsRef.current.get(draftKey);
		if (!draft) {
			return undefined;
		}
		return {
			...draft,
			formValues: {...draft.formValues},
		};
	}, []);

	const setRegisterFormDraft = useCallback((draftKey: string, draft: AuthRegisterFormDraft) => {
		registerFormDraftsRef.current.set(draftKey, {
			...draft,
			formValues: {...draft.formValues},
		});
	}, []);

	const clearRegisterFormDraft = useCallback((draftKey: string) => {
		registerFormDraftsRef.current.delete(draftKey);
	}, []);

	const authRegisterDraftContextValue = useMemo(
		() => ({
			getRegisterFormDraft,
			setRegisterFormDraft,
			clearRegisterFormDraft,
		}),
		[clearRegisterFormDraft, getRegisterFormDraft, setRegisterFormDraft],
	);

	const isAuthenticated = authStoreAuthenticated && !forceUnauthenticatedSetup;

	const clearStepNavigationLock = useCallback(() => {
		if (stepNavigationUnlockTimerRef.current !== null) {
			window.clearTimeout(stepNavigationUnlockTimerRef.current);
			stepNavigationUnlockTimerRef.current = null;
		}
		stepNavigationLockedRef.current = false;
		setStepNavigationLocked(false);
	}, []);

	const beginStepNavigation = useCallback(() => {
		if (stepNavigationLockedRef.current) return false;
		stepNavigationLockedRef.current = true;
		setStepNavigationLocked(true);
		if (stepNavigationUnlockTimerRef.current !== null) {
			window.clearTimeout(stepNavigationUnlockTimerRef.current);
		}
		stepNavigationUnlockTimerRef.current = window.setTimeout(() => {
			stepNavigationUnlockTimerRef.current = null;
			stepNavigationLockedRef.current = false;
			setStepNavigationLocked(false);
		}, STEP_NAVIGATION_LOCK_MS);
		return true;
	}, []);

	useEffect(
		() => () => {
			if (stepNavigationUnlockTimerRef.current !== null) {
				window.clearTimeout(stepNavigationUnlockTimerRef.current);
				stepNavigationUnlockTimerRef.current = null;
			}
			stepNavigationLockedRef.current = false;
		},
		[],
	);

	useEffect(() => {
		if (authStoreAuthenticated || !forceUnauthenticatedSetup) return;
		setForceUnauthenticatedSetup(false);
	}, [authStoreAuthenticated, forceUnauthenticatedSetup]);

	const resetStaleSetupSession = useCallback(async () => {
		logger.warn('Instance config fetch returned 401 during setup; clearing stale local setup session');
		setForceUnauthenticatedSetup(true);
		registerFormDraftsRef.current.clear();
		setConfig(null);
		setLoadError(false);
		setSubmitError(null);
		setSubmitting(false);
		setWizardSnapshot(createSetupWizardSnapshot());
		clearStepNavigationLock();
		setIntegrationDraft({...DEFAULT_INTEGRATION_DRAFT});
		setMediaExpiryDraft({...DEFAULT_MEDIA_EXPIRY_DRAFT});
		setSmtpTesting(false);
		setSmtpTestResult(null);
		try {
			await SessionManager.logout();
		} catch (error) {
			logger.warn('Failed to fully clear stale setup session', error);
			Authentication.handleSessionStart({token: null});
			Authentication.setUserId(null);
		}
		Authentication.handleLogout({skipRedirect: true});
	}, [clearStepNavigationLock]);

	const hydrateFromConfig = useCallback((next: InstanceConfigResponse) => {
		setConfig(next);
		setProductName(next.app_public.branding.product_name);
		setThemeColor(hexToNumber(next.app_public.branding.theme_color));
		setRegistrationMode(next.registration.mode);
		setSingleCommunityEnabled(next.policy.single_community_enabled);
		setDirectMessagesDisabled(next.policy.direct_messages_disabled);
		setPremiumMode(next.policy.premium_mode);
		setServiceSelection({
			gif: next.policy.services_resolved.gif_enabled,
			youtube: next.policy.services_resolved.youtube_enabled,
			bluesky: next.policy.services_resolved.bluesky_enabled,
		});
		setIntegrationDraft((current) => ({
			...current,
			captchaProvider: next.integrations.captcha.effective_provider === 'turnstile' ? 'turnstile' : 'hcaptcha',
			emailEnabled: next.integrations.email.effective_enabled || next.integrations.email.enabled !== false,
			emailFromEmail: next.integrations.email.from_email ?? current.emailFromEmail,
			emailFromName: next.integrations.email.from_name ?? current.emailFromName,
			smtpHost: next.integrations.email.smtp.host ?? current.smtpHost,
			smtpPort: next.integrations.email.smtp.port ? String(next.integrations.email.smtp.port) : current.smtpPort,
			smtpUsername: next.integrations.email.smtp.username ?? current.smtpUsername,
			smtpSecure: next.integrations.email.smtp.secure ?? current.smtpSecure,
			blueskyEnabled: next.integrations.bluesky.effective_enabled || next.integrations.bluesky.enabled !== false,
			blueskyClientName: next.integrations.bluesky.client_name ?? current.blueskyClientName,
			blueskyClientUri: next.integrations.bluesky.client_uri ?? current.blueskyClientUri,
			blueskyLogoUri: next.integrations.bluesky.logo_uri ?? current.blueskyLogoUri,
			blueskyTosUri: next.integrations.bluesky.tos_uri ?? current.blueskyTosUri,
			blueskyPolicyUri: next.integrations.bluesky.policy_uri ?? current.blueskyPolicyUri,
		}));
		setMediaExpiryDraft(() => {
			const attachmentDecay = next.media.attachment_decay;
			const effective = attachmentDecay.effective;
			return {
				enabled: attachmentDecay.enabled ?? false,
				minSizeMb: String(attachmentDecay.min_size_mb ?? effective.min_size_mb),
				maxSizeMb: String(attachmentDecay.max_size_mb ?? effective.max_size_mb),
				maxEligibleSizeMb: String(attachmentDecay.max_eligible_size_mb ?? effective.max_eligible_size_mb),
				minLifetimeDays: String(attachmentDecay.min_lifetime_days ?? effective.min_lifetime_days),
				maxLifetimeDays: String(attachmentDecay.max_lifetime_days ?? effective.max_lifetime_days),
				curve: String(attachmentDecay.curve ?? effective.curve),
				renewThresholdDays: String(attachmentDecay.renew_threshold_days ?? effective.renew_threshold_days),
				renewWindowDays: String(attachmentDecay.renew_window_days ?? effective.renew_window_days),
			};
		});
		setAssets(BRANDING_ASSET_KINDS.map((kind) => ({kind, url: brandingAssetUrl(next, kind), preview: null})));
	}, []);

	useEffect(() => {
		if (!isAuthenticated || config) return;
		let cancelled = false;
		setLoadError(false);
		void (async () => {
			try {
				const next = await fetchInstanceConfig();
				if (cancelled) return;
				hydrateFromConfig(next);
			} catch (error) {
				if (cancelled) return;
				if (error instanceof HttpError && error.status === 401) {
					await resetStaleSetupSession();
					return;
				}
				logger.error('Failed to load instance configuration', error);
				setLoadError(true);
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [isAuthenticated, config, hydrateFromConfig, resetStaleSetupSession]);

	const serviceAvailability: ServiceAvailability = useMemo(
		() => ({
			gif:
				(config?.policy.services_available.gif ?? false) ||
				(integrationDraft.gifMode === 'configure' && integrationDraft.klipyApiKey.trim().length > 0),
			youtube:
				(config?.policy.services_available.youtube ?? false) ||
				(integrationDraft.youtubeMode === 'configure' && integrationDraft.youtubeApiKey.trim().length > 0),
			bluesky:
				(config?.policy.services_available.bluesky ?? false) ||
				(integrationDraft.blueskyMode === 'configure' &&
					integrationDraft.blueskyEnabled &&
					integrationDraft.blueskyKeyId.trim().length > 0 &&
					integrationDraft.blueskyPrivateKey.trim().length > 0),
		}),
		[config, integrationDraft],
	);

	const hasConfig = Boolean(config);
	const syncedSnapshot =
		wizardSnapshot.context.isAuthenticated !== isAuthenticated || wizardSnapshot.context.hasConfig !== hasConfig
			? transitionSetupWizardSnapshot(wizardSnapshot, {type: 'wizard.sync', isAuthenticated, hasConfig})
			: wizardSnapshot;
	if (syncedSnapshot !== wizardSnapshot) {
		setWizardSnapshot(syncedSnapshot);
	}
	const wizardModel = selectSetupWizardModel(syncedSnapshot);
	const {step, steps, direction} = wizardModel;

	const productNameTrimmed = productName.trim();
	const productNameError = productNameTrimmed.length < 1 || productNameTrimmed.length > 80;
	const singleCommunityNameTrimmed = singleCommunityName.trim();
	const singleCommunityNameError =
		singleCommunityEnabled && (singleCommunityNameTrimmed.length < 1 || singleCommunityNameTrimmed.length > 100);

	const canAdvance = useMemo(() => {
		if (step === 'welcome') return !isAuthenticated || Boolean(config);
		if (step === 'admin_account' || step === 'loading') return false;
		if (step === 'branding') return !productNameError;
		if (step === 'community') return !singleCommunityNameError;
		if (step === 'media_expiry') return isMediaExpiryStepValid(mediaExpiryDraft);
		const integrationKind = wizardStepToIntegrationKind(step);
		if (integrationKind) return isIntegrationStepValid(integrationKind, integrationDraft);
		return true;
	}, [step, isAuthenticated, config, productNameError, singleCommunityNameError, mediaExpiryDraft, integrationDraft]);

	const goNext = useCallback(() => {
		if (!beginStepNavigation()) return;
		setSubmitError(null);
		setWizardSnapshot((current) => transitionSetupWizardSnapshot(current, {type: 'wizard.next'}));
	}, [beginStepNavigation]);

	const goBack = useCallback(() => {
		if (!beginStepNavigation()) return;
		setSubmitError(null);
		setWizardSnapshot((current) => transitionSetupWizardSnapshot(current, {type: 'wizard.back'}));
	}, [beginStepNavigation]);

	const handleUploadAsset = useCallback(
		async (kind: SetupBrandingAssetKind) => {
			try {
				const [file] = await openFilePicker({accept: getAcceptString('guild_icon')});
				if (!file) return;
				const dataUrl = await fileToBase64(file);
				setAssets((current) => current.map((asset) => (asset.kind === kind ? {...asset, preview: dataUrl} : asset)));
			} catch (error) {
				logger.error('Failed to read branding asset file', error);
				setSubmitError(i18n._(ASSET_UPLOAD_ERROR_DESCRIPTOR));
			}
		},
		[i18n],
	);

	const handleClearAsset = useCallback((kind: SetupBrandingAssetKind) => {
		setAssets((current) =>
			current.map((asset) => (asset.kind === kind ? {...asset, preview: null, url: null} : asset)),
		);
	}, []);

	const handleToggleService = useCallback((service: keyof ServiceSelection, value: boolean) => {
		setServiceSelection((current) => ({...current, [service]: value}));
	}, []);

	const handleIntegrationDraftChange = useCallback((patch: Partial<ServiceIntegrationDraft>) => {
		setIntegrationDraft((current) => ({...current, ...patch}));
		setSmtpTestResult(null);
	}, []);

	const handleMediaExpiryDraftChange = useCallback((patch: Partial<MediaExpiryDraft>) => {
		setMediaExpiryDraft((current) => ({...current, ...patch}));
	}, []);

	const handleTestSmtp = useCallback(async () => {
		const port = parsePort(integrationDraft.smtpPort);
		if (port === null) return;
		setSmtpTesting(true);
		setSmtpTestResult(null);
		try {
			const result = await testSmtpConfig({
				host: integrationDraft.smtpHost.trim(),
				port,
				username: integrationDraft.smtpUsername.trim(),
				password: integrationDraft.smtpPassword,
				secure: integrationDraft.smtpSecure,
			});
			setSmtpTestResult(result.ok ? 'ok' : (result.error ?? 'SMTP validation failed.'));
		} catch (error) {
			logger.error('Failed to validate SMTP configuration', error);
			setSmtpTestResult(FormUtils.extractErrorMessage(i18n, error));
		} finally {
			setSmtpTesting(false);
		}
	}, [integrationDraft, i18n]);

	const submit = useCallback(async () => {
		if (!config) return;
		setSubmitting(true);
		setSubmitError(null);
		try {
			for (const asset of assets) {
				const original = brandingAssetUrl(config, asset.kind);
				if (asset.preview) {
					await uploadBrandingAsset(asset.kind, asset.preview);
				} else if (asset.url === null && original !== null) {
					await uploadBrandingAsset(asset.kind, null);
				}
			}
			const nextConfig = await updateInstanceConfig({
				integrations: buildIntegrationsPatch(integrationDraft),
				media: buildMediaPatch(mediaExpiryDraft),
				registration: {mode: registrationMode},
				app_public: {
					branding: {
						product_name: productNameTrimmed,
						theme_color: numberToHexColor(themeColor),
					},
					setup: {configured: true},
				},
				policy: {
					single_community_enabled: singleCommunityEnabled,
					single_community_name: singleCommunityEnabled ? singleCommunityNameTrimmed : undefined,
					direct_messages_disabled: directMessagesDisabled,
					premium_mode: premiumMode,
					services: {
						gif_enabled: serviceAvailability.gif ? serviceSelection.gif : undefined,
						youtube_enabled: serviceAvailability.youtube ? serviceSelection.youtube : undefined,
						bluesky_enabled: serviceAvailability.bluesky ? serviceSelection.bluesky : undefined,
					},
				},
			});
			hydrateFromConfig(nextConfig);
			RuntimeConfig.applyAdminInstanceConfig(nextConfig);
		} catch (error) {
			logger.error('Failed to complete instance setup', error);
			setSubmitError(FormUtils.extractErrorMessage(i18n, error));
			setSubmitting(false);
		}
	}, [
		config,
		assets,
		registrationMode,
		integrationDraft,
		mediaExpiryDraft,
		productNameTrimmed,
		themeColor,
		singleCommunityEnabled,
		singleCommunityNameTrimmed,
		directMessagesDisabled,
		premiumMode,
		serviceAvailability,
		serviceSelection,
		hydrateFromConfig,
		i18n,
	]);

	const isLoading = isAuthenticated && !config && !loadError;
	const canGoBack = wizardModel.stepIndex > 0 && !submitting && step !== 'loading';
	const isFinish = step === 'finish';
	const showBackButton = canGoBack;
	const showPrimaryButton = !loadError && step !== 'admin_account' && step !== 'loading';
	const showFooter = !loadError && (showBackButton || showPrimaryButton);
	const primaryButtonLabel = isLoading ? i18n._(LOADING_DESCRIPTOR) : i18n._(NEXT_DESCRIPTOR);
	const primaryButtonDisabled = !canAdvance || stepNavigationLocked;
	const handlePrimaryButton = goNext;
	const footerButtonInitial = shouldReduceMotion ? {opacity: 0} : {opacity: 0, y: 4, scale: 0.98};
	const footerButtonAnimate = shouldReduceMotion ? {opacity: 1} : {opacity: 1, y: 0, scale: 1};
	const footerButtonExit = shouldReduceMotion ? {opacity: 0} : {opacity: 0, y: -4, scale: 0.98};

	useEffect(() => {
		if (isAuthenticated) return;
		Theme.setTheme(setupTheme);
	}, [isAuthenticated, setupTheme, themeHydrated]);

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

	return (
		<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
			<div className={styles.backdrop} data-flx="app.self-hosted-setup-wizard-gate.backdrop">
				<Modal.Root
					size="medium"
					centered
					disableHistoryManagement
					className={styles.modal}
					initialFocusRef={welcomeInitialFocusRef}
					backdropSlot={
						<div className={styles.backdropVisual} data-flx="app.self-hosted-setup-wizard-gate.backdrop-visual" />
					}
					data-flx="app.self-hosted-setup-wizard-gate.modal"
				>
					<Modal.Header
						title={i18n._(HEADER_DESCRIPTOR, {productName: fallbackProductName})}
						icon={<WrenchIcon size={22} weight="bold" data-flx="app.self-hosted-setup-wizard-gate.header-icon" />}
						hideCloseButton
						data-flx="app.self-hosted-setup-wizard-gate.header"
					/>
					<Modal.Content className={styles.contentScroller} data-flx="app.self-hosted-setup-wizard-gate.content">
						<div className={styles.content} data-flx="app.self-hosted-setup-wizard-gate.content-inner">
							{loadError ? (
								<div className={styles.centeredStep} data-flx="app.self-hosted-setup-wizard-gate.load-error-wrap">
									<p
										className={styles.submitError}
										role="alert"
										data-flx="app.self-hosted-setup-wizard-gate.load-error"
									>
										{i18n._(LOAD_ERROR_DESCRIPTOR)}
									</p>
								</div>
							) : (
								<SteppedCarousel
									step={step}
									steps={steps}
									direction={direction}
									focusOnStepChange
									ariaLabel={i18n._(SETUP_WIZARD_DESCRIPTOR)}
									data-flx="app.self-hosted-setup-wizard-gate.carousel"
								>
									{step === 'welcome' && (
										<WelcomeStep
											productName={fallbackProductName}
											isAuthenticated={isAuthenticated}
											initialFocusRef={welcomeInitialFocusRef}
											data-flx="app.setup.self-hosted-setup-wizard-gate.welcome-step"
										/>
									)}
									{step === 'theme' && (
										<ThemeStep
											theme={setupTheme}
											onThemeChange={setSetupTheme}
											data-flx="app.setup.self-hosted-setup-wizard-gate.theme-step"
										/>
									)}
									{step === 'admin_intro' && (
										<AdminIntroStep data-flx="app.setup.self-hosted-setup-wizard-gate.admin-intro-step" />
									)}
									{step === 'admin_account' && (
										<AdminAccountStep
											theme={setupTheme}
											data-flx="app.setup.self-hosted-setup-wizard-gate.admin-account-step"
										/>
									)}
									{step === 'loading' && (
										<LoadingStep data-flx="app.setup.self-hosted-setup-wizard-gate.loading-step" />
									)}
									{step === 'branding' && (
										<BrandingStep
											productName={productName}
											productNameError={productNameError}
											themeColor={themeColor}
											assets={assets}
											disabled={submitting}
											onProductNameChange={setProductName}
											onThemeColorChange={setThemeColor}
											onUploadAsset={handleUploadAsset}
											onClearAsset={handleClearAsset}
											data-flx="app.setup.self-hosted-setup-wizard-gate.branding-step"
										/>
									)}
									{step === 'registration' && (
										<RegistrationStep
											mode={registrationMode}
											disabled={submitting}
											onChange={setRegistrationMode}
											data-flx="app.setup.self-hosted-setup-wizard-gate.registration-step.set-registration-mode"
										/>
									)}
									{step === 'community' && (
										<CommunityStep
											singleCommunityEnabled={singleCommunityEnabled}
											singleCommunityName={singleCommunityName}
											singleCommunityNameError={singleCommunityNameError}
											directMessagesDisabled={directMessagesDisabled}
											disabled={submitting}
											onToggleSingleCommunity={setSingleCommunityEnabled}
											onSingleCommunityNameChange={setSingleCommunityName}
											onToggleDirectMessages={setDirectMessagesDisabled}
											data-flx="app.setup.self-hosted-setup-wizard-gate.community-step"
										/>
									)}
									{step === 'media_expiry' && (
										<MediaExpiryStep
											draft={mediaExpiryDraft}
											disabled={submitting}
											onDraftChange={handleMediaExpiryDraftChange}
											data-flx="app.setup.self-hosted-setup-wizard-gate.media-expiry-step"
										/>
									)}
									{wizardStepToIntegrationKind(step) && (
										<IntegrationStep
											kind={wizardStepToIntegrationKind(step)!}
											draft={integrationDraft}
											disabled={submitting}
											smtpTesting={smtpTesting}
											smtpTestResult={smtpTestResult}
											onDraftChange={handleIntegrationDraftChange}
											onTestSmtp={handleTestSmtp}
											data-flx="app.setup.self-hosted-setup-wizard-gate.integration-step"
										/>
									)}
									{step === 'services' && (
										<ServicesStep
											available={serviceAvailability}
											selection={serviceSelection}
											disabled={submitting}
											onToggle={handleToggleService}
											data-flx="app.setup.self-hosted-setup-wizard-gate.services-step"
										/>
									)}
									{step === 'premium' && (
										<PremiumStep
											mode={premiumMode}
											disabled={submitting}
											onChange={setPremiumMode}
											data-flx="app.setup.self-hosted-setup-wizard-gate.premium-step.set-premium-mode"
										/>
									)}
									{step === 'finish' && (
										<FinishStep
											productName={productNameTrimmed}
											registrationMode={registrationMode}
											singleCommunityEnabled={singleCommunityEnabled}
											directMessagesDisabled={directMessagesDisabled}
											attachmentExpiryEnabled={mediaExpiryDraft.enabled}
											premiumMode={premiumMode}
											submitError={submitError}
											data-flx="app.setup.self-hosted-setup-wizard-gate.finish-step"
										/>
									)}
								</SteppedCarousel>
							)}
						</div>
					</Modal.Content>
					<AnimatePresence initial={false} data-flx="app.self-hosted-setup-wizard-gate.footer-presence">
						{showFooter && (
							<motion.div
								key="footer"
								className={styles.footerReveal}
								initial={{height: 0, opacity: 0}}
								animate={{height: 'auto', opacity: 1}}
								exit={{height: 0, opacity: 0}}
								transition={shouldReduceMotion ? instantTransition : footerRevealTransition}
								data-flx="app.self-hosted-setup-wizard-gate.footer-reveal"
							>
								<Modal.Footer className={styles.footer} data-flx="app.self-hosted-setup-wizard-gate.footer">
									<div className={styles.footerActions} data-flx="app.self-hosted-setup-wizard-gate.footer-actions">
										<AnimatePresence initial={false} data-flx="app.self-hosted-setup-wizard-gate.back-presence">
											{showBackButton && (
												<motion.div
													key="back"
													initial={footerButtonInitial}
													animate={footerButtonAnimate}
													exit={footerButtonExit}
													transition={shouldReduceMotion ? instantTransition : footerButtonTransition}
													data-flx="app.self-hosted-setup-wizard-gate.back-button-wrap"
												>
													<Button
														variant="secondary"
														disabled={stepNavigationLocked}
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
												</motion.div>
											)}
										</AnimatePresence>
										<div className={styles.footerPrimary} data-flx="app.self-hosted-setup-wizard-gate.footer-primary">
											<AnimatePresence
												mode="wait"
												initial={false}
												data-flx="app.self-hosted-setup-wizard-gate.primary-presence"
											>
												{showPrimaryButton && isFinish && (
													<motion.div
														key="finish"
														initial={footerButtonInitial}
														animate={footerButtonAnimate}
														exit={footerButtonExit}
														transition={shouldReduceMotion ? instantTransition : footerButtonTransition}
														data-flx="app.self-hosted-setup-wizard-gate.finish-button-wrap"
													>
														<Button
															submitting={submitting}
															rightIcon={
																<CheckIcon
																	size={18}
																	weight="bold"
																	data-flx="app.setup.self-hosted-setup-wizard-gate.check-icon"
																/>
															}
															onClick={submit}
															data-flx="app.self-hosted-setup-wizard-gate.finish-button"
														>
															{i18n._(FINISH_DESCRIPTOR)}
														</Button>
													</motion.div>
												)}
												{showPrimaryButton && !isFinish && (
													<motion.div
														key="next"
														initial={footerButtonInitial}
														animate={footerButtonAnimate}
														exit={footerButtonExit}
														transition={shouldReduceMotion ? instantTransition : footerButtonTransition}
														data-flx="app.self-hosted-setup-wizard-gate.next-button-wrap"
													>
														<Button
															disabled={primaryButtonDisabled}
															submitting={isLoading}
															rightIcon={
																<ArrowRightIcon
																	size={18}
																	weight="bold"
																	data-flx="app.setup.self-hosted-setup-wizard-gate.arrow-right-icon"
																/>
															}
															onClick={handlePrimaryButton}
															data-flx="app.self-hosted-setup-wizard-gate.next-button"
														>
															{primaryButtonLabel}
														</Button>
													</motion.div>
												)}
											</AnimatePresence>
										</div>
									</div>
								</Modal.Footer>
							</motion.div>
						)}
					</AnimatePresence>
				</Modal.Root>
			</div>
		</AuthRegisterDraftContext.Provider>
	);
});
