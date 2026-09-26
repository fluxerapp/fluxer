// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {useAuthorizeParams} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useAuthorizeParams';
import {
	type BotInviteDestinationOption,
	createBotInviteDestinationKey,
	parseBotInviteDestinationKey,
	useBotInviteDestinations,
} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useBotGuilds';
import {useOAuthPublicApp} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useOAuthPublicApp';
import {usePermissionSelection} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/usePermissionSelection';
import {useScopeSelection} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useScopeSelection';
import {
	type AuthorizeParams,
	isSafeRedirectUri,
	logger,
	type PublicAppData,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import {
	type AuthorizeEvent,
	type AuthorizePhase,
	createAuthorizeSnapshot,
	type ReviewStep,
	selectAuthorizePhase,
	transitionAuthorizeSnapshot,
} from '@app/features/auth/components/pages/oauth_authorize_page/state/authorizeMachine';
import {getDefaultLandingPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import type {BotPermissionOption} from '@app/features/permissions/utils/PermissionUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {failureMessage} from '@app/features/platform/utils/ResponseInspection';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const MISSING_CLIENT_ID_DESCRIPTOR = msg({
	message: 'Missing client_id',
	comment: 'Short label in the authentication authorize flow. Keep the tone plain and specific.',
});
const YOUR_PRODUCT_SESSION_EXPIRED_SIGN_IN_AGAIN_TO_DESCRIPTOR = msg({
	message: 'Your {productName} session expired. Sign in again to continue authorizing this application.',
	comment:
		'OAuth authorize page error shown when the session expired mid-authorization. Keep plain. Preserve {productName}; it is inserted by code.',
});
const UNKNOWN_APPLICATION_DESCRIPTOR = msg({
	message: 'Unknown application, expired session, or invalid authorization request.',
	comment: 'OAuth authorize page error shown when the application, session, or request is invalid.',
});
const THE_PROVIDED_REDIRECT_URI_MUST_USE_HTTP_OR_DESCRIPTOR = msg({
	message: 'The provided redirect_uri must use HTTP or HTTPS.',
	comment: 'Body text in the authentication authorize flow. Keep the tone plain and specific.',
});
const A_REDIRECT_URI_IS_REQUIRED_WHEN_THE_BOT_DESCRIPTOR = msg({
	message: 'A redirect_uri is required when the bot scope is not the only scope.',
	comment:
		'OAuth authorize page error shown when the bot scope requires a redirect_uri that is missing. Technical message; keep "redirect_uri" and "bot" untranslated.',
});
const THE_PROVIDED_REDIRECT_URI_NOT_REGISTERED_DESCRIPTOR = msg({
	message: 'The provided redirect_uri is not registered for this application.',
	comment: 'OAuth authorize page error shown when the redirect_uri is not registered for this OAuth app.',
});
const INVALID_BOT_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Invalid bot permissions.',
	comment: 'OAuth authorize page error shown when the permissions query parameter is not a valid integer.',
});
const BOT_PERMISSIONS_CANNOT_BE_NEGATIVE_DESCRIPTOR = msg({
	message: 'Bot permissions cannot be negative.',
	comment: 'OAuth authorize page error shown when the permissions query parameter is a negative integer.',
});
const THIS_BOT_IS_NOT_PUBLIC_DESCRIPTOR = msg({
	message: 'This bot is not public and cannot be added by other users.',
	comment: 'OAuth authorize page error shown when a private bot cannot be added by non-owners.',
});
const AUTHORIZATION_FAILED_DESCRIPTOR = msg({
	message: 'Authorization failed. Try again.',
	comment: 'OAuth authorize page generic error toast shown when the authorize request fails unexpectedly.',
});
const INVALID_REDIRECT_URI_DESCRIPTOR = msg({
	message: 'Invalid redirect_uri',
	comment: 'Short label in the authentication authorize flow. Keep the tone plain and specific.',
});
const THIS_APPLICATION_DESCRIPTOR = msg({
	message: 'This application',
	comment: 'Short label in the authentication authorize flow. Keep the tone plain and specific.',
});
const CHOOSE_EITHER_A_COMMUNITY_OR_GROUP_DM_DESCRIPTOR = msg({
	message: 'Choose either guild_id or channel_id, not both.',
	comment:
		'OAuth authorize page error shown when a bot invite request provides both guild_id and channel_id. Keep the parameter names literal.',
});

export interface AuthorizeFlow {
	phase: AuthorizePhase;
	sessionExpiredMessage: string;
	authParams: AuthorizeParams | null;
	publicApp: PublicAppData | null;
	clientLabel: string;
	scopes: ReadonlyArray<string>;
	selectedScopes: ReadonlySet<string>;
	isScopeLocked: (scope: string) => boolean;
	toggleScope: (scope: string) => void;
	scopesAdjusted: boolean;
	hasBotScope: boolean;
	botInviteWithoutRedirect: boolean;
	redirectHostname: string | null;
	destinationOptions: ReadonlyArray<BotInviteDestinationOption>;
	destinationsLoading: boolean;
	destinationsError: string | null;
	selectedDestinationKey: string | null;
	selectedDestination: BotInviteDestinationOption | null;
	onSelectDestination: (id: string | null) => void;
	cannotSubmit: boolean;
	hasRequestedBotPermissions: boolean;
	permissionOptions: ReadonlyArray<BotPermissionOption>;
	requestedPermissionKeys: ReadonlyArray<string>;
	selectedPermissions: ReadonlySet<string>;
	togglePermission: (id: string) => void;
	permissionsAdjusted: boolean;
	requestsAdmin: boolean;
	submitting: 'approve' | 'deny' | null;
	submitError: string | null;
	reviewSteps: ReadonlyArray<ReviewStep>;
	hasNextStep: boolean;
	hasPreviousStep: boolean;
	onAuthorize: () => Promise<void>;
	onCancel: () => void;
	goNext: () => void;
	goBack: () => void;
}

export interface UseAuthorizeFlowOptions {
	search?: string;
	includeAccountStep?: boolean;
	onCancel?: () => void;
}

function getCurrentInviteDestinationKey(): string | null {
	if (typeof window === 'undefined') return null;
	const parts = window.location.pathname.split('/').filter(Boolean).map(decodePathSegment);
	if (parts[0] !== 'channels') return null;
	const guildId = parts[1];
	const channelId = parts[2];
	if (!guildId) return null;
	if (guildId === '@me') {
		return channelId ? createBotInviteDestinationKey('group_dm', channelId) : null;
	}
	if (guildId !== '@favorites' && guildId !== '@discover') {
		return createBotInviteDestinationKey('guild', guildId);
	}
	return null;
}

function decodePathSegment(segment: string): string {
	try {
		return decodeURIComponent(segment);
	} catch {
		return segment;
	}
}

function getLoginRedirectPath(): string {
	const current = window.location.pathname + window.location.search;
	const params = new URLSearchParams({redirect_to: current});
	return `/login?${params.toString()}`;
}

export function useAuthorizeFlow(options: UseAuthorizeFlowOptions = {}): AuthorizeFlow {
	const {search, includeAccountStep = true, onCancel: onCancelOverride} = options;
	const {i18n} = useLingui();
	const paramsState = useAuthorizeParams(search);
	const {params, scopes, hasBotScope, isBotOnly, redirectHostname, botInviteWithoutRedirect} = paramsState;
	const [authorizeSnapshot, setAuthorizeSnapshot] = useState(createAuthorizeSnapshot);
	const phase = useMemo<AuthorizePhase>(() => selectAuthorizePhase(authorizeSnapshot), [authorizeSnapshot]);
	const dispatch = useCallback((event: AuthorizeEvent) => {
		setAuthorizeSnapshot((snapshot) => transitionAuthorizeSnapshot(snapshot, event));
	}, []);
	const [submitError, setSubmitError] = useState<string | null>(null);
	const [submitting, setSubmitting] = useState<'approve' | 'deny' | null>(null);
	const [selectedDestinationKey, setSelectedDestinationKey] = useState<string | null>(null);
	const preferredDestinationKeyRef = useRef<string | null>(getCurrentInviteDestinationKey());
	const initialMissingClientId = !params;
	const publicAppState = useOAuthPublicApp(params?.clientId ?? null);
	const scopeSelection = useScopeSelection(scopes);
	const permissionSelection = usePermissionSelection(params?.permissions ?? null);
	const destinations = useBotInviteDestinations(hasBotScope, permissionSelection.requestedBitfield);
	const initialReviewStep: ReviewStep = includeAccountStep ? 'account' : 'scopes';
	const preFetchValidationError = useMemo(() => {
		if (!params) return i18n._(MISSING_CLIENT_ID_DESCRIPTOR);
		if (params.guildId && params.channelId) {
			return i18n._(CHOOSE_EITHER_A_COMMUNITY_OR_GROUP_DM_DESCRIPTOR);
		}
		if (params.redirectUri && !isSafeRedirectUri(params.redirectUri)) {
			return i18n._(THE_PROVIDED_REDIRECT_URI_MUST_USE_HTTP_OR_DESCRIPTOR);
		}
		if (!isBotOnly && !params.redirectUri) {
			return i18n._(A_REDIRECT_URI_IS_REQUIRED_WHEN_THE_BOT_DESCRIPTOR);
		}
		if (params.permissions != null) {
			try {
				if (BigInt(params.permissions) < 0n) {
					return i18n._(BOT_PERMISSIONS_CANNOT_BE_NEGATIVE_DESCRIPTOR);
				}
			} catch {
				return i18n._(INVALID_BOT_PERMISSIONS_DESCRIPTOR);
			}
		}
		return null;
	}, [params, isBotOnly, i18n.locale]);
	const postFetchValidationError = useMemo(() => {
		if (!publicAppState.data || !params) return null;
		if (params.redirectUri && !publicAppState.data.redirect_uris?.includes(params.redirectUri)) {
			return i18n._(THE_PROVIDED_REDIRECT_URI_NOT_REGISTERED_DESCRIPTOR);
		}
		if (hasBotScope && !publicAppState.data.bot_public) {
			return i18n._(THIS_BOT_IS_NOT_PUBLIC_DESCRIPTOR);
		}
		return null;
	}, [publicAppState.data, params, hasBotScope, i18n.locale]);
	const initFiredRef = useRef(false);
	useEffect(() => {
		if (publicAppState.status === 'session_expired') {
			initFiredRef.current = true;
			dispatch({type: 'INIT_SESSION_EXPIRED'});
			return;
		}
		if (initFiredRef.current) return;
		if (initialMissingClientId) {
			initFiredRef.current = true;
			dispatch({type: 'INIT_INVALID', message: i18n._(MISSING_CLIENT_ID_DESCRIPTOR)});
			return;
		}
		if (preFetchValidationError) {
			initFiredRef.current = true;
			dispatch({type: 'INIT_INVALID', message: preFetchValidationError});
			return;
		}
		if (publicAppState.status === 'error') {
			initFiredRef.current = true;
			dispatch({type: 'INIT_INVALID', message: i18n._(UNKNOWN_APPLICATION_DESCRIPTOR)});
			return;
		}
		if (publicAppState.status !== 'ready') return;
		if (postFetchValidationError) {
			initFiredRef.current = true;
			dispatch({type: 'INIT_INVALID', message: postFetchValidationError});
			return;
		}
		initFiredRef.current = true;
		dispatch({type: 'INIT_OK', step: initialReviewStep});
	}, [
		initialMissingClientId,
		preFetchValidationError,
		publicAppState.status,
		postFetchValidationError,
		i18n,
		initialReviewStep,
	]);
	useEffect(() => {
		if (phase.kind !== 'session_expired') return;
		void import('@app/features/platform/state/AuthSession').then(({default: SessionManager}) => {
			const expiredUserId = SessionManager.userId;
			if (expiredUserId) SessionManager.markAccountInvalid(expiredUserId);
			SessionManager.handleConnectionClosed(4004);
			window.location.replace(getLoginRedirectPath());
		});
	}, [phase.kind]);
	const destinationInitRef = useRef(false);
	useEffect(() => {
		if (!hasBotScope) {
			destinationInitRef.current = false;
			setSelectedDestinationKey(null);
			return;
		}
		if (destinationInitRef.current) return;
		const requestedDestinationKey = params?.channelId
			? createBotInviteDestinationKey('group_dm', params.channelId)
			: params?.guildId
				? createBotInviteDestinationKey('guild', params.guildId)
				: null;
		if (requestedDestinationKey) {
			if (destinations.status !== 'ready' && destinations.status !== 'error') return;
			setSelectedDestinationKey(
				destinations.options.some((option) => option.value === requestedDestinationKey)
					? requestedDestinationKey
					: null,
			);
			destinationInitRef.current = true;
			return;
		}
		if (destinations.status !== 'ready') return;
		const preferredDestinationKey = preferredDestinationKeyRef.current;
		if (preferredDestinationKey && destinations.options.some((option) => option.value === preferredDestinationKey)) {
			setSelectedDestinationKey(preferredDestinationKey);
			destinationInitRef.current = true;
			return;
		}
		if (destinations.options.length > 0) {
			setSelectedDestinationKey(destinations.options[0].value);
			destinationInitRef.current = true;
			return;
		}
		destinationInitRef.current = true;
	}, [hasBotScope, params?.guildId, params?.channelId, destinations.options, destinations.status]);
	const selectedDestination = useMemo(
		() => destinations.options.find((option) => option.value === selectedDestinationKey) ?? null,
		[destinations.options, selectedDestinationKey],
	);
	const cannotSubmit = hasBotScope && !selectedDestination;
	const needsPermissionsStep =
		hasBotScope && selectedDestination?.kind !== 'group_dm' && permissionSelection.requestedKeys.length > 0;
	const hasRequestedBotPermissions =
		hasBotScope && selectedDestination?.kind !== 'group_dm' && permissionSelection.requestedBitfield > 0n;
	const clientLabel = publicAppState.data?.name?.trim() || i18n._(THIS_APPLICATION_DESCRIPTOR);
	const reviewSteps = useMemo<ReadonlyArray<ReviewStep>>(() => {
		const steps: Array<ReviewStep> = includeAccountStep ? ['account', 'scopes'] : ['scopes'];
		if (hasBotScope) {
			steps.push('community');
		}
		if (needsPermissionsStep) {
			steps.push('permissions');
		}
		return steps;
	}, [hasBotScope, includeAccountStep, needsPermissionsStep]);
	const currentStepIndex = phase.kind === 'review' ? reviewSteps.indexOf(phase.step) : -1;
	const hasNextStep = currentStepIndex >= 0 && currentStepIndex < reviewSteps.length - 1;
	const hasPreviousStep = currentStepIndex > 0;
	useEffect(() => {
		if (phase.kind !== 'review') return;
		if (reviewSteps.includes(phase.step)) return;
		dispatch({type: 'SET_REVIEW_STEP', step: reviewSteps[reviewSteps.length - 1] ?? 'account'});
	}, [phase, reviewSteps]);
	const goNext = useCallback(() => {
		if (currentStepIndex < 0) return;
		const nextStep = reviewSteps[currentStepIndex + 1];
		if (!nextStep) return;
		dispatch({type: 'SET_REVIEW_STEP', step: nextStep});
	}, [currentStepIndex, reviewSteps]);
	const goBack = useCallback(() => {
		if (currentStepIndex <= 0) {
			onCancelOverride?.();
			return;
		}
		const previousStep = reviewSteps[currentStepIndex - 1];
		if (!previousStep) return;
		dispatch({type: 'SET_REVIEW_STEP', step: previousStep});
	}, [currentStepIndex, onCancelOverride, reviewSteps]);
	const onAuthorize = useCallback(async () => {
		if (!params) return;
		setSubmitError(null);
		setSubmitting('approve');
		try {
			const scopeToSend = scopeSelection.toScopeString() || params.scope;
			const sendsBotScope = scopeToSend.split(/[\s+]+/).includes('bot');
			if (sendsBotScope && !selectedDestination) {
				setSubmitting(null);
				return;
			}
			const body: Record<string, string | undefined> = {
				response_type: params.responseType || 'code',
				client_id: params.clientId,
				scope: scopeToSend,
			};
			if (params.redirectUri) body.redirect_uri = params.redirectUri;
			if (params.state) body.state = params.state;
			if (params.codeChallenge) body.code_challenge = params.codeChallenge;
			if (params.codeChallengeMethod) body.code_challenge_method = params.codeChallengeMethod;
			if (sendsBotScope && selectedDestination?.kind === 'guild') {
				const bits = permissionSelection.toBitfield();
				if (bits) body.permissions = bits;
				body.guild_id = selectedDestination.id;
			} else if (sendsBotScope && selectedDestination?.kind === 'group_dm') {
				body.channel_id = selectedDestination.id;
			}
			const resp = await http.post<{redirect_to: string}>(Endpoints.OAUTH_CONSENT, {body});
			if (botInviteWithoutRedirect) {
				const destinationName = selectedDestination
					? (destinations.labelByKey.get(selectedDestination.value) ?? selectedDestination.label)
					: null;
				setSubmitting(null);
				dispatch({type: 'SUBMIT_BOT_INVITE_DONE', destinationName});
				return;
			}
			if (resp.body?.redirect_to) {
				window.location.href = resp.body.redirect_to;
				return;
			}
			setSubmitting(null);
			setSubmitError(i18n._(AUTHORIZATION_FAILED_DESCRIPTOR));
		} catch (err) {
			logger.error('Authorization failed', err);
			setSubmitting(null);
			setSubmitError(failureMessage(err) ?? i18n._(AUTHORIZATION_FAILED_DESCRIPTOR));
		}
	}, [
		params,
		selectedDestination,
		scopeSelection,
		permissionSelection,
		botInviteWithoutRedirect,
		destinations.labelByKey,
		i18n,
	]);
	const onCancel = useCallback(() => {
		if (!params) return;
		setSubmitting('deny');
		if (onCancelOverride) {
			onCancelOverride();
			return;
		}
		try {
			if (params.redirectUri) {
				if (!isSafeRedirectUri(params.redirectUri)) {
					setSubmitting(null);
					setSubmitError(i18n._(INVALID_REDIRECT_URI_DESCRIPTOR));
					return;
				}
				if (publicAppState.data && !publicAppState.data.redirect_uris?.includes(params.redirectUri)) {
					setSubmitting(null);
					setSubmitError(i18n._(THE_PROVIDED_REDIRECT_URI_NOT_REGISTERED_DESCRIPTOR));
					return;
				}
				const url = new URL(params.redirectUri);
				url.searchParams.set('error', 'access_denied');
				if (params.state) url.searchParams.set('state', params.state);
				window.location.href = url.toString();
				return;
			}
			window.location.href = getDefaultLandingPath();
		} catch (err) {
			logger.error('Failed to redirect on cancel', err);
			setSubmitting(null);
			setSubmitError(i18n._(INVALID_REDIRECT_URI_DESCRIPTOR));
		}
	}, [params, onCancelOverride, publicAppState.data, i18n]);
	const sessionExpiredMessage = useMemo(
		() => i18n._(YOUR_PRODUCT_SESSION_EXPIRED_SIGN_IN_AGAIN_TO_DESCRIPTOR, {productName: PRODUCT_NAME}),
		[i18n.locale],
	);
	return {
		phase,
		sessionExpiredMessage,
		authParams: params,
		publicApp: publicAppState.data,
		clientLabel,
		scopes,
		selectedScopes: scopeSelection.selected,
		isScopeLocked: scopeSelection.isLocked,
		toggleScope: scopeSelection.toggle,
		scopesAdjusted: scopeSelection.adjusted,
		hasBotScope,
		botInviteWithoutRedirect,
		redirectHostname,
		destinationOptions: destinations.options,
		destinationsLoading: destinations.status === 'loading',
		destinationsError: destinations.error,
		selectedDestinationKey,
		selectedDestination,
		onSelectDestination: (value) => {
			setSelectedDestinationKey(parseBotInviteDestinationKey(value) ? value : null);
		},
		cannotSubmit,
		hasRequestedBotPermissions,
		permissionOptions: permissionSelection.options,
		requestedPermissionKeys: permissionSelection.requestedKeys,
		selectedPermissions: permissionSelection.selected,
		togglePermission: permissionSelection.toggle,
		permissionsAdjusted: permissionSelection.adjusted,
		requestsAdmin: permissionSelection.requestsAdmin,
		submitting,
		submitError,
		reviewSteps,
		hasNextStep,
		hasPreviousStep,
		onAuthorize,
		onCancel,
		goNext,
		goBack,
	};
}
