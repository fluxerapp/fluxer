/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {modal} from '~/actions/ModalActionCreators';
import {getOAuth2ScopeDescription, type OAuth2Scope, Permissions} from '~/Constants';
import AccountSwitcherModal from '~/components/accounts/AccountSwitcherModal';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import {createGuildSelectComponents, type GuildSelectOption} from '~/components/modals/shared/GuildSelectComponents';
import {BaseAvatar} from '~/components/uikit/BaseAvatar';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useAuthLayoutContext} from '~/contexts/AuthLayoutContext';
import {Endpoints} from '~/Endpoints';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import FluxerWordmarkMonochrome from '~/images/fluxer-logo-wordmark-monochrome.svg?react';
import http, {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {formatBotPermissionsQuery, getAllBotPermissions} from '~/utils/PermissionUtils';
import styles from './OAuthAuthorizePage.module.css';

const logger = new Logger('OAuthAuthorizePage');

interface AuthorizeParams {
	clientId: string;
	redirectUri?: string | null;
	scope: string;
	state?: string | null;
	permissions?: string | null;
	guildId?: string | null;
	prompt?: string | null;
	responseType?: string | null;
	codeChallenge?: string | null;
	codeChallengeMethod?: string | null;
}

type FlowStep = 'scopes' | 'permissions';

const OAuthAuthorizePage: React.FC = observer(() => {
	const {t, i18n} = useLingui();

	const [loading, setLoading] = React.useState(true);
	const [submitting, setSubmitting] = React.useState<'approve' | 'deny' | null>(null);
	const [error, setError] = React.useState<string | null>(null);
	const [authParams, setAuthParams] = React.useState<AuthorizeParams | null>(null);
	const [selectedScopes, setSelectedScopes] = React.useState<Set<string> | null>(null);
	const [selectedPermissions, setSelectedPermissions] = React.useState<Set<string> | null>(null);
	const [currentStep, setCurrentStep] = React.useState<FlowStep>('scopes');
	const [successState, setSuccessState] = React.useState<{guildName?: string | null} | null>(null);
	const [publicApp, setPublicApp] = React.useState<{
		id: string;
		name: string;
		icon: string | null;
		description: string | null;
		redirect_uris: Array<string>;
		scopes: Array<string>;
		bot_public: boolean;
		bot?: {
			id: string;
			avatar: string | null;
			username?: string | null;
		} | null;
	} | null>(null);
	const [selectedGuildId, setSelectedGuildId] = React.useState<string | null>(null);
	const [guildsLoading, setGuildsLoading] = React.useState(false);
	const [guildsError, setGuildsError] = React.useState<string | null>(null);
	const [guilds, setGuilds] = React.useState<Array<{
		id: string;
		name: string | null;
		icon: string | null;
		permissions?: string | null;
	}> | null>(null);

	const {setShowLogoSide} = useAuthLayoutContext();

	useFluxerDocumentTitle(t`Authorize Application`);

	React.useLayoutEffect(() => {
		setShowLogoSide(false);
		return () => setShowLogoSide(true);
	}, [setShowLogoSide]);

	const openAccountSwitcher = React.useCallback(() => {
		modal(() => <AccountSwitcherModal />);
	}, []);

	const getScopeDescription = React.useCallback(
		(scope: string) => {
			return getOAuth2ScopeDescription(i18n, scope as OAuth2Scope) ?? scope;
		},
		[i18n],
	);

	React.useEffect(() => {
		const qp = new URLSearchParams(window.location.search);
		setLoading(true);

		const clientId = qp.get('client_id') ?? '';
		const redirectUri = qp.get('redirect_uri') ?? null;

		if (!clientId) {
			setError(t`Missing client_id`);
			setLoading(false);
			return;
		}

		const params: AuthorizeParams = {
			clientId,
			redirectUri,
			scope: qp.get('scope') ?? '',
			state: qp.get('state'),
			permissions: qp.get('permissions'),
			guildId: qp.get('guild_id'),
			prompt: qp.get('prompt'),
			responseType: qp.get('response_type') ?? 'code',
			codeChallenge: qp.get('code_challenge'),
			codeChallengeMethod: qp.get('code_challenge_method'),
		};

		setAuthParams(params);

		const fetchPublicApp = async (clientIdValue: string) => {
			try {
				const resp = await http.get<{
					id: string;
					name: string;
					icon: string | null;
					description: string | null;
					redirect_uris: Array<string>;
					scopes: Array<string>;
					bot_public: boolean;
				}>({
					url: Endpoints.OAUTH_PUBLIC_APPLICATION(clientIdValue),
					rejectWithError: true,
				});
				setPublicApp(resp.body);
			} catch (e) {
				logger.error('Failed to fetch public application', e);
				setError(t`Unknown application or it no longer exists.`);
			} finally {
				setLoading(false);
			}
		};

		if (clientId) {
			void fetchPublicApp(clientId);
		} else {
			setLoading(false);
		}
	}, []);

	const scopes = React.useMemo(() => {
		if (!authParams?.scope) return [];
		return authParams.scope.split(/\s+/).filter(Boolean);
	}, [authParams]);

	const botPermissionOptions = React.useMemo(() => getAllBotPermissions(i18n), [i18n]);

	const hasBotScope = React.useMemo(() => scopes.includes('bot'), [scopes]);
	const isBotOnly = React.useMemo(() => {
		return scopes.length === 1 && scopes[0] === 'bot';
	}, [scopes]);

	React.useEffect(() => {
		if (!hasBotScope) return;

		let cancelled = false;
		setGuildsLoading(true);
		setGuildsError(null);

		const fetchGuilds = async () => {
			try {
				const resp = await http.get<
					Array<{id: string; name: string | null; icon: string | null; permissions?: string | null}>
				>({
					url: Endpoints.USER_GUILDS_LIST,
					rejectWithError: true,
				});
				if (cancelled) return;
				setGuilds(resp.body);
			} catch (e) {
				if (cancelled) return;
				logger.error('Failed to fetch user guilds', e);
				const message =
					e instanceof HttpError &&
					e.body &&
					typeof e.body === 'object' &&
					'message' in e.body &&
					typeof (e.body as {message?: unknown}).message === 'string'
						? (e.body as {message: string}).message
						: t`Failed to load your communities.`;
				setGuildsError(message);
				setGuilds([]);
			} finally {
				if (!cancelled) {
					setGuildsLoading(false);
				}
			}
		};

		void fetchGuilds();

		return () => {
			cancelled = true;
		};
	}, [hasBotScope]);

	const guildsWithPermissions = React.useMemo(() => {
		if (!guilds) return [];
		return guilds.map((guild) => {
			let permissionsValue = 0n;
			try {
				if (guild.permissions) {
					permissionsValue = BigInt(guild.permissions);
				}
			} catch {
				permissionsValue = 0n;
			}
			const canManage = (permissionsValue & Permissions.MANAGE_GUILD) === Permissions.MANAGE_GUILD;
			return {
				id: guild.id,
				name: guild.name ?? t`Unknown community`,
				icon: guild.icon ?? null,
				canManage,
			};
		});
	}, [guilds]);

	const guildOptions: Array<GuildSelectOption> = React.useMemo(
		() =>
			guildsWithPermissions.map((guild) => ({
				value: guild.id,
				label: guild.name,
				icon: guild.icon ?? null,
				isDisabled: !guild.canManage,
			})),
		[guildsWithPermissions],
	);

	const guildLabelMap = React.useMemo(() => {
		return new Map(guildOptions.map((option) => [option.value, option.label]));
	}, [guildOptions]);

	const manageableGuildOptions = React.useMemo(
		() => guildOptions.filter((option) => !option.isDisabled),
		[guildOptions],
	);

	const requestedPermissionKeys = React.useMemo(() => {
		if (!authParams?.permissions) return [];
		try {
			const bitfield = BigInt(authParams.permissions);
			return botPermissionOptions
				.filter((opt) => {
					const flag = Permissions[opt.id as keyof typeof Permissions];
					return flag != null && (bitfield & flag) === flag;
				})
				.map((opt) => opt.id);
		} catch (err) {
			logger.warn('Failed to parse requested permissions', err);
			return [];
		}
	}, [authParams?.permissions, botPermissionOptions]);

	React.useEffect(() => {
		if (selectedScopes === null && scopes.length > 0) {
			setSelectedScopes(new Set(scopes));
		}
	}, [scopes, selectedScopes]);

	React.useEffect(() => {
		if (selectedPermissions === null && requestedPermissionKeys.length > 0) {
			setSelectedPermissions(new Set(requestedPermissionKeys));
		}
	}, [requestedPermissionKeys, selectedPermissions]);

	React.useEffect(() => {
		if (!hasBotScope) return;

		if (authParams?.guildId) {
			setSelectedGuildId(authParams.guildId);
			return;
		}

		if (manageableGuildOptions.length > 0) {
			setSelectedGuildId(manageableGuildOptions[0].value);
		}
	}, [authParams?.guildId, hasBotScope, manageableGuildOptions]);

	const toggleScope = React.useCallback((scope: string) => {
		if (scope === 'bot') return;
		setSelectedScopes((prev) => {
			const next = new Set(prev ?? []);
			if (next.has(scope)) {
				next.delete(scope);
			} else {
				next.add(scope);
			}
			return next;
		});
	}, []);

	const togglePermission = React.useCallback((permissionId: string) => {
		setSelectedPermissions((prev) => {
			const next = new Set(prev ?? []);
			if (next.has(permissionId)) {
				next.delete(permissionId);
			} else {
				next.add(permissionId);
			}
			return next;
		});
	}, []);

	const selectedScopeList = React.useMemo(() => Array.from(selectedScopes ?? []), [selectedScopes]);

	const validationError = React.useMemo(() => {
		if (!authParams) return null;
		if (!isBotOnly && !authParams.redirectUri) {
			return t`A redirect_uri is required when the bot scope is not the only scope.`;
		}
		if (!isBotOnly && publicApp && authParams.redirectUri) {
			const allowed = publicApp.redirect_uris?.includes(authParams.redirectUri);
			if (!allowed) {
				return t`The provided redirect_uri is not registered for this application.`;
			}
		}
		return null;
	}, [authParams, isBotOnly, publicApp]);

	const currentUser = UserStore.currentUser;

	const redirectHostname = React.useMemo(() => {
		if (!authParams?.redirectUri) return null;
		try {
			return new URL(authParams.redirectUri).hostname;
		} catch (err) {
			logger.warn('Invalid redirect_uri for authorize request', err);
			return null;
		}
	}, [authParams?.redirectUri]);

	const botInviteWithoutRedirect = React.useMemo(
		() => hasBotScope && !authParams?.redirectUri,
		[authParams?.redirectUri, hasBotScope],
	);

	const appName = publicApp?.name?.trim();
	const clientLabel = appName || t`This application`;

	const appAvatarUrl = React.useMemo<string | null>(() => {
		if (!publicApp?.id || !publicApp.icon) {
			return null;
		}
		const url = AvatarUtils.getUserAvatarURL({id: publicApp.id, avatar: publicApp.icon}, false);
		return url ?? null;
	}, [publicApp?.icon, publicApp?.id]);

	const appInitial = clientLabel.charAt(0).toUpperCase();

	const formattedPermissions = React.useMemo(() => {
		if (!hasBotScope || !authParams?.permissions) return authParams?.permissions ?? undefined;
		return formatBotPermissionsQuery(Array.from(selectedPermissions ?? []));
	}, [authParams?.permissions, hasBotScope, selectedPermissions]);

	const scopesAdjusted = React.useMemo(() => {
		if (selectedScopes === null) return false;
		return scopes.length > 0 && selectedScopes.size < scopes.length;
	}, [scopes, selectedScopes]);

	const permissionsAdjusted = React.useMemo(() => {
		if (selectedPermissions === null) return false;
		return requestedPermissionKeys.length > 0 && selectedPermissions.size < requestedPermissionKeys.length;
	}, [requestedPermissionKeys, selectedPermissions]);

	const requestsAdmin = React.useMemo(() => {
		return requestedPermissionKeys.some((perm) => perm === 'ADMINISTRATOR');
	}, [requestedPermissionKeys]);

	const needsPermissionsStep = React.useMemo(() => {
		return hasBotScope && requestedPermissionKeys.length > 0;
	}, [hasBotScope, requestedPermissionKeys]);

	const goToPermissions = React.useCallback(() => {
		setCurrentStep('permissions');
	}, []);

	const goBack = React.useCallback(() => {
		setCurrentStep('scopes');
	}, []);

	const guildSelectComponents = React.useMemo(
		() =>
			createGuildSelectComponents<GuildSelectOption>({
				styles: {
					optionRow: styles.guildOption,
					valueRow: styles.guildValue,
					avatar: styles.guildAvatar,
					avatarPlaceholder: styles.guildAvatarPlaceholder,
					label: styles.guildOptionLabel,
					rowDisabled: styles.guildOptionDisabled,
					notice: styles.guildOptionNotice,
				},
				getNotice: (_, disabled) => (disabled ? <Trans>No Manage Community permission</Trans> : null),
			}),
		[],
	);

	const getErrorCode = React.useCallback((err: unknown): string | undefined => {
		if (err instanceof HttpError) {
			const body = err.body;
			if (body && typeof body === 'object' && 'code' in body) {
				const {code} = body as {code?: unknown};
				return typeof code === 'string' ? code : undefined;
			}
		}
		return undefined;
	}, []);

	const onAuthorize = React.useCallback(async () => {
		if (!authParams) return;
		setSubmitting('approve');

		try {
			const scopeToSend = (selectedScopeList.length > 0 ? selectedScopeList : scopes).join(' ');
			const guildIdToSend = hasBotScope ? (selectedGuildId ?? authParams.guildId) : authParams.guildId;

			const body: Record<string, string | undefined> = {
				response_type: authParams.responseType || 'code',
				client_id: authParams.clientId,
				scope: scopeToSend || authParams.scope,
			};

			if (authParams.redirectUri) body.redirect_uri = authParams.redirectUri;
			if (authParams.state) body.state = authParams.state;
			if (authParams.codeChallenge) body.code_challenge = authParams.codeChallenge;
			if (authParams.codeChallengeMethod) body.code_challenge_method = authParams.codeChallengeMethod;
			if (formattedPermissions && hasBotScope) body.permissions = formattedPermissions;
			if (guildIdToSend) body.guild_id = guildIdToSend;

			const resp = await http.post<{redirect_to: string}>({
				url: Endpoints.OAUTH_CONSENT,
				body,
				rejectWithError: true,
			});

			if (botInviteWithoutRedirect) {
				const guildName = guildIdToSend ? (guildLabelMap.get(guildIdToSend) ?? null) : null;
				setSuccessState({guildName});
				setSubmitting(null);
				return;
			}

			if (resp.body?.redirect_to) {
				window.location.href = resp.body.redirect_to;
			} else {
				setSubmitting(null);
				setError(t`Authorization failed. Please try again.`);
			}
		} catch (e) {
			logger.error('Authorization failed', e);
			setSubmitting(null);

			const httpError = e instanceof HttpError ? e : null;
			const errorCode = getErrorCode(e);

			if (httpError?.status === 400 && errorCode === 'BOT_ALREADY_IN_GUILD') {
				const message =
					typeof httpError.body === 'object' &&
					httpError.body &&
					'message' in httpError.body &&
					typeof (httpError.body as {message?: unknown}).message === 'string'
						? (httpError.body as {message?: string}).message
						: null;

				setError(message ?? t`Bot is already in this guild.`);
				return;
			}

			setError(t`Authorization failed. Please try again.`);
		}
	}, [
		authParams,
		botInviteWithoutRedirect,
		formattedPermissions,
		getErrorCode,
		guildLabelMap,
		hasBotScope,
		scopes,
		selectedGuildId,
		selectedScopeList,
	]);

	const onCancel = React.useCallback(() => {
		if (!authParams) return;
		setSubmitting('deny');

		try {
			const url = new URL(authParams.redirectUri ?? '/');
			url.searchParams.set('error', 'access_denied');
			if (authParams.state) {
				url.searchParams.set('state', authParams.state);
			}
			window.location.href = url.toString();
		} catch (err) {
			logger.error('Failed to redirect on cancel', err);
			setSubmitting(null);
			setError(t`Invalid redirect_uri`);
		}
	}, [authParams]);

	React.useEffect(() => {
		if (!loading && authParams?.prompt === 'none' && !submitting && !successState) {
			void onAuthorize();
		}
	}, [authParams?.prompt, loading, onAuthorize, submitting, successState]);

	if (loading) {
		return (
			<div className={styles.loadingContainer}>
				<Spinner />
			</div>
		);
	}

	if (error || !authParams || validationError) {
		return (
			<div className={styles.errorContainer}>
				<div className={styles.errorContent}>
					<h1 className={styles.errorTitle}>
						<Trans>Authorization Failed</Trans>
					</h1>
					<p className={styles.errorText}>{error ?? validationError ?? t`Invalid authorization request`}</p>
				</div>
			</div>
		);
	}

	if (successState) {
		return (
			<div className={styles.page}>
				<div className={styles.successScreen}>
					<div className={styles.successIconCircle}>
						<CheckCircleIcon weight="fill" className={styles.successIcon} />
					</div>
					<h1 className={styles.successTitle}>
						<Trans>Bot added</Trans>
					</h1>
					<p className={styles.successSubtitle}>
						{successState.guildName ? (
							<Trans>
								The bot has been added to <strong>{successState.guildName}</strong>.
							</Trans>
						) : (
							<Trans>The bot has been added.</Trans>
						)}
					</p>
				</div>
			</div>
		);
	}

	if (currentStep === 'permissions') {
		return (
			<div className={styles.page}>
				<div className={styles.pageLogo}>
					<FluxerWordmarkMonochrome className={styles.pageWordmark} />
				</div>

				<div className={styles.breadcrumbs}>
					<button type="button" className={styles.breadcrumbStep} onClick={goBack}>
						<span className={styles.breadcrumbNumber}>1</span>
						<span className={styles.breadcrumbLabel}>
							<Trans>Scopes</Trans>
						</span>
					</button>
					<span className={styles.breadcrumbSeparator}>›</span>
					<span className={clsx(styles.breadcrumbStep, styles.breadcrumbActive)}>
						<span className={styles.breadcrumbNumber}>2</span>
						<span className={styles.breadcrumbLabel}>
							<Trans>Permissions</Trans>
						</span>
					</span>
				</div>

				<div className={styles.heroCard}>
					<div className={styles.heroAvatarShell}>
						<BaseAvatar
							size={48}
							avatarUrl={appAvatarUrl || ''}
							shouldPlayAnimated={false}
							className={!appAvatarUrl ? styles.appAvatarFallback : undefined}
							userTag={clientLabel}
						/>
						{!appAvatarUrl && <span className={styles.appAvatarInitial}>{appInitial}</span>}
					</div>
					<div className={styles.heroCopy}>
						<h1 className={styles.heroTitle}>
							<Trans>Configure bot permissions</Trans>
						</h1>
						<p className={styles.heroDescription}>
							<Trans>
								Choose what {clientLabel} can do in your community. Uncheck any permissions you don't want to grant.
							</Trans>
						</p>
					</div>
				</div>

				<div className={styles.sectionDivider} />

				<div className={styles.permissionScrollContainer}>
					<Scroller className={styles.permissionScroller}>
						<div className={styles.permissionList}>
							{requestedPermissionKeys.map((perm) => {
								const option = botPermissionOptions.find((opt) => opt.id === perm);
								if (!option) return null;
								return (
									<div
										key={perm}
										className={styles.permissionRow}
										onClick={() => togglePermission(perm)}
										onKeyDown={(e) => e.key === 'Enter' && togglePermission(perm)}
										role="button"
										tabIndex={0}
									>
										<Checkbox
											checked={selectedPermissions?.has(perm) ?? true}
											onChange={() => togglePermission(perm)}
											size="small"
										>
											<span className={styles.permissionLabel}>{option.label}</span>
										</Checkbox>
									</div>
								);
							})}
						</div>
					</Scroller>
				</div>

				{requestsAdmin && (
					<>
						<div className={styles.sectionDivider} />
						<div className={styles.dangerNotice}>
							<Trans>
								This bot is requesting the Administrator permission. We do not recommend granting this to production
								apps unless you fully trust the developer. Consider asking them to request a reduced set of permissions.
								Close this page if you are unsure.
							</Trans>
						</div>
					</>
				)}

				{permissionsAdjusted && (
					<>
						<div className={styles.sectionDivider} />
						<div className={styles.caution}>
							<Trans>Removing permissions could limit the bot's features.</Trans>
						</div>
					</>
				)}

				<div className={styles.sectionDivider} />

				<div className={styles.actionSection}>
					<div className={styles.actions}>
						<div className={styles.actionButton}>
							<Button type="button" variant="secondary" onClick={goBack} className={styles.actionButton}>
								<Trans>Back</Trans>
							</Button>
						</div>
						<div className={styles.actionButton}>
							<Button
								type="button"
								submitting={submitting === 'approve'}
								onClick={onAuthorize}
								className={styles.actionButton}
							>
								<Trans>Authorize</Trans>
							</Button>
						</div>
					</div>

					{redirectHostname && (
						<p className={styles.footerText}>
							<Trans>
								You will be taken to{' '}
								<Tooltip text={authParams.redirectUri ?? ''} maxWidth="xl">
									<strong>{redirectHostname}</strong>
								</Tooltip>{' '}
								after authorizing.
							</Trans>
						</p>
					)}
				</div>
			</div>
		);
	}

	return (
		<div className={styles.page}>
			<div className={styles.pageLogo}>
				<FluxerWordmarkMonochrome className={styles.pageWordmark} />
			</div>

			{needsPermissionsStep && (
				<div className={styles.breadcrumbs}>
					<span className={clsx(styles.breadcrumbStep, styles.breadcrumbActive)}>
						<span className={styles.breadcrumbNumber}>1</span>
						<span className={styles.breadcrumbLabel}>
							<Trans>Scopes</Trans>
						</span>
					</span>
					<span className={styles.breadcrumbSeparator}>›</span>
					<span className={styles.breadcrumbStep}>
						<span className={styles.breadcrumbNumber}>2</span>
						<span className={styles.breadcrumbLabel}>
							<Trans>Permissions</Trans>
						</span>
					</span>
				</div>
			)}

			<div className={styles.heroCard}>
				<div className={styles.heroAvatarShell}>
					<BaseAvatar
						size={48}
						avatarUrl={appAvatarUrl || ''}
						shouldPlayAnimated={false}
						className={!appAvatarUrl ? styles.appAvatarFallback : undefined}
						userTag={clientLabel}
					/>
					{!appAvatarUrl && <span className={styles.appAvatarInitial}>{appInitial}</span>}
				</div>

				<div className={styles.heroCopy}>
					<p className={styles.eyebrow}>
						<Trans>Authorization request</Trans>
					</p>
					<h1 className={styles.heroTitle}>
						<Trans>{clientLabel} wants to connect</Trans>
					</h1>
					<p className={styles.heroDescription}>
						{publicApp?.description ? (
							publicApp.description
						) : (
							<Trans>Review what this app is asking for before you continue.</Trans>
						)}
					</p>

					<div className={styles.chipRow}>
						{redirectHostname ? (
							<Tooltip text={authParams.redirectUri ?? ''} maxWidth="xl">
								<span className={styles.chip}>
									<Trans>Will send you back to</Trans> {redirectHostname}
								</span>
							</Tooltip>
						) : (
							botInviteWithoutRedirect && <span className={styles.chip}>{t`Bot invite (no external redirect)`}</span>
						)}

						{authParams.guildId && (
							<span className={styles.chip}>
								<Trans>Target community</Trans>: {authParams.guildId}
							</span>
						)}

						{hasBotScope && <span className={styles.chip}>{t`Bot scope requested`}</span>}
					</div>
				</div>
			</div>

			<div className={styles.sectionDivider} />

			{currentUser && (
				<>
					<div className={styles.userCard}>
						<div className={styles.userDetails}>
							<BaseAvatar
								size={32}
								avatarUrl={AvatarUtils.getUserAvatarURL(currentUser, false)}
								shouldPlayAnimated={false}
							/>
							<div className={styles.userText}>
								<div className={styles.userLabel}>
									<Trans>Signed in as</Trans>
								</div>
								<div className={styles.userNameLine}>
									<span className={styles.userName}>{currentUser.displayName || currentUser.username}</span>
									<span className={styles.userTag}>
										{currentUser.username}#{currentUser.discriminator}
									</span>
								</div>
							</div>
						</div>

						<button type="button" className={styles.switchAccountLink} onClick={openAccountSwitcher}>
							<Trans>Switch account</Trans>
						</button>
					</div>

					<div className={styles.sectionDivider} />
				</>
			)}

			<div className={styles.cardGrid}>
				<div className={styles.panel}>
					<div className={styles.sectionHeader}>
						<h3 className={styles.sectionTitle}>
							<Trans>Requested scopes</Trans>
						</h3>
						<p className={styles.sectionDescription}>
							<Trans>Turn off anything you're not comfortable with. Some features may stop working.</Trans>
						</p>
					</div>

					<div className={styles.scopeList}>
						{scopes.length === 0 ? (
							<div className={styles.emptyState}>
								<Trans>No specific scopes requested.</Trans>
							</div>
						) : (
							scopes.map((scope) => {
								const isLocked = scope === 'bot';
								return (
									<div key={scope} className={styles.scopeRow}>
										<Switch
											value={selectedScopes?.has(scope) ?? true}
											onChange={() => toggleScope(scope)}
											disabled={isLocked}
											compact
											label={
												<div className={styles.scopeHeading}>
													<span className={styles.scopeName}>{scope}</span>
													{isLocked && <span className={styles.scopeChip}>{t`Required`}</span>}
												</div>
											}
											description={<span className={styles.scopeDescription}>{getScopeDescription(scope)}</span>}
										/>
									</div>
								);
							})
						)}
					</div>

					{scopesAdjusted && (
						<div className={styles.caution}>
							<Trans>Turning off scopes may prevent the app from working correctly.</Trans>
						</div>
					)}
				</div>

				{hasBotScope && <div className={styles.sectionDivider} />}

				{hasBotScope && (
					<div className={styles.panel}>
						<div className={styles.sectionHeader}>
							<h3 className={styles.sectionTitle}>
								<Trans>Add bot to a community</Trans>
							</h3>
							<p className={styles.sectionDescription}>
								<Trans>
									Select a community where you have <strong className={styles.textEmphasis}>Manage Community</strong>{' '}
									permissions.
								</Trans>
							</p>
						</div>

						<Select
							value={selectedGuildId || ''}
							onChange={(value) => setSelectedGuildId((value as string) || null)}
							options={guildOptions}
							placeholder={guildsLoading ? t`Loading communities...` : t`Choose a community`}
							components={guildSelectComponents}
							isSearchable
							disabled={guildOptions.length === 0 || guildsLoading}
						/>

						{guildsLoading && (
							<div className={styles.sectionDescription}>
								<Spinner />
							</div>
						)}

						{guildsError && <div className={styles.sectionDescription}>{guildsError}</div>}

						{!guildsLoading && manageableGuildOptions.length === 0 && (
							<div className={styles.emptyState}>
								<Trans>
									No communities available with <strong className={styles.textEmphasis}>Manage Community</strong>{' '}
									permissions.
								</Trans>
							</div>
						)}
					</div>
				)}

				{needsPermissionsStep && (
					<>
						<div className={styles.sectionDivider} />
						<p className={styles.permissionsHint}>
							<Trans>You'll configure which permissions the bot receives on the next screen.</Trans>
						</p>
					</>
				)}
			</div>

			<div className={styles.sectionDivider} />

			<div className={styles.actionSection}>
				<div className={styles.actions}>
					<div className={styles.actionButton}>
						<Button
							type="button"
							variant="secondary"
							onClick={onCancel}
							disabled={submitting === 'approve'}
							className={styles.actionButton}
						>
							<Trans>Cancel</Trans>
						</Button>
					</div>

					<div className={styles.actionButton}>
						{needsPermissionsStep ? (
							<Button type="button" onClick={goToPermissions} className={styles.actionButton}>
								<Trans>Next</Trans>
							</Button>
						) : (
							<Button
								type="button"
								submitting={submitting === 'approve'}
								onClick={onAuthorize}
								className={styles.actionButton}
							>
								<Trans>Authorize</Trans>
							</Button>
						)}
					</div>
				</div>

				{redirectHostname && !needsPermissionsStep && (
					<p className={styles.footerText}>
						<Trans>
							You will be taken to{' '}
							<Tooltip text={authParams.redirectUri ?? ''} maxWidth="xl">
								<strong>{redirectHostname}</strong>
							</Tooltip>{' '}
							after authorizing.
						</Trans>
					</p>
				)}
			</div>
		</div>
	);
});

export default OAuthAuthorizePage;
