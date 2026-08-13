// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {useSudo} from '@app/features/auth/hooks/useSudo';
import type {DeveloperApplication} from '@app/features/devtools/models/DeveloperApplication';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatBotPermissionsQuery, getAllBotPermissions} from '@app/features/permissions/utils/PermissionUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import ApplicationsTabState from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTabState';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {SectionCard} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailSectionCard';
import type {ApplicationDetailFormValues} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailTypes';
import {ApplicationHeader} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationHeader';
import {ApplicationInfoSection} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationInfoSection';
import {BotProfileSection} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/BotProfileSection';
import {OAuthBuilderSection} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/OAuthBuilderSection';
import {SecretsSection} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/SecretsSection';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {pushApiErrorModal} from '@app/lib/forms';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {OAuth2Scopes} from '@fluxer/constants/src/OAuth2Constants';
import {PublicUserFlags} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {TrashIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useState} from 'react';
import {type Path, useForm, useWatch} from 'react-hook-form';

const NO_CHANGES_TO_SAVE_DESCRIPTOR = msg({
	message: 'No changes to save',
	comment: 'Empty-state text in the application detail.',
});
const APPLICATION_UPDATED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Application updated successfully',
	comment: 'Short label in the application detail. Keep it concise.',
});
const DELETE_APPLICATION_DESCRIPTOR = msg({
	message: 'Delete application',
	comment: 'Button or menu action label in the application detail. Keep it concise. Keep the tone plain and specific.',
});
const COULD_NOT_DELETE_APPLICATION_DESCRIPTOR = msg({
	message: "Couldn't delete application",
	comment: 'Title of the error modal shown when deleting a developer application fails.',
});
const CLIENT_SECRET_REGENERATED_UPDATE_ANY_CODE_THAT_USES_DESCRIPTOR = msg({
	message: 'Client secret regenerated. Update any code that uses the old secret.',
	comment: 'Description text in the application detail.',
});
const BOT_TOKEN_REGENERATED_UPDATE_ANY_CODE_THAT_USES_DESCRIPTOR = msg({
	message: 'Bot token regenerated. Update any code that uses the old token.',
	comment: 'Description text in the application detail. Keep the tone plain and specific.',
});
const COULD_NOT_REGENERATE_SECRET_DESCRIPTOR = msg({
	message: "Couldn't regenerate secret",
	comment: 'Title of the error modal shown when regenerating a client secret or bot token fails.',
});
const REGENERATE_CLIENT_SECRET_DESCRIPTOR = msg({
	message: 'Regenerate client secret?',
	comment: 'Confirmation prompt in the application detail.',
});
const REGENERATE_BOT_TOKEN_DESCRIPTOR = msg({
	message: 'Regenerate bot token?',
	comment: 'Confirmation prompt in the application detail. Keep the tone plain and specific.',
});
const REGENERATE_DESCRIPTOR = msg({
	message: 'Regenerate',
	comment: 'Short label in the application detail. Keep it concise.',
});
const COPIED_URL_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Copied URL to clipboard',
	comment: 'Label in the application detail.',
});
const logger = new Logger('ApplicationDetail');

interface ApplicationDetailProps {
	applicationId: string;
	onBack: () => void;
	initialApplication?: DeveloperApplication | null;
}

interface ApplicationDetailRemoteValues {
	readonly formValues: ApplicationDetailFormValues;
	readonly clientSecret: string | null;
	readonly botToken: string | null;
}

const APPLICATIONS_TAB_ID = 'applications';
const AVAILABLE_SCOPES = OAuth2Scopes;
const APPLICATION_DETAIL_ERROR_PATH_MAP: Partial<Record<string, Path<ApplicationDetailFormValues>>> = {
	redirect_uris: 'redirectUriInputs',
};
const isFriendlyFlagSet = (flags?: number): boolean =>
	!!flags && (flags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT;
const isManualApprovalFlagSet = (flags?: number): boolean =>
	!!flags && (flags & PublicUserFlags.FRIENDLY_BOT_MANUAL_APPROVAL) === PublicUserFlags.FRIENDLY_BOT_MANUAL_APPROVAL;
export const ApplicationDetail: React.FC<ApplicationDetailProps> = observer(
	({applicationId, onBack, initialApplication}) => {
		const {i18n} = useLingui();
		const store = ApplicationsTabState;
		const application = store.selectedApplication;
		const loading = store.isLoading && store.isDetailView;
		const error = store.error;
		const [idCopied, setIdCopied] = useState(false);
		const [previewAvatarUrl, setPreviewAvatarUrl] = useState<string | null>(null);
		const [hasClearedAvatar, setHasClearedAvatar] = useState(false);
		const [previewBannerUrl, setPreviewBannerUrl] = useState<string | null>(null);
		const [hasClearedBanner, setHasClearedBanner] = useState(false);
		const [isDeleting, setIsDeleting] = useState(false);
		const [initialValues, setInitialValues] = useState<ApplicationDetailFormValues | null>(null);
		const [clientSecret, setClientSecret] = useState<string | null>(null);
		const [botToken, setBotToken] = useState<string | null>(null);
		const [isRotating, setIsRotating] = useState<'client' | 'bot' | null>(null);
		const clientSecretInputId = useId();
		const botTokenInputId = useId();
		const sudo = useSudo();
		const form = useForm<ApplicationDetailFormValues>({
			defaultValues: {
				name: '',
				botPublic: true,
				botRequireCodeGrant: false,
				friendlyBot: false,
				botManualFriendRequestApproval: false,
				redirectUris: [],
				redirectUriInputs: [''],
				builderScopes: {} as Record<string, boolean>,
				builderPermissions: {} as Record<string, boolean>,
				username: '',
				bio: '',
			},
		});
		const buildFormDefaults = useCallback((app: DeveloperApplication): ApplicationDetailFormValues => {
			const builderScopeMap = AVAILABLE_SCOPES.reduce<Record<string, boolean>>((acc, scope) => {
				acc[scope] = false;
				return acc;
			}, {});
			const redirectList = (app.redirect_uris ?? []).length > 0 ? app.redirect_uris : [''];
			return {
				name: app.name,
				redirectUris: app.redirect_uris ?? [],
				redirectUriInputs: redirectList,
				botPublic: app.bot_public,
				botRequireCodeGrant: app.bot_require_code_grant,
				friendlyBot: isFriendlyFlagSet(app.bot?.flags),
				botManualFriendRequestApproval: isManualApprovalFlagSet(app.bot?.flags),
				builderScopes: builderScopeMap,
				builderPermissions: {},
				username: app.bot?.username || '',
				bio: app.bot?.bio ?? '',
			};
		}, []);
		const buildRemoteValues = useCallback(
			(app: DeveloperApplication): ApplicationDetailRemoteValues => ({
				formValues: buildFormDefaults(app),
				clientSecret: app.client_secret ?? null,
				botToken: app.bot?.token ?? null,
			}),
			[buildFormDefaults],
		);
		const remoteValues = useMemo<ApplicationDetailRemoteValues | null>(
			() => (application && application.id === applicationId ? buildRemoteValues(application) : null),
			[application, applicationId, buildRemoteValues],
		);
		useEffect(() => {
			if (initialApplication && initialApplication.id === applicationId) {
				void store.navigateToDetail(applicationId, initialApplication);
			} else if (!store.selectedApplication || store.selectedAppId !== applicationId) {
				void store.navigateToDetail(applicationId);
			}
		}, [applicationId, initialApplication, store]);
		const formIsSubmitting = form.formState.isSubmitting;
		const watchedValues = useWatch<ApplicationDetailFormValues>({control: form.control});
		const hasFormChanges = useMemo(() => {
			if (!initialValues) return false;
			const currentValues =
				(watchedValues as ApplicationDetailFormValues | undefined) ?? ({} as ApplicationDetailFormValues);
			return (
				(currentValues.name ?? '') !== (initialValues.name ?? '') ||
				(currentValues.redirectUris ?? []).join(',') !== (initialValues.redirectUris ?? []).join(',') ||
				(currentValues.redirectUriInputs ?? []).join(',') !== (initialValues.redirectUriInputs ?? []).join(',') ||
				(currentValues.botPublic ?? true) !== (initialValues.botPublic ?? true) ||
				(currentValues.botRequireCodeGrant ?? false) !== (initialValues.botRequireCodeGrant ?? false) ||
				(currentValues.username ?? '') !== (initialValues.username ?? '') ||
				(currentValues.bio ?? '') !== (initialValues.bio ?? '') ||
				(currentValues.banner ?? '') !== (initialValues.banner ?? '') ||
				(currentValues.friendlyBot ?? false) !== (initialValues.friendlyBot ?? false) ||
				(currentValues.botManualFriendRequestApproval ?? false) !==
					(initialValues.botManualFriendRequestApproval ?? false)
			);
		}, [initialValues, watchedValues]);
		const hasUnsavedChanges = useMemo(() => {
			return Boolean(hasFormChanges || previewAvatarUrl || hasClearedAvatar || previewBannerUrl || hasClearedBanner);
		}, [hasFormChanges, previewAvatarUrl, hasClearedAvatar, previewBannerUrl, hasClearedBanner]);
		const applyRemoteValues = useCallback((values: ApplicationDetailRemoteValues) => {
			setClientSecret(values.clientSecret);
			setBotToken(values.botToken);
			setInitialValues(values.formValues);
			setPreviewAvatarUrl(null);
			setHasClearedAvatar(false);
			setPreviewBannerUrl(null);
			setHasClearedBanner(false);
		}, []);
		const {resetToRemoteValues, commitRemoteValues} = useRemoteFormReset<
			ApplicationDetailFormValues,
			ApplicationDetailRemoteValues
		>({
			form,
			identityKey: applicationId,
			remoteValues,
			isDirty: hasUnsavedChanges,
			getFormValues: (values) => values.formValues,
			onApply: applyRemoteValues,
		});
		const onSubmit = useCallback(
			async (data: ApplicationDetailFormValues) => {
				if (!application) return;
				const normalizedName = data.name.trim();
				const redirectUris = (data.redirectUriInputs ?? []).map((u) => u.trim()).filter(Boolean);
				const dirtyFields = form.formState.dirtyFields;
				const buildApplicationPatch = () => {
					const changes: Record<string, unknown> = {};
					if (normalizedName !== application.name) {
						changes.name = normalizedName;
					}
					const initialRedirects = application.redirect_uris ?? [];
					if ((redirectUris ?? []).join(',') !== initialRedirects.join(',')) {
						changes.redirect_uris = redirectUris;
					}
					if ((data.botPublic ?? true) !== (application.bot_public ?? true)) {
						changes.bot_public = data.botPublic;
					}
					if ((data.botRequireCodeGrant ?? false) !== (application.bot_require_code_grant ?? false)) {
						changes.bot_require_code_grant = data.botRequireCodeGrant;
					}
					return changes;
				};
				const buildBotPatch = () => {
					if (!application.bot) return null;
					const botBody: Record<string, unknown> = {};
					const currentBot = application.bot;
					const avatarCleared = hasClearedAvatar;
					const bannerCleared = hasClearedBanner;
					if (dirtyFields.username && data.username && data.username !== currentBot.username) {
						botBody.username = data.username;
					}
					const shouldSendAvatar = dirtyFields.avatar || avatarCleared;
					if (shouldSendAvatar) {
						if (avatarCleared) {
							botBody.avatar = null;
						} else if (data.avatar) {
							botBody.avatar = data.avatar;
						}
					}
					const shouldSendBanner = dirtyFields.banner || bannerCleared;
					if (shouldSendBanner) {
						if (bannerCleared) {
							botBody.banner = null;
						} else if (data.banner) {
							botBody.banner = data.banner;
						}
					}
					if (dirtyFields.bio) {
						const trimmedBio = data.bio?.trim() ?? '';
						const currentBio = currentBot.bio ?? '';
						if (trimmedBio !== currentBio) {
							botBody.bio = trimmedBio.length > 0 ? trimmedBio : null;
						}
					}
					const desiredFriendly = Boolean(data.friendlyBot);
					const desiredManualApproval = Boolean(data.botManualFriendRequestApproval);
					const currentlyFriendly = isFriendlyFlagSet(currentBot.flags);
					const currentlyManualApproval = isManualApprovalFlagSet(currentBot.flags);
					const friendlyFlag = PublicUserFlags.FRIENDLY_BOT;
					const manualApprovalFlag = PublicUserFlags.FRIENDLY_BOT_MANUAL_APPROVAL;
					let updatedFlags = currentBot.flags ?? 0;
					if (desiredFriendly && !currentlyFriendly) {
						updatedFlags |= friendlyFlag;
					} else if (!desiredFriendly && currentlyFriendly) {
						updatedFlags &= ~friendlyFlag;
					}
					if (desiredManualApproval && !currentlyManualApproval) {
						updatedFlags |= manualApprovalFlag;
					} else if (!desiredManualApproval && currentlyManualApproval) {
						updatedFlags &= ~manualApprovalFlag;
					}
					if (updatedFlags !== (currentBot.flags ?? 0)) {
						botBody.bot_flags = updatedFlags;
					}
					return Object.keys(botBody).length > 0 ? botBody : null;
				};
				const appPatch = buildApplicationPatch();
				const botPatch = buildBotPatch();
				if (Object.keys(appPatch).length === 0 && !botPatch) {
					ToastCommands.createToast({type: 'info', children: i18n._(NO_CHANGES_TO_SAVE_DESCRIPTOR)});
					return;
				}
				try {
					if (Object.keys(appPatch).length > 0) {
						await http.patch(Endpoints.OAUTH_APPLICATION(applicationId), {body: appPatch});
					}
					if (botPatch) {
						await http.patch(Endpoints.OAUTH_APPLICATION_BOT_PROFILE(applicationId), {body: botPatch});
					}
					ToastCommands.createToast({type: 'success', children: i18n._(APPLICATION_UPDATED_SUCCESSFULLY_DESCRIPTOR)});
					const updatedApplication = await store.fetchApplication(applicationId);
					if (updatedApplication) {
						commitRemoteValues(buildRemoteValues(updatedApplication));
					}
				} catch (err) {
					logger.error('Failed to update application', err);
					throw err;
				}
			},
			[
				application,
				applicationId,
				store,
				form.formState.dirtyFields,
				hasClearedAvatar,
				hasClearedBanner,
				commitRemoteValues,
				buildRemoteValues,
			],
		);
		const {handleSubmit: handleSave} = useFormSubmit({
			form,
			onSubmit,
			defaultErrorField: 'name',
			pathMap: APPLICATION_DETAIL_ERROR_PATH_MAP,
		});
		const handleReset = useCallback(() => {
			resetToRemoteValues();
		}, [resetToRemoteValues]);
		useEffect(() => {
			UnsavedChangesCommands.setUnsavedChanges(APPLICATIONS_TAB_ID, hasUnsavedChanges);
		}, [hasUnsavedChanges]);
		useEffect(() => {
			UnsavedChangesCommands.setTabData(APPLICATIONS_TAB_ID, {
				onReset: handleReset,
				onSave: handleSave,
				isSubmitting: formIsSubmitting,
			});
		}, [handleReset, handleSave, formIsSubmitting]);
		useEffect(() => {
			return () => {
				UnsavedChangesCommands.clearUnsavedChanges(APPLICATIONS_TAB_ID);
			};
		}, []);
		const handleAvatarChange = useCallback(
			(base64: string) => {
				form.setValue('avatar', base64, {shouldDirty: true});
				setPreviewAvatarUrl(base64);
				setHasClearedAvatar(false);
				form.clearErrors('avatar');
			},
			[form],
		);
		const handleBannerChange = useCallback(
			(base64: string) => {
				form.setValue('banner', base64, {shouldDirty: true});
				setPreviewBannerUrl(base64);
				setHasClearedBanner(false);
				form.clearErrors('banner');
			},
			[form],
		);
		const handleBannerClear = useCallback(() => {
			form.setValue('banner', null, {shouldDirty: true});
			setPreviewBannerUrl(null);
			setHasClearedBanner(true);
		}, [form]);
		const handleAvatarClear = useCallback(() => {
			form.setValue('avatar', null, {shouldDirty: true});
			setPreviewAvatarUrl(null);
			setHasClearedAvatar(true);
		}, [form]);
		const handleCopyId = async () => {
			if (!application) return;
			try {
				await navigator.clipboard.writeText(application.id);
				setIdCopied(true);
				setTimeout(() => setIdCopied(false), 2000);
			} catch (err) {
				logger.error('Failed to copy ID', err);
			}
		};
		const handleDelete = () => {
			if (!application) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_APPLICATION_DESCRIPTOR)}
						description={
							<div data-flx="user.applications-tab.application-detail.handle-delete.div">
								<Trans>
									Are you sure you want to delete{' '}
									<strong data-flx="user.applications-tab.application-detail.handle-delete.strong">
										{application.name}
									</strong>
									?
								</Trans>
								<br data-flx="user.applications-tab.application-detail.handle-delete.br" />
								<br data-flx="user.applications-tab.application-detail.handle-delete.br--2" />
								<Trans>
									This action cannot be undone. All associated data, including the bot user, will be permanently
									deleted.
								</Trans>
							</div>
						}
						primaryText={i18n._(DELETE_APPLICATION_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								setIsDeleting(true);
								await http.delete(Endpoints.OAUTH_APPLICATION(application.id), {body: {}});
								onBack();
							} catch (err) {
								logger.error('Failed to delete application', err);
								pushApiErrorModal(i18n, err, i18n._(COULD_NOT_DELETE_APPLICATION_DESCRIPTOR));
								setIsDeleting(false);
							}
						}}
						data-flx="user.applications-tab.application-detail.handle-delete.confirm-modal"
					/>
				)),
			);
		};
		const rotateSecret = async (type: 'client' | 'bot') => {
			if (!application) return;
			setIsRotating(type);
			try {
				const sudoPayload = await sudo.require();
				const endpoint =
					type === 'client'
						? Endpoints.OAUTH_APPLICATION_CLIENT_SECRET_RESET(application.id)
						: Endpoints.OAUTH_APPLICATION_BOT_TOKEN_RESET(application.id);
				const res = await http.post<{client_secret?: string; token?: string}>(endpoint, {body: sudoPayload});
				if (type === 'client') {
					setClientSecret(res.body.client_secret ?? null);
				} else {
					setBotToken(res.body.token ?? null);
				}
				sudo.finalize();
				ToastCommands.createToast({
					type: 'success',
					children:
						type === 'client'
							? i18n._(CLIENT_SECRET_REGENERATED_UPDATE_ANY_CODE_THAT_USES_DESCRIPTOR)
							: i18n._(BOT_TOKEN_REGENERATED_UPDATE_ANY_CODE_THAT_USES_DESCRIPTOR),
				});
			} catch (err) {
				logger.error('Failed to rotate secret', err);
				pushApiErrorModal(i18n, err, i18n._(COULD_NOT_REGENERATE_SECRET_DESCRIPTOR));
			} finally {
				setIsRotating(null);
			}
		};
		const addRedirectInput = () => {
			const current = form.getValues('redirectUriInputs') ?? [];
			form.setValue('redirectUriInputs', [...current, ''], {shouldDirty: true});
		};
		const removeRedirectInput = (index: number) => {
			const current = form.getValues('redirectUriInputs') ?? [];
			const next = current.filter((_, i) => i !== index);
			form.setValue('redirectUriInputs', next.length > 0 ? next : [''], {shouldDirty: true});
			form.clearErrors('redirectUriInputs');
		};
		const updateRedirectInput = (index: number, value: string) => {
			const current = form.getValues('redirectUriInputs') ?? [];
			const next = [...current];
			next[index] = value;
			form.setValue('redirectUriInputs', next, {shouldDirty: true});
			form.clearErrors(`redirectUriInputs.${index}` as `redirectUriInputs.${number}`);
		};
		const builderScopes = useWatch({control: form.control, name: 'builderScopes'}) || {};
		const builderPermissions = useWatch({control: form.control, name: 'builderPermissions'}) || {};
		const builderRedirectUri = useWatch({control: form.control, name: 'builderRedirectUri'});
		const botRequireCodeGrant = useWatch({control: form.control, name: 'botRequireCodeGrant'}) ?? false;
		const redirectInputs = useWatch({control: form.control, name: 'redirectUriInputs'}) ?? [];
		const bannerValue = useWatch({control: form.control, name: 'banner'});
		const builderScopeList = useMemo(
			() =>
				Object.entries(builderScopes)
					.filter(([, enabled]) => enabled)
					.map(([scope]) => scope),
			[builderScopes],
		);
		const botPermissionsList = useMemo(() => getAllBotPermissions(i18n), [i18n.locale]);
		const builderUrl = useMemo(() => {
			if (!application) return '';
			const authorizeUrl = new URL(Endpoints.OAUTH_AUTHORIZE, window.location.origin);
			authorizeUrl.searchParams.set('client_id', application.id);
			if (builderScopeList.length > 0) {
				authorizeUrl.searchParams.set('scope', builderScopeList.join(' '));
			}
			const isBotOnly = builderScopeList.length === 1 && builderScopeList[0] === 'bot';
			const requireRedirectUri = builderScopeList.length > 0 && (!isBotOnly || botRequireCodeGrant);
			const botPerms = Object.entries(builderPermissions)
				.filter(([, enabled]) => enabled)
				.map(([perm]) => perm);
			if (builderScopeList.includes('bot') && botPerms.length > 0) {
				authorizeUrl.searchParams.set('permissions', formatBotPermissionsQuery(botPerms));
			}
			const redirect = builderRedirectUri?.trim();
			if (requireRedirectUri && !redirect) {
				return '';
			}
			if (redirect) {
				authorizeUrl.searchParams.set('redirect_uri', redirect);
				authorizeUrl.searchParams.set('response_type', 'code');
			}
			if (builderScopeList.length === 0) {
				return '';
			}
			return authorizeUrl.toString();
		}, [application, builderScopeList, builderPermissions, builderRedirectUri, botRequireCodeGrant]);
		const redirectOptions = useMemo(() => {
			const normalized = Array.from(new Set((redirectInputs ?? []).map((u) => u.trim()).filter(Boolean)));
			const current = builderRedirectUri?.trim();
			if (current && !normalized.includes(current)) {
				normalized.push(current);
			}
			return normalized.map((url) => ({value: url, label: url}));
		}, [builderRedirectUri, redirectInputs]);
		const confirmRotate = (type: 'client' | 'bot') => {
			if (!application) return;
			const isClient = type === 'client';
			const description = isClient ? (
				<Trans>Regenerating will invalidate the current secret. Update any code that uses the old value.</Trans>
			) : (
				<Trans>Regenerating will invalidate the current token. Update any code that uses the old value.</Trans>
			);
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={isClient ? i18n._(REGENERATE_CLIENT_SECRET_DESCRIPTOR) : i18n._(REGENERATE_BOT_TOKEN_DESCRIPTOR)}
						description={description}
						primaryText={i18n._(REGENERATE_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={() => rotateSecret(type)}
						data-flx="user.applications-tab.application-detail.confirm-rotate.confirm-modal"
					/>
				)),
			);
		};
		const handleCopyBuilderUrl = useCallback(async () => {
			if (!builderUrl) return;
			await navigator.clipboard.writeText(builderUrl);
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(COPIED_URL_TO_CLIPBOARD_DESCRIPTOR),
			});
		}, [builderUrl]);
		if (loading) {
			return <div className={styles.loadingState} data-flx="user.applications-tab.application-detail.loading-state" />;
		}
		if (error || !application) {
			return (
				<div className={styles.page} data-flx="user.applications-tab.application-detail.page">
					<StatusSlate
						Icon={WarningCircleIcon}
						title={<Trans>Couldn't load this application</Trans>}
						description={<Trans>Try again or go back to the applications list.</Trans>}
						fullHeight={true}
						actions={[
							{
								text: i18n._(TRY_AGAIN_DESCRIPTOR),
								onClick: () => store.fetchApplication(applicationId),
							},
							{
								text: <Trans>Back to list</Trans>,
								onClick: onBack,
								variant: 'secondary',
							},
						]}
						data-flx="user.applications-tab.application-detail.status-slate"
					/>
				</div>
			);
		}
		const avatarUrl = application.bot
			? AvatarUtils.getUserAvatarURL({id: application.bot.id, avatar: application.bot.avatar}, false)
			: null;
		const defaultAvatarUrl = application.bot
			? AvatarUtils.getUserAvatarURL({id: application.bot.id, avatar: null}, false)
			: null;
		const displayAvatarUrl = hasClearedAvatar ? defaultAvatarUrl : previewAvatarUrl || avatarUrl || defaultAvatarUrl;
		const hasAvatar = (!hasClearedAvatar && Boolean(application.bot?.avatar)) || Boolean(previewAvatarUrl);
		const displayBannerUrl =
			previewBannerUrl ||
			(hasClearedBanner
				? null
				: application.bot
					? AvatarUtils.getUserBannerURL({id: application.bot.id, banner: application.bot.banner}, true)
					: null);
		const hasBanner = Boolean(displayBannerUrl || bannerValue);
		return (
			<Form form={form} onSubmit={onSubmit} data-flx="user.applications-tab.application-detail.form.submit">
				<div className={styles.page} data-flx="user.applications-tab.application-detail.page--2">
					<ApplicationHeader
						name={application.name}
						applicationId={application.id}
						onCopyId={handleCopyId}
						idCopied={idCopied}
						data-flx="user.applications-tab.application-detail.application-header"
					/>
					<div className={styles.detailGrid} data-flx="user.applications-tab.application-detail.detail-grid">
						<div className={styles.columnStack} data-flx="user.applications-tab.application-detail.column-stack">
							<SecretsSection
								clientSecret={clientSecret}
								botToken={botToken}
								onRegenerateClientSecret={() => confirmRotate('client')}
								onRegenerateBotToken={() => confirmRotate('bot')}
								isRotatingClient={isRotating === 'client'}
								isRotatingBot={isRotating === 'bot'}
								hasBot={Boolean(application.bot)}
								clientSecretInputId={clientSecretInputId}
								botTokenInputId={botTokenInputId}
								data-flx="user.applications-tab.application-detail.secrets-section"
							/>
							<div
								className={styles.sectionSpacer}
								aria-hidden="true"
								data-flx="user.applications-tab.application-detail.section-spacer"
							/>
							<ApplicationInfoSection
								form={form}
								redirectInputs={form.watch('redirectUriInputs') ?? []}
								onAddRedirect={addRedirectInput}
								onRemoveRedirect={removeRedirectInput}
								onUpdateRedirect={updateRedirectInput}
								data-flx="user.applications-tab.application-detail.application-info-section"
							/>
							{application.bot && (
								<>
									<div
										className={styles.sectionSpacer}
										aria-hidden="true"
										data-flx="user.applications-tab.application-detail.section-spacer--2"
									/>
									<BotProfileSection
										application={application}
										form={form}
										displayAvatarUrl={displayAvatarUrl}
										hasAvatar={hasAvatar}
										hasClearedAvatar={hasClearedAvatar}
										onAvatarChange={handleAvatarChange}
										onAvatarClear={handleAvatarClear}
										onBannerChange={handleBannerChange}
										onBannerClear={handleBannerClear}
										displayBannerUrl={displayBannerUrl}
										hasBanner={hasBanner}
										hasClearedBanner={hasClearedBanner}
										data-flx="user.applications-tab.application-detail.bot-profile-section"
									/>
								</>
							)}
						</div>
						<div className={styles.columnStack} data-flx="user.applications-tab.application-detail.column-stack--2">
							<div
								className={styles.sectionSpacer}
								aria-hidden="true"
								data-flx="user.applications-tab.application-detail.section-spacer--3"
							/>
							<div data-flx="user.applications-tab.application-detail.div">
								<OAuthBuilderSection
									form={form}
									availableScopes={AVAILABLE_SCOPES}
									builderScopeList={builderScopeList}
									botPermissionsList={botPermissionsList}
									builderUrl={builderUrl}
									redirectOptions={redirectOptions}
									onCopyBuilderUrl={handleCopyBuilderUrl}
									data-flx="user.applications-tab.application-detail.o-auth-builder-section"
								/>
							</div>
						</div>
					</div>
					<div
						className={styles.sectionSpacer}
						aria-hidden="true"
						data-flx="user.applications-tab.application-detail.section-spacer--4"
					/>
					<SectionCard
						tone="danger"
						title={<Trans>Danger zone</Trans>}
						subtitle={<Trans>This cannot be undone. Removing the application also deletes its bot.</Trans>}
						data-flx="user.applications-tab.application-detail.section-card"
					>
						<div className={styles.dangerContent} data-flx="user.applications-tab.application-detail.danger-content">
							<p className={styles.helperText} data-flx="user.applications-tab.application-detail.helper-text">
								<Trans>Once deleted, the application and its credentials are permanently removed.</Trans>
							</p>
							<div className={styles.dangerActions} data-flx="user.applications-tab.application-detail.danger-actions">
								<Button
									variant="danger"
									onClick={handleDelete}
									submitting={isDeleting}
									leftIcon={
										<TrashIcon
											size={remFromPx(16)}
											weight="fill"
											data-flx="user.applications-tab.application-detail.trash-icon"
										/>
									}
									fitContent
									data-flx="user.applications-tab.application-detail.button.delete"
								>
									<Trans>Delete application</Trans>
								</Button>
							</div>
						</div>
					</SectionCard>
				</div>
			</Form>
		);
	},
);
