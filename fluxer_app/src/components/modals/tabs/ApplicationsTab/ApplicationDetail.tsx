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
import {TrashIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm, useWatch} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {OAuth2Scopes, UserPremiumTypes} from '~/Constants';
import {Form} from '~/components/form/Form';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import ApplicationsTabStore from '~/components/modals/tabs/ApplicationsTab/ApplicationsTabStore';
import {Button} from '~/components/uikit/Button/Button';
import {Endpoints} from '~/Endpoints';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import {useSudo} from '~/hooks/useSudo';
import HttpClient from '~/lib/HttpClient';
import type {DeveloperApplication} from '~/records/DeveloperApplicationRecord';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {formatBotPermissionsQuery, getAllBotPermissions} from '~/utils/PermissionUtils';
import styles from './application-detail/ApplicationDetail.module.css';
import {ApplicationHeader} from './application-detail/ApplicationHeader';
import {ApplicationInfoSection} from './application-detail/ApplicationInfoSection';
import {BotProfileSection} from './application-detail/BotProfileSection';
import {OAuthBuilderSection} from './application-detail/OAuthBuilderSection';
import {SecretsSection} from './application-detail/SecretsSection';
import {SectionCard} from './application-detail/SectionCard';
import type {ApplicationDetailFormValues} from './application-detail/types';

interface ApplicationDetailProps {
	applicationId: string;
	onBack: () => void;
	initialApplication?: DeveloperApplication | null;
}

const APPLICATIONS_TAB_ID = 'applications';
const AVAILABLE_SCOPES = OAuth2Scopes.filter((scope) => scope !== 'applications.commands');

export const ApplicationDetail: React.FC<ApplicationDetailProps> = observer(
	({applicationId, onBack, initialApplication}) => {
		const {t, i18n} = useLingui();
		const store = ApplicationsTabStore;

		const application = store.selectedApplication;
		const loading = store.isLoading && store.isDetailView;
		const error = store.error;
		const [idCopied, setIdCopied] = React.useState(false);
		const [previewAvatarUrl, setPreviewAvatarUrl] = React.useState<string | null>(null);
		const [hasClearedAvatar, setHasClearedAvatar] = React.useState(false);
		const [previewBannerUrl, setPreviewBannerUrl] = React.useState<string | null>(null);
		const [hasClearedBanner, setHasClearedBanner] = React.useState(false);
		const [isDeleting, setIsDeleting] = React.useState(false);
		const [initialValues, setInitialValues] = React.useState<ApplicationDetailFormValues | null>(null);
		const [clientSecret, setClientSecret] = React.useState<string | null>(null);
		const [botToken, setBotToken] = React.useState<string | null>(null);
		const [isRotating, setIsRotating] = React.useState<'client' | 'bot' | null>(null);
		const clientSecretInputId = React.useId();
		const botTokenInputId = React.useId();

		const currentUser = UserStore.currentUser;
		const isLifetimePremium = currentUser?.premiumType === UserPremiumTypes.LIFETIME;
		const sudo = useSudo();

		const form = useForm<ApplicationDetailFormValues>({
			defaultValues: {
				name: '',
				botPublic: true,
				redirectUris: [],
				redirectUriInputs: [''],
				builderScopes: {} as Record<string, boolean>,
				builderPermissions: {} as Record<string, boolean>,
				username: '',
				discriminator: '',
				avatar: null,
				bio: '',
				banner: null,
			},
		});

		const buildFormDefaults = React.useCallback((app: DeveloperApplication): ApplicationDetailFormValues => {
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
				builderScopes: builderScopeMap,
				builderPermissions: {},
				username: app.bot?.username || '',
				discriminator: app.bot?.discriminator || '',
				avatar: null,
				bio: app.bot?.bio ?? '',
				banner: null,
			};
		}, []);

		React.useEffect(() => {
			if (application && application.id === applicationId) {
				const fetchedClientSecret = application.client_secret ?? null;
				const fetchedBotToken = application.bot?.token ?? null;
				setClientSecret(fetchedClientSecret);
				setBotToken(fetchedBotToken);

				const defaults = buildFormDefaults(application);
				form.reset(defaults);
				setInitialValues(defaults);

				setPreviewAvatarUrl(null);
				setHasClearedAvatar(false);
				setPreviewBannerUrl(null);
				setHasClearedBanner(false);
			}
		}, [application, applicationId, buildFormDefaults, form]);

		React.useEffect(() => {
			if (initialApplication && initialApplication.id === applicationId) {
				void store.navigateToDetail(applicationId, initialApplication);
			} else if (!store.selectedApplication || store.selectedAppId !== applicationId) {
				void store.navigateToDetail(applicationId);
			}
		}, [applicationId, initialApplication, store]);

		const formIsSubmitting = form.formState.isSubmitting;
		const watchedValues = useWatch<ApplicationDetailFormValues>({control: form.control});

		const hasFormChanges = React.useMemo(() => {
			if (!initialValues) return false;
			const currentValues =
				(watchedValues as ApplicationDetailFormValues | undefined) ?? ({} as ApplicationDetailFormValues);

			return (
				(currentValues.name ?? '') !== (initialValues.name ?? '') ||
				(currentValues.redirectUris ?? []).join(',') !== (initialValues.redirectUris ?? []).join(',') ||
				(currentValues.redirectUriInputs ?? []).join(',') !== (initialValues.redirectUriInputs ?? []).join(',') ||
				(currentValues.botPublic ?? true) !== (initialValues.botPublic ?? true) ||
				(currentValues.username ?? '') !== (initialValues.username ?? '') ||
				(currentValues.discriminator ?? '') !== (initialValues.discriminator ?? '') ||
				(currentValues.bio ?? '') !== (initialValues.bio ?? '') ||
				(currentValues.banner ?? '') !== (initialValues.banner ?? '')
			);
		}, [initialValues, watchedValues]);

		const hasUnsavedChanges = React.useMemo(() => {
			return Boolean(hasFormChanges || previewAvatarUrl || hasClearedAvatar || previewBannerUrl || hasClearedBanner);
		}, [hasFormChanges, previewAvatarUrl, hasClearedAvatar, previewBannerUrl, hasClearedBanner]);

		const onSubmit = React.useCallback(
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

					const currentDiscriminator = currentBot.discriminator || '';
					if (
						isLifetimePremium &&
						dirtyFields.discriminator &&
						data.discriminator &&
						data.discriminator !== currentDiscriminator
					) {
						botBody.discriminator = data.discriminator;
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

					return Object.keys(botBody).length > 0 ? botBody : null;
				};

				const appPatch = buildApplicationPatch();
				const botPatch = buildBotPatch();

				if (Object.keys(appPatch).length === 0 && !botPatch) {
					ToastActionCreators.createToast({type: 'info', children: t`No changes to save`});
					return;
				}

				try {
					if (Object.keys(appPatch).length > 0) {
						await HttpClient.patch(Endpoints.OAUTH_APPLICATION(applicationId), appPatch);
					}

					if (botPatch) {
						await HttpClient.patch(Endpoints.OAUTH_APPLICATION_BOT_PROFILE(applicationId), botPatch);
					}

					ToastActionCreators.createToast({type: 'success', children: t`Application updated successfully`});
					setPreviewAvatarUrl(null);
					setHasClearedAvatar(false);
					await store.fetchApplication(applicationId);
				} catch (err) {
					console.error('[ApplicationDetail] Failed to update application:', err);
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
				isLifetimePremium,
			],
		);

		const {handleSubmit: handleSave} = useFormSubmit({
			form,
			onSubmit,
			defaultErrorField: 'name',
		});

		const handleReset = React.useCallback(() => {
			if (!application) return;

			const defaults = buildFormDefaults(application);
			form.reset(defaults, {keepDirty: false});
			setInitialValues(defaults);
			setPreviewAvatarUrl(null);
			setHasClearedAvatar(false);
			setPreviewBannerUrl(null);
			setHasClearedBanner(false);
		}, [application, buildFormDefaults, form]);

		React.useEffect(() => {
			UnsavedChangesActionCreators.setUnsavedChanges(APPLICATIONS_TAB_ID, hasUnsavedChanges);
		}, [hasUnsavedChanges]);

		React.useEffect(() => {
			UnsavedChangesActionCreators.setTabData(APPLICATIONS_TAB_ID, {
				onReset: handleReset,
				onSave: handleSave,
				isSubmitting: formIsSubmitting,
			});
		}, [handleReset, handleSave, formIsSubmitting]);

		React.useEffect(() => {
			return () => {
				UnsavedChangesActionCreators.clearUnsavedChanges(APPLICATIONS_TAB_ID);
			};
		}, []);

		const handleAvatarChange = React.useCallback(
			(base64: string) => {
				form.setValue('avatar', base64, {shouldDirty: true});
				setPreviewAvatarUrl(base64);
				setHasClearedAvatar(false);
				form.clearErrors('avatar');
			},
			[form],
		);

		const handleBannerChange = React.useCallback(
			(base64: string) => {
				form.setValue('banner', base64, {shouldDirty: true});
				setPreviewBannerUrl(base64);
				setHasClearedBanner(false);
				form.clearErrors('banner');
			},
			[form],
		);

		const handleBannerClear = React.useCallback(() => {
			form.setValue('banner', null, {shouldDirty: true});
			setPreviewBannerUrl(null);
			setHasClearedBanner(true);
		}, [form]);

		const handleAvatarClear = React.useCallback(() => {
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
				console.error('[ApplicationDetail] Failed to copy ID:', err);
			}
		};

		const handleDelete = () => {
			if (!application) return;
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Delete Application`}
						description={
							<div>
								<Trans>
									Are you sure you want to delete <strong>{application.name}</strong>?
								</Trans>
								<br />
								<br />
								<Trans>
									This action cannot be undone. All associated data, including the bot user, will be permanently
									deleted.
								</Trans>
							</div>
						}
						primaryText={t`Delete Application`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								setIsDeleting(true);
								await HttpClient.delete({url: Endpoints.OAUTH_APPLICATION(application.id), body: {}});
								onBack();
							} catch (err) {
								console.error('[ApplicationDetail] Failed to delete application:', err);
								ToastActionCreators.createToast({
									type: 'error',
									children: t`Failed to delete application. Please try again.`,
								});
								setIsDeleting(false);
							}
						}}
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
				const res = await HttpClient.post<{client_secret?: string; token?: string}>(endpoint, sudoPayload);
				if (type === 'client') {
					setClientSecret(res.body.client_secret ?? null);
				} else {
					setBotToken(res.body.token ?? null);
				}
				ToastActionCreators.createToast({
					type: 'success',
					children:
						type === 'client'
							? t`Client secret regenerated. Update any code that uses the old secret.`
							: t`Bot token regenerated. Update any code that uses the old token.`,
				});
			} catch (err) {
				console.error('Failed to rotate secret', err);
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to rotate. Please try again.`,
				});
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
		};

		const updateRedirectInput = (index: number, value: string) => {
			const current = form.getValues('redirectUriInputs') ?? [];
			const next = [...current];
			next[index] = value;
			form.setValue('redirectUriInputs', next, {shouldDirty: true});
		};

		const builderScopes = useWatch({control: form.control, name: 'builderScopes'}) || {};
		const builderPermissions = useWatch({control: form.control, name: 'builderPermissions'}) || {};
		const builderRedirectUri = useWatch({control: form.control, name: 'builderRedirectUri'});
		const redirectInputs = useWatch({control: form.control, name: 'redirectUriInputs'}) ?? [];
		const bannerValue = useWatch({control: form.control, name: 'banner'});

		const builderScopeList = React.useMemo(
			() =>
				Object.entries(builderScopes)
					.filter(([, enabled]) => enabled)
					.map(([scope]) => scope),
			[builderScopes],
		);

		const botPermissionsList = React.useMemo(() => getAllBotPermissions(i18n), [i18n]);

		const builderUrl = React.useMemo(() => {
			if (!application) return '';
			const params = new URLSearchParams();
			params.set('client_id', application.id);

			if (builderScopeList.length > 0) {
				params.set('scope', builderScopeList.join(' '));
			}

			const isBot = builderScopeList.includes('bot');
			const botPerms = Object.entries(builderPermissions)
				.filter(([, enabled]) => enabled)
				.map(([perm]) => perm);
			if (isBot && botPerms.length > 0) {
				params.set('permissions', formatBotPermissionsQuery(botPerms));
			}

			const redirect = builderRedirectUri?.trim();
			if (redirect) {
				params.set('redirect_uri', redirect);
				params.set('response_type', 'code');
			} else {
				if (!isBot) {
					params.set('response_type', 'code');
				}
			}

			if (builderScopeList.length === 0) {
				return '';
			}

			return `${window.location.origin}${Endpoints.OAUTH_AUTHORIZE}?${params.toString()}`;
		}, [application, builderScopeList, builderPermissions, builderRedirectUri]);

		const redirectOptions = React.useMemo(() => {
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
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={isClient ? t`Regenerate client secret?` : t`Regenerate bot token?`}
						description={description}
						primaryText={t`Regenerate`}
						primaryVariant="danger-primary"
						onPrimary={() => rotateSecret(type)}
					/>
				)),
			);
		};

		const handleCopyBuilderUrl = React.useCallback(async () => {
			if (!builderUrl) return;
			await navigator.clipboard.writeText(builderUrl);
			ToastActionCreators.createToast({
				type: 'success',
				children: t`Copied URL to clipboard`,
			});
		}, [builderUrl]);

		if (loading) {
			return <div className={styles.loadingState} />;
		}

		if (error || !application) {
			return (
				<div className={styles.page}>
					<StatusSlate
						Icon={WarningCircleIcon}
						title={<Trans>We couldn't load this application</Trans>}
						description={<Trans>Please retry or go back to the applications list.</Trans>}
						fullHeight={true}
						actions={[
							{
								text: <Trans>Retry</Trans>,
								onClick: () => store.fetchApplication(applicationId),
							},
							{
								text: <Trans>Back to list</Trans>,
								onClick: onBack,
								variant: 'secondary',
							},
						]}
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
			<Form form={form} onSubmit={onSubmit}>
				<div className={styles.page}>
					<ApplicationHeader
						name={application.name}
						applicationId={application.id}
						onBack={onBack}
						onCopyId={handleCopyId}
						idCopied={idCopied}
					/>

					<div className={styles.detailGrid}>
						<div className={styles.columnStack}>
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
							/>
							<div className={styles.sectionSpacer} aria-hidden="true" />

							<ApplicationInfoSection
								form={form}
								redirectInputs={form.watch('redirectUriInputs') ?? []}
								onAddRedirect={addRedirectInput}
								onRemoveRedirect={removeRedirectInput}
								onUpdateRedirect={updateRedirectInput}
							/>

							{application.bot && (
								<>
									<div className={styles.sectionSpacer} aria-hidden="true" />
									<BotProfileSection
										application={application}
										form={form}
										displayAvatarUrl={displayAvatarUrl}
										hasAvatar={hasAvatar}
										hasClearedAvatar={hasClearedAvatar}
										isLifetimePremium={Boolean(isLifetimePremium)}
										onAvatarChange={handleAvatarChange}
										onAvatarClear={handleAvatarClear}
										onBannerChange={handleBannerChange}
										onBannerClear={handleBannerClear}
										displayBannerUrl={displayBannerUrl}
										hasBanner={hasBanner}
										hasClearedBanner={hasClearedBanner}
									/>
								</>
							)}
						</div>

						<div className={styles.columnStack}>
							<div className={styles.sectionSpacer} aria-hidden="true" />
							<div>
								<OAuthBuilderSection
									form={form}
									availableScopes={AVAILABLE_SCOPES}
									builderScopeList={builderScopeList}
									botPermissionsList={botPermissionsList}
									builderUrl={builderUrl}
									redirectOptions={redirectOptions}
									onCopyBuilderUrl={handleCopyBuilderUrl}
								/>
							</div>
						</div>
					</div>

					<div className={styles.sectionSpacer} aria-hidden="true" />

					<SectionCard
						tone="danger"
						title={<Trans>Danger zone</Trans>}
						subtitle={<Trans>This cannot be undone. Removing the application also deletes its bot.</Trans>}
					>
						<div className={styles.dangerContent}>
							<p className={styles.helperText}>
								<Trans>Once deleted, the application and its credentials are permanently removed.</Trans>
							</p>
							<div className={styles.dangerActions}>
								<Button
									variant="danger-primary"
									onClick={handleDelete}
									submitting={isDeleting}
									leftIcon={<TrashIcon size={16} weight="fill" />}
									fitContent
								>
									<Trans>Delete Application</Trans>
								</Button>
							</div>
						</div>
					</SectionCard>
				</div>
			</Form>
		);
	},
);
