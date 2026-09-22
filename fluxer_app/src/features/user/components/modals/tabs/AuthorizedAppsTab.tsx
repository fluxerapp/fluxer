// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {getOAuth2ScopeDescription} from '@app/features/app/constants/AppConstants';
import type {OAuth2Authorization} from '@app/features/auth/commands/OAuth2AuthorizationCommands';
import * as OAuth2AuthorizationCommands from '@app/features/auth/commands/OAuth2AuthorizationCommands';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/modals/tabs/AuthorizedAppsTab.module.css';
import {
	getSelectableItemProps,
	SelectionCheckbox,
	type SelectionToggleHandler,
	selectOnShiftActivation,
	selectOnShiftClick,
	useSelectableSettingsList,
} from '@app/features/user/components/modals/tabs/components/SelectableSettingsList';
import {getUserAvatarURL} from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as FormUtils from '@app/lib/forms';
import type {OAuth2Scope} from '@fluxer/constants/src/OAuth2Constants';
import {getFormattedShortDate} from '@fluxer/date_utils/src/DateFormatting';
import {msg, plural} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {AppWindowIcon, InfoIcon, NetworkSlashIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const AUTHORIZED_APPS_TAB_ID = 'authorized-applications';

const FAILED_TO_LOAD_AUTHORIZED_APPLICATIONS_DESCRIPTOR = msg({
	message: 'Failed to load authorized applications',
	comment: 'Error message in the authorized apps tab. Keep the tone plain and specific.',
});
const CLEAR_SELECTION_DESCRIPTOR = msg({
	message: 'Clear selection',
	comment: 'Button or menu action label in the authorized apps tab. Keep it concise.',
});
const VIEW_PERMISSIONS_DESCRIPTOR = msg({
	message: 'View permissions',
	comment: 'Tooltip and accessibility label for opening an authorized app permissions modal.',
});
const REVOKE_ACCESS_DESCRIPTOR = msg({
	message: 'Revoke access',
	comment: 'Short danger action label in the authorized apps tab. Keep it concise.',
});
const REVOKE_SELECTED_APPS_DESCRIPTOR = msg({
	message: 'Revoke {appCount, plural, one {# app} other {# apps}}',
	comment: 'Danger button in the authorized apps tab. Revokes the currently selected authorized applications.',
});
const REVOKE_ALL_APPS_DESCRIPTOR = msg({
	message: 'Revoke all apps',
	comment: 'Danger button in the authorized apps management modal. Revokes every authorized application.',
});
const SELECTED_APPS_FOR_REVOKE_DESCRIPTOR = msg({
	message:
		'{appCount, plural, one {# app selected for access revocation} other {# apps selected for access revocation}}',
	comment: 'Unsaved-changes banner text in the authorized apps tab. Counts selected apps that will be revoked.',
});
const APPLICATION_ACCESS_REVOKED_DESCRIPTOR = msg({
	message: 'Application access revoked',
	comment: 'Toast shown after OAuth application access was revoked.',
});
const formatDate = (dateString: string): string => {
	return getFormattedShortDate(dateString, getCurrentLocale());
};

function getAuthorizationSelectionId(authorization: OAuth2Authorization): string {
	return authorization.application.id;
}

const AuthorizedAppScopesModal = observer(({authorization}: {authorization: OAuth2Authorization}) => {
	const {i18n} = useLingui();
	return (
		<Modal.Root size="small" centered data-flx="user.authorized-apps-tab.scopes-modal.modal-root">
			<Modal.Header
				title={<Trans>Permissions for {authorization.application.name}</Trans>}
				data-flx="user.authorized-apps-tab.scopes-modal.header"
			/>
			<Modal.Content data-flx="user.authorized-apps-tab.scopes-modal.content">
				<Modal.ContentLayout data-flx="user.authorized-apps-tab.scopes-modal.content-layout">
					<div className={styles.scopeList} data-flx="user.authorized-apps-tab.scopes-modal.scope-list">
						{authorization.scopes.map((scope) => (
							<div key={scope} className={styles.scopeTag} data-flx="user.authorized-apps-tab.scopes-modal.scope-tag">
								<span className={styles.scopeName} data-flx="user.authorized-apps-tab.scopes-modal.scope-name">
									{scope}
								</span>
								<span
									className={styles.scopeDescription}
									data-flx="user.authorized-apps-tab.scopes-modal.scope-description"
								>
									{getOAuth2ScopeDescription(i18n, scope as OAuth2Scope) || scope}
								</span>
							</div>
						))}
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

const AuthorizedAppsRevokeModal = observer(
	({
		authorizations,
		onRevoked,
	}: {
		authorizations: Array<OAuth2Authorization>;
		onRevoked: (applicationIds: Array<string>) => void;
	}) => {
		const {i18n} = useLingui();
		const [isSubmitting, setIsSubmitting] = useState(false);
		const appCount = authorizations.length;
		const title = plural(
			{count: appCount},
			{
				one: 'Revoke # app',
				other: 'Revoke # apps',
			},
		);
		const handleConfirm = async () => {
			setIsSubmitting(true);
			const applicationIds = authorizations.map((authorization) => authorization.application.id);
			try {
				await OAuth2AuthorizationCommands.deauthorizeMany(applicationIds);
				onRevoked(applicationIds);
				ModalCommands.pop();
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(APPLICATION_ACCESS_REVOKED_DESCRIPTOR),
				});
			} catch (error) {
				FormUtils.pushApiErrorModal(i18n, error);
			} finally {
				setIsSubmitting(false);
			}
		};
		return (
			<Modal.Root size="small" centered data-flx="user.authorized-apps-tab.revoke-modal.modal-root">
				<Modal.Header title={title} data-flx="user.authorized-apps-tab.revoke-modal.header" />
				<Modal.Content data-flx="user.authorized-apps-tab.revoke-modal.content">
					<Modal.ContentLayout data-flx="user.authorized-apps-tab.revoke-modal.content-layout">
						<Modal.Description data-flx="user.authorized-apps-tab.revoke-modal.description">
							<Trans comment="Security warning explaining that selected OAuth application authorizations will be revoked.">
								This will revoke access for the selected{' '}
								<Plural
									value={appCount}
									one="application"
									other="applications"
									data-flx="user.authorized-apps-tab.revoke-modal.plural"
								/>
								. They will no longer be able to access your account.
							</Trans>
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="user.authorized-apps-tab.revoke-modal.footer">
					<Button
						onClick={ModalCommands.pop}
						variant="secondary"
						data-flx="user.authorized-apps-tab.revoke-modal.button.cancel"
					>
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={handleConfirm}
						submitting={isSubmitting}
						variant="danger"
						data-flx="user.authorized-apps-tab.revoke-modal.button.confirm"
					>
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);

const AuthorizedAppItem = observer(function AuthorizedAppItem({
	authorization,
	isSelected,
	index,
	selectionMode,
	onSelect,
	onOpenScopes,
	onRevoke,
}: {
	authorization: OAuth2Authorization;
	isSelected?: boolean;
	index?: number;
	selectionMode?: boolean;
	onSelect?: SelectionToggleHandler;
	onOpenScopes: (authorization: OAuth2Authorization) => void;
	onRevoke: (authorizations: Array<OAuth2Authorization>) => void;
}) {
	const {i18n} = useLingui();
	const iconUrl = authorization.application.icon
		? getUserAvatarURL({
				id: authorization.application.id,
				avatar: authorization.application.icon,
			})
		: null;
	const authorizedOn = formatDate(authorization.authorized_at);
	const applicationId = authorization.application.id;
	const isSelectionInteractive = Boolean(selectionMode && onSelect && index !== undefined);
	const handleRevokeClick = (event: React.MouseEvent<HTMLButtonElement>) => {
		if (selectOnShiftClick(event, applicationId, index, onSelect)) return;
		onRevoke([authorization]);
	};
	const handleRevokeKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
		selectOnShiftActivation(event, applicationId, index, onSelect);
	};
	const selectionItemProps = getSelectableItemProps({
		enabled: Boolean(selectionMode),
		id: applicationId,
		index,
		selected: isSelected,
		onSelect,
	});
	return (
		<div
			className={clsx(styles.appCard, isSelectionInteractive && styles.appCardSelectable)}
			data-flx="user.authorized-apps-tab.app-card"
			{...selectionItemProps}
		>
			<div className={styles.appMain} data-flx="user.authorized-apps-tab.app-main">
				<div className={styles.appAvatar} aria-hidden data-flx="user.authorized-apps-tab.app-avatar">
					{iconUrl ? (
						<img
							src={iconUrl}
							alt={authorization.application.name}
							className={styles.appAvatarImage}
							data-flx="user.authorized-apps-tab.app-avatar-image"
						/>
					) : (
						<AppWindowIcon
							className={styles.appAvatarPlaceholder}
							data-flx="user.authorized-apps-tab.app-avatar-placeholder"
						/>
					)}
				</div>
				<div className={styles.textBlock} data-flx="user.authorized-apps-tab.text-block">
					<span className={styles.appName} data-flx="user.authorized-apps-tab.app-name">
						{authorization.application.name}
					</span>
					<span className={styles.metaText} data-flx="user.authorized-apps-tab.meta-text">
						<Trans>Authorized on {authorizedOn}</Trans>
					</span>
				</div>
			</div>
			<div className={styles.appActions} data-flx="user.authorized-apps-tab.app-actions">
				{!selectionMode && (
					<>
						<Tooltip text={i18n._(VIEW_PERMISSIONS_DESCRIPTOR)} data-flx="user.authorized-apps-tab.tooltip.scopes">
							<Button
								type="button"
								square
								compact
								variant="secondary"
								aria-label={i18n._(VIEW_PERMISSIONS_DESCRIPTOR)}
								onClick={() => onOpenScopes(authorization)}
								className={styles.iconButton}
								icon={
									<InfoIcon className={styles.actionIcon} weight="bold" data-flx="user.authorized-apps-tab.info-icon" />
								}
								data-flx="user.authorized-apps-tab.button.scopes"
							/>
						</Tooltip>
						<Tooltip text={i18n._(REVOKE_ACCESS_DESCRIPTOR)} data-flx="user.authorized-apps-tab.tooltip.revoke">
							<Button
								type="button"
								square
								compact
								variant="secondary"
								aria-label={i18n._(REVOKE_ACCESS_DESCRIPTOR)}
								aria-keyshortcuts="Shift+Enter Shift+Space"
								onClick={handleRevokeClick}
								onKeyDown={handleRevokeKeyDown}
								className={styles.revokeButton}
								icon={
									<XIcon className={styles.actionIcon} weight="bold" data-flx="user.authorized-apps-tab.revoke-icon" />
								}
								data-flx="user.authorized-apps-tab.button.revoke"
							/>
						</Tooltip>
					</>
				)}
				{selectionMode && (
					<SelectionCheckbox checked={!!isSelected} data-flx="user.authorized-apps-tab.custom-checkbox" />
				)}
			</div>
		</div>
	);
});

interface AuthorizedAppsContentProps {
	presentation?: 'settings' | 'modal';
	initialAuthorizations?: Array<OAuth2Authorization>;
}

export const AuthorizedAppsContent = observer(function AuthorizedAppsContent({
	initialAuthorizations,
	presentation = 'settings',
}: AuthorizedAppsContentProps) {
	const {i18n} = useLingui();
	const isModalPresentation = presentation === 'modal';
	const [authorizations, setAuthorizations] = useState<Array<OAuth2Authorization>>(() => initialAuthorizations ?? []);
	const [loading, setLoading] = useState(initialAuthorizations == null);
	const [error, setError] = useState<string | null>(null);
	const {
		selectedIds: selectedApplicationIds,
		selectedItems: selectedAuthorizations,
		selectionMode,
		selectAllShortcutLabel,
		clearSelection,
		toggleSelection: toggleApplication,
	} = useSelectableSettingsList({
		items: authorizations,
		getId: getAuthorizationSelectionId,
	});
	const loadAuthorizations = useCallback(async () => {
		setLoading(true);
		setError(null);
		try {
			const data = await OAuth2AuthorizationCommands.listAuthorizations();
			setAuthorizations(data);
		} catch (_err) {
			setError(i18n._(FAILED_TO_LOAD_AUTHORIZED_APPLICATIONS_DESCRIPTOR));
		} finally {
			setLoading(false);
		}
	}, [i18n]);
	useEffect(() => {
		if (initialAuthorizations != null) return;
		loadAuthorizations();
	}, [initialAuthorizations, loadAuthorizations]);
	useEffect(() => {
		if (initialAuthorizations == null) return;
		setAuthorizations(initialAuthorizations);
		setLoading(false);
		setError(null);
	}, [initialAuthorizations]);
	const handleRevoked = useCallback((applicationIdsToRemove: Array<string>) => {
		const removedIds = new Set(applicationIdsToRemove);
		setAuthorizations((prev) => prev.filter((authorization) => !removedIds.has(authorization.application.id)));
	}, []);
	const openScopesModal = useCallback((authorization: OAuth2Authorization) => {
		ModalCommands.push(
			modal(() => (
				<AuthorizedAppScopesModal authorization={authorization} data-flx="user.authorized-apps-tab.scopes-modal.push" />
			)),
		);
	}, []);
	const openRevokeModal = useCallback(
		(authorizationsToRevoke: Array<OAuth2Authorization>) => {
			if (authorizationsToRevoke.length === 0) return;
			ModalCommands.push(
				modal(() => (
					<AuthorizedAppsRevokeModal
						authorizations={authorizationsToRevoke}
						onRevoked={handleRevoked}
						data-flx="user.authorized-apps-tab.revoke-modal.push"
					/>
				)),
			);
		},
		[handleRevoked],
	);
	const openSelectedAppsRevokeModal = useCallback(() => {
		openRevokeModal(selectedAuthorizations);
	}, [openRevokeModal, selectedAuthorizations]);
	const openAllAppsRevokeModal = useCallback(() => {
		openRevokeModal(authorizations);
	}, [authorizations, openRevokeModal]);
	useEffect(() => {
		if (isModalPresentation) return;
		if (selectedAuthorizations.length === 0) {
			UnsavedChangesCommands.clearUnsavedChanges(AUTHORIZED_APPS_TAB_ID);
			return;
		}
		UnsavedChangesCommands.setUnsavedChanges(AUTHORIZED_APPS_TAB_ID, true);
		UnsavedChangesCommands.setTabData(AUTHORIZED_APPS_TAB_ID, {
			onReset: clearSelection,
			onSave: openSelectedAppsRevokeModal,
			bannerText: i18n._(SELECTED_APPS_FOR_REVOKE_DESCRIPTOR, {appCount: selectedAuthorizations.length}),
			resetLabel: i18n._(CLEAR_SELECTION_DESCRIPTOR),
			saveLabel: i18n._(REVOKE_SELECTED_APPS_DESCRIPTOR, {appCount: selectedAuthorizations.length}),
			saveVariant: 'danger',
		});
		return () => UnsavedChangesCommands.clearUnsavedChanges(AUTHORIZED_APPS_TAB_ID);
	}, [clearSelection, i18n, isModalPresentation, openSelectedAppsRevokeModal, selectedAuthorizations.length]);
	if (loading) {
		const loadingState = (
			<div className={styles.loadingContainer} data-flx="user.authorized-apps-tab.loading-container">
				<Spinner size="large" data-flx="user.authorized-apps-tab.spinner" />
			</div>
		);
		if (!isModalPresentation) return loadingState;
		return <Modal.Content data-flx="user.authorized-apps-tab.modal-content.loading">{loadingState}</Modal.Content>;
	}
	const authorizedAppsContent = error ? (
		<StatusSlate
			Icon={NetworkSlashIcon}
			title={i18n._(FAILED_TO_LOAD_AUTHORIZED_APPLICATIONS_DESCRIPTOR)}
			description={error}
			actions={[
				{
					text: i18n._(TRY_AGAIN_DESCRIPTOR),
					onClick: loadAuthorizations,
					variant: 'primary',
				},
			]}
			data-flx="user.authorized-apps-tab.status-slate"
		/>
	) : authorizations.length === 0 ? (
		<StatusSlate
			Icon={AppWindowIcon}
			title={<Trans>No authorized applications</Trans>}
			description={<Trans>You haven't authorized any applications to access your account.</Trans>}
			data-flx="user.authorized-apps-tab.status-slate.empty"
		/>
	) : (
		<>
			<p className={styles.selectionHint} data-flx="user.authorized-apps-tab.selection-hint">
				<Trans>Hold Shift and press X to select apps. Press {selectAllShortcutLabel} to select all.</Trans>
			</p>
			<div className={styles.appList} data-flx="user.authorized-apps-tab.app-list">
				{authorizations.map((authorization, index) => (
					<AuthorizedAppItem
						key={authorization.application.id}
						authorization={authorization}
						isSelected={selectedApplicationIds.has(authorization.application.id)}
						index={index}
						selectionMode={selectionMode}
						onSelect={toggleApplication}
						onOpenScopes={openScopesModal}
						onRevoke={openRevokeModal}
						data-flx="user.authorized-apps-tab.app-item"
					/>
				))}
			</div>
		</>
	);
	if (isModalPresentation) {
		return (
			<>
				<Modal.Content data-flx="user.authorized-apps-tab.modal-content">
					<SettingsTabContainer data-flx="user.authorized-apps-tab.modal-settings-tab-container">
						<SettingsTabContent data-flx="user.authorized-apps-tab.modal-settings-tab-content">
							{authorizedAppsContent}
						</SettingsTabContent>
					</SettingsTabContainer>
				</Modal.Content>
				{(selectionMode || authorizations.length > 0) && (
					<Modal.Footer data-flx="user.authorized-apps-tab.modal-footer">
						{selectionMode ? (
							<>
								<Button
									variant="secondary"
									onClick={clearSelection}
									data-flx="user.authorized-apps-tab.button.clear-selection"
								>
									{i18n._(CLEAR_SELECTION_DESCRIPTOR)}
								</Button>
								<Button
									variant="danger"
									onClick={openSelectedAppsRevokeModal}
									data-flx="user.authorized-apps-tab.button.revoke-selected"
								>
									{i18n._(REVOKE_SELECTED_APPS_DESCRIPTOR, {appCount: selectedAuthorizations.length})}
								</Button>
							</>
						) : (
							<Button
								variant="danger"
								onClick={openAllAppsRevokeModal}
								data-flx="user.authorized-apps-tab.button.revoke-all"
							>
								{i18n._(REVOKE_ALL_APPS_DESCRIPTOR)}
							</Button>
						)}
					</Modal.Footer>
				)}
			</>
		);
	}
	return authorizedAppsContent;
});

const AuthorizedAppsTab = observer(function AuthorizedAppsTab() {
	return (
		<SettingsTabContainer data-flx="user.authorized-apps-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.authorized-apps-tab.settings-tab-content">
				<SettingsSection
					id={AUTHORIZED_APPS_TAB_ID}
					title={<Trans>Authorized apps</Trans>}
					data-flx="user.authorized-apps-tab.settings-section"
				>
					<AuthorizedAppsContent data-flx="user.authorized-apps-tab.authorized-apps-content" />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default AuthorizedAppsTab;
