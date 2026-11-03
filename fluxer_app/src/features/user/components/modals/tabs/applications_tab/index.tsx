// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {FLUXER_DOCS_DOMAIN, FLUXER_DOCS_URL} from '@app/features/app/config/I18nDisplayConstants';
import type {DeveloperApplication} from '@app/features/devtools/models/DeveloperApplication';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {ApplicationCreateModal} from '@app/features/user/components/modals/tabs/applications_tab/ApplicationCreateModal';
import {ApplicationDetail} from '@app/features/user/components/modals/tabs/applications_tab/ApplicationDetail';
import {ApplicationsList} from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsList';
import styles from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTab.module.css';
import ApplicationsTabState from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTabState';
import {useSettingsContentKey} from '@app/features/user/hooks/useSettingsContentKey';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import Users from '@app/features/user/state/Users';
import {accountGateReason} from '@app/features/user/utils/AccountGatePresentation';
import {canPerform} from '@app/features/user/utils/AccountGates';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BookOpenIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect} from 'react';

const READ_DOCUMENTATION_DESCRIPTOR = msg({
	message: 'Read the documentation ({domain})',
	comment: 'Developer applications link label. domain is the documentation domain.',
});
const ApplicationsTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {checkUnsavedChanges} = useUnsavedChangesFlash('applications');
	const {setContentKey} = useSettingsContentKey();
	const store = ApplicationsTabState;
	const createGate = canPerform(Users.currentUser, TierGatedActions.APPLICATION_CREATE);
	useLayoutEffect(() => {
		setContentKey(store.contentKey);
	}, [store.contentKey, setContentKey]);
	useEffect(() => {
		void store.fetchApplications({showLoading: store.applications.length === 0});
	}, [store]);
	const handleSelectApplication = useCallback(
		(appId: string) => {
			if (checkUnsavedChanges()) return;
			void store.navigateToDetail(appId);
		},
		[store, checkUnsavedChanges],
	);
	const openCreateModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ApplicationCreateModal
					onCreated={async (app: DeveloperApplication) => {
						await store.navigateToDetail(app.id, app);
						void store.fetchApplications({showLoading: false});
					}}
					data-flx="user.applications-tab.open-create-modal.application-create-modal"
				/>
			)),
		);
	}, [store]);
	const handleBackToList = useCallback(() => {
		if (checkUnsavedChanges()) return;
		void store.navigateToList();
	}, [store, checkUnsavedChanges]);
	if (store.navigationState === 'LOADING_LIST' || (store.isLoading && store.isListView)) {
		return (
			<SettingsTabContainer data-flx="user.applications-tab.settings-tab-container">
				<SettingsTabContent data-flx="user.applications-tab.settings-tab-content">
					<div className={styles.spinnerContainer} data-flx="user.applications-tab.spinner-container">
						<Spinner data-flx="user.applications-tab.spinner" />
					</div>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	}
	if (store.navigationState === 'ERROR' && store.isListView) {
		return (
			<SettingsTabContainer data-flx="user.applications-tab.settings-tab-container--2">
				<SettingsTabContent data-flx="user.applications-tab.settings-tab-content--2">
					<SettingsSection
						id="applications-list"
						title={<Trans>Applications</Trans>}
						data-flx="user.applications-tab.settings-tab-section"
					>
						<StatusSlate
							Icon={WarningCircleIcon}
							title={<Trans>Unable to load applications</Trans>}
							description={<Trans>Check your connection and try again.</Trans>}
							actions={[
								{
									text: i18n._(TRY_AGAIN_DESCRIPTOR),
									onClick: () => store.fetchApplications({showLoading: true}),
								},
							]}
							data-flx="user.applications-tab.status-slate"
						/>
					</SettingsSection>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	}
	if (store.isDetailView && store.selectedAppId) {
		return (
			<SettingsTabContainer data-flx="user.applications-tab.settings-tab-container--3">
				<SettingsTabContent data-flx="user.applications-tab.settings-tab-content--3">
					<ApplicationDetail
						applicationId={store.selectedAppId}
						onBack={handleBackToList}
						initialApplication={store.selectedApplication}
						data-flx="user.applications-tab.application-detail"
					/>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	}
	return (
		<SettingsTabContainer data-flx="user.applications-tab.settings-tab-container--4">
			<SettingsTabContent data-flx="user.applications-tab.settings-tab-content--4">
				<SettingsSection
					id="applications-list"
					title={<Trans>Applications</Trans>}
					data-flx="user.applications-tab.settings-tab-section--2"
				>
					<div className={styles.buttonContainer} data-flx="user.applications-tab.button-container">
						{!createGate.allowed ? (
							<Tooltip text={accountGateReason(i18n, createGate)} data-flx="user.applications-tab.tooltip">
								<div data-flx="user.applications-tab.div">
									<Button
										variant="primary"
										fitContent
										onClick={openCreateModal}
										disabled
										data-flx="user.applications-tab.button.open-create-modal"
									>
										<Trans>Create application</Trans>
									</Button>
								</div>
							</Tooltip>
						) : (
							<Button
								variant="primary"
								fitContent
								onClick={openCreateModal}
								data-flx="user.applications-tab.button.open-create-modal--2"
							>
								<Trans>Create application</Trans>
							</Button>
						)}
						<a
							className={styles.documentationLink}
							href={FLUXER_DOCS_URL}
							target="_blank"
							rel="noreferrer"
							data-flx="user.applications-tab.documentation-link"
						>
							<BookOpenIcon
								weight="fill"
								size={18}
								className={styles.documentationIcon}
								data-flx="user.applications-tab.documentation-icon"
							/>
							{i18n._(READ_DOCUMENTATION_DESCRIPTOR, {domain: FLUXER_DOCS_DOMAIN})}
						</a>
					</div>
					<ApplicationsList
						applications={store.applications}
						onSelectApplication={handleSelectApplication}
						data-flx="user.applications-tab.applications-list"
					/>
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default ApplicationsTab;
