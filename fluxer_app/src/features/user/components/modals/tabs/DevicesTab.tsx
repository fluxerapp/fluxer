// SPDX-License-Identifier: AGPL-3.0-or-later

import {Slate} from '@app/features/app/components/dialogs/components/Slate';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {useElementOverflow} from '@app/features/app/hooks/useTextOverflow';
import * as AuthSessionCommands from '@app/features/auth/commands/AuthSessionCommands';
import {DeviceRevokeModal} from '@app/features/auth/components/modals/DeviceRevokeModal';
import type {AuthSession} from '@app/features/auth/models/AuthSession';
import AuthSessionState from '@app/features/auth/state/AuthSession';
import {TRY_AGAIN_DESCRIPTOR, UNKNOWN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatTimestamp} from '@app/features/messaging/utils/markdown/DateFormatter';
import {TimestampStyle} from '@app/features/messaging/utils/markdown/parser/Enums';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	getSelectableItemProps,
	SelectionCheckbox,
	type SelectionToggleHandler,
	selectOnShiftActivation,
	selectOnShiftClick,
	useSelectableSettingsList,
} from '@app/features/user/components/modals/tabs/components/SelectableSettingsList';
import styles from '@app/features/user/components/modals/tabs/DevicesTab.module.css';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DeviceMobileIcon, InfoIcon, MonitorIcon, WifiSlashIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const DEVICES_TAB_ID = 'devices';

const REVOKE_DEVICE_DESCRIPTOR = msg({
	message: 'Revoke device',
	comment: 'Short label in the devices tab. Keep it concise.',
});
const VIEW_DEVICE_DETAILS_DESCRIPTOR = msg({
	message: 'View details',
	comment: 'Tooltip and accessibility label for opening a linked device details modal.',
});
const NETWORK_ERROR_DESCRIPTOR = msg({
	message: 'Network error',
	comment: 'Error message in the devices tab.',
});
const WE_RE_HAVING_TROUBLE_CONNECTING_TO_THE_SPACE_DESCRIPTOR = msg({
	message: "We're having trouble connecting to the space-time continuum. Check your connection and try again.",
	comment: 'Error message in the devices tab.',
});
const CLEAR_SELECTION_DESCRIPTOR = msg({
	message: 'Clear selection',
	comment: 'Button or menu action label in the devices tab. Keep it concise.',
});
const UNKNOWN_DEVICE_DESCRIPTOR = msg({
	message: 'Unknown device',
	comment: 'Fallback device or operating-system label in the devices tab when session client data is unavailable.',
});
const SIGN_OUT_SELECTED_DEVICES_DESCRIPTOR = msg({
	message: 'Sign out {deviceCount, plural, one {# device} other {# devices}}',
	comment: 'Danger button in the devices tab. Signs out the currently selected devices.',
});
const SELECTED_DEVICES_FOR_LOGOUT_DESCRIPTOR = msg({
	message: '{deviceCount, plural, one {# device selected for sign out} other {# devices selected for sign out}}',
	comment: 'Unsaved-changes banner text in the devices tab. Counts selected devices that will be signed out.',
});

const StatusDot = observer(() => (
	<div aria-hidden={true} className={styles.statusDot} data-flx="user.devices-tab.status-dot.status-dot" />
));

function getAuthSessionSelectionId(authSession: AuthSession): string {
	return authSession.id;
}

function formatAuthSessionLastUsed(authSession: AuthSession, i18n: I18n): string {
	if (!authSession.approxLastUsedAt) return i18n._(UNKNOWN_DESCRIPTOR);
	return formatTimestamp(Math.floor(authSession.approxLastUsedAt.getTime() / 1000), TimestampStyle.RelativeTime, i18n);
}

const DeviceMetadataLine = ({value, dataFlx}: {value: string; dataFlx: string}) => {
	const [element, setElement] = useState<HTMLSpanElement | null>(null);
	const isOverflowing = useElementOverflow(element);
	const content = (
		<span ref={setElement} className={styles.metadataLine} data-flx={dataFlx}>
			{value}
		</span>
	);
	return (
		<Tooltip text={isOverflowing ? value : ''} maxWidth="xl" data-flx={`${dataFlx}.tooltip`}>
			{content}
		</Tooltip>
	);
};

const DeviceDetailRow = ({
	label,
	children,
	dataFlx,
}: {
	label: React.ReactNode;
	children: React.ReactNode;
	dataFlx: string;
}) => (
	<div className={styles.deviceDetailRow} data-flx={dataFlx}>
		<div className={styles.deviceDetailLabel} data-flx={`${dataFlx}.label`}>
			{label}
		</div>
		<div className={styles.deviceDetailValue} data-flx={`${dataFlx}.value`}>
			{children}
		</div>
	</div>
);

const DeviceDetailsModal = observer(({authSession, isCurrent}: {authSession: AuthSession; isCurrent: boolean}) => {
	const {i18n} = useLingui();
	const clientOs = authSession.clientOs ?? i18n._(UNKNOWN_DEVICE_DESCRIPTOR);
	const platformLabel = authSession.clientPlatform ?? i18n._(UNKNOWN_DESCRIPTOR);
	return (
		<Modal.Root size="small" centered data-flx="user.devices-tab.device-details-modal.modal-root">
			<Modal.Header title={<Trans>Device details</Trans>} data-flx="user.devices-tab.device-details-modal.header" />
			<Modal.Content data-flx="user.devices-tab.device-details-modal.content">
				<Modal.ContentLayout data-flx="user.devices-tab.device-details-modal.content-layout">
					<div className={styles.deviceDetailList} data-flx="user.devices-tab.device-details-modal.detail-list">
						<DeviceDetailRow
							label={<Trans>Device</Trans>}
							dataFlx="user.devices-tab.device-details-modal.device"
							data-flx="user.devices-tab.device-details-modal.device-detail-row"
						>
							{clientOs}
						</DeviceDetailRow>
						<DeviceDetailRow
							label={<Trans>Client</Trans>}
							dataFlx="user.devices-tab.device-details-modal.client"
							data-flx="user.devices-tab.device-details-modal.device-detail-row--2"
						>
							{platformLabel}
						</DeviceDetailRow>
						{authSession.clientLocation && (
							<DeviceDetailRow
								label={<Trans>Location</Trans>}
								dataFlx="user.devices-tab.device-details-modal.location"
								data-flx="user.devices-tab.device-details-modal.device-detail-row--3"
							>
								{authSession.clientLocation}
							</DeviceDetailRow>
						)}
						{authSession.maskedIp && (
							<DeviceDetailRow
								label={<Trans>IP address</Trans>}
								dataFlx="user.devices-tab.device-details-modal.ip"
								data-flx="user.devices-tab.device-details-modal.device-detail-row--4"
							>
								<span className={styles.deviceDetailMono} data-flx="user.devices-tab.device-details-modal.ip-text">
									{authSession.maskedIp}
								</span>
							</DeviceDetailRow>
						)}
						<DeviceDetailRow
							label={<Trans>Last used</Trans>}
							dataFlx="user.devices-tab.device-details-modal.last-used"
							data-flx="user.devices-tab.device-details-modal.device-detail-row--5"
						>
							{isCurrent ? <Trans>Current session</Trans> : formatAuthSessionLastUsed(authSession, i18n)}
						</DeviceDetailRow>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

interface AuthSessionProps {
	authSession: AuthSession;
	isCurrent?: boolean;
	isSelected?: boolean;
	onSelect?: SelectionToggleHandler;
	index?: number;
	selectionMode?: boolean;
}

const AuthSessionItem: React.FC<AuthSessionProps> = observer(
	({authSession, isCurrent = false, isSelected, onSelect, index, selectionMode}) => {
		const {i18n} = useLingui();
		const clientOs = authSession.clientOs ?? i18n._(UNKNOWN_DEVICE_DESCRIPTOR);
		const platformLabel = authSession.clientPlatform ?? i18n._(UNKNOWN_DESCRIPTOR);
		const isMobile = authSession.clientDevice === 'mobile';
		const isSelectionInteractive = Boolean(selectionMode && !isCurrent && onSelect && index !== undefined);
		const openRevokeModal = () => {
			ModalCommands.push(
				modal(() => (
					<DeviceRevokeModal
						sessionIdHashes={[authSession.id]}
						data-flx="user.devices-tab.auth-session-item.device-revoke-modal"
					/>
				)),
			);
		};
		const openDetailsModal = () => {
			ModalCommands.push(
				modal(() => (
					<DeviceDetailsModal
						authSession={authSession}
						isCurrent={isCurrent}
						data-flx="user.devices-tab.open-details-modal.device-details-modal"
					/>
				)),
			);
		};
		const handleRevokeClick = (event: React.MouseEvent<HTMLButtonElement>) => {
			if (selectOnShiftClick(event, authSession.id, index, onSelect)) return;
			openRevokeModal();
		};
		const handleRevokeKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
			selectOnShiftActivation(event, authSession.id, index, onSelect);
		};
		const selectionItemProps = getSelectableItemProps({
			enabled: Boolean(selectionMode && !isCurrent),
			id: authSession.id,
			index,
			selected: isSelected,
			onSelect,
		});
		const metadataLine = [authSession.clientLocation, isCurrent ? null : formatAuthSessionLastUsed(authSession, i18n)]
			.filter((value): value is string => Boolean(value))
			.join(' · ');
		return (
			<div
				className={clsx(styles.authSession, isSelectionInteractive && styles.authSessionSelectable)}
				data-flx="user.devices-tab.auth-session-item.auth-session"
				{...selectionItemProps}
			>
				<div className={styles.authSessionContent} data-flx="user.devices-tab.auth-session-item.auth-session-content">
					<div className={styles.iconContainer} data-flx="user.devices-tab.auth-session-item.icon-container">
						{isMobile ? (
							<DeviceMobileIcon className={styles.icon} data-flx="user.devices-tab.auth-session-item.icon" />
						) : (
							<MonitorIcon className={styles.icon} data-flx="user.devices-tab.auth-session-item.icon--2" />
						)}
					</div>
					<div className={styles.authSessionInfo} data-flx="user.devices-tab.auth-session-item.auth-session-info">
						<span className={styles.authSessionTitle} data-flx="user.devices-tab.auth-session-item.auth-session-title">
							{authSession.clientBrowser === null ? (
								platformLabel
							) : (
								<>
									{clientOs}
									<StatusDot data-flx="user.devices-tab.auth-session-item.status-dot" />
									{platformLabel}
								</>
							)}
						</span>
						{metadataLine && (
							<div
								className={styles.authSessionLocation}
								data-flx="user.devices-tab.auth-session-item.auth-session-location"
							>
								<DeviceMetadataLine
									value={metadataLine}
									dataFlx="user.devices-tab.auth-session-item.metadata-line"
									data-flx="user.devices-tab.auth-session-item.device-metadata-line"
								/>
							</div>
						)}
					</div>
				</div>
				<div className={styles.authSessionActions} data-flx="user.devices-tab.auth-session-item.auth-session-actions">
					{!selectionMode && (
						<Tooltip
							text={i18n._(VIEW_DEVICE_DETAILS_DESCRIPTOR)}
							data-flx="user.devices-tab.auth-session-item.tooltip.details"
						>
							<Button
								type="button"
								square
								compact
								variant="secondary"
								aria-label={i18n._(VIEW_DEVICE_DETAILS_DESCRIPTOR)}
								onClick={openDetailsModal}
								className={styles.infoButton}
								icon={
									<InfoIcon
										className={styles.actionIcon}
										weight="bold"
										data-flx="user.devices-tab.auth-session-item.info-icon"
									/>
								}
								data-flx="user.devices-tab.auth-session-item.details-button.push"
							/>
						</Tooltip>
					)}
					{!isCurrent && !selectionMode && (
						<Tooltip
							text={i18n._(REVOKE_DEVICE_DESCRIPTOR)}
							data-flx="user.devices-tab.auth-session-item.tooltip.revoke"
						>
							<Button
								type="button"
								square
								compact
								variant="secondary"
								aria-label={i18n._(REVOKE_DEVICE_DESCRIPTOR)}
								aria-keyshortcuts="Shift+Enter Shift+Space"
								onClick={handleRevokeClick}
								onKeyDown={handleRevokeKeyDown}
								className={styles.revokeButton}
								icon={
									<XIcon
										className={styles.actionIcon}
										weight="bold"
										data-flx="user.devices-tab.auth-session-item.revoke-icon"
									/>
								}
								data-flx="user.devices-tab.auth-session-item.revoke-button.push"
							/>
						</Tooltip>
					)}
					{selectionMode && !isCurrent && (
						<SelectionCheckbox checked={!!isSelected} data-flx="user.devices-tab.auth-session-item.custom-checkbox" />
					)}
				</div>
			</div>
		);
	},
);
interface DevicesTabContentProps {
	presentation?: 'settings' | 'modal';
}

export const DevicesTabContent: React.FC<DevicesTabContentProps> = observer(({presentation = 'settings'}) => {
	const {i18n} = useLingui();
	const isModalPresentation = presentation === 'modal';
	const authSessionIdHash = AuthSessionState.authSessionIdHash;
	const authSessions = AuthSessionState.authSessions;
	const fetchStatus = AuthSessionState.fetchStatus;
	const otherDevices = useMemo(
		() => authSessions.filter((authSession) => authSession.id !== authSessionIdHash),
		[authSessionIdHash, authSessions],
	);
	const {
		selectedIds: selectedDevices,
		selectedIdList: selectedDeviceIds,
		itemIds: otherDeviceIds,
		selectionMode,
		selectAllShortcutLabel,
		clearSelection,
		toggleSelection: toggleDevice,
	} = useSelectableSettingsList({
		items: otherDevices,
		getId: getAuthSessionSelectionId,
	});
	useEffect(() => {
		AuthSessionCommands.fetch();
	}, []);
	const openSelectedDevicesRevokeModal = useCallback(() => {
		if (selectedDeviceIds.length === 0) return;
		ModalCommands.push(
			modal(() => (
				<DeviceRevokeModal
					sessionIdHashes={selectedDeviceIds}
					data-flx="user.devices-tab.selected-device-revoke-modal"
				/>
			)),
		);
	}, [selectedDeviceIds]);
	const openAllOtherDevicesRevokeModal = useCallback(() => {
		if (otherDeviceIds.length === 0) return;
		ModalCommands.push(
			modal(() => (
				<DeviceRevokeModal
					sessionIdHashes={otherDeviceIds}
					data-flx="user.devices-tab.all-other-devices-revoke-modal"
				/>
			)),
		);
	}, [otherDeviceIds]);
	useEffect(() => {
		if (isModalPresentation) return;
		if (selectedDeviceIds.length === 0) {
			UnsavedChangesCommands.clearUnsavedChanges(DEVICES_TAB_ID);
			return;
		}
		UnsavedChangesCommands.setUnsavedChanges(DEVICES_TAB_ID, true);
		UnsavedChangesCommands.setTabData(DEVICES_TAB_ID, {
			onReset: clearSelection,
			onSave: openSelectedDevicesRevokeModal,
			bannerText: i18n._(SELECTED_DEVICES_FOR_LOGOUT_DESCRIPTOR, {deviceCount: selectedDeviceIds.length}),
			resetLabel: i18n._(CLEAR_SELECTION_DESCRIPTOR),
			saveLabel: i18n._(SIGN_OUT_SELECTED_DEVICES_DESCRIPTOR, {deviceCount: selectedDeviceIds.length}),
			saveVariant: 'danger',
		});
		return () => UnsavedChangesCommands.clearUnsavedChanges(DEVICES_TAB_ID);
	}, [clearSelection, i18n, isModalPresentation, openSelectedDevicesRevokeModal, selectedDeviceIds.length]);
	if (fetchStatus === 'idle' || fetchStatus === 'pending') {
		const loadingState = (
			<div className={styles.loadingContainer} data-flx="user.devices-tab.loading-container">
				<Spinner data-flx="user.devices-tab.spinner" />
			</div>
		);
		if (!isModalPresentation) return loadingState;
		return <Modal.Content data-flx="user.devices-tab.modal-content.loading">{loadingState}</Modal.Content>;
	}
	const currentSession = authSessions.find((authSession) => authSession.id === authSessionIdHash);
	if (fetchStatus === 'error' || !currentSession) {
		const errorState = (
			<Slate
				icon={WifiSlashIcon}
				title={i18n._(NETWORK_ERROR_DESCRIPTOR)}
				description={i18n._(WE_RE_HAVING_TROUBLE_CONNECTING_TO_THE_SPACE_DESCRIPTOR)}
				buttonText={i18n._(TRY_AGAIN_DESCRIPTOR)}
				onClick={() => AuthSessionCommands.fetch()}
				data-flx="user.devices-tab.slate.fetch"
			/>
		);
		if (!isModalPresentation) return errorState;
		return <Modal.Content data-flx="user.devices-tab.modal-content.error">{errorState}</Modal.Content>;
	}
	const devicesContent = (
		<>
			<SettingsTabSection title={<Trans>Current device</Trans>} data-flx="user.devices-tab.current-device">
				<AuthSessionItem authSession={currentSession} isCurrent={true} data-flx="user.devices-tab.auth-session-item" />
			</SettingsTabSection>
			{otherDevices.length > 0 && (
				<SettingsTabSection
					title={<Trans>Other devices</Trans>}
					className={styles.compactDeviceSubsection}
					data-flx="user.devices-tab.other-devices"
				>
					<div className={styles.deviceListBody} data-flx="user.devices-tab.device-list-body">
						<p className={styles.selectionHint} data-flx="user.devices-tab.selection-hint">
							<Trans>
								Hold Shift and press X to mark devices for sign out. Press {selectAllShortcutLabel} to select all.
							</Trans>
						</p>
						<div className={styles.devicesGrid} data-flx="user.devices-tab.devices-grid">
							{otherDevices.map((authSession, index) => (
								<AuthSessionItem
									key={authSession.id}
									authSession={authSession}
									isSelected={selectedDevices.has(authSession.id)}
									onSelect={toggleDevice}
									index={index}
									selectionMode={selectionMode}
									data-flx="user.devices-tab.auth-session-item.toggle-device"
								/>
							))}
						</div>
						{!isModalPresentation && !selectionMode && otherDevices.length > 1 && (
							<div className={styles.logoutSection} data-flx="user.devices-tab.logout-section">
								<Button variant="danger" onClick={openAllOtherDevicesRevokeModal} data-flx="user.devices-tab.button">
									<Trans>Sign out all other devices</Trans>
								</Button>
								<p className={styles.logoutDescription} data-flx="user.devices-tab.logout-description">
									<Trans>You'll have to sign back in on all signed-out devices</Trans>
								</p>
							</div>
						)}
					</div>
				</SettingsTabSection>
			)}
		</>
	);
	if (isModalPresentation) {
		return (
			<>
				<Modal.Content data-flx="user.devices-tab.modal-content">
					<SettingsTabContainer data-flx="user.devices-tab.modal-settings-tab-container">
						<SettingsTabContent data-flx="user.devices-tab.modal-settings-tab-content">
							{devicesContent}
						</SettingsTabContent>
					</SettingsTabContainer>
				</Modal.Content>
				{(selectionMode || otherDeviceIds.length > 1) && (
					<Modal.Footer data-flx="user.devices-tab.modal-footer">
						{selectionMode && (
							<Button variant="secondary" onClick={clearSelection} data-flx="user.devices-tab.button.clear-selection">
								{i18n._(CLEAR_SELECTION_DESCRIPTOR)}
							</Button>
						)}
						{selectionMode ? (
							<Button
								variant="danger"
								onClick={openSelectedDevicesRevokeModal}
								data-flx="user.devices-tab.button.sign-out-selected"
							>
								{i18n._(SIGN_OUT_SELECTED_DEVICES_DESCRIPTOR, {deviceCount: selectedDeviceIds.length})}
							</Button>
						) : otherDeviceIds.length > 1 ? (
							<Button
								variant="danger"
								onClick={openAllOtherDevicesRevokeModal}
								data-flx="user.devices-tab.button.sign-out-all-other"
							>
								<Trans>Sign out all other devices</Trans>
							</Button>
						) : null}
					</Modal.Footer>
				)}
			</>
		);
	}
	return (
		<SettingsTabContainer data-flx="user.devices-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.devices-tab.settings-tab-content">
				<SettingsSection
					id="signed-in-devices"
					title={<Trans>Signed-in devices</Trans>}
					data-flx="user.devices-tab.signed-in-devices"
				>
					{devicesContent}
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export const LinkedDevicesManagementModal = observer(() => (
	<Modal.Root size="medium" centered data-flx="user.devices-tab.linked-devices-management-modal.modal-root">
		<Modal.Header
			title={<Trans>Devices</Trans>}
			data-flx="user.devices-tab.linked-devices-management-modal.modal-header"
		/>
		<DevicesTabContent
			presentation="modal"
			data-flx="user.devices-tab.linked-devices-management-modal.devices-tab-content"
		/>
	</Modal.Root>
));

const DevicesTab: React.FC = observer(() => <DevicesTabContent data-flx="user.devices-tab.devices-tab-content" />);

export default DevicesTab;
