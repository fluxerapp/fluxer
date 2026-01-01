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
import {
	CheckIcon,
	CheckSquareIcon,
	DeviceMobileIcon,
	MonitorIcon,
	NetworkSlashIcon,
	SquareIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AuthSessionActionCreators from '~/actions/AuthSessionActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {DeviceRevokeModal} from '~/components/modals/DeviceRevokeModal';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabHeader,
} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {AuthSessionRecord} from '~/records/AuthSessionRecord';
import AuthSessionStore from '~/stores/AuthSessionStore';
import * as DateUtils from '~/utils/DateUtils';
import {Slate} from '../components/Slate';
import styles from './DevicesTab.module.css';

const MOBILE_DEVICE_REGEX = /iOS|Android|Windows Phone|BlackBerry|Mobile/i;

const StatusDot = observer(() => <div aria-hidden={true} className={styles.statusDot} />);

const CustomCheckbox = observer(({checked}: {checked: boolean}) => (
	<div className={clsx(styles.checkbox, checked ? styles.checkboxChecked : styles.checkboxUnchecked)}>
		{checked && <CheckIcon weight="regular" className={styles.checkIcon} />}
	</div>
));

interface AuthSessionProps {
	authSession: AuthSessionRecord;
	isCurrent?: boolean;
	isSelected?: boolean;
	onSelect?: (id: string, index: number, shiftKey: boolean) => void;
	index?: number;
	selectionMode?: boolean;
}

const AuthSession: React.FC<AuthSessionProps> = observer(
	({authSession, isCurrent = false, isSelected, onSelect, index, selectionMode}) => {
		const {t} = useLingui();
		const isMobile = MOBILE_DEVICE_REGEX.test(authSession.clientOs);
		const isInteractive = selectionMode && !isCurrent && onSelect;

		const handleClick = isInteractive
			? (e: React.MouseEvent) => onSelect(authSession.id, index!, e.shiftKey)
			: undefined;
		const handleKeyDown = isInteractive
			? (e: React.KeyboardEvent) => {
					if (e.key === 'Enter' || e.key === ' ') {
						e.preventDefault();
						onSelect(authSession.id, index!, e.shiftKey);
					}
				}
			: undefined;

		const interactiveProps = isInteractive
			? {
					onClick: handleClick,
					onKeyDown: handleKeyDown,
					role: 'button' as const,
					tabIndex: 0,
				}
			: {};

		const platformLabel =
			authSession.clientPlatform === 'Fluxer Desktop' ? t`Fluxer Desktop` : authSession.clientPlatform;

		return (
			<div
				className={clsx(styles.authSession, selectionMode && !isCurrent && styles.authSessionSelectable)}
				{...interactiveProps}
			>
				<div className={styles.authSessionContent}>
					<div className={styles.iconContainer}>
						{isMobile ? <DeviceMobileIcon className={styles.icon} /> : <MonitorIcon className={styles.icon} />}
					</div>

					<div className={styles.authSessionInfo}>
						<span className={styles.authSessionTitle}>
							{authSession.clientOs}
							<StatusDot />
							{platformLabel}
						</span>

						<div className={styles.authSessionLocation}>
							{authSession.clientLocation}
							{!isCurrent && (
								<>
									<StatusDot />
									<span className={styles.lastUsed}>
										{DateUtils.getShortRelativeDateString(authSession.approxLastUsedAt ?? new Date(0))}
									</span>
								</>
							)}
						</div>
					</div>
				</div>

				<div className={styles.authSessionActions}>
					{!isCurrent && !selectionMode && (
						<Tooltip text={t`Revoke device`}>
							<button
								type="button"
								onClick={() =>
									ModalActionCreators.push(modal(() => <DeviceRevokeModal sessionIdHashes={[authSession.id]} />))
								}
								className={styles.revokeButton}
							>
								<XIcon className={styles.revokeIcon} weight="regular" />
							</button>
						</Tooltip>
					)}

					{selectionMode && !isCurrent && <CustomCheckbox checked={!!isSelected} />}
				</div>
			</div>
		);
	},
);

const DevicesTab: React.FC = observer(() => {
	const {t} = useLingui();
	const authSessionIdHash = AuthSessionStore.authSessionIdHash;
	const authSessions = AuthSessionStore.authSessions;
	const fetchStatus = AuthSessionStore.fetchStatus;
	const otherDevices = authSessions.filter((authSession) => authSession.id !== authSessionIdHash);
	const [selectionMode, setSelectionMode] = React.useState(false);
	const [selectedDevices, setSelectedDevices] = React.useState(new Set<string>());
	const [lastToggledIndex, setLastToggledIndex] = React.useState(-1);

	React.useEffect(() => {
		AuthSessionActionCreators.fetch();
	}, []);

	const toggleSelectionMode = (value: boolean) => {
		setSelectionMode(value);
		if (!value) {
			setSelectedDevices(new Set());
			setLastToggledIndex(-1);
		}
	};

	const toggleDevice = (deviceId: string, index: number, isShiftSelect: boolean) => {
		if (!selectionMode) {
			toggleSelectionMode(true);
		}

		setSelectedDevices((prev) => {
			const newSelection = new Set(prev);

			if (isShiftSelect && lastToggledIndex !== -1) {
				const start = Math.min(lastToggledIndex, index);
				const end = Math.max(lastToggledIndex, index);
				const shouldAdd = !prev.has(deviceId);

				otherDevices.slice(start, end + 1).forEach((device) => {
					if (shouldAdd) {
						newSelection.add(device.id);
					} else {
						newSelection.delete(device.id);
					}
				});
			} else {
				if (prev.has(deviceId)) {
					newSelection.delete(deviceId);
				} else {
					newSelection.add(deviceId);
				}
			}

			return newSelection;
		});
		setLastToggledIndex(index);
	};

	const handleSelectAll = () => {
		if (selectedDevices.size === otherDevices.length) {
			setSelectedDevices(new Set());
		} else {
			setSelectedDevices(new Set(otherDevices.map((device) => device.id)));
		}
	};

	if (fetchStatus === 'idle' || fetchStatus === 'pending') {
		return (
			<div className={styles.loadingContainer}>
				<Spinner />
			</div>
		);
	}

	const currentSession = authSessions.find((authSession) => authSession.id === authSessionIdHash);
	if (fetchStatus === 'error' || !currentSession) {
		return (
			<Slate
				icon={NetworkSlashIcon}
				title={t`Network error`}
				description={t`We're having trouble connecting to the space-time continuum. Please check your connection and try again.`}
				buttonText={t`Retry`}
				onClick={() => AuthSessionActionCreators.fetch()}
			/>
		);
	}

	return (
		<SettingsTabContainer>
			<SettingsTabHeader
				title={<Trans>My Devices</Trans>}
				description={
					<Trans>
						See all devices that are currently logged into your account. Revoke any sessions that you don't recognize.
					</Trans>
				}
			/>

			<SettingsTabContent>
				<div className={styles.devicesSection}>
					<div className={styles.deviceGroup}>
						<h3 className={styles.deviceGroupTitle}>
							<Trans>Current Device</Trans>
						</h3>
						<AuthSession authSession={currentSession} isCurrent={true} />
					</div>

					{otherDevices.length > 0 && (
						<div className={styles.deviceGroup}>
							<div className={styles.deviceGroupHeader}>
								<h3 className={styles.deviceGroupTitle}>
									<Trans>Other Devices</Trans>
								</h3>

								{otherDevices.length > 1 && (
									<div className={styles.actionsContainer}>
										<Tooltip text={selectionMode ? t`Exit Selection Mode` : t`Enter Selection Mode`}>
											<button
												type="button"
												onClick={() => toggleSelectionMode(!selectionMode)}
												className={styles.actionButton}
											>
												{selectionMode ? (
													<XIcon weight="regular" className={styles.actionIcon} />
												) : (
													<CheckSquareIcon className={styles.actionIcon} />
												)}
											</button>
										</Tooltip>

										{selectionMode && (
											<Tooltip text={selectedDevices.size === otherDevices.length ? t`Clear Selection` : t`Select All`}>
												<button type="button" onClick={handleSelectAll} className={styles.actionButton}>
													{selectedDevices.size === otherDevices.length ? (
														<SquareIcon className={styles.actionIcon} />
													) : (
														<CheckSquareIcon className={styles.actionIcon} />
													)}
												</button>
											</Tooltip>
										)}
									</div>
								)}
							</div>

							<div className={styles.devicesGrid}>
								{otherDevices.map((authSession, index) => (
									<AuthSession
										key={authSession.id}
										authSession={authSession}
										isSelected={selectedDevices.has(authSession.id)}
										onSelect={toggleDevice}
										index={index}
										selectionMode={selectionMode}
									/>
								))}
							</div>

							{(otherDevices.length > 1 || (selectionMode && selectedDevices.size > 0)) && (
								<div className={styles.logoutSection}>
									<Button
										variant="danger-secondary"
										onClick={() => {
											if (selectionMode && selectedDevices.size > 0) {
												ModalActionCreators.push(
													modal(() => <DeviceRevokeModal sessionIdHashes={Array.from(selectedDevices)} />),
												);
											} else {
												ModalActionCreators.push(
													modal(() => (
														<DeviceRevokeModal sessionIdHashes={otherDevices.map((authSession) => authSession.id)} />
													)),
												);
											}
										}}
									>
										{selectionMode && selectedDevices.size > 0 ? (
											selectedDevices.size === 1 ? (
												<Trans>Log out 1 device</Trans>
											) : (
												<Trans>Log out {selectedDevices.size} devices</Trans>
											)
										) : (
											<Trans>Log Out All Other Devices</Trans>
										)}
									</Button>
									<p className={styles.logoutDescription}>
										<Trans>You'll have to log back in on all logged out devices</Trans>
									</p>
								</div>
							)}
						</div>
					)}
				</div>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default DevicesTab;
