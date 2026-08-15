// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_INSTANCE_DOMAIN} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/auth/flow/InstanceSelector.module.css';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CheckCircleIcon, GlobeIcon, TrashIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const FAILED_TO_CONNECT_TO_INSTANCE_DESCRIPTOR = msg({
	message: 'Failed to connect to instance',
	comment: 'Short label in the authentication instance selector. Keep the tone plain and specific.',
});
const ENTER_INSTANCE_URL_E_G_DESCRIPTOR = msg({
	message: 'Enter instance URL (e.g. {exampleInstanceDomain})',
	comment: 'Instance selector text input placeholder. Example instance domain is interpolated.',
});
const SHOW_RECENT_INSTANCES_DESCRIPTOR = msg({
	message: 'Show recent instances',
	comment: 'Short label in the authentication instance selector. Keep the tone plain and specific.',
});
const INSTANCE_URL_DESCRIPTOR = msg({
	message: 'Instance URL',
	comment: 'Short label in the authentication instance selector. Keep the tone plain and specific.',
});
const REMOVE_FROM_RECENT_INSTANCES_DESCRIPTOR = msg({
	message: 'Remove {domain} from recent instances',
	comment:
		'Short label in the authentication instance selector. Preserve {domain}; it is inserted by code. Keep the tone plain and specific.',
});
const RECENT_INSTANCES_KEY = 'recent_instances';
const MAX_RECENT_INSTANCES = 5;

export type InstanceDiscoveryStatus = 'idle' | 'discovering' | 'success' | 'error';

export interface InstanceInfo {
	domain: string;
	name?: string;
	lastUsed: number;
}

interface InstanceSelectorProps {
	value: string;
	onChange: (value: string) => void;
	onInstanceDiscovered?: (domain: string) => void;
	onDiscoveryStatusChange?: (status: InstanceDiscoveryStatus) => void;
	disabled?: boolean;
	className?: string;
}

function loadRecentInstances(): Array<InstanceInfo> {
	const stored = AppStorage.getJSON<Array<InstanceInfo>>(RECENT_INSTANCES_KEY);
	if (!stored || !Array.isArray(stored)) {
		return [];
	}
	return stored.sort((a, b) => b.lastUsed - a.lastUsed).slice(0, MAX_RECENT_INSTANCES);
}

function saveRecentInstance(domain: string, name?: string): void {
	const recent = loadRecentInstances();
	const normalizedDomain = domain.toLowerCase().trim();
	const existingIndex = recent.findIndex((inst) => inst.domain.toLowerCase() === normalizedDomain);
	if (existingIndex !== -1) {
		recent.splice(existingIndex, 1);
	}
	recent.unshift({
		domain: normalizedDomain,
		name,
		lastUsed: Date.now(),
	});
	AppStorage.setJSON(RECENT_INSTANCES_KEY, recent.slice(0, MAX_RECENT_INSTANCES));
}

function removeRecentInstance(domain: string): void {
	const recent = loadRecentInstances();
	const normalizedDomain = domain.toLowerCase().trim();
	const filtered = recent.filter((inst) => inst.domain.toLowerCase() !== normalizedDomain);
	AppStorage.setJSON(RECENT_INSTANCES_KEY, filtered);
}

export const InstanceSelector = observer(function InstanceSelector({
	value,
	onChange,
	onInstanceDiscovered,
	onDiscoveryStatusChange,
	disabled = false,
	className,
}: InstanceSelectorProps) {
	const {i18n} = useLingui();
	const [discoveryStatus, setDiscoveryStatus] = useState<InstanceDiscoveryStatus>('idle');
	const [discoveryError, setDiscoveryError] = useState<string | null>(null);
	const [recentInstances, setRecentInstances] = useState<Array<InstanceInfo>>(() => loadRecentInstances());
	const [showDropdown, setShowDropdown] = useState(false);
	const inputRef = useRef<HTMLInputElement>(null);
	const dropdownRef = useRef<HTMLDivElement>(null);
	const discoveryTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const updateDiscoveryStatus = useCallback(
		(status: InstanceDiscoveryStatus) => {
			setDiscoveryStatus(status);
			onDiscoveryStatusChange?.(status);
		},
		[onDiscoveryStatusChange],
	);
	const discoverInstance = useCallback(
		async (instanceUrl: string) => {
			if (!instanceUrl.trim()) {
				updateDiscoveryStatus('idle');
				setDiscoveryError(null);
				return;
			}
			updateDiscoveryStatus('discovering');
			setDiscoveryError(null);
			try {
				await RuntimeConfig.connectToEndpoint(instanceUrl);
				updateDiscoveryStatus('success');
				saveRecentInstance(instanceUrl);
				setRecentInstances(loadRecentInstances());
				onInstanceDiscovered?.(instanceUrl);
			} catch (error) {
				updateDiscoveryStatus('error');
				const errorMessage = error instanceof Error ? error.message : i18n._(FAILED_TO_CONNECT_TO_INSTANCE_DESCRIPTOR);
				setDiscoveryError(errorMessage);
			}
		},
		[onInstanceDiscovered, updateDiscoveryStatus, i18n],
	);
	const handleInputChange = useCallback(
		(event: React.ChangeEvent<HTMLInputElement>) => {
			const newValue = event.target.value;
			onChange(newValue);
			updateDiscoveryStatus('idle');
			setDiscoveryError(null);
			if (discoveryTimeoutRef.current) {
				clearTimeout(discoveryTimeoutRef.current);
			}
			if (newValue.trim()) {
				discoveryTimeoutRef.current = setTimeout(() => {
					discoverInstance(newValue);
				}, 800);
			}
		},
		[onChange, discoverInstance, updateDiscoveryStatus],
	);
	const handleSelectRecent = useCallback(
		(instance: InstanceInfo) => {
			onChange(instance.domain);
			setShowDropdown(false);
			discoverInstance(instance.domain);
		},
		[onChange, discoverInstance],
	);
	const handleRemoveRecent = useCallback((event: React.MouseEvent, domain: string) => {
		event.stopPropagation();
		removeRecentInstance(domain);
		setRecentInstances(loadRecentInstances());
	}, []);
	const handleConnectClick = useCallback(() => {
		if (value.trim()) {
			discoverInstance(value);
		}
	}, [value, discoverInstance]);
	const handleDropdownToggle = useCallback(() => {
		if (recentInstances.length > 0 && !disabled) {
			setShowDropdown((prev) => !prev);
		}
	}, [recentInstances.length, disabled]);
	const handleInputFocus = useCallback(() => {
		if (recentInstances.length > 0 && !value.trim()) {
			setShowDropdown(true);
		}
	}, [recentInstances.length, value]);
	useEffect(() => {
		function handleClickOutside(event: MouseEvent) {
			if (
				dropdownRef.current &&
				!dropdownRef.current.contains(event.target as Node) &&
				inputRef.current &&
				!inputRef.current.contains(event.target as Node)
			) {
				setShowDropdown(false);
			}
		}
		document.addEventListener('mousedown', handleClickOutside);
		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	}, []);
	useEffect(() => {
		return () => {
			if (discoveryTimeoutRef.current) {
				clearTimeout(discoveryTimeoutRef.current);
			}
		};
	}, []);
	const statusIcon = useMemo(() => {
		if (discoveryStatus === 'discovering') {
			return (
				<Spinner
					size="small"
					className={styles.statusSpinner}
					data-flx="auth.flow.instance-selector.status-icon.status-spinner"
				/>
			);
		}
		if (discoveryStatus === 'success') {
			return (
				<CheckCircleIcon
					weight="fill"
					className={styles.statusSuccess}
					size={remFromPx(18)}
					data-flx="auth.flow.instance-selector.status-icon.status-success"
				/>
			);
		}
		if (discoveryStatus === 'error') {
			return (
				<WarningCircleIcon
					weight="fill"
					className={styles.statusError}
					size={remFromPx(18)}
					data-flx="auth.flow.instance-selector.status-icon.status-error"
				/>
			);
		}
		return null;
	}, [discoveryStatus]);
	const placeholder = i18n._(ENTER_INSTANCE_URL_E_G_DESCRIPTOR, {exampleInstanceDomain: EXAMPLE_INSTANCE_DOMAIN});
	return (
		<div className={clsx(styles.container, className)} data-flx="auth.flow.instance-selector.container">
			<div className={styles.inputContainer} data-flx="auth.flow.instance-selector.input-container">
				<Input
					ref={inputRef}
					value={value}
					onChange={handleInputChange}
					onFocus={handleInputFocus}
					placeholder={placeholder}
					disabled={disabled}
					leftIcon={
						<GlobeIcon size={remFromPx(18)} weight="regular" data-flx="auth.flow.instance-selector.globe-icon" />
					}
					rightElement={
						<div className={styles.inputActions} data-flx="auth.flow.instance-selector.input-actions">
							{statusIcon}
							{recentInstances.length > 0 && (
								<button
									type="button"
									className={styles.dropdownToggle}
									onClick={handleDropdownToggle}
									disabled={disabled}
									aria-label={i18n._(SHOW_RECENT_INSTANCES_DESCRIPTOR)}
									data-flx="auth.flow.instance-selector.dropdown-toggle.button"
								>
									<CaretDownIcon
										size={remFromPx(16)}
										weight="bold"
										className={clsx(styles.caretIcon, showDropdown && styles.caretIconOpen)}
										data-flx="auth.flow.instance-selector.caret-icon"
									/>
								</button>
							)}
						</div>
					}
					aria-label={i18n._(INSTANCE_URL_DESCRIPTOR)}
					aria-describedby={discoveryError ? 'instance-error' : undefined}
					data-flx="auth.flow.instance-selector.input"
				/>
				{showDropdown && recentInstances.length > 0 && (
					<div ref={dropdownRef} className={styles.dropdown} data-flx="auth.flow.instance-selector.dropdown">
						<div className={styles.dropdownHeader} data-flx="auth.flow.instance-selector.dropdown-header">
							<Trans>Recent instances</Trans>
						</div>
						<ul className={styles.dropdownList} data-flx="auth.flow.instance-selector.dropdown-list">
							{recentInstances.map((instance) => (
								<li key={instance.domain} data-flx="auth.flow.instance-selector.li">
									<button
										type="button"
										className={styles.dropdownItem}
										onClick={() => handleSelectRecent(instance)}
										data-flx="auth.flow.instance-selector.dropdown-item.select-recent.button"
									>
										<GlobeIcon
											size={remFromPx(16)}
											weight="regular"
											className={styles.instanceIcon}
											data-flx="auth.flow.instance-selector.instance-icon"
										/>
										<span className={styles.instanceDomain} data-flx="auth.flow.instance-selector.instance-domain">
											{instance.domain}
										</span>
										{instance.name && (
											<span className={styles.instanceName} data-flx="auth.flow.instance-selector.instance-name">
												{instance.name}
											</span>
										)}
										<button
											type="button"
											className={styles.removeButton}
											onClick={(e) => handleRemoveRecent(e, instance.domain)}
											aria-label={i18n._(REMOVE_FROM_RECENT_INSTANCES_DESCRIPTOR, {domain: instance.domain})}
											data-flx="auth.flow.instance-selector.remove-button.remove-recent"
										>
											<TrashIcon
												size={remFromPx(14)}
												weight="regular"
												data-flx="auth.flow.instance-selector.trash-icon"
											/>
										</button>
									</button>
								</li>
							))}
						</ul>
					</div>
				)}
			</div>
			{discoveryError && (
				<div id="instance-error" className={styles.errorMessage} data-flx="auth.flow.instance-selector.instance-error">
					{discoveryError}
				</div>
			)}
			{discoveryStatus !== 'success' && value.trim() && (
				<Button
					onClick={handleConnectClick}
					disabled={disabled}
					submitting={discoveryStatus === 'discovering'}
					variant="secondary"
					small
					className={styles.connectButton}
					data-flx="auth.flow.instance-selector.connect-button.connect-click"
				>
					<Trans>Connect</Trans>
				</Button>
			)}
		</div>
	);
});
