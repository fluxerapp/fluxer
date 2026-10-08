// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {EXAMPLE_DOMAIN} from '@app/features/app/config/I18nDisplayConstants';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {describeAPIEndpoint} from '@app/features/app/state/RuntimeConfig';
import {resolveSnapshotInstanceDomain} from '@app/features/auth/AccountDisplayUtils';
import styles from '@app/features/auth/flow/InstanceSelector.module.css';
import {
	deleteKnownInstance,
	type InstanceInfo,
	loadKnownInstances,
	normalizeInstanceDomain,
	normalizeInstanceName,
	resolveInstanceLabel,
	resolveLocalDevelopmentInstance,
	saveKnownInstance,
	shouldSaveKnownInstance,
} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {
	CONNECTING_TO_INSTANCE_DESCRIPTOR,
	describeInstanceDiscoveryFailure,
	INSTANCE_ADDRESS_INVALID_DESCRIPTOR,
} from '@app/features/auth/flow/instance_selector/InstanceDiscoveryFailure';
import {InstanceDiscoveryStatusIcon} from '@app/features/auth/flow/instance_selector/InstanceDiscoveryStatusIcon';
import {InstancePickerRows} from '@app/features/auth/flow/instance_selector/InstancePickerRows';
import {InstanceSelectorStatusRow} from '@app/features/auth/flow/instance_selector/InstanceSelectorStatusRow';
import {
	INSTANCE_SELECTOR_STEPS,
	InstanceDiscoveryStatus,
	InstanceSelectorStep,
} from '@app/features/auth/flow/instance_selector/InstanceSelectorTypes';
import {
	BACK_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	INSTANCE_URL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {TextInput} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {flxElementClassName} from '@app/lib/react';
import type {DesktopKnownInstanceRecord} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import {NETWORK_ENDPOINT_INPUT_MAX_BYTES} from '@fluxer/instance_bootstrap/src/NetworkOrigin';
import {officialMarketingOrigin} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, GlobeIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {
	type AriaAttributes,
	type ChangeEvent,
	type FormEvent,
	type ReactNode,
	useCallback,
	useEffect,
	useId,
	useRef,
	useState,
} from 'react';

const ENTER_INSTANCE_URL_E_G_DESCRIPTOR = msg({
	message: 'Enter self-hosted instance URL (e.g. {exampleInstanceDomain})',
	comment: 'Instance selector text input placeholder. Example instance domain is interpolated.',
});
const INSTANCE_URL_REQUIRED_DESCRIPTOR = msg({
	message: 'Enter an instance URL',
	comment: 'Validation message shown when the authentication instance selector is submitted empty.',
});
const INSTANCE_URL_TOO_LONG_DESCRIPTOR = msg({
	message: 'Instance URL is too long',
	comment: 'Validation message shown when the authentication instance URL exceeds the supported length.',
});
const FAILED_TO_REMOVE_INSTANCE_DESCRIPTOR = msg({
	message: 'Failed to remove instance from recent instances',
	comment: 'Status message shown when a recent instance cannot be removed from the authentication instance selector.',
});
const INSTANCE_SELECTION_DESCRIPTOR = msg({
	message: 'Instance selection',
	comment: 'Accessible label for the authentication instance selector carousel.',
});

const logger = new Logger('InstanceSelector');

interface InstanceDiscoveryRequest {
	readonly requestId: number;
	readonly signal: AbortSignal;
}

class InstanceDiscoveryRequestOwner {
	private controller: AbortController | null = null;
	private requestId = 0;
	private disposed = false;

	public begin(): InstanceDiscoveryRequest {
		if (this.disposed) {
			throw new Error('Cannot begin a disposed instance discovery request');
		}
		this.controller?.abort();
		this.requestId += 1;
		const controller = new AbortController();
		this.controller = controller;
		return {requestId: this.requestId, signal: controller.signal};
	}

	public cancel(): void {
		this.requestId += 1;
		if (this.controller != null) {
			this.controller.abort();
			this.controller = null;
		}
	}

	public complete(request: InstanceDiscoveryRequest): void {
		if (this.controller != null && this.controller.signal === request.signal) {
			this.controller = null;
		}
	}

	public isCurrent(requestId: number): boolean {
		return !this.disposed && this.requestId === requestId;
	}

	public get currentRequestId(): number {
		return this.requestId;
	}

	public dispose(): void {
		if (this.disposed) {
			return;
		}
		this.disposed = true;
		this.cancel();
	}
}

async function recordKnownInstance(record: DesktopKnownInstanceRecord): Promise<InstanceInfo | null> {
	try {
		return await saveKnownInstance(record);
	} catch (error) {
		logger.warn('Failed to record a resolved instance in the known instance directory', error);
		return null;
	}
}

export interface InstanceDiscoveredEvent {
	readonly domain: string;
	readonly instance: InstanceInfo | null;
	readonly snapshot: RuntimeConfigSnapshot;
}

interface InstanceSelectorProps {
	readonly value: string;
	readonly onChange: (value: string) => void;
	readonly onInstanceDiscovered: ((event: InstanceDiscoveredEvent) => void) | null;
	readonly onDiscoveryStatusChange: ((status: InstanceDiscoveryStatus) => void) | null;
	readonly onBackActionChange: ((action: (() => void) | null) => void) | null;
	readonly disabled: boolean;
	readonly className: string | null;
	readonly suppressInlineBackButton: boolean;
}

function resolveInstanceDisplayName(snapshot: RuntimeConfigSnapshot, productName: string, domain: string): string {
	const name = normalizeInstanceName(snapshot.appPublic?.branding?.product_name) ?? normalizeInstanceName(productName);
	return resolveInstanceLabel(name, domain);
}

interface ProgressMessageRequest {
	readonly discoveryProgressDomain: string | null;
	readonly i18n: I18n;
	readonly isDiscovering: boolean;
}

function resolveProgressMessage({discoveryProgressDomain, i18n, isDiscovering}: ProgressMessageRequest): string | null {
	if (!isDiscovering) {
		return null;
	}
	if (discoveryProgressDomain == null || discoveryProgressDomain.length === 0) {
		return null;
	}
	return i18n._(CONNECTING_TO_INSTANCE_DESCRIPTOR, {domain: discoveryProgressDomain});
}

function statusDescriptionAria(statusId: string, statusMessage: string | null): AriaAttributes {
	if (statusMessage == null || statusMessage.length === 0) {
		return {};
	}
	return {'aria-describedby': statusId};
}

export const InstanceSelector = observer(function InstanceSelector({
	value,
	onChange,
	onInstanceDiscovered,
	onDiscoveryStatusChange,
	onBackActionChange,
	disabled,
	className,
	suppressInlineBackButton,
}: InstanceSelectorProps) {
	const {i18n} = useLingui();
	const [step, setStep] = useState<InstanceSelectorStep>(InstanceSelectorStep.PICK);
	const [discoveryStatus, setDiscoveryStatus] = useState<InstanceDiscoveryStatus>(InstanceDiscoveryStatus.IDLE);
	const [discoveryError, setDiscoveryError] = useState<string | null>(null);
	const [discoveryProgressDomain, setDiscoveryProgressDomain] = useState<string | null>(null);
	const [knownInstances, setKnownInstances] = useState<ReadonlyArray<InstanceInfo>>([]);
	const inputRef = useRef<HTMLInputElement>(null);
	const [discoveryRequestOwner] = useState(() => new InstanceDiscoveryRequestOwner());
	const statusId = useId();
	const refreshKnownInstances = useCallback(
		(requestId: number | null) => {
			loadKnownInstances()
				.then((instances) => {
					if (requestId != null && requestId !== discoveryRequestOwner.currentRequestId) {
						return;
					}
					setKnownInstances(instances);
				})
				.catch((error) => {
					logger.warn('Failed to refresh the known instance directory', error);
				});
		},
		[discoveryRequestOwner],
	);
	useEffect(() => {
		refreshKnownInstances(null);
	}, [refreshKnownInstances]);
	const updateDiscoveryStatus = useCallback(
		(status: InstanceDiscoveryStatus) => {
			setDiscoveryStatus(status);
			onDiscoveryStatusChange?.(status);
		},
		[onDiscoveryStatusChange],
	);
	const discoverInstance = useCallback(
		async (instanceUrl: string, shouldRecordKnownInstance: boolean) => {
			const submittedUrl = instanceUrl.trim();
			const request = discoveryRequestOwner.begin();
			const {requestId, signal} = request;
			const normalizedDomain = normalizeInstanceDomain(submittedUrl);
			const displayDomain = normalizedDomain ?? submittedUrl;
			try {
				if (submittedUrl.length === 0) {
					updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
					setDiscoveryError(i18n._(INSTANCE_URL_REQUIRED_DESCRIPTOR));
					return;
				}
				if (instanceUrl.length > NETWORK_ENDPOINT_INPUT_MAX_BYTES) {
					updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
					setDiscoveryError(i18n._(INSTANCE_URL_TOO_LONG_DESCRIPTOR));
					return;
				}
				if (normalizedDomain == null) {
					updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
					setDiscoveryError(i18n._(INSTANCE_ADDRESS_INVALID_DESCRIPTOR, {exampleInstanceDomain: EXAMPLE_DOMAIN}));
					return;
				}
				updateDiscoveryStatus(InstanceDiscoveryStatus.DISCOVERING);
				setDiscoveryProgressDomain(displayDomain);
				setDiscoveryError(null);
				const resolution = await RuntimeConfig.resolveEndpoint({input: submittedUrl, signal});
				if (!discoveryRequestOwner.isCurrent(requestId)) {
					return;
				}
				const snapshot = resolution.snapshot;
				const submittedDomain = describeAPIEndpoint(snapshot.apiEndpoint);
				const recordedDomain =
					normalizedDomain === submittedDomain
						? (resolveSnapshotInstanceDomain(snapshot) ?? normalizedDomain)
						: normalizedDomain;
				const displayName = resolveInstanceDisplayName(snapshot, resolution.productName, recordedDomain);
				let discoveredInstance: InstanceInfo | null = null;
				if (shouldRecordKnownInstance && shouldSaveKnownInstance(submittedDomain)) {
					discoveredInstance = await recordKnownInstance({
						instanceKey: resolution.instanceKey,
						domain: recordedDomain,
						displayName,
						lastUsed: Date.now(),
					});
				}
				if (!discoveryRequestOwner.isCurrent(requestId)) {
					return;
				}
				updateDiscoveryStatus(InstanceDiscoveryStatus.IDLE);
				if (onInstanceDiscovered != null) {
					onInstanceDiscovered({domain: submittedDomain, instance: discoveredInstance, snapshot});
				} else {
					onChange(submittedDomain);
				}
				refreshKnownInstances(requestId);
			} catch (error) {
				if (!discoveryRequestOwner.isCurrent(requestId)) {
					return;
				}
				logger.warn('Instance discovery failed for', displayDomain, error);
				updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
				setDiscoveryError(describeInstanceDiscoveryFailure({error, domain: displayDomain, i18n}));
			} finally {
				discoveryRequestOwner.complete(request);
			}
		},
		[discoveryRequestOwner, i18n, onChange, onInstanceDiscovered, refreshKnownInstances, updateDiscoveryStatus],
	);
	const startDiscovery = useCallback(
		(instanceUrl: string, shouldRecordKnownInstance: boolean) => {
			discoverInstance(instanceUrl, shouldRecordKnownInstance).catch((error) => {
				logger.error('Instance discovery failed unexpectedly', error);
			});
		},
		[discoverInstance],
	);
	const handleInputChange = useCallback(
		(event: ChangeEvent<HTMLInputElement>) => {
			discoveryRequestOwner.cancel();
			onChange(event.target.value);
			updateDiscoveryStatus(InstanceDiscoveryStatus.IDLE);
			setDiscoveryError(null);
		},
		[discoveryRequestOwner, onChange, updateDiscoveryStatus],
	);
	const handleSelectOfficial = useCallback(() => {
		const origin = officialMarketingOrigin(Config.PUBLIC_RELEASE_CHANNEL);
		onChange(origin);
		startDiscovery(origin, false);
	}, [onChange, startDiscovery]);
	const handleSelectKnown = useCallback(
		(instance: InstanceInfo) => {
			onChange(instance.domain);
			startDiscovery(instance.domain, true);
		},
		[onChange, startDiscovery],
	);
	const handleRemoveKnownInstance = useCallback(
		(instance: InstanceInfo) => {
			deleteKnownInstance(instance.instanceKey)
				.then(() => {
					setKnownInstances((current) =>
						current.filter((knownInstance) => knownInstance.instanceKey !== instance.instanceKey),
					);
					updateDiscoveryStatus(InstanceDiscoveryStatus.IDLE);
					setDiscoveryError(null);
					refreshKnownInstances(null);
				})
				.catch((error) => {
					logger.warn('Failed to remove a known instance', error);
					updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
					setDiscoveryError(i18n._(FAILED_TO_REMOVE_INSTANCE_DESCRIPTOR));
				});
		},
		[i18n, refreshKnownInstances, updateDiscoveryStatus],
	);
	const handleAddInstance = useCallback(() => {
		discoveryRequestOwner.cancel();
		onChange('');
		updateDiscoveryStatus(InstanceDiscoveryStatus.IDLE);
		setDiscoveryError(null);
		setStep(InstanceSelectorStep.ADD);
	}, [discoveryRequestOwner, onChange, updateDiscoveryStatus]);
	const handleBackToPicker = useCallback(() => {
		discoveryRequestOwner.cancel();
		updateDiscoveryStatus(InstanceDiscoveryStatus.IDLE);
		setDiscoveryError(null);
		setStep(InstanceSelectorStep.PICK);
	}, [discoveryRequestOwner, updateDiscoveryStatus]);
	const handleSubmit = useCallback(
		(event: FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			if (discoveryStatus === InstanceDiscoveryStatus.DISCOVERING) {
				return;
			}
			if (value.trim().length === 0) {
				updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
				setDiscoveryError(i18n._(INSTANCE_URL_REQUIRED_DESCRIPTOR));
				inputRef.current?.focus();
				return;
			}
			if (value.length > NETWORK_ENDPOINT_INPUT_MAX_BYTES) {
				updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
				setDiscoveryError(i18n._(INSTANCE_URL_TOO_LONG_DESCRIPTOR));
				inputRef.current?.focus();
				return;
			}
			if (normalizeInstanceDomain(value) == null) {
				updateDiscoveryStatus(InstanceDiscoveryStatus.ERROR);
				setDiscoveryError(i18n._(INSTANCE_ADDRESS_INVALID_DESCRIPTOR, {exampleInstanceDomain: EXAMPLE_DOMAIN}));
				inputRef.current?.focus();
				return;
			}
			startDiscovery(value, true);
		},
		[discoveryStatus, i18n, startDiscovery, updateDiscoveryStatus, value],
	);
	useEffect(() => () => discoveryRequestOwner.dispose(), [discoveryRequestOwner]);
	useEffect(() => {
		if (onBackActionChange == null) {
			return;
		}
		if (step === InstanceSelectorStep.ADD) {
			onBackActionChange(handleBackToPicker);
		} else {
			onBackActionChange(null);
		}
		return () => onBackActionChange(null);
	}, [handleBackToPicker, onBackActionChange, step]);
	const isDiscovering = discoveryStatus === InstanceDiscoveryStatus.DISCOVERING;
	const progressMessage = resolveProgressMessage({discoveryProgressDomain, i18n, isDiscovering});
	const statusMessage = progressMessage ?? discoveryError;
	const canSubmit = value.trim().length > 0 && value.length <= NETWORK_ENDPOINT_INPUT_MAX_BYTES && !disabled;
	const placeholder = i18n._(ENTER_INSTANCE_URL_E_G_DESCRIPTOR, {exampleInstanceDomain: EXAMPLE_DOMAIN});
	const renderStatusActions = (): ReactNode => {
		if (discoveryStatus === InstanceDiscoveryStatus.IDLE) {
			return null;
		}
		return (
			<flx-auth-instance-selector-input-actions
				className={flxElementClassName(styles.inputActions)}
				data-flx="auth.flow.instance-selector.render-status-actions.input-actions"
			>
				<InstanceDiscoveryStatusIcon
					status={discoveryStatus}
					data-flx="auth.flow.instance-selector.render-status-actions.instance-discovery-status-icon"
				/>
			</flx-auth-instance-selector-input-actions>
		);
	};
	const renderInlineBackButton = (): ReactNode => {
		if (suppressInlineBackButton) {
			return null;
		}
		return (
			<Button
				type="button"
				variant={ButtonVariant.SECONDARY}
				leftIcon={
					<ArrowLeftIcon
						size={remFromPx(18)}
						weight="bold"
						data-flx="auth.flow.instance-selector.render-inline-back-button.arrow-left-icon"
					/>
				}
				onClick={handleBackToPicker}
				disabled={disabled || isDiscovering}
				data-flx="auth.flow.instance-selector.button.back"
			>
				{i18n._(BACK_DESCRIPTOR)}
			</Button>
		);
	};
	let stepContent: ReactNode;
	if (step === InstanceSelectorStep.PICK) {
		stepContent = (
			<flx-auth-instance-selector-pick-step className="flx-element" data-flx="auth.flow.instance-selector.flx-element">
				<InstancePickerRows
					disabled={disabled}
					discoveryStatus={discoveryStatus}
					instances={knownInstances}
					pinnedInstance={resolveLocalDevelopmentInstance(knownInstances)}
					onAddInstance={handleAddInstance}
					onRemoveInstance={handleRemoveKnownInstance}
					onSelectOfficial={handleSelectOfficial}
					onSelectInstance={handleSelectKnown}
					data-flx="auth.flow.instance-selector.instance-picker-rows"
				/>
				<InstanceSelectorStatusRow
					status={discoveryStatus}
					statusId={statusId}
					statusMessage={statusMessage}
					data-flx="auth.flow.instance-selector.instance-selector-status-row"
				/>
			</flx-auth-instance-selector-pick-step>
		);
	} else {
		stepContent = (
			<form className={styles.form} onSubmit={handleSubmit} data-flx="auth.flow.instance-selector.form.submit">
				<flx-auth-instance-selector-input
					className={flxElementClassName(styles.inputContainer)}
					data-flx="auth.flow.instance-selector.input-container"
				>
					<TextInput
						ref={inputRef}
						value={value}
						onChange={handleInputChange}
						maxLength={NETWORK_ENDPOINT_INPUT_MAX_BYTES}
						placeholder={placeholder}
						disabled={disabled}
						leftIcon={
							<GlobeIcon size={remFromPx(18)} weight="regular" data-flx="auth.flow.instance-selector.globe-icon" />
						}
						rightElement={renderStatusActions()}
						aria-label={i18n._(INSTANCE_URL_DESCRIPTOR)}
						{...statusDescriptionAria(statusId, progressMessage)}
						error={discoveryError ?? undefined}
						data-step-focus="true"
						data-flx="auth.flow.instance-selector.text-input"
					/>
				</flx-auth-instance-selector-input>
				<InstanceSelectorStatusRow
					status={discoveryStatus}
					statusId={statusId}
					statusMessage={progressMessage}
					data-flx="auth.flow.instance-selector.instance-selector-status-row--2"
				/>
				<flx-auth-instance-selector-footer
					className={flxElementClassName(styles.footer)}
					data-flx="auth.flow.instance-selector.footer"
				>
					{renderInlineBackButton()}
					<Button
						type="submit"
						disabled={!canSubmit}
						submitting={isDiscovering}
						rightIcon={
							<ArrowRightIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="auth.flow.instance-selector.arrow-right-icon"
							/>
						}
						data-flx="auth.flow.instance-selector.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</flx-auth-instance-selector-footer>
			</form>
		);
	}
	return (
		<flx-auth-instance-selector
			className={flxElementClassName(styles.container, className)}
			data-flx="auth.flow.instance-selector.container"
		>
			<SteppedCarousel
				step={step}
				steps={INSTANCE_SELECTOR_STEPS}
				focusOnStepChange
				ariaLabel={i18n._(INSTANCE_SELECTION_DESCRIPTOR)}
				data-flx="auth.flow.instance-selector.carousel"
			>
				{stepContent}
			</SteppedCarousel>
		</flx-auth-instance-selector>
	);
});
