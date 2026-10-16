// SPDX-License-Identifier: AGPL-3.0-or-later

import {InstanceBrandMark} from '@app/features/auth/components/InstanceBrandMark';
import styles from '@app/features/auth/flow/InstanceSelector.module.css';
import {
	type InstanceInfo,
	isOfficialInstanceInfo,
	resolveInstanceLabel,
} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {InstanceDiscoveryStatus} from '@app/features/auth/flow/instance_selector/InstanceSelectorTypes';
import {resolveInstanceBrandIconUrl, resolveInstanceProductName} from '@app/features/auth/InstanceBranding';
import {findInstanceSnapshot} from '@app/features/auth/InstanceSnapshotLookup';
import {OFFICIAL_INSTANCE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {flxElementClassName} from '@app/lib/react';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {OFFICIAL_INSTANCE_DISPLAY_HOST, OFFICIAL_INSTANCE_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowRightIcon, PlusIcon, TrashIcon} from '@phosphor-icons/react';
import {type MouseEvent, type ReactNode, useCallback} from 'react';

const USE_OFFICIAL_INSTANCE_DESCRIPTOR = msg({
	message: 'Use the official Fluxer instance',
	comment: 'Accessible label for the official Fluxer instance option.',
});
const USE_INSTANCE_DESCRIPTOR = msg({
	message: 'Use {name} on {domain}',
	comment:
		'Accessible label for a known authentication instance option. Preserve {name} and {domain}. Code inserts them.',
});
const ADD_INSTANCE_DESCRIPTOR = msg({
	message: 'Add another instance',
	comment: 'Button label that opens the self-hosted instance URL step.',
});
const REMOVE_FROM_RECENT_INSTANCES_DESCRIPTOR = msg({
	message: 'Remove from recent instances',
	comment: 'Context menu item that removes a self-hosted instance from the authentication instance picker history.',
});

interface InstancePickerRowsProps {
	readonly disabled: boolean;
	readonly discoveryStatus: InstanceDiscoveryStatus;
	readonly instances: ReadonlyArray<InstanceInfo>;
	readonly pinnedInstance: InstanceInfo | null;
	readonly onAddInstance: () => void;
	readonly onRemoveInstance: (instance: InstanceInfo) => void;
	readonly onSelectOfficial: () => void;
	readonly onSelectInstance: (instance: InstanceInfo) => void;
}

export function InstancePickerRows({
	disabled,
	discoveryStatus,
	instances,
	pinnedInstance,
	onAddInstance,
	onRemoveInstance,
	onSelectOfficial,
	onSelectInstance,
}: InstancePickerRowsProps) {
	const {i18n} = useLingui();
	const isDiscovering = discoveryStatus === InstanceDiscoveryStatus.DISCOVERING;
	const handleKnownInstanceContextMenu = useCallback(
		(event: MouseEvent<HTMLButtonElement>, instance: InstanceInfo) => {
			if (disabled || isDiscovering) {
				return;
			}
			ContextMenuCommands.openFromEvent(event, () => (
				<MenuItem
					danger
					icon={
						<TrashIcon
							size={20}
							data-flx="auth.flow.instance-selector.instance-picker-rows.handle-known-instance-context-menu.trash-icon"
						/>
					}
					onClick={() => onRemoveInstance(instance)}
					data-flx="auth.flow.instance-selector.instance-picker-rows.menu-item.remove"
				>
					{i18n._(REMOVE_FROM_RECENT_INSTANCES_DESCRIPTOR)}
				</MenuItem>
			));
		},
		[disabled, i18n, isDiscovering, onRemoveInstance],
	);
	const renderKnownInstance = (instance: InstanceInfo, removable: boolean): ReactNode => {
		const snapshot = findInstanceSnapshot(instance.instanceKey);
		const storedName = instance.name !== instance.domain ? instance.name : null;
		const name = resolveInstanceLabel(storedName ?? resolveInstanceProductName(snapshot), instance.domain);
		const iconUrl = resolveInstanceBrandIconUrl(snapshot);
		return (
			<div
				key={instance.instanceKey}
				className={styles.instanceRow}
				role="listitem"
				data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-row"
			>
				<FocusRing
					offset={-2}
					data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.focus-ring"
				>
					<button
						type="button"
						className={styles.instanceOption}
						onClick={() => onSelectInstance(instance)}
						onContextMenu={removable ? (event) => handleKnownInstanceContextMenu(event, instance) : undefined}
						disabled={disabled || isDiscovering}
						aria-label={i18n._(USE_INSTANCE_DESCRIPTOR, {name, domain: instance.domain})}
						data-flx="auth.flow.instance-selector.instance-picker-rows.button.select-known"
					>
						<span
							className={styles.instanceLogo}
							data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-logo"
						>
							<InstanceBrandMark
								isOfficial={false}
								iconUrl={iconUrl}
								size={20}
								data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-brand-mark"
							/>
						</span>
						<span
							className={styles.instanceMeta}
							data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-meta"
						>
							<span
								className={styles.instanceName}
								data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-name"
							>
								{name}
							</span>
							<span
								className={styles.instanceDomain}
								data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-domain"
							>
								{instance.domain}
							</span>
						</span>
						<ArrowRightIcon
							size={remFromPx(17)}
							weight="bold"
							className={styles.instanceArrow}
							data-flx="auth.flow.instance-selector.instance-picker-rows.render-known-instance.instance-arrow"
						/>
					</button>
				</FocusRing>
			</div>
		);
	};
	return (
		<flx-auth-instance-picker-rows
			className={flxElementClassName(styles.instancePicker)}
			data-flx="auth.flow.instance-selector.instance-picker-rows.instance-picker"
		>
			<div
				className={styles.instanceGroup}
				role="list"
				data-flx="auth.flow.instance-selector.instance-picker-rows.instance-group"
			>
				<div
					className={styles.instanceRow}
					role="listitem"
					data-flx="auth.flow.instance-selector.instance-picker-rows.instance-row"
				>
					<FocusRing offset={-2} data-flx="auth.flow.instance-selector.instance-picker-rows.focus-ring">
						<button
							type="button"
							className={styles.instanceOption}
							onClick={onSelectOfficial}
							disabled={disabled || isDiscovering}
							aria-label={i18n._(USE_OFFICIAL_INSTANCE_DESCRIPTOR)}
							data-step-focus="true"
							data-flx="auth.flow.instance-selector.instance-picker-rows.button.select-official"
						>
							<span
								className={styles.instanceLogo}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-logo"
							>
								<FluxerLogoAsset
									role="img"
									aria-label={OFFICIAL_INSTANCE_NAME}
									data-flx="auth.flow.instance-selector.instance-picker-rows.img"
								/>
							</span>
							<span
								className={styles.instanceMeta}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-meta"
							>
								<span
									className={styles.instanceNameRow}
									data-flx="auth.flow.instance-selector.instance-picker-rows.instance-name-row"
								>
									<span
										className={styles.instanceName}
										data-flx="auth.flow.instance-selector.instance-picker-rows.instance-name"
									>
										{OFFICIAL_INSTANCE_NAME}
									</span>
									<span
										className={styles.instanceOfficialBadge}
										role="img"
										aria-label={i18n._(OFFICIAL_INSTANCE_DESCRIPTOR)}
										data-flx="auth.flow.instance-selector.instance-picker-rows.instance-official-badge"
									>
										<VerifiedConnectionIcon
											size={16}
											data-flx="auth.flow.instance-selector.instance-picker-rows.verified-connection-icon"
										/>
									</span>
								</span>
								<span
									className={styles.instanceDomain}
									data-flx="auth.flow.instance-selector.instance-picker-rows.instance-domain"
								>
									{OFFICIAL_INSTANCE_DISPLAY_HOST}
								</span>
							</span>
							<ArrowRightIcon
								size={remFromPx(17)}
								weight="bold"
								className={styles.instanceArrow}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-arrow"
							/>
						</button>
					</FocusRing>
				</div>
				{pinnedInstance != null && renderKnownInstance(pinnedInstance, false)}
				{instances
					.filter((instance) => !isOfficialInstanceInfo(instance))
					.map((instance) => renderKnownInstance(instance, true))}
				<div
					className={styles.instanceRow}
					role="listitem"
					data-flx="auth.flow.instance-selector.instance-picker-rows.instance-row--2"
				>
					<FocusRing offset={-2} data-flx="auth.flow.instance-selector.instance-picker-rows.focus-ring--2">
						<button
							type="button"
							className={styles.instanceOption}
							onClick={onAddInstance}
							disabled={disabled || isDiscovering}
							aria-label={i18n._(ADD_INSTANCE_DESCRIPTOR)}
							data-flx="auth.flow.instance-selector.instance-picker-rows.button.add-instance"
						>
							<span
								className={styles.instanceLogo}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-logo--2"
							>
								<PlusIcon
									size={remFromPx(20)}
									weight="bold"
									data-flx="auth.flow.instance-selector.instance-picker-rows.plus-icon"
								/>
							</span>
							<span
								className={styles.instanceMeta}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-meta--2"
							>
								<span
									className={styles.instanceName}
									data-flx="auth.flow.instance-selector.instance-picker-rows.instance-name--2"
								>
									<Trans>Add another instance</Trans>
								</span>
								<span
									className={styles.instanceDomain}
									data-flx="auth.flow.instance-selector.instance-picker-rows.instance-domain--2"
								>
									<Trans>Use a self-hosted instance URL</Trans>
								</span>
							</span>
							<ArrowRightIcon
								size={remFromPx(17)}
								weight="bold"
								className={styles.instanceArrow}
								data-flx="auth.flow.instance-selector.instance-picker-rows.instance-arrow--2"
							/>
						</button>
					</FocusRing>
				</div>
			</div>
		</flx-auth-instance-picker-rows>
	);
}
