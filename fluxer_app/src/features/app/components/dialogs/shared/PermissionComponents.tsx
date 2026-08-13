// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/shared/PermissionComponents.module.css';
import PermissionLayout from '@app/features/permissions/state/PermissionLayout';
import type * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Switch as UISwitch} from '@app/features/ui/components/form/FormSwitch';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, MinusIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DENY_DESCRIPTOR = msg({
	message: 'Deny',
	comment: 'Short label in the settings dialog permission components. Keep the tone plain and specific.',
});
const NEUTRAL_INHERIT_DESCRIPTOR = msg({
	message: 'Neutral (inherit)',
	comment: 'Short label in the settings dialog permission components.',
});
const ALLOW_DESCRIPTOR = msg({
	message: 'Allow',
	comment: 'Short label in the settings dialog permission components.',
});

export type PermissionState = 'ALLOW' | 'DENY' | 'NEUTRAL';

export function getPermissionState(permission: bigint, allow: bigint, deny: bigint): PermissionState {
	if ((deny & permission) === permission) return 'DENY';
	if ((allow & permission) === permission) return 'ALLOW';
	return 'NEUTRAL';
}

export const PermissionStateButtons: React.FC<{
	currentState?: PermissionState;
	onStateChange: (state: PermissionState) => void;
	disabled: boolean;
	showActiveState?: boolean;
}> = observer(({currentState, onStateChange, disabled, showActiveState = true}) => {
	const {i18n} = useLingui();
	const getButtonClasses = (state: PermissionState) => {
		const isActive = showActiveState && currentState === state;
		const classes = [styles.stateButton, disabled ? styles.stateButtonDisabled : styles.stateButtonEnabled];
		if (isActive) {
			if (state === 'DENY') {
				classes.push(styles.stateButtonDeny);
			} else if (state === 'NEUTRAL') {
				classes.push(styles.stateButtonNeutral);
			} else {
				classes.push(styles.stateButtonAllow);
			}
		} else {
			classes.push(styles.stateButtonInactive);
		}
		return classes.join(' ');
	};
	return (
		<div
			className={styles.stateButtonsContainer}
			data-flx="app.permission-components.permission-state-buttons.state-buttons-container"
		>
			<button
				type="button"
				className={getButtonClasses('DENY')}
				onClick={() => !disabled && onStateChange('DENY')}
				disabled={disabled}
				aria-label={i18n._(DENY_DESCRIPTOR)}
				data-flx="app.permission-components.permission-state-buttons.deny.button"
			>
				<XIcon
					weight="bold"
					size={remFromPx(16)}
					data-flx="app.permission-components.permission-state-buttons.x-icon"
				/>
			</button>
			<div
				className={styles.stateDivider}
				data-flx="app.permission-components.permission-state-buttons.state-divider"
			/>
			<button
				type="button"
				className={getButtonClasses('NEUTRAL')}
				onClick={() => !disabled && onStateChange('NEUTRAL')}
				disabled={disabled}
				aria-label={i18n._(NEUTRAL_INHERIT_DESCRIPTOR)}
				data-flx="app.permission-components.permission-state-buttons.neutral.button"
			>
				<MinusIcon
					weight="bold"
					size={remFromPx(16)}
					data-flx="app.permission-components.permission-state-buttons.minus-icon"
				/>
			</button>
			<div
				className={styles.stateDivider}
				data-flx="app.permission-components.permission-state-buttons.state-divider--2"
			/>
			<button
				type="button"
				className={getButtonClasses('ALLOW')}
				onClick={() => !disabled && onStateChange('ALLOW')}
				disabled={disabled}
				aria-label={i18n._(ALLOW_DESCRIPTOR)}
				data-flx="app.permission-components.permission-state-buttons.allow.button"
			>
				<CheckIcon
					weight="bold"
					size={remFromPx(16)}
					data-flx="app.permission-components.permission-state-buttons.check-icon"
				/>
			</button>
		</div>
	);
});
export const PermissionHelpLink: React.FC<{onClick: () => void; children: React.ReactNode}> = ({onClick, children}) => {
	return (
		<button
			type="button"
			className={styles.permissionHelpLink}
			onClick={(event) => {
				event.stopPropagation();
				onClick();
			}}
			data-flx="app.permission-components.permission-help-link.permission-help-link.stop-propagation.button"
		>
			{children}
		</button>
	);
};
const PermissionOverwriteToggle: React.FC<{
	title: string;
	description?: string;
	permission: bigint;
	allow: bigint;
	deny: bigint;
	onChange: (state: PermissionState) => void;
	disabled: boolean;
	disabledReason?: string;
	warning?: string;
	extra?: React.ReactNode;
}> = observer(({title, description, permission, allow, deny, onChange, disabled, disabledReason, warning, extra}) => {
	const state = getPermissionState(permission, allow, deny);
	const buttons = (
		<PermissionStateButtons
			currentState={state}
			onStateChange={onChange}
			disabled={disabled}
			data-flx="app.permission-components.permission-overwrite-toggle.permission-state-buttons"
		/>
	);
	const buttonsTooltipTrigger = (
		<div
			className={styles.tooltipTriggerInline}
			data-flx="app.permission-components.permission-overwrite-toggle.tooltip-trigger-inline"
		>
			{buttons}
		</div>
	);
	const showDescription = PermissionLayout.isComfy;
	const tooltipText = (disabled && disabledReason) || warning;
	return (
		<div
			className={clsx(styles.overwriteToggle, PermissionLayout.isDense && styles.overwriteToggleDense)}
			data-flx="app.permission-components.permission-overwrite-toggle.overwrite-toggle"
		>
			<div
				className={styles.overwriteToggleContent}
				data-flx="app.permission-components.permission-overwrite-toggle.overwrite-toggle-content"
			>
				<div
					className={clsx(
						styles.overwriteToggleTitle,
						disabled ? styles.overwriteToggleTitleDisabled : styles.overwriteToggleTitleEnabled,
					)}
					data-flx="app.permission-components.permission-overwrite-toggle.overwrite-toggle-title"
				>
					{title}
				</div>
				{showDescription && description && (
					<p
						className={styles.overwriteToggleDescription}
						data-flx="app.permission-components.permission-overwrite-toggle.overwrite-toggle-description"
					>
						{description}
					</p>
				)}
				{extra}
			</div>
			<div
				className={styles.overwriteToggleActions}
				data-flx="app.permission-components.permission-overwrite-toggle.overwrite-toggle-actions"
			>
				{tooltipText ? (
					<Tooltip text={tooltipText} data-flx="app.permission-components.permission-overwrite-toggle.tooltip">
						{buttonsTooltipTrigger}
					</Tooltip>
				) : (
					buttons
				)}
			</div>
		</div>
	);
});
export const PermissionOverwriteCategory: React.FC<{
	spec: PermissionUtils.PermissionSpec;
	allow: bigint;
	deny: bigint;
	onPermissionChange: (permission: bigint, state: PermissionState) => void;
	disabled: boolean;
	getPermissionDisabledReason?: (permission: bigint) => string | undefined;
	getPermissionWarning?: (permission: bigint) => string | undefined;
	getPermissionExtra?: (permission: bigint, state: PermissionState) => React.ReactNode;
	isFirst?: boolean;
}> = observer(
	({
		spec,
		allow,
		deny,
		onPermissionChange,
		disabled,
		getPermissionDisabledReason,
		getPermissionWarning,
		getPermissionExtra,
		isFirst,
	}) => {
		return (
			<div
				className={styles.categoryContainer}
				data-flx="app.permission-components.permission-overwrite-category.category-container"
			>
				{!isFirst && (
					<div
						className={styles.categoryDivider}
						data-flx="app.permission-components.permission-overwrite-category.category-divider"
					/>
				)}
				<h3
					className={styles.categoryTitle}
					data-flx="app.permission-components.permission-overwrite-category.category-title"
				>
					{spec.title}
				</h3>
				<div
					className={clsx(
						styles.categoryPermissions,
						PermissionLayout.isDense && styles.categoryPermissionsDense,
						PermissionLayout.isGrid && styles.categoryPermissionsGrid,
					)}
					data-flx="app.permission-components.permission-overwrite-category.category-permissions"
				>
					{spec.permissions.map((perm) => {
						const permDisabledReason = getPermissionDisabledReason?.(perm.flag);
						const isPermDisabled = disabled || permDisabledReason !== undefined;
						const permWarning = getPermissionWarning?.(perm.flag);
						const permState = getPermissionState(perm.flag, allow, deny);
						const permExtra = getPermissionExtra?.(perm.flag, permState);
						return (
							<PermissionOverwriteToggle
								key={perm.flag.toString()}
								title={perm.title}
								description={perm.description}
								permission={perm.flag}
								allow={allow}
								deny={deny}
								onChange={(state) => onPermissionChange(perm.flag, state)}
								disabled={isPermDisabled}
								disabledReason={permDisabledReason}
								warning={permWarning}
								extra={permExtra}
								data-flx="app.permission-components.permission-overwrite-category.permission-overwrite-toggle.permission-change"
							/>
						);
					})}
				</div>
			</div>
		);
	},
);
const PermissionRoleToggle: React.FC<{
	title: string;
	description?: string;
	permission: bigint;
	rolePermissions: bigint;
	onToggle: (permission: bigint) => void;
	disabled: boolean;
	disabledReason?: string;
	warning?: string;
	extra?: React.ReactNode;
}> = observer(
	({title, description, permission, rolePermissions, onToggle, disabled, disabledReason, warning, extra}) => {
		const enabled = (rolePermissions & permission) === permission;
		const showDescription = PermissionLayout.isComfy;
		const switchEl = (
			<UISwitch
				label={title}
				description={showDescription ? description : undefined}
				value={enabled}
				onChange={() => onToggle(permission)}
				disabled={disabled}
				compact={PermissionLayout.isDense}
				data-flx="app.permission-components.permission-role-toggle.ui-switch.toggle"
			/>
		);
		const switchTooltipTrigger = (
			<div
				className={styles.tooltipTriggerBlock}
				data-flx="app.permission-components.permission-role-toggle.tooltip-trigger-block"
			>
				{switchEl}
			</div>
		);
		const tooltipText = (disabled && disabledReason) || warning;
		return (
			<div
				className={clsx(styles.roleToggle, PermissionLayout.isDense && styles.roleToggleDense)}
				data-flx="app.permission-components.permission-role-toggle.role-toggle"
			>
				{tooltipText ? (
					<Tooltip text={tooltipText} data-flx="app.permission-components.permission-role-toggle.tooltip">
						{switchTooltipTrigger}
					</Tooltip>
				) : (
					switchEl
				)}
				{extra}
			</div>
		);
	},
);
export const PermissionRoleCategory: React.FC<{
	spec: PermissionUtils.PermissionSpec;
	rolePermissions: bigint;
	onPermissionToggle: (permission: bigint) => void;
	disabled: boolean;
	getPermissionDisabledReason?: (permission: bigint) => string | undefined;
	getPermissionWarning?: (permission: bigint) => string | undefined;
	getPermissionExtra?: (permission: bigint, enabled: boolean) => React.ReactNode;
	isFirst?: boolean;
}> = observer(
	({
		spec,
		rolePermissions,
		onPermissionToggle,
		disabled,
		getPermissionDisabledReason,
		getPermissionWarning,
		getPermissionExtra,
		isFirst,
	}) => {
		return (
			<div
				className={styles.roleCategoryContainer}
				data-flx="app.permission-components.permission-role-category.role-category-container"
			>
				{!isFirst && (
					<div
						className={styles.roleCategoryDivider}
						data-flx="app.permission-components.permission-role-category.role-category-divider"
					/>
				)}
				<h3
					className={styles.roleCategoryTitle}
					data-flx="app.permission-components.permission-role-category.role-category-title"
				>
					{spec.title}
				</h3>
				<div
					className={clsx(
						styles.roleCategoryPermissions,
						PermissionLayout.isDense && styles.roleCategoryPermissionsDense,
						PermissionLayout.isGrid && styles.roleCategoryPermissionsGrid,
					)}
					data-flx="app.permission-components.permission-role-category.role-category-permissions"
				>
					{spec.permissions.map((perm) => {
						const permDisabledReason = getPermissionDisabledReason?.(perm.flag);
						const isPermDisabled = disabled || permDisabledReason !== undefined;
						const permWarning = getPermissionWarning?.(perm.flag);
						const permEnabled = (rolePermissions & perm.flag) === perm.flag;
						const permExtra = getPermissionExtra?.(perm.flag, permEnabled);
						return (
							<PermissionRoleToggle
								key={perm.flag.toString()}
								title={perm.title}
								description={perm.description}
								permission={perm.flag}
								rolePermissions={rolePermissions}
								onToggle={onPermissionToggle}
								disabled={isPermDisabled}
								disabledReason={permDisabledReason}
								warning={permWarning}
								extra={permExtra}
								data-flx="app.permission-components.permission-role-category.permission-role-toggle"
							/>
						);
					})}
				</div>
			</div>
		);
	},
);
export const getRoleColor = (color: number): string => {
	return `#${color.toString(16).padStart(6, '0')}`;
};
export const DEFAULT_ROLE_COLOR_HEX = '#99aab5';
export const sortRolesByPosition = <T extends {id: string; position: number}>(roles: Array<T>): Array<T> => {
	return roles.sort((a, b) => {
		if (b.position !== a.position) {
			return b.position - a.position;
		}
		return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
	});
};
