// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherTile.module.css';
import type {AccountInstanceBadgeInfo} from '@app/features/auth/components/accounts/account_switcher/useAccountInstanceBadge';
import {InstanceBrandMark} from '@app/features/auth/components/InstanceBrandMark';
import {instanceDomainHost} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {OFFICIAL_INSTANCE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

function instanceBadgeName(badge: AccountInstanceBadgeInfo): string | null {
	return badge.label !== instanceDomainHost(badge.title) ? badge.label : null;
}

interface AccountInstanceBadgeProps {
	readonly badge: AccountInstanceBadgeInfo;
}

export function AccountInstanceBadge({badge}: AccountInstanceBadgeProps): React.ReactElement {
	const {i18n} = useLingui();
	const officialLabel = i18n._(OFFICIAL_INSTANCE_DESCRIPTOR);
	const instanceName = instanceBadgeName(badge);
	const renderTooltip = () => (
		<span className={styles.instanceTooltip} data-flx="auth.accounts.account-switcher.instance-badge.tooltip">
			<span className={styles.instanceTooltipLabel} data-flx="auth.accounts.account-switcher.instance-badge.label">
				{instanceName == null ? badge.title : `${instanceName} (${badge.title})`}
			</span>
			{badge.isOfficial ? (
				<span
					className={styles.instanceTooltipOfficial}
					role="img"
					aria-label={officialLabel}
					data-flx="auth.accounts.account-switcher.instance-badge.official"
				>
					<VerifiedConnectionIcon size={14} data-flx="auth.accounts.account-switcher.instance-badge.verified-icon" />
				</span>
			) : null}
		</span>
	);
	return (
		<Tooltip
			text={renderTooltip}
			position="top"
			maxWidth="none"
			data-flx="auth.accounts.account-switcher.instance-badge.tooltip-trigger"
		>
			<span
				className={styles.instanceBadge}
				aria-hidden="true"
				data-badge-cutout=""
				data-flx="auth.accounts.account-switcher.instance-badge"
			>
				<InstanceBrandMark
					isOfficial={badge.isOfficial}
					iconUrl={badge.iconUrl}
					size={15}
					globeWeight="bold"
					data-flx="auth.accounts.account-switcher.instance-badge.brand-mark"
				/>
			</span>
		</Tooltip>
	);
}
