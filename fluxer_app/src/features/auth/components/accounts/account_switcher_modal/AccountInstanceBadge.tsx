// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/components/accounts/account_switcher_modal/AccountProfilePicker.module.css';
import type {AccountInstanceBadgeInfo} from '@app/features/auth/components/accounts/account_switcher_modal/useAccountInstanceBadge';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {GlobeIcon} from '@phosphor-icons/react';
import type React from 'react';

interface AccountInstanceBadgeProps {
	readonly badge: AccountInstanceBadgeInfo | null;
	readonly fallbackLabel: string;
	readonly officialLabel: string;
}

export function AccountInstanceBadge({
	badge,
	fallbackLabel,
	officialLabel,
}: AccountInstanceBadgeProps): React.ReactElement {
	let domainLabel = fallbackLabel;
	let isOfficial = false;
	if (badge != null) {
		domainLabel = badge.title;
		isOfficial = badge.isOfficial;
	}
	const ariaLabels = [domainLabel];
	if (isOfficial) {
		ariaLabels.push(officialLabel);
	}
	const renderTooltip = () => (
		<span
			className={styles.instanceTooltip}
			data-flx="auth.accounts.account-switcher-modal.account-instance-badge.render-tooltip.instance-tooltip"
		>
			<span
				className={styles.instanceTooltipLabel}
				data-flx="auth.accounts.account-switcher-modal.account-instance-badge.render-tooltip.instance-tooltip-label"
			>
				{domainLabel}
			</span>
			{isOfficial && (
				<span
					className={styles.instanceTooltipOfficial}
					role="img"
					aria-label={officialLabel}
					data-flx="auth.accounts.account-switcher-modal.account-instance-badge.render-tooltip.instance-tooltip-official"
				>
					<VerifiedConnectionIcon
						size={14}
						data-flx="auth.accounts.account-switcher-modal.account-instance-badge.render-tooltip.verified-connection-icon"
					/>
				</span>
			)}
		</span>
	);
	return (
		<Tooltip
			text={renderTooltip}
			position="top"
			maxWidth="none"
			data-flx="auth.accounts.account-switcher-modal.account-instance-badge.tooltip"
		>
			<span
				className={styles.instanceBadge}
				role="img"
				aria-label={ariaLabels.join(', ')}
				data-flx="auth.accounts.account-switcher-modal.account-instance-badge.instance-badge"
			>
				{isOfficial ? (
					<FluxerLogoAsset data-flx="auth.accounts.account-switcher-modal.account-instance-badge.fluxer-logo-asset" />
				) : (
					<GlobeIcon
						size={remFromPx(15)}
						weight="bold"
						data-flx="auth.accounts.account-switcher-modal.account-instance-badge.globe-icon"
					/>
				)}
			</span>
		</Tooltip>
	);
}
