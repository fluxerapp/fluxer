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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import type React from 'react';
import type {NagbarStore, NagbarToggleKey} from '~/stores/NagbarStore';

export type DismissibleNagbarKey = NagbarToggleKey;

export interface NagbarControlDefinition {
	key: string;
	label: React.ReactNode;
	forceKey: DismissibleNagbarKey;
	forceHideKey: NagbarToggleKey;
	resetKeys: Array<DismissibleNagbarKey>;
	status: (state: NagbarStore) => string;
	useActualDisabled?: (state: NagbarStore) => boolean;
	forceShowDisabled?: (state: NagbarStore) => boolean;
	forceHideDisabled?: (state: NagbarStore) => boolean;
}

const FORCE_ENABLED_DESCRIPTOR = msg`Force enabled`;
const FORCE_DISABLED_DESCRIPTOR = msg`Force disabled`;
const USING_ACTUAL_ACCOUNT_STATE_DESCRIPTOR = msg`Using actual account state`;
const USING_ACTUAL_VERIFICATION_STATE_DESCRIPTOR = msg`Using actual verification state`;
const CURRENTLY_DISMISSED_DESCRIPTOR = msg`Currently dismissed`;
const CURRENTLY_SHOWING_DESCRIPTOR = msg`Currently showing`;
const USING_ACTUAL_PREMIUM_STATE_DESCRIPTOR = msg`Using actual premium state`;
const USING_ACTUAL_GIFT_INVENTORY_STATE_DESCRIPTOR = msg`Using actual gift inventory state`;
const USING_ACTUAL_STATE_DESCRIPTOR = msg`Using actual state`;

export const getNagbarControls = (t: (message: MessageDescriptor) => string): Array<NagbarControlDefinition> => [
	{
		key: 'forceUnclaimedAccount',
		label: <Trans>Unclaimed Account Nagbar</Trans>,
		forceKey: 'forceUnclaimedAccount',
		forceHideKey: 'forceHideUnclaimedAccount',
		resetKeys: ['forceUnclaimedAccount'],
		status: (state) =>
			state.forceUnclaimedAccount
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHideUnclaimedAccount
					? t(FORCE_DISABLED_DESCRIPTOR)
					: t(USING_ACTUAL_ACCOUNT_STATE_DESCRIPTOR),
		useActualDisabled: (state) => !state.forceUnclaimedAccount && !state.forceHideUnclaimedAccount,
		forceShowDisabled: (state) => state.forceUnclaimedAccount,
		forceHideDisabled: (state) => state.forceHideUnclaimedAccount,
	},
	{
		key: 'forceEmailVerification',
		label: <Trans>Email Verification Nagbar</Trans>,
		forceKey: 'forceEmailVerification',
		forceHideKey: 'forceHideEmailVerification',
		resetKeys: ['forceEmailVerification'],
		status: (state) =>
			state.forceEmailVerification
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHideEmailVerification
					? t(FORCE_DISABLED_DESCRIPTOR)
					: t(USING_ACTUAL_VERIFICATION_STATE_DESCRIPTOR),
		useActualDisabled: (state) => !state.forceEmailVerification && !state.forceHideEmailVerification,
		forceShowDisabled: (state) => state.forceEmailVerification,
		forceHideDisabled: (state) => state.forceHideEmailVerification,
	},
	{
		key: 'forceDesktopNotification',
		label: <Trans>Desktop Notification Nagbar</Trans>,
		forceKey: 'forceDesktopNotification',
		forceHideKey: 'forceHideDesktopNotification',
		resetKeys: ['forceDesktopNotification', 'desktopNotificationDismissed'],
		status: (state) =>
			state.forceDesktopNotification
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHideDesktopNotification
					? t(FORCE_DISABLED_DESCRIPTOR)
					: state.desktopNotificationDismissed
						? t(CURRENTLY_DISMISSED_DESCRIPTOR)
						: t(CURRENTLY_SHOWING_DESCRIPTOR),
		useActualDisabled: (state) =>
			!state.forceDesktopNotification && !state.desktopNotificationDismissed && !state.forceHideDesktopNotification,
		forceShowDisabled: (state) => state.forceDesktopNotification,
		forceHideDisabled: (state) => state.forceHideDesktopNotification,
	},
	{
		key: 'forcePremiumGracePeriod',
		label: <Trans>Premium Grace Period Nagbar</Trans>,
		forceKey: 'forcePremiumGracePeriod',
		forceHideKey: 'forceHidePremiumGracePeriod',
		resetKeys: ['forcePremiumGracePeriod'],
		status: (state) =>
			state.forcePremiumGracePeriod
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHidePremiumGracePeriod
					? t(FORCE_DISABLED_DESCRIPTOR)
					: t(USING_ACTUAL_PREMIUM_STATE_DESCRIPTOR),
		useActualDisabled: (state) => !state.forcePremiumGracePeriod && !state.forceHidePremiumGracePeriod,
		forceShowDisabled: (state) => state.forcePremiumGracePeriod,
		forceHideDisabled: (state) => state.forceHidePremiumGracePeriod,
	},
	{
		key: 'forcePremiumExpired',
		label: <Trans>Premium Expired Nagbar</Trans>,
		forceKey: 'forcePremiumExpired',
		forceHideKey: 'forceHidePremiumExpired',
		resetKeys: ['forcePremiumExpired'],
		status: (state) =>
			state.forcePremiumExpired
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHidePremiumExpired
					? t(FORCE_DISABLED_DESCRIPTOR)
					: t(USING_ACTUAL_PREMIUM_STATE_DESCRIPTOR),
		useActualDisabled: (state) => !state.forcePremiumExpired && !state.forceHidePremiumExpired,
		forceShowDisabled: (state) => state.forcePremiumExpired,
		forceHideDisabled: (state) => state.forceHidePremiumExpired,
	},
	{
		key: 'forcePremiumOnboarding',
		label: <Trans>Premium Onboarding Nagbar</Trans>,
		forceKey: 'forcePremiumOnboarding',
		forceHideKey: 'forceHidePremiumOnboarding',
		resetKeys: ['forcePremiumOnboarding', 'premiumOnboardingDismissed'],
		status: (state) =>
			state.forcePremiumOnboarding
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHidePremiumOnboarding
					? t(FORCE_DISABLED_DESCRIPTOR)
					: state.premiumOnboardingDismissed
						? t(CURRENTLY_DISMISSED_DESCRIPTOR)
						: t(USING_ACTUAL_PREMIUM_STATE_DESCRIPTOR),
		useActualDisabled: (state) =>
			!state.forcePremiumOnboarding && !state.premiumOnboardingDismissed && !state.forceHidePremiumOnboarding,
		forceShowDisabled: (state) => state.forcePremiumOnboarding,
		forceHideDisabled: (state) => state.forceHidePremiumOnboarding,
	},
	{
		key: 'forceGiftInventory',
		label: <Trans>Gift Inventory Nagbar</Trans>,
		forceKey: 'forceGiftInventory',
		forceHideKey: 'forceHideGiftInventory',
		resetKeys: ['forceGiftInventory', 'giftInventoryDismissed'],
		status: (state) =>
			state.forceGiftInventory
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHideGiftInventory
					? t(FORCE_DISABLED_DESCRIPTOR)
					: state.giftInventoryDismissed
						? t(CURRENTLY_DISMISSED_DESCRIPTOR)
						: t(USING_ACTUAL_GIFT_INVENTORY_STATE_DESCRIPTOR),
		useActualDisabled: (state) =>
			!state.forceGiftInventory && !state.giftInventoryDismissed && !state.forceHideGiftInventory,
		forceShowDisabled: (state) => state.forceGiftInventory,
		forceHideDisabled: (state) => state.forceHideGiftInventory,
	},
	{
		key: 'forceInvitesDisabled',
		label: <Trans>Invites Disabled Nagbar</Trans>,
		forceKey: 'forceInvitesDisabled',
		forceHideKey: 'forceHideInvitesDisabled',
		resetKeys: ['forceInvitesDisabled'],
		status: (state) =>
			state.forceInvitesDisabled
				? t(FORCE_ENABLED_DESCRIPTOR)
				: state.forceHideInvitesDisabled
					? t(FORCE_DISABLED_DESCRIPTOR)
					: t(USING_ACTUAL_STATE_DESCRIPTOR),
		useActualDisabled: (state) => !state.forceInvitesDisabled && !state.forceHideInvitesDisabled,
		forceShowDisabled: (state) => state.forceInvitesDisabled,
		forceHideDisabled: (state) => state.forceHideInvitesDisabled,
	},
];
