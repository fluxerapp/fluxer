// SPDX-License-Identifier: AGPL-3.0-or-later

import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {AccountGateActions, openAccountGateFlow} from '@app/features/user/utils/AccountGatePresentation';
import {
	AccountTiers,
	type AccountVerificationNeed,
	type PurchaseBlockedReason,
} from '@fluxer/constants/src/AccountTierConstants';
import {resolvePurchaseBlockedReason} from '@fluxer/constants/src/AccountTierRules';
import {useLingui} from '@lingui/react/macro';

type PurchaseAccountUser = Pick<User, 'reportsAccountTier' | 'accountTier' | 'verificationNeed'>;

type PurchaseAccountState =
	| {readonly kind: 'ready'}
	| {readonly kind: 'claim_inline'; readonly need: AccountVerificationNeed}
	| {readonly kind: 'blocked'; readonly reason: PurchaseBlockedReason};

export function resolvePurchaseAccountState(user: PurchaseAccountUser | null | undefined): PurchaseAccountState {
	const reason = resolvePurchaseBlockedReason({tier: user?.accountTier ?? AccountTiers.UNCLAIMED});
	if (reason === null) return {kind: 'ready'};
	const need = user?.verificationNeed ?? null;
	if (user?.reportsAccountTier === true && need !== null) {
		return {kind: 'claim_inline', need};
	}
	return {kind: 'blocked', reason};
}

interface PurchaseAccountGate {
	readonly blockedReason: PurchaseBlockedReason | null;
	readonly start: (checkout: () => void, onDismiss?: () => void) => void;
}

export function usePurchaseAccountGate(): PurchaseAccountGate {
	const {i18n} = useLingui();
	const state = resolvePurchaseAccountState(Users.currentUser);
	return {
		blockedReason: state.kind === 'blocked' ? state.reason : null,
		start: (checkout, onDismiss) => {
			switch (state.kind) {
				case 'ready':
					checkout();
					return;
				case 'claim_inline':
					openAccountGateFlow(i18n, {action: AccountGateActions.PURCHASE, need: state.need}, checkout, onDismiss);
					return;
				case 'blocked':
					return;
			}
		},
	};
}
