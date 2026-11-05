// SPDX-License-Identifier: AGPL-3.0-or-later

import {isOperatorPassBenefitsRemoved} from '@app/features/premium/utils/OperatorPassPassRows';
import {getSubscriptionStatus} from '@app/features/premium/utils/SubscriptionStatus';
import type {User} from '@app/features/user/models/User';
import type {OperatorPassPassesResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

interface LoadedPremiumState {
	state: PremiumStateResponse | null;
	loadedForUserId: string | null;
}

export function loadedPremiumStateFor(premium: LoadedPremiumState, user: User | null): PremiumStateResponse | null {
	return user != null && premium.loadedForUserId === user.id ? premium.state : null;
}

export function hasSubscriptionToManage(premium: LoadedPremiumState, user: User | null): boolean {
	const state = loadedPremiumStateFor(premium, user);
	return state != null && getSubscriptionStatus(user, state).shouldShowPremiumCard;
}

export function hasPaymentHistory(premium: LoadedPremiumState, user: User | null): boolean {
	const state = loadedPremiumStateFor(premium, user);
	return state != null && (state.billing.documents.length > 0 || state.actual.has_ever_purchased);
}

export function holdsOperatorPass(
	user: Pick<User, 'operatorPassSince'> | null,
	holdings: OperatorPassPassesResponse | null,
): boolean {
	if (user?.operatorPassSince != null) return true;
	if (holdings == null) return false;
	return (
		holdings.passes.length > 0 ||
		(holdings.passed_on?.length ?? 0) > 0 ||
		holdings.incoming_offers.length > 0 ||
		holdings.visionary?.state === 'available' ||
		holdings.visionary?.state === 'ended'
	);
}

export function showsOperatorPassWallSettings(
	user: Pick<User, 'operatorPassSince'> | null,
	holdings: OperatorPassPassesResponse | null,
): boolean {
	return user?.operatorPassSince != null || (holdings != null && isOperatorPassBenefitsRemoved(holdings));
}

export function showsOperatorPassLabsSettings(
	user: Pick<User, 'operatorPassSince'> | null,
	holdings: OperatorPassPassesResponse | null,
): boolean {
	return holdings?.labs != null && user?.operatorPassSince != null;
}
