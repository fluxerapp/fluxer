// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';

interface TraitBearingUser {
	traits: ReadonlyArray<string>;
}

export interface AccountSecurityCapabilities {
	canManageLocalEmail: boolean;
	canManageLocalPassword: boolean;
	canManageLocalTotp: boolean;
	canManageLocalPasskeys: boolean;
}

export function isSsoManagedUser(user: TraitBearingUser): boolean {
	return user.traits.includes('sso');
}

export function getAccountSecurityCapabilities(user: TraitBearingUser): AccountSecurityCapabilities {
	const isSso = isSsoManagedUser(user);
	const ssoEnforced = RuntimeConfig.sso?.enforced ?? false;
	const ssoDisableAdditional = RuntimeConfig.sso?.disable_additional_auth ?? false;
	const canManageLocalAuth = !isSso || (!ssoEnforced && !ssoDisableAdditional);
	return {
		canManageLocalEmail: canManageLocalAuth,
		canManageLocalPassword: canManageLocalAuth,
		canManageLocalTotp: canManageLocalAuth,
		canManageLocalPasskeys: canManageLocalAuth,
	};
}
