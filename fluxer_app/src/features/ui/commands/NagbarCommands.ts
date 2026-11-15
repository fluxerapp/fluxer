// SPDX-License-Identifier: AGPL-3.0-or-later

import Nagbar, {type NagbarToggleKey} from '@app/features/ui/state/Nagbar';

export function dismissNagbar(nagbarType: NagbarToggleKey): void {
	Nagbar.dismiss(nagbarType);
}

export function dismissInvitesDisabledNagbar(guildId: string): void {
	Nagbar.dismissInvitesDisabled(guildId);
}

export function dismissGuildMfaRequirementNagbar(guildId: string): void {
	Nagbar.dismissGuildMfaRequirement(guildId);
}

export function resetNagbar(nagbarType: NagbarToggleKey): void {
	Nagbar.reset(nagbarType);
}

export function resetAllNagbars(): void {
	Nagbar.resetAll();
}

export function setForceHideNagbar(key: NagbarToggleKey, value: boolean): void {
	Nagbar.setFlag(key, value);
}
