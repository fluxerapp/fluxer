// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import MobileLayout from '@app/features/ui/state/MobileLayout';

export function openSelfAssignableRolesPage(guildId: string): void {
	if (!MobileLayout.enabled) {
		RouterUtils.transitionTo(Routes.guildRoles(guildId));
		return;
	}
	void import('@app/features/guild/components/self_assignable_roles/SelfAssignableRolesModal').then(
		({openSelfAssignableRolesModal}) => openSelfAssignableRolesModal(guildId),
	);
}
