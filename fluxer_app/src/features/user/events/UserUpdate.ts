// SPDX-License-Identifier: AGPL-3.0-or-later

import {syncAccountUserData} from '@app/features/auth/state/AccountUserDataSync';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Permission from '@app/features/permissions/state/Permission';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import * as OwnProfileBadgesCommands from '@app/features/user/commands/OwnProfileBadgesCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import Users from '@app/features/user/state/Users';
import type {User} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface UserUpdatePayload {
	id: string;
	username: string;
	discriminator: string;
	global_name?: string | null;
	email?: string | null;
	avatar: string | null;
	flags: number;
	is_staff?: boolean;
	operator_pass_since?: string | null;
}

function operatorPassSinceChanged(data: UserUpdatePayload): boolean {
	const current = Users.currentUser;
	if (!current || current.id !== data.id || data.operator_pass_since === undefined) return false;
	const next = data.operator_pass_since ? new Date(data.operator_pass_since).getTime() : null;
	return (current.operatorPassSince?.getTime() ?? null) !== next;
}

export function handleUserUpdate(data: UserUpdatePayload, context: GatewayHandlerContext): void {
	const passChanged = operatorPassSinceChanged(data);
	Users.handleUserUpdate(data as User);
	if (context.accountKey !== null && data.id === context.expectedUserId) {
		syncAccountUserData(context.accountKey, data);
	}
	if (passChanged) {
		UserProfileCommands.clearCurrentUserProfiles();
		void OwnProfileBadgesCommands.refresh();
	} else if (data.id === Users.currentUser?.id) {
		OwnProfileBadgesCommands.refreshIfSettingsChanged();
	}
	Messages.handleUserUpdate({user: {id: data.id}});
	Permission.handleUserUpdate(data.id);
	QuickSwitcher.recomputeIfOpen();
	GuildVerification.handleUserUpdate();
}
