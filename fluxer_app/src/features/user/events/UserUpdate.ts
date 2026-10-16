// SPDX-License-Identifier: AGPL-3.0-or-later

import {syncAccountUserData} from '@app/features/auth/state/AccountUserDataSync';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Permission from '@app/features/permissions/state/Permission';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
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
}

export function handleUserUpdate(data: UserUpdatePayload, context: GatewayHandlerContext): void {
	Users.handleUserUpdate(data as User);
	if (context.accountKey !== null && data.id === context.expectedUserId) {
		syncAccountUserData(context.accountKey, data);
	}
	Messages.handleUserUpdate({user: {id: data.id}});
	Permission.handleUserUpdate(data.id);
	QuickSwitcher.recomputeIfOpen();
	GuildVerification.handleUserUpdate();
}
