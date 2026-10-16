// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/api/models/Channel';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadState} from '@app/api/models/ThreadState';
import type {ThreadActorContext} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export interface AuthenticatedThread {
	state: ThreadState;
	parent: Channel;
	member: ThreadMember | null;
	parentPermissions: bigint;
	actor: ThreadActorContext;
	isModerator: boolean;
	enforceMfa: (permission: bigint) => void;
}

export interface AuthenticatedChannel {
	channel: Channel;
	guild: GuildResponse | null;
	member: GuildMemberResponse | null;
	hasPermission: (permission: bigint) => Promise<boolean>;
	checkPermission: (permission: bigint) => Promise<void>;
	thread?: AuthenticatedThread;
}
