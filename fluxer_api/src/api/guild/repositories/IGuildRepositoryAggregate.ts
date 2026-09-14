// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGuildContentRepository} from '@app/api/guild/repositories/IGuildContentRepository';
import type {IGuildDataRepository} from '@app/api/guild/repositories/IGuildDataRepository';
import type {IGuildMemberRepository} from '@app/api/guild/repositories/IGuildMemberRepository';
import type {IGuildModerationRepository} from '@app/api/guild/repositories/IGuildModerationRepository';
import type {IGuildRoleRepository} from '@app/api/guild/repositories/IGuildRoleRepository';

export interface IGuildRepositoryAggregate
	extends IGuildDataRepository,
		IGuildMemberRepository,
		IGuildRoleRepository,
		IGuildModerationRepository,
		IGuildContentRepository {}
