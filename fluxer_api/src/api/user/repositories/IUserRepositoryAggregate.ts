// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import type {IUserAuthRepository} from '@app/api/user/repositories/IUserAuthRepository';
import type {IUserChannelRepository} from '@app/api/user/repositories/IUserChannelRepository';
import type {IUserContentRepository} from '@app/api/user/repositories/IUserContentRepository';
import type {IUserRelationshipRepository} from '@app/api/user/repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '@app/api/user/repositories/IUserSettingsRepository';

export interface IUserRepositoryAggregate
	extends IUserAccountRepository,
		IUserAuthRepository,
		IUserSettingsRepository,
		IUserRelationshipRepository,
		IUserChannelRepository,
		IUserContentRepository {}
