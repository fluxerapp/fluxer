// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '@app/api/types/HonoEnv';
import {UserAccountController} from '@app/api/user/controllers/UserAccountController';
import {UserAuthController} from '@app/api/user/controllers/UserAuthController';
import {UserChannelController} from '@app/api/user/controllers/UserChannelController';
import {UserContentController} from '@app/api/user/controllers/UserContentController';
import {UserRelationshipController} from '@app/api/user/controllers/UserRelationshipController';
import {EntranceSoundController} from '@app/api/user/entrance_sound/EntranceSoundController';
import {EntranceSoundPlayController} from '@app/api/user/entrance_sound/EntranceSoundPlayController';

export function UserController(app: HonoApp) {
	UserAccountController(app);
	UserAuthController(app);
	UserRelationshipController(app);
	UserChannelController(app);
	UserContentController(app);
	EntranceSoundController(app);
	EntranceSoundPlayController(app);
}
